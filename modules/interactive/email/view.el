;;; view.el --- Email: mu4e viewing UX and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Keys and helpers for mu4e main and headers views.

;;; Code:

(require 'hub-utils)
(require 'hub-keys)

(require 'cl-lib)
(require 'ffap)
(require 'hydra)
(require 'rx)
(require 'evil)
(require 'subr-x)
(require 'shr nil t)
(require 'email/actions)
;; Ensure sidebar helper is available at runtime
(ignore-errors (require 'email/dashboard))


(defvar mu4e-headers-advance-after-mark)
(defvar-local hub/mu4e-view--displaying-html nil
  "Non-nil when the current mu4e view buffer shows the HTML rendition.")
(defgroup hub/mu4e-view nil
  "Helpers and UX tweaks for mu4e view buffers."
  :group 'mu4e)
(defvar hub/mu4e-view--last-plain-enforced nil
  "Docid last forced into plain-text rendering to avoid recursion.
Global (not buffer-local) so the guard survives view buffer recreation
by `mu4e-headers-view-message'.")
(defvar hub/mu4e-prefer-plain-rules nil
  "Alist of identifiers that should default to plain-text rendering in mu4e.
Each entry is a cons of the form (:list-id . \"list.id\") or
(:from . \"sender@example.com\"). List-Id matches take precedence when present.")
(defcustom hub/mu4e-prefer-plain-rules-file
  (expand-file-name "var/mu4e-prefer-plain-rules.el" user-emacs-directory)
  "File where plain-text preference rules are persisted."
  :type 'file
  :group 'hub/mu4e-view)
(defcustom hub/mu4e-view-max-image-proportion 0.9
  "Maximum inline image width as a proportion of the window for HTML view."
  :type 'number
  :group 'hub/mu4e-view)

(defun hub/mu4e-search-mark-refile ()
  "Dispatch refiling to the appropriate mu4e context."
  (interactive)
  (cond
   ((derived-mode-p 'mu4e-view-mode)
    (call-interactively #'hub/mu4e-view-mark-refile-and-next))
   ((derived-mode-p 'mu4e-headers-mode)
    (call-interactively #'hub/mu4e-headers-mark-refile))
   (t (user-error "Not in a mu4e buffer"))))

(defun hub/mu4e-search-mark-spam ()
  "Dispatch spam marking to the appropriate mu4e context."
  (interactive)
  (cond
   ((derived-mode-p 'mu4e-view-mode)
    (call-interactively #'hub/mu4e-view-mark-spam-and-next))
   ((derived-mode-p 'mu4e-headers-mode)
    (call-interactively #'mu4e-headers-mark-for-spam))
   (t (user-error "Not in a mu4e buffer"))))

(defun hub/mu4e-search-next-primary (&optional n)
  "Dispatch primary next navigation to the active mu4e context."
  (interactive "P")
  (cond
   ((derived-mode-p 'mu4e-view-mode)
    (hub/mu4e-view-headers-next-primary n))
   ((derived-mode-p 'mu4e-headers-mode)
    (hub/mu4e-headers-next-primary n))
   (t (user-error "Not in a mu4e buffer"))))

(defun hub/mu4e-search-prev-primary (&optional n)
  "Dispatch primary previous navigation to the active mu4e context."
  (interactive "P")
  (cond
   ((derived-mode-p 'mu4e-view-mode)
    (hub/mu4e-view-headers-prev-primary n))
   ((derived-mode-p 'mu4e-headers-mode)
    (hub/mu4e-headers-prev-primary n))
   (t (user-error "Not in a mu4e buffer"))))

(defun hub/mu4e-search-next-unread ()
  "Dispatch unread-next navigation to the active mu4e context."
  (interactive)
  (cond
   ((derived-mode-p 'mu4e-view-mode)
    (call-interactively #'hub/mu4e-view-next-unread))
   ((derived-mode-p 'mu4e-headers-mode)
    (call-interactively #'mu4e-headers-next-unread))
   (t (user-error "Not in a mu4e buffer"))))

(defun hub/mu4e-search-prev-unread ()
  "Dispatch unread-previous navigation to the active mu4e context."
  (interactive)
  (cond
   ((derived-mode-p 'mu4e-view-mode)
    (call-interactively #'hub/mu4e-view-prev-unread))
   ((derived-mode-p 'mu4e-headers-mode)
    (call-interactively #'mu4e-headers-prev-unread))
   (t (user-error "Not in a mu4e buffer"))))

(defconst hub/mu4e--shared-semantic-bindings
  '(("C" . mu4e-compose-new)
    ("R" . mu4e-compose-reply)
    ("A" . mu4e-compose-wide-reply)
    ("F" . mu4e-compose-forward)
    ("à" . hub/mu4e-search-mark-refile)
    ("T" . hub/mu4e-search-next-primary)
    ("S" . hub/mu4e-search-prev-primary)
    ("J" . hub/mu4e-search-next-primary)
    ("K" . hub/mu4e-search-prev-primary)
    ("!" . hub/mu4e-search-mark-spam))
  "Direct mu4e bindings that should stay semantically aligned across views.")

(defconst hub/mu4e--shared-unread-bindings
  '(("t" . hub/mu4e-search-next-unread)
    ("s" . hub/mu4e-search-prev-unread))
  "Shared mu4e unread bindings used under the `g' prefix.")

(defun hub/mu4e--apply-evil-normal-bindings (keymap bindings)
  "Apply BINDINGS to KEYMAP in Evil normal state."
  (when (featurep 'evil)
    (dolist (binding bindings)
      (evil-define-key 'normal keymap
		       (kbd (car binding)) (cdr binding)))))

(declare-function gnus-mime-view-part-as-type "gnus-mime")
(declare-function hub/mu4e-headers-next-primary "email/view")
(declare-function hub/mu4e-headers-prev-primary "email/view")

(defun hub/mu4e--headers-mark-and-next (orig mark)
  "Around advice so mark-and-next obeys smart jump rules."
  (let ((docid (funcall orig mark)))
    (when (and mu4e-headers-advance-after-mark
	       (fboundp 'hub/mu4e-headers-next-primary)
	       (or (hub/mu4e--headers-related-p)
		   (not (hub/mu4e--headers-matches-query-p))))
      (setq docid (hub/mu4e-headers-next-primary)))
    docid))

(defun hub/mu4e-headers-mark-and-advance (mark)
  "Mark the current header with MARK and jump to the next primary match.
Return the docid of the header we land on (or nil when at the end)."
  (require 'mu4e-headers)
  (require 'mu4e-mark)
  (let ((docid (mu4e-headers-mark-and-next mark)))
    (unless docid
      (setq docid (or (and (fboundp 'hub/mu4e-headers-next-primary)
			   (hub/mu4e-headers-next-primary 1))
		      (and (fboundp 'hub/mu4e-headers-prev-primary)
			   (hub/mu4e-headers-prev-primary 1)))))
    docid))

(defun hub/mu4e-headers-mark-refile (&optional raw)
  "Mark the current message for refile, advancing intelligently.
With prefix RAW, just mark without moving."
  (interactive "P")
  (require 'mu4e-headers)
  (require 'mu4e-mark)
  (if raw
      (mu4e-headers-mark-for-refile)
    (hub/mu4e-headers-mark-and-advance 'refile)))

(defun hub/mu4e--load-prefer-plain-rules ()
  "Load `hub/mu4e-prefer-plain-rules' from disk when available."
  (when (and hub/mu4e-prefer-plain-rules-file
	     (file-readable-p hub/mu4e-prefer-plain-rules-file))
    (with-temp-buffer
      (insert-file-contents hub/mu4e-prefer-plain-rules-file)
      (goto-char (point-min))
      (setq hub/mu4e-prefer-plain-rules (read (current-buffer))))))
(hub/mu4e--load-prefer-plain-rules)

(defun hub/mu4e--persist-prefer-plain-rules ()
  "Persist `hub/mu4e-prefer-plain-rules' to disk."
  (when hub/mu4e-prefer-plain-rules-file
    (let ((dir (file-name-directory hub/mu4e-prefer-plain-rules-file)))
      (when dir (make-directory dir t)))
    (with-temp-file hub/mu4e-prefer-plain-rules-file
      (insert ";; Auto-generated; prefer plain-text mu4e render rules\n")
      (prin1 hub/mu4e-prefer-plain-rules (current-buffer)))))

(defun hub/mu4e--plain-preferred-p (msg)
  "Return non-nil when MSG should render in plain text per rules."
  (let* ((lid (and (fboundp 'hub/mu4e--message-list-id)
		   (hub/mu4e--message-list-id msg)))
	 (from (and (fboundp 'hub/mu4e--sender-email)
		    (hub/mu4e--sender-email msg))))
    (cond
     ((and lid (member (cons :list-id lid) hub/mu4e-prefer-plain-rules)) t)
     ((and from (member (cons :from from) hub/mu4e-prefer-plain-rules)) t)
     (t nil))))

(defun hub/mu4e--ensure-plain-for-preference (&rest _)
  "Force plain-text rendering when the current message matches preferences."
  (when (derived-mode-p 'mu4e-view-mode)
    (let* ((msg (and (fboundp 'mu4e-message-at-point)
		     (mu4e-message-at-point)))
	   (docid (and (fboundp 'mu4e-message-field)
		       msg
		       (mu4e-message-field msg :docid))))
      (if (not (hub/mu4e--plain-preferred-p msg))
	  (setq hub/mu4e-view--last-plain-enforced nil)
	(when (and docid (not (equal docid hub/mu4e-view--last-plain-enforced)))
	  (setq hub/mu4e-view--last-plain-enforced docid)
	  (hub/mu4e-view--show-plain-text))))))

(defun hub/mu4e-view-add-plain-preference (&optional msg)
  "Add MSG's list-id (preferred) or sender to `hub/mu4e-prefer-plain-rules'."
  (interactive (list (and (fboundp 'mu4e-message-at-point)
			  (mu4e-message-at-point))))
  (let* ((msg (or msg (and (fboundp 'mu4e-message-at-point)
			   (mu4e-message-at-point))))
	 (lid (and (fboundp 'hub/mu4e--message-list-id)
		   (hub/mu4e--message-list-id msg)))
	 (from (and (fboundp 'hub/mu4e--sender-email)
		    (hub/mu4e--sender-email msg)))
	 (entry (cond (lid (cons :list-id lid))
		      (from (cons :from from))
		      (t nil))))
    (unless msg (user-error "No message at point"))
    (unless entry (user-error "No List-Id or sender found"))
    (add-to-list 'hub/mu4e-prefer-plain-rules entry t #'equal)
    (hub/mu4e--persist-prefer-plain-rules)
    (message "Prefer plain text for %s"
	     (if lid (format "list-id:%s" lid) (format "from:%s" from)))
    (hub/mu4e--ensure-plain-for-preference)))

(defun hub/copy-url-at-point-dwim (url)
  "Copy URL under point to kill-ring; consider shr and plain links.
If URL is non-nil (from `shr-url-at-point'), prefer it. Otherwise try
`ffap-url-at-point'."
  (interactive (list (when (fboundp 'shr-url-at-point)
		       (shr-url-at-point current-prefix-arg))))
  (let ((target (or url (ffap-url-at-point))))
    (if (not target)
	(message "No URL under point")
      (setq target (url-encode-url target))
      (kill-new target)
      (message "Copied %s" target))))

(defun hub/mu4e--define-keys (map bindings)
  "Bind each key in BINDINGS directly on MAP.
BINDINGS is an alist of (KEY . COMMAND) pairs accepted by `kbd' and
`define-key'."
  (dolist (binding bindings)
    (define-key map (kbd (car binding)) (cdr binding))))

(defun hub/mu4e--ensure-prefix-map (map key)
  "Ensure KEY is a prefix map in MAP and return that prefix map.
If KEY already has a direct binding, preserve it on KEY KEY."
  (let* ((key-desc (kbd key))
	 (existing (lookup-key map key-desc)))
    (if (keymapp existing)
	existing
      (let ((prefix (make-sparse-keymap)))
	(when (and existing (not (numberp existing)))
	  (define-key prefix key-desc existing))
	(define-key map key-desc prefix)
	prefix))))

(defun hub/mu4e--define-prefix-keys (map prefix bindings)
  "Bind BINDINGS beneath PREFIX in MAP.
PREFIX is preserved as a real prefix map via `hub/mu4e--ensure-prefix-map'."
  (hub/mu4e--define-keys
   (hub/mu4e--ensure-prefix-map map prefix)
   bindings))

(with-eval-after-load 'mu4e
  ;; mu4e main buffer
  (hub/mu4e--define-keys
   mu4e-main-mode-map
   '(("b" . mu4e-search-bookmark)
     ("ê" . mu4e-headers-search)))
  (hub/mu4e--define-prefix-keys
   mu4e-main-mode-map
   "z"
   '(("O" . org-msg-mode)
     ("ê" . mu4e-headers-toggle-full-search)))
  ;; Bind manual on a non-leader key; `,` must stay free for the global leader.
  (define-key mu4e-main-mode-map (kbd "H") #'mu4e-display-manual))

(with-eval-after-load 'mu4e-headers
  (defun hub/mu4e--headers-related-p (&optional pos)
    "Return non-nil when message at POS (default point) is related-only."
    (when-let* ((msg (get-text-property (or pos (point)) 'msg))
		(meta (plist-get msg :meta)))
      (plist-get meta :related)))

  (defvar hub/mu4e--search-maildirs nil
    "Maildirs that must match for the current mu4e search (nil = no restriction).")

  (defun hub/mu4e--extract-maildirs (expr)
    "Return list of positive maildirs referenced in query EXPR.
The parser intentionally keeps things simple: if EXPR contains
`or', fall back to nil to avoid false positives."
    (let (result)
      (when (and (stringp expr)
		 (not (string-match-p "\\_<or\\_>" expr)))
	(let ((case-fold-search t))
	  (with-temp-buffer
	    (insert expr)
	    (goto-char (point-min))
	    (let ((pattern (rx "maildir:"
			       (? "\"")
			       (group "/"
				      (+ (not (any ?\" ?\\ space)))))))
	      (while (re-search-forward pattern nil t)
		(let* ((match (match-string 1))
		       (start (match-beginning 0))
		       negative)
		  (save-excursion
		    (goto-char start)
		    (skip-chars-backward " \t)")
		    (setq negative (looking-back "\\(?:-\\|not\\)\\s-*" nil)))
		  (unless negative
		    (push match result)))))))
	(delete-dups result))))

  (defun hub/mu4e--update-maildirs-cache (&optional expr)
    "Update cached maildirs based on query EXPR.
Falls back to `mu4e--search-last-query' when EXPR is nil."
    (let ((query (or expr
		     (and (boundp 'mu4e--search-last-query)
			  mu4e--search-last-query))))
      (setq hub/mu4e--search-maildirs
	    (or (hub/mu4e--extract-maildirs query)
		nil))))

  (defvar mu4e-search-hook)
  (add-hook 'mu4e-search-hook #'hub/mu4e--update-maildirs-cache)
  (when (boundp 'mu4e--search-last-query)
    (hub/mu4e--update-maildirs-cache mu4e--search-last-query))

  (defun hub/mu4e--headers-matches-query-p (&optional pos)
    "Return non-nil when message at POS belongs to the active maildir filter."
    (let ((maildirs hub/mu4e--search-maildirs))
      (if (null maildirs)
	  t
	(when-let* ((msg (get-text-property (or pos (point)) 'msg))
		    (maildir (plist-get msg :maildir)))
	  (member maildir maildirs)))))

  (defun hub/mu4e--headers-move-skip-related (steps)
    "Move STEPS headers, skipping ones added via include-related."
    (let* ((direction (if (< steps 0) -1 1))
	   (remaining (max 1 (abs steps)))
	   docid)
      (catch 'end
	(while (> remaining 0)
	  (let ((candidate (mu4e~headers-move direction)))
	    (unless candidate
	      (setq docid nil)
	      (throw 'end nil))
	    (unless (or (hub/mu4e--headers-related-p)
			(not (hub/mu4e--headers-matches-query-p)))
	      (setq docid candidate)
	      (setq remaining (1- remaining))))))
      docid))

  (defun hub/mu4e-headers-next-primary (&optional n)
    "Move to the next header that matched the active search."
    (interactive "P")
    (hub/mu4e--headers-move-skip-related (prefix-numeric-value n)))

  (defun hub/mu4e-headers-prev-primary (&optional n)
    "Move to the previous header that matched the active search."
    (interactive "P")
    (hub/mu4e--headers-move-skip-related (- (prefix-numeric-value n)))))

;; headers view (bind only after mu4e-headers is available)
(defhydra hub/mu4e-headers-help (:hint nil :foreign-keys run :color blue)
	  "
mu4e Headers

Search  ê search      Ê edit
Marks   à refile      À archive    d/D trash/delete   f flag   m move   % by-pattern   u/U unmark-all   x execute
Jump    T/J next-primary  S/K prev-primary  g t/g s unread
Thread  z! read-thr   zD del-thr   zà refile-thr      É mark-thread
Toggle  zé threading  zÉ include-related  zê full-search
Spam    ! spam(mark)  zS spam-from-sender
Noise   n add-noise-rule
Other   O org-capture a actions     gL log
"
	  ("ê" mu4e-headers-search)
	  ("Ê" mu4e-headers-search-edit)
	  ("T" hub/mu4e-search-next-primary)
	  ("S" hub/mu4e-search-prev-primary)
	  ("à" hub/mu4e-search-mark-refile)
	  ("À" mu4e-headers-mark-for-archive)
	  ("J" hub/mu4e-search-next-primary)
	  ("K" hub/mu4e-search-prev-primary)
	  ("d" mu4e-headers-mark-for-trash)
	  ("D" mu4e-headers-mark-for-delete)
	  ("f" mu4e-headers-mark-for-flag)
	  ("m" mu4e-headers-mark-for-move)
	  ("%" mu4e-headers-mark-pattern)
	  ("u" mu4e-headers-mark-for-unmark)
	  ("U" mu4e-mark-unmark-all)
	  ("x" mu4e-mark-execute-all)
	  ("É" mu4e-headers-mark-thread)
	  ("z!" (lambda () (interactive) (mu4e-headers-mark-thread nil '(read))))
	  ("zD" (lambda () (interactive) (mu4e-headers-mark-thread nil '(delete))))
	  ("zà" (lambda () (interactive) (mu4e-headers-mark-thread nil '(refile))))
	  ("zé" mu4e-headers-toggle-threading)
	  ("zÉ" mu4e-headers-toggle-include-related)
	  ("zê" mu4e-headers-toggle-full-search)
	  ("!"  hub/mu4e-search-mark-spam)
	  ("zS" hub/mu4e-headers-block-sender-and-spam)
	  ("n"  hub/mu4e-headers-add-noise-rule)
	  ("O"  mu4e-org-store-and-capture)
	  (",à" mu4e-org-store-and-capture)
	  ("a"  mu4e-headers-action)
	  ("g t" hub/mu4e-search-next-unread)
	  ("g s" hub/mu4e-search-prev-unread)
	  ("gL" mu4e-show-log)
	  ("q" nil))
(defun hub/mu4e-headers--apply-keys ()
  "Ensure custom header bindings are present."
  (when (boundp 'mu4e-headers-mode-map)
    (hub/mu4e--define-keys
     mu4e-headers-mode-map
     (append hub/mu4e--shared-semantic-bindings
	     '(("O" . mu4e-org-store-and-capture)
	       ("ê" . mu4e-headers-search)
	       ("Ê" . mu4e-headers-search-edit)
	       ("À" . mu4e-headers-mark-for-archive)
	       ("C-t" . hub/mu4e-headers-next-primary)
	       ("C-s" . hub/mu4e-headers-prev-primary)
	       ("%" . mu4e-headers-mark-pattern)
	       ("É" . mu4e-headers-mark-thread)
	       ("'" . hub/mu4e-headers-help/body)
	       ("SPC" . nil)
	       ("b" . mu4e-search-bookmark)
	       ("," . nil))))
    ;; mu4e now runs in Evil normal state again, so mirror the shared
    ;; semantic bindings there as well instead of re-stating them per mode.
    (hub/mu4e--apply-evil-normal-bindings
     mu4e-headers-mode-map
     (append hub/mu4e--shared-semantic-bindings
	     '(("g t" . hub/mu4e-search-next-unread)
	       ("g s" . hub/mu4e-search-prev-unread))))
    (hub/mu4e--define-prefix-keys
     mu4e-headers-mode-map
     "z"
     '(("O" . org-msg-mode)
       ("É" . mu4e-headers-toggle-include-related)
       ("é" . mu4e-headers-toggle-threading)
       ("ê" . mu4e-headers-toggle-full-search)
       ("!" . (lambda () (interactive) (mu4e-headers-mark-thread nil '(read))))
       ("D" . (lambda () (interactive) (mu4e-headers-mark-thread nil '(delete))))
       ("à" . (lambda () (interactive) (mu4e-headers-mark-thread nil '(refile))))
       ("S" . hub/mu4e-headers-block-sender-and-spam)))
    (let ((g-map (hub/mu4e--ensure-prefix-map mu4e-headers-mode-map "g")))
      (dolist (binding hub/mu4e--shared-unread-bindings)
	(define-key g-map (kbd (car binding)) (cdr binding)))
      (define-key g-map (kbd "L") #'mu4e-show-log))))
(hub/mu4e-headers--apply-keys)
(add-hook 'mu4e-headers-mode-hook #'hub/mu4e-headers--apply-keys)

(advice-add 'mu4e-headers-mark-and-next :around #'hub/mu4e--headers-mark-and-next)

(with-eval-after-load 'mu4e-view
  ;; Reclaim top-level primary navigation as soon as mu4e installs its view
  ;; map; later helpers reassert the same semantics after live view updates.
  (define-key mu4e-view-mode-map (kbd "T") #'hub/mu4e-search-next-primary)
  (define-key mu4e-view-mode-map (kbd "S") #'hub/mu4e-search-prev-primary)
  (define-key mu4e-view-mode-map (kbd "J") #'hub/mu4e-search-next-primary)
  (define-key mu4e-view-mode-map (kbd "K") #'hub/mu4e-search-prev-primary)

  (defun hub/mu4e-view--reset-view-state (&rest _)
    "Ensure plain-text wrapping is active for freshly rendered messages."
    (setq hub/mu4e-view--displaying-html nil)
    (hub/email-enable-visual-wrap))

  (add-hook 'mu4e-view-rendered-hook #'hub/mu4e-view--reset-view-state)
  (add-hook 'mu4e-view-rendered-hook #'hub/mu4e--ensure-plain-for-preference)

  (defun hub/mu4e-view-headers-next-primary (&optional n)
    "In view buffers, jump to the next header matching the search."
    (interactive "P")
    (mu4e--view-in-headers-context
     (unless (fboundp 'hub/mu4e-headers-next-primary)
       (require 'mu4e-headers nil t))
     (if (fboundp 'hub/mu4e-headers-next-primary)
	 (hub/mu4e-headers-next-primary (prefix-numeric-value n))
       (mu4e-view-headers-next (prefix-numeric-value n)))))

  (defun hub/mu4e-view-headers-prev-primary (&optional n)
    "In view buffers, jump to the previous header matching the search."
    (interactive "P")
    (mu4e--view-in-headers-context
     (unless (fboundp 'hub/mu4e-headers-prev-primary)
       (require 'mu4e-headers nil t))
     (if (fboundp 'hub/mu4e-headers-prev-primary)
	 (hub/mu4e-headers-prev-primary (prefix-numeric-value n))
       (mu4e-view-headers-prev (prefix-numeric-value n)))))

  (add-hook 'mu4e-view-mode-hook #'hub/email-enable-visual-wrap)

  ;; Helpers: mark in headers and advance view
  (defun hub/mu4e-view--mark-and-next (mark)
    (interactive)
    (require 'mu4e-mark)
    (mu4e--view-in-headers-context
     (when (fboundp 'hub/mu4e-headers-mark-and-advance)
       (let ((docid (hub/mu4e-headers-mark-and-advance mark)))
	 (unless docid
	   (setq docid (or (hub/mu4e-headers-next-primary 1)
			   (hub/mu4e-headers-prev-primary 1))))
	 (when docid
	   (mu4e-headers-view-message))
	 docid))))

  (defun hub/mu4e-view-mark-refile-and-next ()
    (interactive)
    ;; Use mu4e's view helper directly to avoid headers-mode assertions.
    (let ((mu4e-view-advance-after-mark t)
	  (mu4e-headers-advance-after-mark t))
      (mu4e-view-mark-for-refile)))

  (defun hub/mu4e-view-mark-spam-and-next ()
    (interactive)
    (let ((mu4e-view-advance-after-mark t)
	  (mu4e-headers-advance-after-mark t))
      (hub/mu4e-view--mark-and-next 'spam)))

  (defun hub/mu4e-view-mark-flag-and-next ()
    (interactive)
    (hub/mu4e-view--mark-and-next 'flag))

  (defun hub/mu4e-view-next-unread ()
    (interactive)
    (mu4e--view-in-headers-context
     (when-let ((docid (mu4e-headers-next-unread)))
       (mu4e-headers-view-message)
       docid)))

  (defun hub/mu4e-view-prev-unread ()
    (interactive)
    (mu4e--view-in-headers-context
     (when-let ((docid (mu4e-headers-prev-unread)))
       (mu4e-headers-view-message)
       docid)))

  (defun hub/mu4e--apply-view-navigation-keys ()
    (let ((bindings (append
		     (cl-remove-if-not
		      (lambda (binding)
			(member (car binding) '("T" "S" "J" "K")))
		      hub/mu4e--shared-semantic-bindings)
		     '(("\C-t" . mu4e-view-headers-next)
		       ("\C-s" . mu4e-view-headers-prev)))))
      (when (boundp 'mu4e-view-mode-map)
	(dolist (binding bindings)
	  (define-key mu4e-view-mode-map (kbd (car binding)) (cdr binding))))
      (hub/mu4e--apply-evil-normal-bindings
       mu4e-view-mode-map
       (append (cl-remove-if-not
		(lambda (binding)
		  (member (car binding) '("T" "S" "J" "K")))
		hub/mu4e--shared-semantic-bindings)
	       '(("g t" . hub/mu4e-search-next-unread)
		 ("g s" . hub/mu4e-search-prev-unread)
		 ("C-t" . mu4e-view-headers-next)
		 ("C-s" . mu4e-view-headers-prev))))
      (when (boundp 'mu4e-search-minor-mode-map)
	(let* ((existing (assoc 'mu4e-search-minor-mode minor-mode-overriding-map-alist))
	       (map (or (cdr existing) (make-sparse-keymap))))
	  (dolist (binding bindings)
	    (define-key map (kbd (car binding)) (cdr binding)))
	  (setq minor-mode-overriding-map-alist
		(assq-delete-all 'mu4e-search-minor-mode minor-mode-overriding-map-alist))
	  (push (cons 'mu4e-search-minor-mode map) minor-mode-overriding-map-alist)))))

  (hub/mu4e--apply-view-navigation-keys)
  (add-hook 'mu4e-view-mode-hook #'hub/mu4e--apply-view-navigation-keys)
  (add-hook 'mu4e-view-rendered-hook #'hub/mu4e--apply-view-navigation-keys)

  (defun hub/mu4e-view--press-mime-button (mime-type)
    "Simulate clicking the MIME button for MIME-TYPE.
Walks the buffer for `gnus-data' text properties, finds the handle
matching MIME-TYPE, and funcalls its `gnus-callback' — the same
code path as `gnus-article-press-button' on user click."
    (save-excursion
      (goto-char (point-min))
      (let (found)
	(while (and (not found) (not (eobp)))
	  (let ((data (get-text-property (point) 'gnus-data))
		(callback (get-text-property (point) 'gnus-callback)))
	    (when (and data callback
		       (fboundp 'mm-handle-media-type)
		       (equal (mm-handle-media-type data) mime-type))
	      (funcall callback data)
	      (setq found t)))
	  (unless found
	    (goto-char (or (next-single-property-change (point) 'gnus-data)
			   (point-max)))))
	found)))

  (defun hub/mu4e-view-toggle-rendering ()
    "Toggle between the HTML and plain-text rendition of the message."
    (interactive)
    (let ((target (if hub/mu4e-view--displaying-html "text/plain" "text/html")))
      (if (hub/mu4e-view--press-mime-button target)
	  (progn
	    (when (fboundp 'gnus-article-hide-headers)
	      (gnus-article-hide-headers))
	    (setq hub/mu4e-view--displaying-html
		  (not hub/mu4e-view--displaying-html))
	    (if hub/mu4e-view--displaying-html
		(progn
		  (setq-local shr-max-image-proportion
			      hub/mu4e-view-max-image-proportion)
		  (visual-line-mode 0)
		  (when (fboundp 'adaptive-wrap-prefix-mode)
		    (adaptive-wrap-prefix-mode 0))
		  (message "Viewing HTML part"))
	      (hub/email-enable-visual-wrap)
	      (message "Viewing plain text part")))
	(message "No %s part in this message" target))))

  (defun hub/mu4e-view--show-html ()
    "Switch the current view to the HTML rendition."
    (if (hub/mu4e-view--press-mime-button "text/html")
	(progn
	  (setq hub/mu4e-view--displaying-html t)
	  (setq-local shr-max-image-proportion
		      hub/mu4e-view-max-image-proportion)
	  (visual-line-mode 0)
	  (when (fboundp 'adaptive-wrap-prefix-mode)
	    (adaptive-wrap-prefix-mode 0))
	  (message "Viewing HTML part (images scaled)"))
      (setq hub/mu4e-view--displaying-html nil)
      (message "No HTML part; staying in plain text")))

  (defun hub/mu4e-view--show-plain-text ()
    "Switch the current view to the plain-text rendition."
    (hub/mu4e-view--press-mime-button "text/plain")
    (when (fboundp 'gnus-article-hide-headers)
      (gnus-article-hide-headers))
    (setq hub/mu4e-view--displaying-html nil)
    (hub/email-enable-visual-wrap)
    (message "Viewing plain text part"))

  (defun hub/mu4e-view--link-all-caps-p (text)
    "Return non-nil when TEXT contains no lowercase letters and at least one uppercase."
    (let ((str (string-trim text)))
      (and (> (length str) 0)
	   (string-match-p "[A-Z]" str)
	   (not (string-match-p "[a-z]" str)))))

  (defun hub/mu4e-view--collect-links ()
    "Collect link spans in the current mu4e view buffer with context."
    (let (links)
      (dolist (prop '(mu4e-url shr-url))
	(save-excursion
	  (goto-char (point-min))
	  (while-let ((match (text-property-search-forward prop nil t)))
	    (let* ((start (prop-match-beginning match))
		   (end (prop-match-end match))
		   (url (prop-match-value match))
		   (line-start (line-beginning-position))
		   (line-end (line-end-position))
		   (line (buffer-substring-no-properties line-start line-end))
		   (label (save-excursion
			    (goto-char start)
			    (skip-chars-backward "[:alnum:]-_")
			    (let* ((lab-start (point))
				   (lab-end start)
				   (raw (buffer-substring-no-properties lab-start lab-end))
				   (trimmed (string-trim raw)))
			      (when (> (length trimmed) 0) trimmed)))))
	      (push (list :start start
			  :end end
			  :url url
			  :text (buffer-substring-no-properties start end)
			  :label label
			  :line line)
		    links)))))
      (setq links (cl-delete-duplicates
		   links
		   :from-end t
		   :test (lambda (a b)
			   (and (= (plist-get a :start) (plist-get b :start))
				(= (plist-get a :end) (plist-get b :end))))))
      (cl-sort links #'< :key (lambda (l) (plist-get l :start)))))

  (defun hub/mu4e-view--main-link-candidate ()
    "Pick the \"main\" link based on uppercase or positional heuristics."
    (let ((links (hub/mu4e-view--collect-links)))
      (when links
	(let* ((caps (cl-remove-if-not
		      (lambda (l)
			(or (hub/mu4e-view--link-all-caps-p (plist-get l :text))
			    (hub/mu4e-view--link-all-caps-p (or (plist-get l :label) ""))))
		      links))
	       (hint (cl-find-if
		      (lambda (l)
			(let* ((text (downcase (plist-get l :text)))
			       (label (downcase (or (plist-get l :label) ""))))
			  (or (string-match-p "\\bopen\\b" text)
			      (string-match-p "\\brequest\\b" text)
			      (string-match-p "\\bopen\\b" label)
			      (string-match-p "\\brequest\\b" label))))
		      links))
	       (mid-candidate
		(let* ((buflen (max 1 (- (point-max) (point-min))))
		       (mid (+ (point-min) (/ buflen 2.0)))
		       (low (+ (point-min) (* 0.30 buflen)))
		       (high (+ (point-min) (* 0.70 buflen)))
		       (spacing-threshold 120)
		       (best nil)
		       (best-dist nil))
		  (cl-loop for link in links
			   for idx from 0
			   for start = (plist-get link :start)
			   for end = (plist-get link :end)
			   for center = (/ (+ start end) 2.0)
			   for prev = (nth (1- idx) links)
			   for next = (nth (1+ idx) links)
			   for gap-prev = (if prev (- start (plist-get prev :end)) most-positive-fixnum)
			   for gap-next = (if next (- (plist-get next :start) end) most-positive-fixnum)
			   for gap = (min gap-prev gap-next)
			   when (and (>= center low) (<= center high) (> gap spacing-threshold))
			   do (let ((dist (abs (- center mid))))
				(when (or (null best-dist) (< dist best-dist))
				  (setq best link)
				  (setq best-dist dist))))
		  best)))
	  (or (and (= (length caps) 1) (car caps))
	      hint
	      mid-candidate
	      (car (last links)))))))

  (defvar-local hub/mu4e-view--readable-html-state nil
    "Internal storage for readable-html toggle (shr colors/luminance).")

  (defun hub/mu4e-view-toggle-readable-html ()
    "Toggle high-contrast HTML rendering for the current message."
    (interactive)
    (let* ((state hub/mu4e-view--readable-html-state)
	   (orig-use (or (car state) shr-use-colors))
	   (orig-lum (or (cadr state) shr-color-visible-luminance-min)))
      (if hub/mu4e-view--readable-html-state
	  (progn
	    (setq shr-use-colors orig-use
		  shr-color-visible-luminance-min orig-lum
		  hub/mu4e-view--readable-html-state nil)
	    (hub/mu4e-view--show-plain-text)
	    (hub/mu4e-view--show-html)
	    (message "Readable HTML off"))
	(setq hub/mu4e-view--readable-html-state (list shr-use-colors shr-color-visible-luminance-min))
	(setq shr-use-colors nil
	      shr-color-visible-luminance-min 0.6)
	(hub/mu4e-view--show-plain-text)
	(hub/mu4e-view--show-html)
	(message "Readable HTML on"))))

  (defun hub/mu4e-view-jump-to-main-link ()
    "Move point to the most likely primary link in the current message."
    (interactive)
    (if-let ((link (hub/mu4e-view--main-link-candidate)))
	(progn
	  (goto-char (plist-get link :start))
	  (message "Jumped to main link: %s" (or (plist-get link :url) "")))
      (user-error "No suitable link found"))))

(defhydra hub/mu4e-view-help (:hint nil :foreign-keys run :color blue)
	  "
	mu4e Message View

Compose   ;c n new      ;c r reply      ;c a reply-all   ;c f forward
Attach    ;f s save     ;f a part action
Marks     ! spam        ;m s spam       ;m r refile      ;m f flag        ;m d subthread
Jump      T/J next-primary  S/K prev-primary  C-t/C-s next/prev msg  g t/g s unread
Noise     zS move→spam  ;n n add rule   ;n s move→spam
Open      ;o b browser  ;o u visit URL  ;o f fetch URL   ;o s save URL
Toggle    ;t t threads  ;t r related    ;t f full search ;t h html/text
Plain     zp prefer txt ;t p prefer txt (persist)  zz toggle html/text  zC readable-html
Links     g l main link
Actions   ;a a message  ;a m mime part  ;y u copy URL
"
	  ("C" mu4e-compose-new)
	  ("R" mu4e-compose-reply)
	  ("A" mu4e-compose-wide-reply)
	  ("F" mu4e-compose-forward)
	  ("!" hub/mu4e-search-mark-spam)
	  ("T" hub/mu4e-search-next-primary)
	  ("S" hub/mu4e-search-prev-primary)
	  ("J" hub/mu4e-search-next-primary)
	  ("K" hub/mu4e-search-prev-primary)
	  ("C-t" mu4e-view-headers-next)
	  ("C-s" mu4e-view-headers-prev)
	  ("n" hub/mu4e-view-add-noise-rule)
	  ("zé" mu4e-headers-toggle-threading)
	  ("zÉ" mu4e-headers-toggle-include-related)
	  ("zê" mu4e-headers-toggle-full-search)
	  ("zp" hub/mu4e-view-add-plain-preference)
	  ("g t" hub/mu4e-search-next-unread)
	  ("g s" hub/mu4e-search-prev-unread)
	  ("g l" hub/mu4e-view-jump-to-main-link)
	  ("a" mu4e-view-action)
	  ("gL" mu4e-show-log)
	  ("q" nil))

(defun hub/mu4e-view--apply-keys ()
  "Ensure custom view bindings are present."
  (when (boundp 'mu4e-view-mode-map)
    (hub/mu4e--define-keys
     mu4e-view-mode-map
     (append hub/mu4e--shared-semantic-bindings
	     '(("'" . hub/mu4e-view-help/body)
	       ("f" . hub/mu4e-view-mark-flag-and-next)
	       ("," . nil))))
    (hub/mu4e--apply-evil-normal-bindings
     mu4e-view-mode-map
     (append hub/mu4e--shared-semantic-bindings
	     '(("g t" . hub/mu4e-search-next-unread)
	       ("g s" . hub/mu4e-search-prev-unread))))
    (hub/mu4e--define-prefix-keys
     mu4e-view-mode-map
     "z"
     '(("z" . hub/mu4e-view-toggle-rendering)
       ("C" . hub/mu4e-view-toggle-readable-html)
       ("p" . hub/mu4e-view-add-plain-preference)
       ("S" . hub/mu4e-view-move-to-spam)))
    (let ((g-map (hub/mu4e--ensure-prefix-map mu4e-view-mode-map "g")))
      (dolist (binding hub/mu4e--shared-unread-bindings)
	(define-key g-map (kbd (car binding)) (cdr binding)))
      (define-key g-map (kbd "l") #'hub/mu4e-view-jump-to-main-link)
      (define-key g-map (kbd "L") #'mu4e-show-log))))

(hub/mu4e-view--apply-keys)
(add-hook 'mu4e-view-mode-hook #'hub/mu4e-view--apply-keys)

(with-eval-after-load 'general
  (hub/define-leaders)
  (hub/localleader
   :states '(normal visual)
   :keymaps 'mu4e-view-mode-map
   "c"  '(:ignore t :which-key "compose")
   "c n" #'mu4e-compose-new
   "c r" #'mu4e-compose-reply
   "c a" #'mu4e-compose-wide-reply
   "c f" #'mu4e-compose-forward
   "c s" #'mu4e-compose-supersede

   "f"  '(:ignore t :which-key "attachments")
   "f a" #'mu4e-view-mime-part-action
   "f s" #'mu4e-view-save-attachments

   "m"  '(:ignore t :which-key "mark/move")
   "m s" #'hub/mu4e-view-mark-spam-and-next
   "m r" #'hub/mu4e-view-mark-refile-and-next
   "m f" #'hub/mu4e-view-mark-flag-and-next
   "m d" #'mu4e-view-mark-subthread
   "m D" #'mu4e-view-mark-thread

   "t"  '(:ignore t :which-key "toggles")
   "t t" #'mu4e-headers-toggle-threading
   "t r" #'mu4e-headers-toggle-include-related
   "t f" #'mu4e-headers-toggle-full-search
   "t h" #'hub/mu4e-view-toggle-rendering
   "t p" #'hub/mu4e-view-add-plain-preference

   "n"  '(:ignore t :which-key "noise/spam")
   "n n" #'hub/mu4e-view-add-noise-rule
   "n s" #'hub/mu4e-view-move-to-spam

   "o"  '(:ignore t :which-key "open/visit")
   "o b" #'mu4e-action-view-in-browser
   "o u" #'mu4e-view-go-to-url
   "o f" #'mu4e-view-fetch-url
   "o s" #'mu4e-view-save-url

   "a"  '(:ignore t :which-key "actions")
   "a a" #'mu4e-view-action
   "a m" #'mu4e-view-mime-part-action

   "y"  '(:ignore t :which-key "yank/copy")
   "y u" #'hub/copy-url-at-point-dwim))

(with-eval-after-load 'mu4e-thread
  ;; Auto-fold every thread when mu4e enables thread-mode after a search.
  ;; This uses mu4e's built-in folding support (>= 1.10) for stability.
  (add-hook 'mu4e-thread-mode-hook #'mu4e-thread-fold-all))


(with-eval-after-load 'gnus-art
  ;; Keep `t`/`s` for line motions and leave top-level `T` available for
  ;; mu4e primary navigation even on MIME buttons.
  (when (boundp 'gnus-mime-button-map)
    (define-key gnus-mime-button-map (kbd "t") #'evil-next-visual-line)
    (define-key gnus-mime-button-map (kbd "s") #'evil-previous-visual-line)
    (define-key gnus-mime-button-map (kbd "T") nil)
    (define-key gnus-mime-button-map (kbd "V") #'gnus-mime-view-part-as-type)))


(with-eval-after-load 'evil-collection-mu4e
  (when (fboundp 'hub/mu4e-headers--apply-keys)
    (hub/mu4e-headers--apply-keys))
  (when (fboundp 'hub/mu4e-view--apply-keys)
    (hub/mu4e-view--apply-keys))
  (when (fboundp 'hub/mu4e--apply-view-navigation-keys)
    (hub/mu4e--apply-view-navigation-keys)))
;; --- Deep-linking ------------------------------------------------------------

(defun hub/mu4e-open-message-by-id (msgid)
  "Open mu4e in the mails perspective and jump to message MSGID.
Falls back to a headers search if needed; ensures sidebar is visible."
  (interactive "sMessage-Id: ")
  (require 'mu4e)
  ;; Switch to the mail perspective if available
  (when (fboundp 'persp-switch)
    (persp-switch "mails"))
  ;; Ensure mu4e is running and the sidebar is present
  (mu4e t)
  (when (fboundp 'hub/mu4e-ensure-dashboard-sidebar)
    (hub/mu4e-ensure-dashboard-sidebar))
  (let ((id (string-trim msgid)))
    (condition-case _
	(mu4e-view-message-with-msgid id)
      (error
       (mu4e-headers-search (format "msgid:%s" id)))))
  (when (fboundp 'select-frame-set-input-focus)
    (select-frame-set-input-focus (selected-frame))))

(provide 'email/view)
;;; view.el ends here
