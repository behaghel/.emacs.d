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
(defvar-local hub/mu4e-view--last-plain-enforced nil
  "Docid last forced into plain-text rendering to avoid recursion.")
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

(with-eval-after-load 'mu4e
  ;; mu4e main buffer
  (evil-collection-define-key 'normal 'mu4e-main-mode-map
			      "ê" 'mu4e-headers-search
			      ",hh" 'mu4e-display-manual
			      "zO" 'org-msg-mode
			      "zê" 'mu4e-headers-toggle-full-search))

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
Jump    T next-unread S prev-unread
Thread  z! read-thr   zD del-thr   zà refile-thr      É mark-thread
Toggle  zé threading  zÉ include-related  zê full-search
Spam    ç spam(mark)  zS spam-from-sender
Noise   n add-noise-rule
Other   O org-capture a actions     gL log
"
	  ("ê" mu4e-headers-search)
	  ("Ê" mu4e-headers-search-edit)
	  ("à" mu4e-headers-mark-for-refile)
	  ("À" mu4e-headers-mark-for-archive)
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
	  ("S"  mu4e-headers-mark-for-spam)
	  ("zS" hub/mu4e-headers-block-and-spam)
	  ("n"  hub/mu4e-headers-add-noise-rule)
	  ("O"  mu4e-org-store-and-capture)
	  (",à" mu4e-org-store-and-capture)
	  ("a"  mu4e-headers-action)
	  ("gL" mu4e-show-log)
	  ("q" nil))
(defun hub/mu4e-headers--apply-keys ()
  "Ensure custom header bindings are present."
  (when (boundp 'mu4e-headers-mode-map)
    (define-key mu4e-headers-mode-map (kbd "à") #'hub/mu4e-headers-mark-refile)
    (evil-collection-define-key 'normal 'mu4e-headers-mode-map
				"à"  'hub/mu4e-headers-mark-refile)))
(hub/mu4e-headers--apply-keys)
(add-hook 'mu4e-headers-mode-hook #'hub/mu4e-headers--apply-keys)

(evil-collection-define-key 'normal 'mu4e-headers-mode-map
			    ;; Compose actions (single-keystroke where possible)
			    "C"  'mu4e-compose-new
			    "R"  'mu4e-compose-reply
			    "A"  'mu4e-compose-wide-reply
			    "F"  'mu4e-compose-forward
			    "O"  'mu4e-org-store-and-capture
			    "zO" 'org-msg-mode
			    ",à" 'mu4e-org-store-and-capture
			    "ê"  'mu4e-headers-search
			    "Ê"  'mu4e-headers-search-edit
			    "à"  'hub/mu4e-headers-mark-refile
			    "À"  'mu4e-headers-mark-for-archive
			    "gs" 'mu4e-headers-prev-unread
			    "gt" 'mu4e-headers-next-unread
			    "\C-t" 'hub/mu4e-headers-next-primary
			    "\C-s" 'hub/mu4e-headers-prev-primary
			    "zÉ" 'mu4e-headers-toggle-include-related
			    "zé" 'mu4e-headers-toggle-threading
			    "zê" 'mu4e-headers-toggle-full-search
			    "gL" 'mu4e-show-log
			    "%"  'mu4e-headers-mark-pattern
			    ",é"  'mu4e-headers-mark-pattern
			    "É"   'mu4e-headers-mark-thread
			    "'"   'hub/mu4e-headers-help/body
			    "SPC" nil
			    "z!" (lambda () (interactive) (mu4e-headers-mark-thread nil '(read)))
			    "zD" (lambda () (interactive) (mu4e-headers-mark-thread nil '(delete)))
			    "zà" (lambda () (interactive) (mu4e-headers-mark-thread nil '(refile)))
			    ;; Spam: single current mark; zS keeps block+pattern helper
			    "ç"  'mu4e-headers-mark-for-spam
			    "zS" 'hub/mu4e-headers-block-and-spam)

(advice-add 'mu4e-headers-mark-and-next :around #'hub/mu4e--headers-mark-and-next)

(with-eval-after-load 'mu4e-view
  ;; Free mu4e's default binding so capital T can be reused for unread navigation.
  (define-key mu4e-view-mode-map (kbd "T") nil)

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
     (when (fboundp 'mu4e-headers-mark-and-next)
       (let ((docid (mu4e-headers-mark-and-next mark)))
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
    (hub/mu4e-view--mark-and-next 'spam))

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
    (let ((bindings '(("T"    . hub/mu4e-view-next-unread)
		      ("S"    . hub/mu4e-view-prev-unread)
		      ("J"    . hub/mu4e-view-next-unread)
		      ("K"    . hub/mu4e-view-prev-unread)
		      ("\C-t" . mu4e-view-headers-next)
		      ("\C-s" . mu4e-view-headers-prev))))
      (when (boundp 'mu4e-view-mode-map)
	(dolist (binding bindings)
	  (define-key mu4e-view-mode-map (kbd (car binding)) (cdr binding))))
      (dolist (binding bindings)
	(evil-local-set-key 'normal (kbd (car binding)) (cdr binding)))
      (when (boundp 'mu4e-search-minor-mode-map)
	(let* ((existing (assoc 'mu4e-search-minor-mode minor-mode-overriding-map-alist))
	       (map (or (cdr existing) (make-sparse-keymap))))
	  (dolist (binding bindings)
	    (define-key map (kbd (car binding)) (cdr binding)))
	  (setq minor-mode-overriding-map-alist
		(assq-delete-all 'mu4e-search-minor-mode minor-mode-overriding-map-alist))
	  (push (cons 'mu4e-search-minor-mode map) minor-mode-overriding-map-alist)))))

  (add-hook 'mu4e-view-mode-hook #'hub/mu4e--apply-view-navigation-keys)

  (defun hub/mu4e-view-toggle-rendering ()
    "Toggle between the HTML and plain-text rendition of the message."
    (interactive)
    (if hub/mu4e-view--displaying-html
	(hub/mu4e-view--show-plain-text)
      (hub/mu4e-view--show-html)))

  (defun hub/mu4e-view--show-html ()
    "Render the HTML part inline and suspend plain-text wrapping."
    (let* ((shr-max-image-proportion hub/mu4e-view-max-image-proportion)
	   (html-present (cl-some (lambda (handle)
				    (and (fboundp 'mm-handle-media-type)
					 (equal (mm-handle-media-type (cdr handle)) "text/html")))
				  (and (boundp 'gnus-article-mime-handle-alist)
				       gnus-article-mime-handle-alist))))
      (if (not html-present)
	  (progn
	    (setq hub/mu4e-view--displaying-html nil)
	    (message "No HTML part; staying in plain text"))
	(setq-local shr-max-image-proportion shr-max-image-proportion)
	(condition-case err
	    (progn
	      (mu4e-view-toggle-html)
	      (setq hub/mu4e-view--displaying-html t)
	      (visual-line-mode 0)
	      (when (fboundp 'adaptive-wrap-prefix-mode)
		(adaptive-wrap-prefix-mode 0))
	      (message "Viewing HTML part (images scaled)"))
	  (error
	   (setq hub/mu4e-view--displaying-html nil)
	   (message "%s" (or (cadr err) err)))))))

  (defun hub/mu4e-view--show-plain-text ()
    "Re-render the current message as plain text with visual wrapping."
    (mu4e--view-in-headers-context
     (mu4e-headers-view-message))
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
Marks     ;m s spam     ;m r refile     ;m f flag        ;m d subthread
Jump      T next-unread S prev-unread
Noise     ;n n add rule ;n s move→spam
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
	  ("ç" hub/mu4e-view-spam-current)
	  ("S" hub/mu4e-view-prev-unread)
	  ("T" hub/mu4e-view-next-unread)
	  ("n" hub/mu4e-view-add-noise-rule)
	  ("zé" mu4e-headers-toggle-threading)
	  ("zÉ" mu4e-headers-toggle-include-related)
	  ("zê" mu4e-headers-toggle-full-search)
	  ("zp" hub/mu4e-view-add-plain-preference)
	  ("g l" hub/mu4e-view-jump-to-main-link)
	  ("a" mu4e-view-action)
	  ("gL" mu4e-show-log)
	  ("q" nil))
(define-key mu4e-view-mode-map (kbd "à") #'hub/mu4e-view-mark-refile-and-next)
(evil-collection-define-key 'normal 'mu4e-view-mode-map
			    ;; Compose convenience in view
			    "C"  'mu4e-compose-new
			    "R"  'mu4e-compose-reply
			    "A"  'mu4e-compose-wide-reply
			    "F"  'mu4e-compose-forward
			    "'"  'hub/mu4e-view-help/body
			    ;; Refile (BÉPO select/apply)
			    "à"  'hub/mu4e-view-mark-refile-and-next
			    ;; Flag/star current then load next
			    "f"  'hub/mu4e-view-mark-flag-and-next
			    ;; Next/prev message convenience
			    "\C-t" 'hub/mu4e-view-headers-next-primary
			    "\C-s" 'hub/mu4e-view-headers-prev-primary
			    ;; Spam/Block single key
			    "ç"  'hub/mu4e-view-mark-spam-and-next
			    "zz" 'hub/mu4e-view-toggle-rendering
			    "zC" 'hub/mu4e-view-toggle-readable-html
			    "zp" 'hub/mu4e-view-add-plain-preference
			    "zS" 'hub/mu4e-view-spam-current
			    "gl" 'hub/mu4e-view-jump-to-main-link)

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

(when (boundp 'mu4e-search-minor-mode-map)
  (define-key mu4e-search-minor-mode-map (kbd "à") #'hub/mu4e-search-mark-refile)
  (define-key mu4e-search-minor-mode-map (kbd "ç") #'hub/mu4e-search-mark-spam))

(with-eval-after-load 'mu4e-search
  (define-key mu4e-search-minor-mode-map (kbd "à") #'hub/mu4e-search-mark-refile)
  (define-key mu4e-search-minor-mode-map (kbd "ç") #'hub/mu4e-search-mark-spam))

(defun hub/mu4e-apply-evil-bindings ()
  "Ensure hub mu4e bindings are applied in Evil normal state."
  (when (and (boundp 'mu4e-headers-mode-map) (featurep 'evil))
    (let* ((map mu4e-headers-mode-map)
	   (gmap (or (when (keymapp (lookup-key map (kbd "g")))
		       (lookup-key map (kbd "g")))
		     (make-sparse-keymap))))
      (define-key map (kbd "g") gmap)
      (define-key gmap (kbd "L") #'mu4e-show-log))
    (evil-define-key 'normal mu4e-headers-mode-map
		     (kbd "à") #'hub/mu4e-headers-mark-refile
		     (kbd "ç") #'mu4e-headers-mark-for-spam))
  (when (and (boundp 'mu4e-view-mode-map) (featurep 'evil))
    (let* ((map mu4e-view-mode-map)
	   (gmap (or (when (keymapp (lookup-key map (kbd "g")))
		       (lookup-key map (kbd "g")))
		     (make-sparse-keymap))))
      (define-key map (kbd "g") gmap)
      (define-key gmap (kbd "l") #'hub/mu4e-view-jump-to-main-link)
      (define-key gmap (kbd "L") #'mu4e-show-log))
    (evil-define-key 'normal mu4e-view-mode-map
		     (kbd "à") #'hub/mu4e-view-mark-refile-and-next
		     (kbd "ç") #'hub/mu4e-view-mark-spam-and-next)))

(add-hook 'mu4e-headers-mode-hook #'hub/mu4e-apply-evil-bindings)
(add-hook 'mu4e-view-mode-hook #'hub/mu4e-apply-evil-bindings)
(with-eval-after-load 'mu4e (hub/mu4e-apply-evil-bindings))
(with-eval-after-load 'evil-collection-mu4e (hub/mu4e-apply-evil-bindings))
(with-eval-after-load 'evil (hub/mu4e-apply-evil-bindings))

(with-eval-after-load 'gnus-art
  ;; Keep `t`/`s` free for line motions; move part picker to `T`.
  (when (boundp 'gnus-mime-button-map)
    (define-key gnus-mime-button-map (kbd "t") #'evil-next-visual-line)
    (define-key gnus-mime-button-map (kbd "s") #'evil-previous-visual-line)
    (define-key gnus-mime-button-map (kbd "T") #'gnus-mime-view-part-as-type)))


(with-eval-after-load 'evil-collection-mu4e
  (dolist (key '("T" "S" "J" "K"))
    (define-key mu4e-view-mode-map (kbd key) nil))
  (define-key mu4e-view-mode-map (kbd "T") #'hub/mu4e-view-next-unread)
  (define-key mu4e-view-mode-map (kbd "S") #'hub/mu4e-view-prev-unread)
  (define-key mu4e-view-mode-map (kbd "J") #'hub/mu4e-view-next-unread)
  (define-key mu4e-view-mode-map (kbd "K") #'hub/mu4e-view-prev-unread))
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
