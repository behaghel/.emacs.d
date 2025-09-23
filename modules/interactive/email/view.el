;;; view.el --- Email: mu4e viewing UX and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Keys and helpers for mu4e main and headers views.

;;; Code:

(require 'hub-utils)
(require 'hub-keys)

(require 'ffap)
(require 'hydra)
;; Ensure sidebar helper is available at runtime
(ignore-errors (require 'email/dashboard))

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

  (defun hub/mu4e--headers-move-skip-related (steps)
    "Move STEPS headers, skipping ones added via include-related."
    (let* ((direction (if (< steps 0) -1 1))
	   (remaining (max 1 (abs steps)))
	   (move-fn (if (> direction 0) #'mu4e-headers-next #'mu4e-headers-prev))
	   docid)
      (catch 'end
	(while (> remaining 0)
	  (let ((candidate (funcall move-fn)))
	    (unless candidate
	      (setq docid nil)
	      (throw 'end nil))
	    (unless (hub/mu4e--headers-related-p)
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
    (hub/mu4e--headers-move-skip-related (- (prefix-numeric-value n))))

  ;; headers view (bind only after mu4e-headers is available)
  (defhydra hub/mu4e-headers-help (:hint nil :foreign-keys run :color blue)
	    "
mu4e Headers

Search  ê search      Ê edit
Marks   à refile      À archive    d/D trash/delete   f flag   m move   % by-pattern   u/U unmark/all   x execute
Thread  z! read-thr   zD del-thr   zà refile-thr      É mark-thread
Toggle  zé threading  zÉ include-related  zê full-search
Spam    S spam(mark)  zS spam-from-sender
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
			      "à"  'mu4e-headers-mark-for-refile
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
			      "S"  'mu4e-headers-mark-for-spam
			      "zS" 'hub/mu4e-headers-block-and-spam))

(with-eval-after-load 'mu4e-view
  (defun hub/mu4e-view-headers-next-primary (&optional n)
    "In view buffers, jump to the next header matching the search."
    (interactive "P")
    (mu4e--view-in-headers-context
     (hub/mu4e-headers-next-primary (prefix-numeric-value n))))

  (defun hub/mu4e-view-headers-prev-primary (&optional n)
    "In view buffers, jump to the previous header matching the search."
    (interactive "P")
    (mu4e--view-in-headers-context
     (hub/mu4e-headers-prev-primary (prefix-numeric-value n))))

  (add-hook 'mu4e-view-mode-hook #'hub/email-enable-visual-wrap)

  ;; Helpers: mark in headers and advance view
  (defun hub/mu4e-view--mark-and-next (mark)
    (interactive)
    (require 'mu4e-mark)
    (let ((hdr (when (fboundp 'mu4e-get-headers-buffer) (mu4e-get-headers-buffer))))
      (when (buffer-live-p hdr)
	(with-current-buffer hdr
	  (when (fboundp 'mu4e-headers-mark-and-next)
	    (mu4e-headers-mark-and-next mark))))
      (when (fboundp 'mu4e-view-headers-next)
	(mu4e-view-headers-next))))
  (defun hub/mu4e-view-mark-refile-and-next ()
    (interactive)
    (hub/mu4e-view--mark-and-next 'refile))
  (defun hub/mu4e-view-mark-spam-and-next ()
    (interactive)
    (hub/mu4e-view--mark-and-next 'spam))
  (defun hub/mu4e-view-mark-flag-and-next ()
    (interactive)
    (hub/mu4e-view--mark-and-next 'flag))
  (defhydra hub/mu4e-view-help (:hint nil :foreign-keys run :color blue)
	    "
mu4e Message View

Compose   ;c n new      ;c r reply      ;c a reply-all   ;c f forward
Attach    ;f s save     ;f a part action
Marks     ;m s spam     ;m r refile     ;m f flag        ;m d subthread
Noise     ;n n add rule ;n s move→spam
Open      ;o b browser  ;o u visit URL  ;o f fetch URL   ;o s save URL
Toggle    ;t t threads  ;t r related    ;t f full search
Actions   ;a a message  ;a m mime part  ;y u copy URL
"
	    ("C" mu4e-compose-new)
	    ("R" mu4e-compose-reply)
	    ("A" mu4e-compose-wide-reply)
	    ("F" mu4e-compose-forward)
	    ("S" hub/mu4e-view-spam-current)
	    ("n" hub/mu4e-view-add-noise-rule)
	    ("zé" mu4e-headers-toggle-threading)
	    ("zÉ" mu4e-headers-toggle-include-related)
	    ("zê" mu4e-headers-toggle-full-search)
	    ("a" mu4e-view-action)
	    ("gL" mu4e-show-log)
	    ("q" nil))
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
			      "S"  'hub/mu4e-view-mark-spam-and-next
			      "zS" 'hub/mu4e-view-spam-current))

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
  (defun hub/mu4e-thread-fold-initially ()
    "Fold all threads once a headers buffer enables thread mode."
    (when (derived-mode-p 'mu4e-headers-mode)
      (mu4e-thread-fold-all)))

  (add-hook 'mu4e-thread-mode-hook #'hub/mu4e-thread-fold-initially))

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
