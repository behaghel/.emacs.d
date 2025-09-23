;;; actions.el --- Email: mu4e custom actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Additional actions for mu4e headers/view buffers:
;; - spam helpers (move to spam + block sender)
;; - one-tap noise predicates to feed bookmark filters

;;; Code:

(require 'hub-utils)
(require 'email/noise)

(defgroup hub/mu4e-actions nil
  "Custom mu4e actions."
  :group 'mu4e)

(defcustom hub/mu4e-spam-suffix "/spam"
  "Folder suffix used when resolving the Spam target for a context."
  :type 'string
  :group 'hub/mu4e-actions)

(defun hub/mu4e--current-context ()
  "Return the currently active mu4e context object, if any."
  (when (fboundp 'mu4e-context-current)
    (or (mu4e-context-current)
	(car-safe (and (boundp 'mu4e-contexts) mu4e-contexts)))))

(defun hub/mu4e--resolve-folder (suffix &optional msg)
  "Resolve SUFFIX (e.g. `/spam') relative to MSG's account or current context."
  (cond
   ((and (fboundp 'contextual-default-folder)
	 (fboundp 'funcall))
    (funcall (contextual-default-folder suffix) msg))
   ((hub/mu4e--current-context)
    (concat "/" (mu4e-context-name (hub/mu4e--current-context)) suffix))
   (t nil)))

(defun hub/mu4e--spam-folder (msg)
  "Return the Spam folder to use for MSG."
  (hub/mu4e--resolve-folder hub/mu4e-spam-suffix msg))

(defun hub/mu4e--sender-email (msg)
  "Extract sender email address from MSG."
  (let* ((from (and (fboundp 'mu4e-message-field)
		    (mu4e-message-field msg :from)))
	 (entry (car-safe from)))
    (cond
     ((and (consp entry) (cdr entry)) (cdr entry))
     ((and (listp entry) (plist-get entry :email)) (plist-get entry :email))
     ((stringp entry) entry)
     (t nil))))

;;; Spam helpers --------------------------------------------------------------

(defun hub/mu4e-view-move-to-spam (msg)
  "In view buffers: move MSG to the Spam folder for its context."
  (interactive (list (and (fboundp 'mu4e-message-at-point)
			  (mu4e-message-at-point))))
  (let ((target (hub/mu4e--spam-folder msg)))
    (unless target (user-error "Unable to resolve spam folder for this context"))
    (mu4e-view-move-to-folder target)))

(defun hub/mu4e-headers-block-sender-and-spam (msg)
  "In headers buffers: mark all mail from MSG's sender for the Spam folder."
  (interactive (list (and (fboundp 'mu4e-message-at-point)
			  (mu4e-message-at-point))))
  (let* ((email (hub/mu4e--sender-email msg))
	 (target (hub/mu4e--spam-folder msg)))
    (unless email (user-error "Cannot determine sender email"))
    (unless target (user-error "Unable to resolve spam folder for this context"))
    ;; Re-use mu4e's interactive mark helper with a pre-filled pattern.
    (minibuffer-with-setup-hook (lambda () (insert (concat "from:" email)))
      (call-interactively #'mu4e-headers-mark-pattern))))

(with-eval-after-load 'mu4e-view
  (add-to-list 'mu4e-view-actions
	       '("Move to spam" . hub/mu4e-view-move-to-spam)
	       t))

(with-eval-after-load 'mu4e-headers
  (add-to-list 'mu4e-headers-actions
	       '("Block sender → spam" . hub/mu4e-headers-block-sender-and-spam)
	       t))

(with-eval-after-load 'mu4e-mark
  (add-to-list 'mu4e-marks
	       `(spam
		 :char ("S" . "S")
		 :prompt "Spam"
		 :dyn-target (lambda (target msg)
			       (or (hub/mu4e--spam-folder msg) target))
		 :action (lambda (docid _msg target)
			   (mu4e--server-move docid (mu4e--mark-check-target target) "-N"))))
  (with-eval-after-load 'mu4e-headers
    (defun mu4e-headers-mark-for-spam ()
      "Mark the current header for the custom Spam action."
      (interactive)
      (mu4e-headers-mark-and-next 'spam))))

;;; Noise helpers -------------------------------------------------------------

(defun hub/mu4e--message-list-id (msg)
  "Best-effort extraction of the List-Id header from MSG."
  (or (and (fboundp 'mu4e-message-field) (mu4e-message-field msg :list))
      (and (fboundp 'mu4e-message-field) (mu4e-message-field msg :mailing-list))
      (let* ((path (and (fboundp 'mu4e-message-readable-path)
			(ignore-errors (mu4e-message-readable-path msg))))
	     (value (when path
		      (with-temp-buffer
			(insert-file-contents path nil 0 4096)
			(goto-char (point-min))
			(when (re-search-forward "^List-Id:\\s-*\\(<[^>]+>\\|.+\\)$" nil t)
			  (string-trim (replace-regexp-in-string
					"[<>]" "" (match-string 1))))))))
	value)))

(defun hub/mu4e--noise-kind ()
  "Prompt the user for the type of noise predicate to add."
  (intern (completing-read "Noise by: " '("list-id" "sender" "subject-regex")
			   nil t nil nil "list-id")))

(defun hub/mu4e--register-noise (entry)
  "Append ENTRY to `hub/noise-predicates' and announce it."
  (unless (listp hub/noise-predicates)
    (setq hub/noise-predicates nil))
  (add-to-list 'hub/noise-predicates entry t #'equal)
  (message "Added noise predicate: %S" entry))

(defun hub/mu4e--noise-entry (msg)
  "Build a noise predicate plist from MSG via user choice."
  (pcase (hub/mu4e--noise-kind)
    ('list-id
     (let* ((lid (or (hub/mu4e--message-list-id msg)
		     (read-string "List-Id: "))))
       (list :name (format "List: %s" lid)
	     :query (format "list:%s" lid)
	     :category :list)))
    ('sender
     (let* ((email (or (hub/mu4e--sender-email msg)
		       (read-string "From address: "))))
       (list :name (format "From: %s" email)
	     :query (format "from:%s" email)
	     :category :sender)))
    ('subject-regex
     (let* ((subject (and (fboundp 'mu4e-message-field)
			  (mu4e-message-field msg :subject)))
	    (pattern (read-string (format "Subject regex [default %s]: " (or subject ""))
				  nil nil subject)))
       (list :name (format "Subject: %s" pattern)
	     :query (format "subject:%s" pattern)
	     :category :subject)))
    (_ (user-error "Unsupported noise rule kind"))))

(defun hub/mu4e-view-add-noise-rule (msg)
  "View buffer entry point to add a noise predicate for MSG."
  (interactive (list (mu4e-message-at-point)))
  (hub/mu4e--register-noise (hub/mu4e--noise-entry msg)))

(defun hub/mu4e-headers-add-noise-rule (msg)
  "Headers buffer entry point to add a noise predicate for MSG."
  (interactive (list (mu4e-message-at-point)))
  (hub/mu4e--register-noise (hub/mu4e--noise-entry msg)))

(with-eval-after-load 'mu4e-view
  (add-to-list 'mu4e-view-actions
	       '("Add noise rule" . hub/mu4e-view-add-noise-rule)
	       t))

(with-eval-after-load 'mu4e-headers
  (add-to-list 'mu4e-headers-actions
	       '("Add noise rule" . hub/mu4e-headers-add-noise-rule)
	       t))

(provide 'email/actions)
;;; actions.el ends here
