;;; contexts.el --- Email: mu4e contexts and folders -*- lexical-binding: t; -*-

;;; Commentary:
;; Multiple-account contexts, dynamic folders, and shortcuts.

;;; Code:

(require 'hub-utils)
(require 'seq)

(defgroup hub/mu4e nil
  "Customizations for mu4e contexts and shortcuts."
  :group 'mu4e)

(defcustom hub/mu4e-contexts nil
  "Override for `mu4e-contexts`. When non-nil, used instead of defaults."
  :type '(repeat sexp)
  :group 'hub/mu4e)

(defcustom hub/mu4e-maildir-shortcuts nil
  "Override for `mu4e-maildir-shortcuts`. When non-nil, used instead of defaults."
  :type '(repeat plist)
  :group 'hub/mu4e)

(with-eval-after-load 'mu4e
  (defun hub/mu4e-context-var (context var)
    "Return CONTEXT variable VAR, if present."
    (cdr (assoc var (mu4e-context-vars context))))

  (defun hub/mu4e-context-for-message (msg)
    "Resolve the mu4e context for MSG.
Falls back to `mu4e-context-determine' with a permissive policy so a
matching context is always returned when available."
    (when msg
      (mu4e-context-determine msg 'pick-first)))

  (defun hub/mu4e-context-for-address (address)
    "Return the context that owns ADDRESS, if any."
    (when address
      (let ((normalized (downcase address)))
	(seq-find (lambda (context)
		    (let ((ctx-address (hub/mu4e-context-var context 'user-mail-address)))
		      (and ctx-address (string= normalized (downcase ctx-address)))))
		  mu4e-contexts))))

  (setq mu4e-contexts
	(or hub/mu4e-contexts
	    (list
	     (make-mu4e-context
	      :name "gmail"
	      :enter-func (lambda () (mu4e-message ">> GMail context"))
	      :match-func (lambda (msg)
			    (when msg (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
	      :vars '((user-mail-address . "behaghel@gmail.com")
		      (smtpmail-smtp-user . "behaghel@gmail.com")))
	     (make-mu4e-context
	      :name "fbehaghel.fr"
	      :enter-func (lambda () (mu4e-message ">> behaghel.fr context"))
	      :match-func (lambda (msg)
			    (when msg (mu4e-message-contact-field-matches msg '(:cc :from :to)
									  "hubert@behaghel.fr")))
	      :vars '((user-mail-address . "hubert@behaghel.fr")
		      (smtpmail-smtp-user . "hubert@behaghel.fr")))
	     (make-mu4e-context
	      :name "obehaghel.org"
	      :enter-func (lambda () (mu4e-message ">> behaghel.org context"))
	      :match-func (lambda (msg)
			    (when msg (mu4e-message-contact-field-matches msg '(:cc :from :to)
									  "hubert@behaghel.org")))
	      :vars '((user-mail-address . "hubert@behaghel.org")
		      (smtpmail-smtp-user . "hubert@behaghel.org")))
	     (make-mu4e-context
	      :name "work"
	      :enter-func (lambda () (mu4e-message ">> work context"))
	      :match-func (lambda (msg)
			    (when msg (string-match-p "^/work" (mu4e-message-field msg :maildir))))
	      :vars '((user-mail-address . "hubert.behaghel@veriff.net")
		      (smtpmail-smtp-user . "hubert.behaghel@veriff.net"))))))

  (setq mu4e-context-policy 'pick-first
	mu4e-compose-context-policy 'ask-if-none)

  (defun hub/mu4e-ensure-context-for-current-message ()
    "Keep the active mu4e context aligned with the message at point."
    (when-let* ((msg (mu4e-message-at-point))
		(context (hub/mu4e-context-for-message msg))
		(name (mu4e-context-name context)))
      (unless (eq context (mu4e-context-current))
	(mu4e-context-switch nil name))))

  (add-hook 'mu4e-view-rendered-hook #'hub/mu4e-ensure-context-for-current-message)

  (defun contextual-default-folder (suffix)
    "Return a folder resolver that derives the account root from MSG.
Falls back to the active context name when MSG is nil. Ensures actions
operate within the message's account to prevent cross-account moves."
    (lambda (msg)
      (let* ((maildir (and msg (mu4e-message-field msg :maildir)))
	     (root (cond
		    ;; Prefer deriving from the message maildir, e.g., "/work/inbox" -> "work"
		    ((and (stringp maildir)
			  (string-match "^/\\([^/]+\\)" maildir))
		     (match-string 1 maildir))
		    ;; Otherwise, use current context name, dropping any single-letter prefix for legacy names
		    ((mu4e-context-current)
		     (let* ((ctx (mu4e-context-current))
			    (name (mu4e-context-name ctx)))
		       (if (string-match "^.[ ]*\n?\(behaghel\\..*\)$" name)
			   (match-string 1 name)
			 (if (string-match "^\\([^.]+\\)" name)
			     name
			   name))))
		    (t nil))))
	(when (not root)
	  ;; As a last resort, try the first context's name (legacy behavior)
	  (let* ((first (car-safe mu4e-contexts))
		 (name (and first (mu4e-context-name first))))
	    (when (and name (string-match "^/\\?\\([^/]+\\)" (concat "/" name)))
	      (setq root (match-string 1 (concat "/" name))))))
	(concat "/" root suffix))))

  (setq mu4e-sent-folder   (contextual-default-folder "/sent")
	mu4e-drafts-folder (contextual-default-folder "/drafts")
	mu4e-refile-folder (lambda (msg)
			     (let ((maildir (or (and msg (mu4e-message-field msg :maildir)) "")))
			       (if (string-match-p "/sent\\'" maildir)
				   ;; do not archive from /sent
				   maildir
				 (funcall (contextual-default-folder "/archive") msg))))
	mu4e-trash-folder  (contextual-default-folder "/trash")
	mu4e-maildir-shortcuts (or hub/mu4e-maildir-shortcuts
				   '((:maildir "/gmail/inbox" :key ?g)
				     (:maildir "/behaghel.fr/inbox" :key ?f)
				     (:maildir "/behaghel.org/inbox" :key ?o)
				     (:maildir "/work/inbox" :key ?w)
				     (:maildir "/gmail/archive" :key ?G)
				     (:maildir "/behaghel.fr/archive" :key ?F)
				     (:maildir "/behaghel.org/archive" :key ?O)
				     (:maildir "/work/archive" :key ?W)
				     (:maildir "/gmail/sent" :key ?s)
				     (:maildir "/behaghel.fr/sent" :key ?h)
				     (:maildir "/behaghel.org/sent" :key ?S)
				     (:maildir "/work/sent" :key ?m)))))

(provide 'email/contexts)
;;; contexts.el ends here
