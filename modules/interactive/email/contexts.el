;;; contexts.el --- Email: mu4e contexts and folders -*- lexical-binding: t; -*-

;;; Commentary:
;; Multiple-account contexts, dynamic folders, and shortcuts.

;;; Code:

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
		      (smtpmail-smtp-user . "hubert@behaghel.org"))))))

  (setq mu4e-context-policy 'pick-first
	mu4e-compose-context-policy 'ask-if-none)

  (defun contextual-default-folder (suffix)
    (lambda (_msg)
      (let* ((context (or (mu4e-context-current) (car mu4e-contexts)))
	     (ctx-name (mu4e-context-name context))
	     (box-path (if (string-match-p "behaghel." ctx-name) (substring ctx-name 1 nil) ctx-name)))
	(concat "/" box-path suffix))))

  (setq mu4e-sent-folder   (contextual-default-folder "/sent")
	mu4e-drafts-folder (contextual-default-folder "/drafts")
	mu4e-refile-folder (contextual-default-folder "/archive")
	mu4e-trash-folder  (contextual-default-folder "/trash")
	mu4e-maildir-shortcuts (or hub/mu4e-maildir-shortcuts
				   '((:maildir "/gmail/inbox" :key ?g)
				     (:maildir "/behaghel.fr/inbox" :key ?f)
				     (:maildir "/behaghel.org/inbox" :key ?o)
				     (:maildir "/gmail/archive" :key ?G)
				     (:maildir "/behaghel.fr/archive" :key ?F)
				     (:maildir "/behaghel.org/archive" :key ?O)
				     (:maildir "/gmail/sent" :key ?s)
				     (:maildir "/behaghel.fr/sent" :key ?h)
				     (:maildir "/behaghel.org/sent" :key ?S)))))

(provide 'email/contexts)
;;; contexts.el ends here
