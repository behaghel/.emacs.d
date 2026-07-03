;;; google-docs.el --- Org: Google Docs publishing integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Activation and personal configuration for the local Org -> Google Docs
;; adapter.  Reusable behavior belongs in packages/org-google-docs/; this file
;; owns dependency declaration, local load-path wiring, sync defaults, and
;; secret-store backed account configuration.

;;; Code:

(require 'auth-source)
(require 'auth-source-pass nil 'noerror)
(require 'hub-utils)
(require 'subr-x)

(defgroup hub/org-google-docs nil
  "Personal Google Docs integration activation."
  :group 'org)

(defcustom hub/org-google-docs-package-directory
  (expand-file-name "packages/org-google-docs" user-emacs-directory)
  "Directory containing the Org Google Docs adapter package files."
  :type 'directory
  :group 'hub/org-google-docs)

(defcustom hub/org-google-docs-auth-source-accounts
  '(("personal"
     . ((client-id-host . "dev/emacs-gdocs/client-id")
	(client-secret-host . "dev/emacs-gdocs/client-secret"))))
  "Auth-source account mapping used to populate upstream `gdocs-accounts'.

Each entry has the form:

  (ACCOUNT-NAME . ((client-id-host . HOST)
		   (client-secret-host . HOST)
		   (user . USER)))

HOST values name auth-source/pass entries whose secrets contain the OAuth
client ID and client secret respectively.  USER is optional; when omitted, the
auth-source lookup does not constrain the user field.  Leave this nil to
configure `gdocs-accounts' elsewhere."
  :type '(alist :key-type string
		:value-type (alist :key-type symbol :value-type string))
  :group 'hub/org-google-docs)

(defun hub/org-google-docs--package-file (name)
  "Return absolute path for Org Google Docs adapter file NAME."
  (expand-file-name name hub/org-google-docs-package-directory))

(defun hub/org-google-docs--auth-source-secret (host user description)
  "Return auth-source secret for HOST and optional USER described by DESCRIPTION.
HOST may also name an exact pass entry, such as
`dev/emacs-gdocs/client-id'."
  (let* ((query (append (list :host host)
			(when user (list :user user))
			'(:require (:secret) :max 1)))
	 (entry (car (apply #'auth-source-search query)))
	 (secret (or (plist-get entry :secret)
		     (when (fboundp 'auth-source-pass-get)
		       (auth-source-pass-get 'secret host)))))
    (unless secret
      (user-error "Missing Google Docs %s in auth-source host/pass entry %s" description host))
    (if (functionp secret)
	(funcall secret)
      secret)))

(defun hub/org-google-docs--account-from-auth-source (account)
  "Return a `gdocs-accounts' entry built from auth-source ACCOUNT config."
  (let* ((name (car account))
	 (config (cdr account))
	 (user (alist-get 'user config))
	 (client-id-host (or (alist-get 'client-id-host config)
			     (user-error "Missing client-id-host for Google Docs account %s" name)))
	 (client-secret-host (or (alist-get 'client-secret-host config)
				 (user-error "Missing client-secret-host for Google Docs account %s" name)))
	 (client-id (hub/org-google-docs--auth-source-secret
		     client-id-host user "client ID"))
	 (client-secret (hub/org-google-docs--auth-source-secret
			 client-secret-host user "client secret")))
    (cons name `((client-id . ,client-id)
		 (client-secret . ,client-secret)))))

(defun hub/org-google-docs-configure-accounts-from-auth-source (&optional noerror)
  "Populate upstream `gdocs-accounts' from auth-source account mappings.
When NOERROR is non-nil, leave `gdocs-accounts' untouched if a secret is
missing.  Interactive use is strict and reports missing secret entries.  Never
return the configured account value, because it contains OAuth client secrets."
  (interactive)
  (let ((configured-count 0))
    (when hub/org-google-docs-auth-source-accounts
      (let ((accounts (if noerror
			  (condition-case nil
			      (mapcar #'hub/org-google-docs--account-from-auth-source
				      hub/org-google-docs-auth-source-accounts)
			    (error nil))
			(mapcar #'hub/org-google-docs--account-from-auth-source
				hub/org-google-docs-auth-source-accounts))))
	(when accounts
	  (setq gdocs-accounts accounts
		configured-count (length accounts)))))
    (when (called-interactively-p 'interactive)
      (message "Configured %d Google Docs account(s) from auth-source" configured-count))
    configured-count))

(add-to-list 'load-path hub/org-google-docs-package-directory)

(use-package gdocs
  :straight (:type git :host github :repo "benthamite/gdocs")
  :commands (gdocs-authenticate
	     gdocs-create
	     gdocs-open
	     gdocs-link
	     gdocs-push
	     gdocs-pull
	     gdocs-status
	     gdocs-open-in-browser
	     gdocs-menu)
  :init
  (setq gdocs-auto-push-on-save nil
	gdocs-auto-pull-on-open nil)
  (hub/org-google-docs-configure-accounts-from-auth-source 'noerror)
  :config
  (hub/org-google-docs-configure-accounts-from-auth-source 'noerror))

(require 'org-google-docs)

(provide 'org/google-docs)
;;; google-docs.el ends here
