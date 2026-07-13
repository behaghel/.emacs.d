;;; org-sync-remote.el --- Provider-neutral remote document protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; Small registry for Org synchronization providers.  Provider packages own
;; vendor APIs; callers use normalized describe/pull/scan functions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar org-sync-remote-providers nil
  "Registered Org sync remote providers.
Each entry is a plist containing at least :kind and optional :describe-url,
:pull, and :scan functions.")

(defun org-sync-remote-register-provider (&rest provider)
  "Register remote document PROVIDER plist.
Required key: :kind.  Optional keys: :describe-url, :pull, :scan."
  (let ((kind (plist-get provider :kind)))
    (unless (and (stringp kind) (not (string-empty-p kind)))
      (user-error "Org sync remote provider requires non-empty :kind"))
    (setq org-sync-remote-providers
	  (cons provider
		(cl-remove-if (lambda (existing)
				(equal (plist-get existing :kind) kind))
			      org-sync-remote-providers)))
    provider))

(defun org-sync-remote-provider (kind)
  "Return registered provider for KIND, or nil."
  (cl-find-if (lambda (provider) (equal (plist-get provider :kind) kind))
	      org-sync-remote-providers))

(defun org-sync-remote-describe-url (url)
  "Return normalized remote document descriptor for URL.
The returned plist should include :kind, :id, :url, and optional :title and
:supports."
  (or (cl-some (lambda (provider)
		 (when-let* ((fn (plist-get provider :describe-url)))
		   (funcall fn url)))
	       org-sync-remote-providers)
      (list :kind "url" :id (string-trim url) :url (string-trim url)
	    :supports '(:pull nil :activity nil :comments nil))))

(defun org-sync-remote-pull (kind id local-path &rest options)
  "Pull remote document KIND/ID to LOCAL-PATH through its provider.
OPTIONS are provider-specific keyword arguments."
  (let* ((provider (or (org-sync-remote-provider kind)
		       (user-error "No Org sync remote provider registered for %s" kind)))
	 (fn (or (plist-get provider :pull)
		 (user-error "Org sync remote provider %s does not support pull" kind))))
    (apply fn id local-path options)))

(defun org-sync-remote-scan (kind id baseline &rest options)
  "Scan remote document KIND/ID against BASELINE through its provider.
Return normalized activity plist.  OPTIONS are provider-specific keyword
arguments."
  (let* ((provider (or (org-sync-remote-provider kind)
		       (user-error "No Org sync remote provider registered for %s" kind)))
	 (fn (or (plist-get provider :scan)
		 (user-error "Org sync remote provider %s does not support activity scan" kind))))
    (apply fn id baseline options)))

(provide 'org-sync-remote)
;;; org-sync-remote.el ends here
