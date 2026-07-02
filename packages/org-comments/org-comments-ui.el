;;; org-comments-ui.el --- UI extension adapters for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Explicit extension points for richer Org comments user interfaces.  The core
;; package stays usable with local sidecars while external packages or personal
;; configuration can register panel, overlay, and compose integrations.

;;; Code:

(require 'org-comments-core)

(defcustom org-comments-ui-open-function nil
  "Function used by `org-comments-open' to open a rich comments UI.
When nil, `org-comments-open' falls back to opening the local sidecar."
  :type '(choice (const :tag "No rich UI" nil) function)
  :group 'org-comments)

(defcustom org-comments-ui-page-open-function nil
  "Function used by `org-comments-page-open' to open page comments."
  :type '(choice (const :tag "No page UI" nil) function)
  :group 'org-comments)

(defcustom org-comments-ui-refresh-function nil
  "Function used after comment mutations to refresh visible UI state."
  :type '(choice (const :tag "No refresh UI" nil) function)
  :group 'org-comments)

(defcustom org-comments-ui-compose-reply-function nil
  "Function used by `org-comments-reply' to compose a comment reply."
  :type '(choice (const :tag "No reply UI" nil) function)
  :group 'org-comments)

(defcustom org-comments-ui-open-comment-function nil
  "Function used to open an inline comment by id from an `org-comment:' link."
  :type '(choice (const :tag "No inline comment UI" nil) function)
  :group 'org-comments)

(defcustom org-comments-ui-page-open-comment-function nil
  "Function used to open a page comment by id from an `org-comment:' link."
  :type '(choice (const :tag "No page comment UI" nil) function)
  :group 'org-comments)

(defun org-comments-ui-open (&optional fallback)
  "Open the registered rich comments UI, or call FALLBACK.
Signal a user error when neither a registered UI nor FALLBACK is available."
  (cond
   (org-comments-ui-open-function
    (funcall org-comments-ui-open-function))
   (fallback
    (funcall fallback))
   (t
    (user-error "No Org comments UI is available"))))

(defun org-comments-ui-page-open (&optional fallback)
  "Open the registered page comments UI, or call FALLBACK.
Signal a user error when neither a registered UI nor FALLBACK is available."
  (cond
   (org-comments-ui-page-open-function
    (funcall org-comments-ui-page-open-function))
   (fallback
    (funcall fallback))
   (t
    (user-error "No Org page comments UI is available"))))

(defun org-comments-ui-refresh ()
  "Refresh registered comment UI state when available."
  (when org-comments-ui-refresh-function
    (funcall org-comments-ui-refresh-function)))

(defun org-comments-ui-compose-reply (&optional fallback)
  "Compose a reply through the registered comments UI, or call FALLBACK."
  (cond
   (org-comments-ui-compose-reply-function
    (funcall org-comments-ui-compose-reply-function))
   (fallback
    (funcall fallback))
   (t
    (user-error "Reply composition UI is not available"))))

(defun org-comments-ui-open-comment (comment-id &optional position)
  "Open inline COMMENT-ID through the registered UI at POSITION.
Return non-nil when an inline comment UI handled the request."
  (when org-comments-ui-open-comment-function
    (funcall org-comments-ui-open-comment-function comment-id position)
    t))

(defun org-comments-ui-page-open-comment (comment-id)
  "Open page COMMENT-ID through the registered UI.
Return non-nil when a page comment UI handled the request."
  (when org-comments-ui-page-open-comment-function
    (funcall org-comments-ui-page-open-comment-function comment-id)
    t))

(provide 'org-comments-ui)
;;; org-comments-ui.el ends here
