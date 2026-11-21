;;; org-google-docs-disposable-transport.el --- Narrow disposable fetch -*- lexical-binding: t; -*-

;;; Commentary:
;; Reuse the already loaded API transport, never load another gdocs library version into Emacs.
;; Only a document recorded by this disposable session can be fetched.

;;; Code:

(require 'json)
(require 'subr-x)
(defvar hub/gdocs-disposable-live-state nil)
(defvar gdocs-api--max-retries)
(declare-function gdocs-api--request "gdocs-api" (method url callback &rest options))
(declare-function gdocs-api--build-headers "gdocs-api" (token body))
(declare-function gdocs-auth--resolve-account "gdocs-auth" (account))
(declare-function gdocs-auth-get-access-token "gdocs-auth" (account callback &optional on-error))
(declare-function plz "plz" (method url &rest options))

(defun org-google-docs-disposable-fetch (destination expected-operation)
  "Fetch the owned disposable document to DESTINATION after EXPECTED-OPERATION.
Only explicit document fields are written.  No credentials or transport records
are returned.  A failed or uncertain operation is never retried automatically."
  (unless (and (eq (plist-get hub/gdocs-disposable-live-state :operation) expected-operation)
	       (eq (plist-get hub/gdocs-disposable-live-state :status) 'complete)
	       (equal (plist-get hub/gdocs-disposable-live-state :account) "personal")
	       (string-prefix-p "Pi disposable list conformance "
				(or (plist-get hub/gdocs-disposable-live-state :title) ""))
	       (stringp (plist-get hub/gdocs-disposable-live-state :document-id)))
    (user-error "Disposable document ownership or prior completion is missing"))
  (when (file-exists-p destination) (user-error "Use a new disposable snapshot destination"))
  (let ((id (plist-get hub/gdocs-disposable-live-state :document-id)))
    (setq hub/gdocs-disposable-live-state (plist-put hub/gdocs-disposable-live-state :operation 'fetch)
	  hub/gdocs-disposable-live-state (plist-put hub/gdocs-disposable-live-state :status 'pending))
    (gdocs-api--request
     'get (concat "https://docs.googleapis.com/v1/documents/" id)
     (lambda (json)
       (with-temp-file destination
	 (insert (json-encode
		  (delq nil (mapcar (lambda (field) (assq field json))
				    '(documentId title revisionId body lists namedRanges documentStyle footnotes inlineObjects))))))
       (set-file-modes destination #o600)
       (setq hub/gdocs-disposable-live-state (plist-put hub/gdocs-disposable-live-state :status 'complete)))
     :account "personal" :attempt gdocs-api--max-retries
     :on-error (lambda (err)
		 (setq hub/gdocs-disposable-live-state (plist-put hub/gdocs-disposable-live-state :status 'failed)
		       hub/gdocs-disposable-live-state (plist-put hub/gdocs-disposable-live-state :private-error (error-message-string err)))))
    (list :operation 'fetch :status (plist-get hub/gdocs-disposable-live-state :status))))

(defun org-google-docs-disposable-export-pdf (destination)
  "Export the last fetched disposable document as PDF to new DESTINATION.
Reuse the installed account resolver and HTTP client.  Authentication stays
inside callbacks; return only operation status, never headers or credentials."
  (unless (and (eq (plist-get hub/gdocs-disposable-live-state :operation) 'fetch)
	       (eq (plist-get hub/gdocs-disposable-live-state :status) 'complete)
	       (equal (plist-get hub/gdocs-disposable-live-state :account) "personal")
	       (string-prefix-p "Pi disposable list conformance "
				(or (plist-get hub/gdocs-disposable-live-state :title) ""))
	       (stringp (plist-get hub/gdocs-disposable-live-state :document-id)))
    (user-error "PDF export requires the completed owned disposable fetch"))
  (when (file-exists-p destination) (user-error "Use a new disposable PDF destination"))
  (let ((id (plist-get hub/gdocs-disposable-live-state :document-id))
	(failed (lambda (err)
		  (setq hub/gdocs-disposable-live-state
			(plist-put hub/gdocs-disposable-live-state :status 'failed))
		  (setq hub/gdocs-disposable-live-state
			(plist-put hub/gdocs-disposable-live-state :private-error err)))))
    (setq hub/gdocs-disposable-live-state
	  (plist-put hub/gdocs-disposable-live-state :operation 'export-pdf))
    (setq hub/gdocs-disposable-live-state
	  (plist-put hub/gdocs-disposable-live-state :status 'pending))
    (gdocs-auth-get-access-token
     (gdocs-auth--resolve-account "personal")
     (lambda (opaque-token)
       (plz 'get (concat "https://www.googleapis.com/drive/v3/files/" id
			 "/export?mimeType=application%2Fpdf")
	    :headers (gdocs-api--build-headers opaque-token nil)
	    :as (list 'file destination)
	    :then (lambda (_filename)
		    (set-file-modes destination #o600)
		    (setq hub/gdocs-disposable-live-state
			  (plist-put hub/gdocs-disposable-live-state :status 'complete)))
	    :else failed))
     failed)
    (list :operation 'export-pdf :status (plist-get hub/gdocs-disposable-live-state :status))))

(provide 'org-google-docs-disposable-transport)
;;; org-google-docs-disposable-transport.el ends here
