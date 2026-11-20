;;; org-google-docs-disposable-transport-test.el --- Disposable fetch safety -*- lexical-binding: t; -*-

;;; Commentary:
;; No credentials or network: stub the existing transport at its public seam.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(load (expand-file-name "org-google-docs-disposable-transport.el"
			(file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
(defvar hub/gdocs-disposable-live-state)
(defvar gdocs-api--max-retries)

(ert-deftest org-google-docs-disposable-fetch-records-only-document-fields ()
  "Fetch uses explicit ownership/account, disables retries and filters response fields."
  (let* ((directory (make-temp-file "gdocs-disposable-test-" t))
	 (destination (expand-file-name "returned.json" directory))
	 (hub/gdocs-disposable-live-state '(:operation initial-publish :status complete :account "personal" :title "Pi disposable list conformance test" :document-id "synthetic-id"))
	 (gdocs-api--max-retries 3) seen)
    (unwind-protect
	(cl-letf (((symbol-function 'gdocs-api--request)
		   (lambda (method url callback &rest options)
		     (setq seen (list method url options))
		     (funcall callback '((documentId . "synthetic-id") (body . ((content . []))) (unselected . "excluded"))))))
	  (org-google-docs-disposable-fetch destination 'initial-publish)
	  (should (equal (car seen) 'get))
	  (should (equal (cadr seen) "https://docs.googleapis.com/v1/documents/synthetic-id"))
	  (should (= (plist-get (nth 2 seen) :attempt) 3))
	  (should (equal (plist-get (nth 2 seen) :account) "personal"))
	  (should (eq (plist-get hub/gdocs-disposable-live-state :status) 'complete))
	  (should (= (file-modes destination) #o600))
	  (should-not (assq 'unselected (json-read-file destination))))
      (delete-directory directory t))))

(ert-deftest org-google-docs-disposable-fetch-refuses-pending-or-unowned-target ()
  "No request can start when ownership or prior completion is missing."
  (dolist (state '((:operation initial-publish :status pending :account "personal" :title "Pi disposable list conformance test" :document-id "synthetic-id")
		   (:operation initial-publish :status complete :account "personal" :title "Other document" :document-id "synthetic-id")
		   (:operation initial-publish :status complete :account "other" :title "Pi disposable list conformance test" :document-id "synthetic-id")
		   (:operation different-operation :status complete :account "personal" :title "Pi disposable list conformance test" :document-id "synthetic-id")))
    (let ((hub/gdocs-disposable-live-state state))
      (cl-letf (((symbol-function 'gdocs-api--request) (lambda (&rest _) (ert-fail "Unexpected network call"))))
	(should-error (org-google-docs-disposable-fetch "/tmp/unused-disposable.json" 'initial-publish) :type 'user-error)))))

(ert-deftest org-google-docs-disposable-fetch-never-overwrites-a-snapshot ()
  "An existing artifact is evidence, not a new fetch destination."
  (let ((destination (make-temp-file "gdocs-disposable-existing-"))
	(hub/gdocs-disposable-live-state
	 '(:operation fetch :status complete :account "personal"
		      :title "Pi disposable list conformance test" :document-id "synthetic-id")))
    (unwind-protect
	(cl-letf (((symbol-function 'gdocs-api--request)
		   (lambda (&rest _) (ert-fail "Unexpected network call"))))
	  (should-error (org-google-docs-disposable-fetch destination 'fetch) :type 'user-error)
	  (should (eq (plist-get hub/gdocs-disposable-live-state :status) 'complete)))
      (delete-file destination))))

(ert-deftest org-google-docs-disposable-pdf-export-keeps-auth-opaque ()
  "PDF export reuses the owned account and returns only a saved filename."
  (let* ((directory (make-temp-file "gdocs-disposable-pdf-" t))
	 (destination (expand-file-name "render.pdf" directory))
	 (hub/gdocs-disposable-live-state
	  '(:operation fetch :status complete :account "personal"
		       :title "Pi disposable list conformance test" :document-id "synthetic-id"))
	 calls)
    (unwind-protect
	(cl-letf (((symbol-function 'gdocs-auth--resolve-account)
		   (lambda (account) (should (equal account "personal")) account))
		  ((symbol-function 'gdocs-auth-get-access-token)
		   (lambda (account callback &optional _on-error)
		     (should (equal account "personal")) (funcall callback nil)))
		  ((symbol-function 'gdocs-api--build-headers) (lambda (&rest _) nil))
		  ((symbol-function 'plz)
		   (lambda (method url &rest options)
		     (push (list method url) calls)
		     (should (equal (plist-get options :as) (list 'file destination)))
		     (with-temp-file destination (insert "%PDF-synthetic\n"))
		     (funcall (plist-get options :then) destination))))
	  (org-google-docs-disposable-export-pdf destination)
	  (should (equal calls '((get "https://www.googleapis.com/drive/v3/files/synthetic-id/export?mimeType=application%2Fpdf"))))
	  (should (eq (plist-get hub/gdocs-disposable-live-state :status) 'complete))
	  (should (= (file-modes destination) #o600))
	  (should-error (org-google-docs-disposable-export-pdf destination) :type 'user-error))
      (delete-directory directory t))))

(provide 'org-google-docs-disposable-transport-test)
;;; org-google-docs-disposable-transport-test.el ends here
