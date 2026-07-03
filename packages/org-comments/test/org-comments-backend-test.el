;;; org-comments-backend-test.el --- Backend tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the backend registry and dispatch protocol.

;;; Code:

(require 'ert)
(require 'org-comments-backend)

(defun org-comments-backend-test--list (source-buffer include-stale)
  "Return fake comments for SOURCE-BUFFER and INCLUDE-STALE."
  (list :source-buffer source-buffer :include-stale include-stale))

(defun org-comments-backend-test--create (record)
  "Return fake created RECORD."
  (plist-put (copy-sequence record) :created t))

(defun org-comments-backend-test--push (record)
  "Return fake pushed RECORD."
  (plist-put (copy-sequence record) :pushed t))

(defun org-comments-backend-test--pull (record)
  "Return fake pulled RECORD."
  (plist-put (copy-sequence record) :pulled t))

(ert-deftest org-comments-backend-registers-and-dispatches-fake-backend ()
  "Backend specs register capabilities and dispatch operation functions."
  (let ((org-comments-backends (make-hash-table :test #'eq)))
    (org-comments-register-backend
     'fake
     '(:name "Fake"
	     :capabilities (:list-comments :create-inline :push :pull)
	     :list org-comments-backend-test--list
	     :create org-comments-backend-test--create
	     :push org-comments-backend-test--push
	     :pull org-comments-backend-test--pull))
    (should (org-comments-backend-capable-p 'fake :list-comments))
    (should-not (org-comments-backend-capable-p 'fake :delete))
    (should (equal (org-comments-backend-list 'fake 'buffer t)
		   '(:source-buffer buffer :include-stale t)))
    (should (equal (org-comments-backend-create 'fake '(:id "local-1"))
		   '(:id "local-1" :created t)))
    (should (equal (org-comments-backend-push 'fake '(:id "local-1"))
		   '(:id "local-1" :pushed t)))
    (should (equal (org-comments-backend-pull 'fake '(:id "local-1"))
		   '(:id "local-1" :pulled t)))))

(ert-deftest org-comments-backend-errors-for-unsupported-operation ()
  "Dispatch reports unsupported backend operations clearly."
  (let ((org-comments-backends (make-hash-table :test #'eq)))
    (org-comments-register-backend
     'fake
     '(:name "Fake" :capabilities (:list-comments)))
    (let ((error (should-error (org-comments-backend-delete 'fake "local-1")
			       :type 'user-error)))
      (should (string-match-p "Fake does not support delete"
			      (error-message-string error))))))

(ert-deftest org-comments-backend-unsupported-includes-alternative ()
  "Unsupported helper can include a supported next step."
  (let ((org-comments-backends (make-hash-table :test #'eq)))
    (org-comments-register-backend 'fake '(:name "Fake"))
    (let ((error (should-error
		  (org-comments-backend-unsupported
		   'fake :create-inline "reply to an existing remote thread")
		  :type 'user-error)))
      (should (equal (error-message-string error)
		     "Fake does not support create inline; reply to an existing remote thread")))))

(ert-deftest org-comments-backend-detects-default-backend ()
  "Backend detection falls back to `org-comments-default-backend'."
  (let ((org-comments-backends (make-hash-table :test #'eq))
	(org-comments-backend-detectors nil)
	(org-comments-default-backend 'org))
    (org-comments-register-backend 'org '(:name "Org"))
    (with-temp-buffer
      (should (eq (org-comments-backend-detect) 'org)))))

(ert-deftest org-comments-backend-detects-registered-backend ()
  "Backend detectors select registered matching backends."
  (let ((org-comments-backends (make-hash-table :test #'eq))
	(org-comments-backend-detectors nil))
    (org-comments-register-backend 'org '(:name "Org"))
    (org-comments-register-backend 'fake '(:name "Fake"))
    (org-comments-register-backend-detector
     'fake (lambda (_buffer) t))
    (with-temp-buffer
      (should (eq (org-comments-backend-detect) 'fake)))))

(ert-deftest org-comments-backend-detection-ignores-unregistered-backend ()
  "Backend detectors cannot select backends that are not registered."
  (let ((org-comments-backends (make-hash-table :test #'eq))
	(org-comments-backend-detectors nil)
	(org-comments-default-backend 'org))
    (org-comments-register-backend 'org '(:name "Org"))
    (org-comments-register-backend-detector
     'missing (lambda (_buffer) t))
    (with-temp-buffer
      (should (eq (org-comments-backend-detect) 'org)))))

(provide 'org-comments-backend-test)
;;; org-comments-backend-test.el ends here
