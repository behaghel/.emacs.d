;;; org-google-docs-images-test.el --- Google Docs image plan tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for preflighting standalone Org image links before mutating Google Docs.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-sync"
					  (file-name-directory load-file-name)))

(require 'org-google-docs-images)

(defmacro org-google-docs-images-test--with-buffer (contents &rest body)
  "Evaluate BODY in an Org temp buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(ert-deftest org-google-docs-images-expands-tilde-paths ()
  "Image preflight expands tilde paths before readability checks."
  (should (equal (org-google-docs-images--absolute-path "~/Pictures/example.png")
		 (expand-file-name "~/Pictures/example.png"))))

(ert-deftest org-google-docs-images-plans-readable-standalone-image ()
  "Readable standalone local image links become image plan entries."
  (let ((image-file (make-temp-file "org-google-docs-image" nil ".png")))
    (unwind-protect
	(org-google-docs-images-test--with-buffer
	 (format "#+CAPTION: Logo\n[[file:%s]]\n" image-file)
	 (let* ((plan (org-google-docs-images-plan-buffer))
		(image (car (plist-get plan :images))))
	   (should (plist-get plan :ready-p))
	   (should (equal (plist-get image :path) image-file))
	   (should (equal (plist-get image :absolute-path) image-file))
	   (should (equal (plist-get image :caption) "Logo"))))
      (delete-file image-file))))

(ert-deftest org-google-docs-images-ignores-described-image-link ()
  "Described image links remain normal links, not embedded images."
  (org-google-docs-images-test--with-buffer "[[file:img/logo.png][Open image]]\n"
					    (let ((plan (org-google-docs-images-plan-buffer)))
					      (should-not (plist-get plan :images))
					      (should (plist-get plan :ready-p)))))

(ert-deftest org-google-docs-images-enriches-image-ir-with-uploaded-uri ()
  "Conversion advice adds uploaded image URIs to matching image IR."
  (let ((org-google-docs-images--push-session
	 (list :images (list (list :path "img/logo.png"
				   :uri "https://drive.google.com/uc?export=download&id=file-1")))))
    (cl-labels ((orig () (list (list :type 'image :path "img/logo.png"))))
      (let ((ir (org-google-docs-images--around-org-buffer-to-ir #'orig)))
	(should (equal (plist-get (car ir) :uri)
		       "https://drive.google.com/uc?export=download&id=file-1"))
	(should-not org-google-docs-images--push-session)))))

(ert-deftest org-google-docs-images-upload-all-sets-public-drive-uris ()
  "Image upload creates public permissions and stores direct Drive URIs."
  (let ((org-google-docs-images-make-uploaded-files-public t)
	calls)
    (cl-letf (((symbol-function 'gdocs-api-upload-image)
	       (lambda (file callback &optional account _folder-id)
		 (push (list :upload file account) calls)
		 (funcall callback '((id . "file-1")))))
	      ((symbol-function 'gdocs-api-create-anyone-reader-permission)
	       (lambda (file-id callback &optional account)
		 (push (list :permission file-id account) calls)
		 (funcall callback '((id . "perm-1"))))))
      (org-google-docs-images--upload-all
       (list (list :path "img/logo.png" :absolute-path "/tmp/logo.png"))
       "acct"
       (lambda (images)
	 (should (equal (plist-get (car images) :uri)
			"https://drive.google.com/uc?export=download&id=file-1"))))
      (should (equal (nreverse calls)
		     '((:upload "/tmp/logo.png" "acct")
		       (:permission "file-1" "acct")))))))

(ert-deftest org-google-docs-images-reports-missing-file ()
  "Missing standalone image files block before remote mutation."
  (org-google-docs-images-test--with-buffer "[[file:missing.png]]\n"
					    (let ((plan (org-google-docs-images-plan-buffer)))
					      (should-not (plist-get plan :ready-p))
					      (should (equal (plist-get (car (plist-get plan :diagnostics)) :code)
							     :missing-image-file)))))

(provide 'org-google-docs-images-test)
;;; org-google-docs-images-test.el ends here
