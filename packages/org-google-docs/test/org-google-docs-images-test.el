;;; org-google-docs-images-test.el --- Google Docs image plan tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for preflighting standalone Org image links before mutating Google Docs.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-google-docs-images)

(defmacro org-google-docs-images-test--with-buffer (contents &rest body)
  "Evaluate BODY in an Org temp buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (org-mode)
     (goto-char (point-min))
     ,@body))

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

(ert-deftest org-google-docs-images-reports-missing-file ()
  "Missing standalone image files block before remote mutation."
  (org-google-docs-images-test--with-buffer "[[file:missing.png]]\n"
					    (let ((plan (org-google-docs-images-plan-buffer)))
					      (should-not (plist-get plan :ready-p))
					      (should (equal (plist-get (car (plist-get plan :diagnostics)) :code)
							     :missing-image-file)))))

(provide 'org-google-docs-images-test)
;;; org-google-docs-images-test.el ends here
