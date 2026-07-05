;;; org-sync-assets-test.el --- Tests for shared Org sync assets -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for provider-neutral image asset planning.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-sync-assets)

(ert-deftest org-sync-assets-plans-standalone-image-with-caption ()
  "Standalone image planning returns local path, filename, and caption."
  (let ((image-file (make-temp-file "org-sync-image" nil ".png")))
    (unwind-protect
	(with-temp-buffer
	  (insert (format "#+CAPTION: Logo\n[[file:%s]]\n" image-file))
	  (org-mode)
	  (let* ((plan (org-sync-assets-plan-buffer))
		 (asset (car (plist-get plan :assets))))
	    (should (plist-get plan :ready-p))
	    (should (equal (plist-get asset :source-link) image-file))
	    (should (equal (plist-get asset :source-path) image-file))
	    (should (string-match-p "org-sync-image.*-[0-9a-f]\\{12\\}\\.png"
				    (plist-get asset :filename)))
	    (should (equal (plist-get asset :caption) "Logo"))))
      (delete-file image-file))))

(ert-deftest org-sync-assets-ignores-described-image-link ()
  "Described image links remain normal links, not standalone image assets."
  (with-temp-buffer
    (insert "[[file:img/logo.png][Open image]]\n")
    (org-mode)
    (should-not (plist-get (org-sync-assets-plan-buffer) :assets))))

(ert-deftest org-sync-assets-reports-missing-local-image ()
  "Missing local image files produce diagnostics."
  (with-temp-buffer
    (insert "[[file:missing.png]]\n")
    (org-mode)
    (let* ((plan (org-sync-assets-plan-buffer))
	   (diagnostic (car (plist-get plan :diagnostics))))
      (should-not (plist-get plan :ready-p))
      (should (eq (plist-get diagnostic :code) :missing-image-file))
      (should (equal (plist-get diagnostic :path) "missing.png")))))

(ert-deftest org-sync-assets-detects-standalone-remote-link ()
  "Standalone remote links are detectable for provider cache commands."
  (with-temp-buffer
    (insert "#+CAPTION: Logo\n[[https://example.invalid/logo.png]]\n")
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
	   (paragraph (car (org-element-map tree 'paragraph #'identity)))
	   (link (org-sync-assets-standalone-remote-link paragraph)))
      (should link)
      (should (equal (org-element-property :raw-link link)
		     "https://example.invalid/logo.png")))))

(ert-deftest org-sync-assets-cache-filename-uses-content-type ()
  "Remote cache filenames use stable URL hashes and content types."
  (should (string-match-p
	   "\\`remote-[0-9a-f]\\{12\\}\\.png\\'"
	   (org-sync-assets-cache-filename
	    "https://example.invalid/image" "image/png"))))

(ert-deftest org-sync-assets-reuses-imported-missing-source ()
  "Generated imported filenames can be marked reusable without local source."
  (with-temp-buffer
    (insert "[[file:logo-123456abcdef.png]]\n")
    (org-mode)
    (let* ((plan (org-sync-assets-plan-buffer
		  (list :reuse-imported-missing-source t)))
	   (asset (car (plist-get plan :assets))))
      (should (plist-get plan :ready-p))
      (should (plist-get asset :missing-source))
      (should (equal (plist-get asset :filename) "logo-123456abcdef.png")))))

(provide 'org-sync-assets-test)
;;; org-sync-assets-test.el ends here
