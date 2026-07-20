;;; hb-static-site-test.el --- Tests for static-site authoring helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for project-local Hugo/Denote static-site workflow glue.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'hb-static-site)
(require 'org)

(ert-deftest hb-static-site-derives-content-directory-from-denote-directory ()
  "Project-local `denote-directory' is the preferred content-org source root."
  (let ((denote-directory "/tmp/site/content-org")
	(org-hugo-base-dir "/tmp/site")
	(hb-static-site-content-org-directory nil))
    (should (equal (hb-static-site-content-org-directory)
		   (file-name-as-directory "/tmp/site/content-org")))
    (should (equal (hb-static-site-posts-directory)
		   "/tmp/site/content-org/posts"))
    (should (equal (hb-static-site-pages-directory)
		   "/tmp/site/content-org/pages"))))

(ert-deftest hb-static-site-mode-degrades-without-ox-hugo ()
  "Enabling site mode does not require ox-hugo to be available."
  (let ((hb-static-site-enable-auto-export t)
	(hb-static-site-content-org-directory temporary-file-directory)
	(messages nil)
	(original-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
	       (lambda (feature &optional filename noerror)
		 (cond
		  ((eq feature 'ox-hugo) (and (not noerror) (error "missing ox-hugo")))
		  (t (funcall original-require feature filename noerror)))))
	      ((symbol-function 'message)
	       (lambda (format-string &rest args)
		 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
	(org-mode)
	(hb-static-site-mode 1)
	(should hb-static-site-mode)
	(should (equal denote-prompts '(subdirectory title keywords)))
	(should (string-match-p "ox-hugo unavailable" (car messages)))))))

(ert-deftest hb-static-site-validates-buffer-under-content-org ()
  "Validation accepts an Org file under the configured content root."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (file (expand-file-name "posts/test.org" content))
	 (org-hugo-base-dir root)
	 (denote-directory content)
	 (hb-static-site-content-org-directory nil))
    (make-directory (file-name-directory file) t)
    (with-current-buffer (find-file-noselect file)
      (unwind-protect
	  (progn
	    (org-mode)
	    (insert "#+title: Test\n")
	    (save-buffer)
	    (should (hb-static-site-validate-buffer)))
	(kill-buffer)))))

(ert-deftest hb-static-site-create-section-inserts-ox-hugo-index ()
  "Section creation creates content-org/SECTION/_index.org with Hugo metadata."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (denote-directory content)
	 (org-hugo-base-dir root))
    (make-directory content t)
    (with-current-buffer (hb-static-site-create-section "notes" "Notes")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/notes/_index.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+title: Notes" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_section: notes" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_bundle: _index" (buffer-string))))
	(kill-buffer)))))

(ert-deftest hb-static-site-create-page-uses-root-or-section-conventions ()
  "Page creation creates root pages under pages/ and section pages in sections."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (denote-directory content)
	 (org-hugo-base-dir root))
    (make-directory content t)
    (with-current-buffer (hb-static-site-create-page "about" "About")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/pages/about.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+hugo_section: /" (buffer-string))))
	(kill-buffer)))
    (with-current-buffer (hb-static-site-create-page "notes/first-note" "First note")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/notes/first-note.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+hugo_section: notes" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_slug: first-note" (buffer-string))))
	(kill-buffer)))))

(ert-deftest hb-static-site-export-validates-then-calls-ox-hugo ()
  "Export command validates the buffer before delegating to ox-hugo."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (file (expand-file-name "posts/test.org" content))
	 (org-hugo-base-dir root)
	 (denote-directory content)
	 (called nil)
	 (original-require (symbol-function 'require)))
    (make-directory (file-name-directory file) t)
    (with-current-buffer (find-file-noselect file)
      (unwind-protect
	  (cl-letf (((symbol-function 'require)
		     (lambda (feature &optional filename noerror)
		       (if (eq feature 'ox-hugo) t
			 (funcall original-require feature filename noerror))))
		    ((symbol-function 'org-hugo-export-wim-to-md)
		     (lambda (&optional all-subtrees _async _visible-only _noerror)
		       (setq called all-subtrees))))
	    (org-mode)
	    (insert "#+title: Test\n")
	    (save-buffer)
	    (hb-static-site-export-buffer t)
	    (should called))
	(kill-buffer)))))

(provide 'hb-static-site-test)
;;; hb-static-site-test.el ends here
