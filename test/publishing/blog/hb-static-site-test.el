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

(ert-deftest hb-static-site-relative-dir-locals-stay-anchored-at-site-root ()
  "Relative .dir-locals paths do not recurse under the current content file."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (nested (expand-file-name "pages/about" content))
	 (default-directory nested)
	 (denote-directory "content-org")
	 (org-hugo-base-dir ".")
	 (hb-static-site-content-org-directory nil))
    (make-directory nested t)
    (write-region "" nil (expand-file-name "hugo.toml" root))
    (should (equal (hb-static-site-hugo-base-dir)
		   (file-name-as-directory root)))
    (should (equal (hb-static-site-content-org-directory)
		   (file-name-as-directory content)))
    (should-not (string-match-p "content-org/.*/content-org"
				(hb-static-site-content-org-directory)))))

(ert-deftest hb-static-site-callout-block-exports-to-hugo-shortcode ()
  "Callout attributes survive as Hugo shortcode parameters."
  (with-temp-buffer
    (org-mode)
    (insert "#+ATTR_CALLOUT: :type info :title \"Consumables (pods, tablets…)\"\n")
    (insert "#+begin_callout\nStarter pack included.\n#+end_callout\n")
    (let* ((ast (org-element-parse-buffer))
	   (block (org-element-map ast 'special-block #'identity nil t))
	   (markdown (hb-static-site--hugo-callout-special-block
		      block "Starter pack included." nil)))
      (should (string-match-p
	       "{{< callout type=\"info\" title=\"Consumables (pods, tablets…)\" >}}"
	       markdown))
      (should (string-match-p "Starter pack included." markdown))
      (should (string-match-p "{{< /callout >}}" markdown)))))

(ert-deftest hb-static-site-create-section-inserts-ox-hugo-index ()
  "Section creation creates content-org/SECTION/_index.org with Hugo metadata."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (nested (expand-file-name "pages/about" content))
	 (default-directory nested)
	 (denote-directory "content-org")
	 (org-hugo-base-dir "."))
    (make-directory nested t)
    (write-region "" nil (expand-file-name "hugo.toml" root))
    (with-current-buffer (hb-static-site-create-section "notes" "Notes")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/notes/_index.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+title: Notes" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_section: notes" (buffer-string)))
	    (should-not (string-match-p "^#\\+hugo_bundle:" (buffer-string))))
	(kill-buffer)))))

(ert-deftest hb-static-site-section-names-come-from-existing-section-indexes ()
  "Page section choices are based on existing section directories."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (denote-directory content)
	 (org-hugo-base-dir root))
    (make-directory (expand-file-name "notes" content) t)
    (make-directory (expand-file-name "draft-no-index" content) t)
    (make-directory (expand-file-name "pages" content) t)
    (write-region "#+title: Notes\n" nil (expand-file-name "notes/_index.org" content))
    (should (equal (hb-static-site-section-names) '("/" "notes")))))

(ert-deftest hb-static-site-create-basic-page-uses-root-or-section-conventions ()
  "Basic page creation creates flat root pages and section pages."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (denote-directory content)
	 (org-hugo-base-dir root))
    (make-directory content t)
    (with-current-buffer (hb-static-site-create-basic-page "about" "About")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/pages/about.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+hugo_section: /" (buffer-string))))
	(kill-buffer)))
    (with-current-buffer (hb-static-site-create-basic-page "notes/first-note" "First note")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/notes/first-note.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+hugo_section: notes" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_slug: first-note" (buffer-string))))
	(kill-buffer)))))

(ert-deftest hb-static-site-create-page-uses-leaf-bundles ()
  "Default page creation creates Hugo leaf bundles for page-owned resources."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (denote-directory content)
	 (org-hugo-base-dir root))
    (make-directory content t)
    (with-current-buffer (hb-static-site-create-page "about" "About")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/pages/about/index.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+hugo_base_dir: ../../.." (buffer-string)))
	    (should (string-match-p "^#\\+hugo_section: /" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_bundle: about" (buffer-string))))
	(kill-buffer)))
    (with-current-buffer (hb-static-site-create-page "notes/first-note" "First note")
      (unwind-protect
	  (progn
	    (should (string-suffix-p "content-org/notes/first-note/index.org" (buffer-file-name)))
	    (should (string-match-p "^#\\+hugo_section: notes" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_slug: first-note" (buffer-string)))
	    (should (string-match-p "^#\\+hugo_bundle: first-note" (buffer-string))))
	(kill-buffer)))))

(ert-deftest hb-static-site-create-page-interactive-prompts-for-existing-section ()
  "Interactive page creation prompts for a section before page slug/title."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (nested (expand-file-name "pages/about" content))
	 (default-directory nested)
	 (denote-directory "content-org")
	 (org-hugo-base-dir "."))
    (make-directory (expand-file-name "notes" content) t)
    (make-directory nested t)
    (write-region "" nil (expand-file-name "hugo.toml" root))
    (write-region "#+title: Notes\n" nil (expand-file-name "notes/_index.org" content))
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (_prompt collection &rest _args)
		 (should (member "notes" collection))
		 "notes"))
	      ((symbol-function 'read-string)
	       (lambda (prompt &rest _args)
		 (if (string-match-p "slug" prompt) "first-note" "First note"))))
      (with-current-buffer (call-interactively #'hb-static-site-create-page)
	(unwind-protect
	    (should (string-suffix-p "content-org/notes/first-note/index.org" (buffer-file-name)))
	  (kill-buffer))))))

(ert-deftest hb-static-site-export-all-exports-every-org-source ()
  "Export-all visits every Org file under content-org and delegates to ox-hugo."
  (let* ((root (make-temp-file "hb-site-" t))
	 (content (expand-file-name "content-org" root))
	 (first (expand-file-name "pages/about.org" content))
	 (second (expand-file-name "howtos/barbecue/index.org" content))
	 (default-directory root)
	 (org-hugo-base-dir root)
	 (denote-directory content)
	 (exported nil)
	 (original-require (symbol-function 'require)))
    (make-directory (file-name-directory first) t)
    (make-directory (file-name-directory second) t)
    (write-region "#+title: About\n" nil first)
    (write-region "#+title: Barbecue\n" nil second)
    (cl-letf (((symbol-function 'require)
	       (lambda (feature &optional filename noerror)
		 (if (eq feature 'ox-hugo) t
		   (funcall original-require feature filename noerror))))
	      ((symbol-function 'org-hugo-export-wim-to-md)
	       (lambda (&optional all-subtrees _async _visible-only _noerror)
		 (should all-subtrees)
		 (push (buffer-file-name) exported))))
      (should (equal (sort (mapcar #'file-truename (hb-static-site-export-all content))
			   #'string<)
		     (sort (mapcar #'file-truename (list first second)) #'string<)))
      (should (equal (sort (mapcar #'file-truename exported) #'string<)
		     (sort (mapcar #'file-truename (list first second)) #'string<))))))

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
