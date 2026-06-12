;;; org-export-test.el --- Non-regression tests for org/export -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'ox)
(require 'ox-latex)

;; Ensure modules under modules/org/ are reachable for batch test runners.
(let ((root (file-name-as-directory
	     (locate-dominating-file (or load-file-name buffer-file-name)
				     "domains.yaml"))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "core" root)))

(require 'org/export-latex)

(ert-deftest hub/org-export-no-toc-in-hub-article ()
  "Export a minimal hub-article Org buffer to LaTeX and verify no TOC."
  (let ((org-file (make-temp-file "hub-article-test-" nil ".org"
				  "#+TITLE: Test Article
#+AUTHOR: Test Author
#+DATE: 2026-06-01
#+LANGUAGE: en
#+LATEX_CLASS: hub-article
#+OPTIONS: H:4 toc:3 ^:{}

* First heading
Content here.

** Sub-heading
More content."))
	(tex-file (make-temp-file "hub-article-test-" nil ".tex"))
	result)
    (unwind-protect
	(with-current-buffer (find-file-noselect org-file)
	  ;; Register the class (simulates what export.el does at load)
	  (hub/org-export-register-hub-article)
	  ;; Export to LaTeX string via org-export-as
	  (let ((output (org-export-as 'latex nil nil nil nil)))
	    (should output)
	    ;; Check no tableofcontents in output
	    (should-not (string-match-p "\\\\tableofcontents" output))
	    ;; Check no setcounter tocdepth in output
	    (should-not (string-match-p "\\\\setcounter{tocdepth}" output))
	    (message "PASS: No \\\\tableofcontents or \\\\setcounter{tocdepth} found")))
      (ignore-errors (delete-file org-file))
      (ignore-errors (delete-file tex-file)))))

(ert-deftest hub/org-export-no-toc-in-hub-article-subtree ()
  "Export a hub-article subtree and verify no TOC."
  (let ((org-file (make-temp-file "hub-article-test-" nil ".org"
				  "#+TITLE: Test Article
#+AUTHOR: Test Author
#+DATE: 2026-06-01
#+LATEX_CLASS: hub-article
#+OPTIONS: H:4 toc:3 ^:{}

* First heading
Some text.

** Subtree heading
:PROPERTIES:
:EXPORT_TITLE: Exported Subtree
:END:
Subtree content."))
	result)
    (unwind-protect
	(with-current-buffer (find-file-noselect org-file)
	  (hub/org-export-register-hub-article)
	  ;; Navigate to the subtree heading and export as subtree
	  (goto-char (point-min))
	  (re-search-forward "^\\*\\* Subtree heading")
	  (let ((output (org-export-as 'latex t nil nil nil)))
	    (should output)
	    (should-not (string-match-p "\\\\tableofcontents" output))
	    (message "PASS: No TOC in subtree export")))
      (ignore-errors (delete-file org-file)))))
