;;; org-epub-export-test.el --- Tests for Org EPUB export -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies the Org -> controlled XHTML -> Pandoc EPUB export path.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'org)
(require 'seq)
(require 'test-helpers)

(add-to-list 'load-path (expand-file-name "modules" default-directory))

(require 'org/export-epub)

(defmacro hub/org-epub-test--with-export-buffer (org &rest body)
  "Visit a temporary Org file containing ORG, then run BODY."
  (declare (indent 1))
  `(hub/org-epub-test--with-export-file "minimal-book.org" ,org
					,@body))

(defmacro hub/org-epub-test--with-export-file (filename org &rest body)
  "Visit a temporary Org FILENAME containing ORG, then run BODY."
  (declare (indent 2))
  `(let* ((root (make-temp-file "hub-org-epub-test-" t))
	  (source (expand-file-name ,filename root)))
     (make-directory (file-name-directory source) t)
     (with-current-buffer (find-file-noselect source)
       (erase-buffer)
       (insert ,org)
       (save-buffer)
       (let ((default-directory root))
	 ,@body))))

(defun hub/org-epub-test--minimal-org ()
  "Return a minimal Org book used by first-slice export tests."
  (string-join
   '("#+TITLE: Minimal Book"
     "#+AUTHOR: Ada Lovelace"
     "#+EPUB_IDENTIFIER: minimal-book"
     ""
     "This is the first paragraph of the exported book.")
   "\n"))

(defun hub/org-epub-test--latest-run-directory (work-root)
  "Return the single run directory created under WORK-ROOT."
  (let* ((book-dirs (directory-files work-root t "^[^.]"))
	 (run-dirs (and (= 1 (length book-dirs))
			(directory-files (car book-dirs) t "^[^.]"))))
    (should (= 1 (length book-dirs)))
    (should (= 1 (length run-dirs)))
    (car run-dirs)))

(defun hub/org-epub-test--read-json-file (file)
  "Read JSON object from FILE as an alist."
  (let ((json-object-type 'alist)
	(json-array-type 'list))
    (json-read-file file)))

(defun hub/org-epub-test--slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defmacro hub/org-epub-test--stub-pandoc (&rest body)
  "Run BODY with `process-file' stubbed as a successful Pandoc call."
  (declare (indent 0))
  `(let (hub/org-epub-test--pandoc-calls)
     (cl-letf (((symbol-function 'process-file)
		(lambda (program infile destination display &rest args)
		  (push (list :program program :infile infile :destination destination
			      :display display :args args)
			hub/org-epub-test--pandoc-calls)
		  (when-let* ((output-index (cl-position "-o" args :test #'equal))
			      (output-file (nth (1+ output-index) args)))
		    (make-directory (file-name-directory output-file) t)
		    (with-temp-file output-file
		      (insert "stub epub")))
		  0)))
       ,@body)))

(ert-deftest hub/org-epub-export-minimal-book-writes-title-directory ()
  "Export a minimal Org book into the configured title directory."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (let* ((result (hub/org-epub-export-to-epub))
						     (epub-file (plist-get result :epub-file))
						     (title-dir (expand-file-name "Minimal Book" output-root)))
						(should (equal epub-file
							       (expand-file-name "Minimal Book.epub" title-dir)))
						(should (file-exists-p epub-file))))))))

(ert-deftest hub/org-epub-export-minimal-book-generates-title-toc-body-spine ()
  "Exporting the smallest book writes controlled XHTML in spine order."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (hub/org-epub-export-to-epub)
					      (let* ((run-dir (hub/org-epub-test--latest-run-directory work-root))
						     (title-page (expand-file-name "titlepage.xhtml" run-dir))
						     (toc-page (expand-file-name "toc.xhtml" run-dir))
						     (body-page (expand-file-name "body.xhtml" run-dir)))
						(should (file-exists-p title-page))
						(should (file-exists-p toc-page))
						(should (file-exists-p body-page))
						(should (string-match-p "<h1[^>]*>Minimal Book</h1>"
									(with-temp-buffer
									  (insert-file-contents title-page)
									  (buffer-string))))
						(should (string-match-p "This is the first paragraph"
									(with-temp-buffer
									  (insert-file-contents body-page)
									  (buffer-string))))))))))

(ert-deftest hub/org-epub-export-invokes-pandoc-with-argument-list ()
  "The exporter invokes Pandoc directly with XHTML inputs in spine order."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (hub/org-epub-export-to-epub)
					      (let* ((call (car hub/org-epub-test--pandoc-calls))
						     (args (plist-get call :args))
						     (input-names (mapcar #'file-name-nondirectory
									  (seq-filter (lambda (arg)
											(string-match-p "\\.xhtml\\'" arg))
										      args))))
						(should (equal (plist-get call :program) "pandoc"))
						(should (member "-o" args))
						(should (equal input-names
							       '("titlepage.xhtml" "toc.xhtml" "body.xhtml")))))))))

(ert-deftest hub/org-epub-title-page-localizes-org-date ()
  "EPUB title page formats Org dates by document language."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Dated Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: dated-book"
		    "#+LANGUAGE: fr"
		    "#+DATE: [2026-07-11 Sat 07:01]"
		    ""
		    "Body.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (titlepage (hub/org-epub-test--slurp
			   (expand-file-name "titlepage.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "11 juillet 2026" titlepage))
	  (should-not (string-match-p "2026-07-11" titlepage))))))))

(ert-deftest hub/org-epub-denote-stem-provides-identifier-title-subjects ()
  "Denote filenames provide fallback title, stable identifier, and subjects."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-file
     "20260719T123000--my-book-title__ebook_draft.org"
     (string-join '("#+AUTHOR: Ada Lovelace"
		    "#+KEYWORDS: alpha, beta"
		    ""
		    "Body text.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (metadata (hub/org-epub-test--read-json-file
			  (expand-file-name "metadata.json"
					    (file-name-directory (plist-get result :epub-file))))))
	  (should (equal (plist-get result :epub-file)
			 (expand-file-name "My Book Title/My Book Title.epub" output-root)))
	  (should (equal (alist-get 'identifier metadata)
			 "20260719T123000--my-book-title"))
	  (should (equal (alist-get 'title metadata) "My Book Title"))
	  (should (equal (alist-get 'subjects metadata)
			 '("ebook" "draft" "alpha" "beta")))))))))

(ert-deftest hub/org-epub-requires-epub-identifier-without-denote ()
  "Strict export requires `EPUB_IDENTIFIER' when no Denote stem is available."
  (hub/org-epub-test--with-export-file
   "plain-book.org"
   (string-join '("#+TITLE: Plain Book"
		  "#+AUTHOR: Ada Lovelace"
		  ""
		  "Body text.")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error))))

(ert-deftest hub/org-epub-infers-author-from-user-full-name ()
  "Missing `AUTHOR' falls back to `user-full-name' and records it."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Inferred Author"
		    "#+EPUB_IDENTIFIER: inferred-author"
		    ""
		    "Body text.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc")
	   (user-full-name "Ada Lovelace"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (metadata (hub/org-epub-test--read-json-file
			  (expand-file-name "metadata.json"
					    (file-name-directory (plist-get result :epub-file))))))
	  (should (equal (alist-get 'author metadata) "Ada Lovelace"))
	  (should (eq (alist-get 'author_inferred metadata) t))))))))

(ert-deftest hub/org-epub-prompts-before-interactive-overwrite ()
  "Interactive export prompts before overwriting an existing EPUB file."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t))
	(prompted nil))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let* ((hub/org-epub-output-root output-root)
						  (hub/org-epub-work-root work-root)
						  (hub/org-epub-pandoc-executable "pandoc")
						  (target-dir (expand-file-name "Minimal Book" output-root))
						  (target (expand-file-name "Minimal Book.epub" target-dir))
						  (noninteractive nil))
					     (make-directory target-dir t)
					     (with-temp-file target
					       (insert "old epub"))
					     (cl-letf (((symbol-function 'y-or-n-p)
							(lambda (_prompt)
							  (setq prompted t)
							  t)))
					       (hub/org-epub-test--stub-pandoc
						(hub/org-epub-export-to-epub)
						(should prompted)))))))

(ert-deftest hub/org-epub-cover-is-packaged-and-visible-first-page ()
  "A configured EPUB cover is copied and passed as Pandoc cover metadata."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Covered Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: covered-book"
		    "#+EPUB_COVER: cover.png"
		    ""
		    "Body text.")
		  "\n")
     (with-temp-file (expand-file-name "cover.png" default-directory)
       (insert "fake png"))
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (run-dir (plist-get result :work-dir))
	       (metadata (hub/org-epub-test--read-json-file
			  (expand-file-name "metadata.json"
					    (file-name-directory (plist-get result :epub-file)))))
	       (args (plist-get (car hub/org-epub-test--pandoc-calls) :args))
	       (input-names (mapcar #'file-name-nondirectory
				    (seq-filter (lambda (arg)
						  (string-match-p "\\.xhtml\\'" arg))
						args))))
	  (should (file-exists-p (expand-file-name "cover.png"
						   (file-name-directory (plist-get result :epub-file)))))
	  (should (file-exists-p (expand-file-name "cover.xhtml" run-dir)))
	  (should (member "--epub-cover-image" args))
	  (should (member "cover.png" args))
	  (should (equal input-names '("titlepage.xhtml" "toc.xhtml" "body.xhtml")))
	  (should (equal (alist-get 'cover metadata) "cover.png"))))))))

(ert-deftest hub/org-epub-cover-absent-is-informational ()
  "Missing cover does not block strict export and is recorded in metadata."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (let* ((result (hub/org-epub-export-to-epub))
						     (metadata (hub/org-epub-test--read-json-file
								(expand-file-name "metadata.json"
										  (file-name-directory (plist-get result :epub-file))))))
						(should (eq (alist-get 'cover metadata) :json-false))))))))

(ert-deftest hub/org-epub-standfirst-becomes-frontmatter-and-is-removed-from-body ()
  "The first standfirst block becomes its own page and leaves body flow."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Standfirst Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: standfirst-book"
		    ""
		    "#+begin_standfirst"
		    "A calm lead page."
		    "#+end_standfirst"
		    ""
		    "Body text.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (run-dir (plist-get result :work-dir))
	       (standfirst (expand-file-name "standfirst.xhtml" run-dir))
	       (body (expand-file-name "body.xhtml" run-dir)))
	  (should (string-match-p "A calm lead page" (hub/org-epub-test--slurp standfirst)))
	  (should-not (string-match-p "A calm lead page" (hub/org-epub-test--slurp body)))))))))

(ert-deftest hub/org-epub-multiple-standfirsts-fail-strict ()
  "Strict export rejects multiple standfirst blocks."
  (hub/org-epub-test--with-export-buffer
   (string-join '("#+TITLE: Standfirst Book"
		  "#+AUTHOR: Ada Lovelace"
		  "#+EPUB_IDENTIFIER: standfirst-book"
		  ""
		  "#+begin_standfirst"
		  "First."
		  "#+end_standfirst"
		  ""
		  "#+begin_standfirst"
		  "Second."
		  "#+end_standfirst")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error))))

(ert-deftest hub/org-epub-footer-note-is-backmatter-page ()
  "Footer note metadata renders as a final backmatter page."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Footer Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: footer-book"
		    "#+EXPORT_FOOTER_NOTE: Private circulation."
		    ""
		    "Body text.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (run-dir (plist-get result :work-dir))
	       (args (plist-get (car hub/org-epub-test--pandoc-calls) :args))
	       (input-names (mapcar #'file-name-nondirectory
				    (seq-filter (lambda (arg)
						  (string-match-p "\\.xhtml\\'" arg))
						args))))
	  (should (string-match-p "Private circulation"
				  (hub/org-epub-test--slurp
				   (expand-file-name "footer-note.xhtml" run-dir))))
	  (should (equal (car (last input-names)) "footer-note.xhtml"))))))))

(ert-deftest hub/org-epub-localizes-visible-toc-title ()
  "French EPUB exports use a localized visible table-of-contents title."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Livre"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: livre"
		    "#+LANGUAGE: fr"
		    ""
		    "Corps.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (toc (expand-file-name "toc.xhtml" (plist-get result :work-dir))))
	  (should (string-match-p "Table des matières"
				  (hub/org-epub-test--slurp toc)))))))))

(ert-deftest hub/org-epub-visible-toc-lists-level-one-chapters ()
  "Visible EPUB TOC lists generated level-one chapters."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: TOC Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: toc-book"
		    ""
		    "* First"
		    "One."
		    ""
		    "* Second"
		    "Two.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (toc-html (hub/org-epub-test--slurp
			  (expand-file-name "toc.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<ol" toc-html))
	  (should (string-match-p "href=\"#first\"" toc-html))
	  (should-not (string-match-p "chapter-1.xhtml#first" toc-html))
	  (should (string-match-p "First" toc-html))
	  (should (string-match-p "href=\"#second\"" toc-html))))))))

(ert-deftest hub/org-epub-splits-level-one-headings-into-chapters ()
  "Level-one Org headings become separate chapter XHTML files."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Chaptered Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: chaptered-book"
		    ""
		    "* First Chapter"
		    "First body."
		    ""
		    "* Second Chapter"
		    "Second body.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (run-dir (plist-get result :work-dir))
	       (args (plist-get (car hub/org-epub-test--pandoc-calls) :args))
	       (input-names (mapcar #'file-name-nondirectory
				    (seq-filter (lambda (arg)
						  (string-match-p "\\.xhtml\\'" arg))
						args))))
	  (should (member "chapter-1.xhtml" input-names))
	  (should (member "chapter-2.xhtml" input-names))
	  (should (string-match-p "<h1[^>]*>First Chapter</h1>"
				  (hub/org-epub-test--slurp (expand-file-name "chapter-1.xhtml" run-dir))))
	  (should (string-match-p "Second body"
				  (hub/org-epub-test--slurp (expand-file-name "chapter-2.xhtml" run-dir))))))))))

(ert-deftest hub/org-epub-preheading-prose-becomes-visible-introduction-only-before-chapters ()
  "Pre-heading prose becomes a visible introduction only when chapters follow."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Intro Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: intro-book"
		    ""
		    "Opening prose."
		    ""
		    "* Chapter"
		    "Chapter prose.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (intro (expand-file-name "introduction.xhtml" (plist-get result :work-dir))))
	  (should (string-match-p "<h1[^>]*>Introduction</h1>" (hub/org-epub-test--slurp intro)))
	  (should (string-match-p "Opening prose" (hub/org-epub-test--slurp intro)))))))))

(ert-deftest hub/org-epub-no-heading-document-does-not-repeat-title ()
  "A no-heading document keeps body prose without a duplicate title heading."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (let* ((result (hub/org-epub-export-to-epub))
						     (body (hub/org-epub-test--slurp
							    (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
						(should-not (string-match-p "<h1[^>]*>Minimal Book</h1>" body))))))))

(ert-deftest hub/org-epub-rewrites-headline-links-across-chapters ()
  "Links to headings point at generated chapter files and anchors."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Linked Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: linked-book"
		    ""
		    "* First"
		    "See [[*Second][the next chapter]]."
		    ""
		    "* Second"
		    "Target.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (first (hub/org-epub-test--slurp
		       (expand-file-name "chapter-1.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "href=\"chapter-2.xhtml#second\"" first))))))))

(ert-deftest hub/org-epub-broken-internals-fail-strict ()
  "Broken internal headline links fail strict export."
  (hub/org-epub-test--with-export-buffer
   (string-join '("#+TITLE: Broken Links"
		  "#+AUTHOR: Ada Lovelace"
		  "#+EPUB_IDENTIFIER: broken-links"
		  ""
		  "* First"
		  "See [[*Missing][missing]].")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error))))

(ert-deftest hub/org-epub-renders-basic-inline-and-block-org-semantics ()
  "Core Org inline markup, lists, quotes, rules, and tables render as XHTML."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Semantic Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: semantic-book"
		    ""
		    "This has *bold*, /italic/, ~code~, and [[https://example.com][a link]]."
		    ""
		    "- one"
		    "- two"
		    ""
		    "#+begin_quote"
		    "Quoted text."
		    "#+end_quote"
		    ""
		    "-----"
		    ""
		    "| Name | Score |"
		    "|------+-------|"
		    "| Ada  | 10    |")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<strong>bold</strong>" body))
	  (should (string-match-p "<em>italic</em>" body))
	  (should (string-match-p "<code>code</code>" body))
	  (should (string-match-p "<a href=\"https://example.com\">a link</a>" body))
	  (should (string-match-p "<ul[^>]*>" body))
	  (should (string-match-p "<blockquote>" body))
	  (should (string-match-p "<hr class=\"section-break\"" body))
	  (should (string-match-p "<table" body))))))))

(ert-deftest hub/org-epub-source-blocks-use-pre-code-language-class ()
  "Source blocks render as plain pre/code with a semantic language class."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Code Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: code-book"
		    ""
		    "#+begin_src emacs-lisp"
		    "(message \"hi\")"
		    "#+end_src")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<pre><code class=\"language-emacs-lisp\">" body))
	  (should (string-match-p "(message" body))))))))

(ert-deftest hub/org-epub-omits-drawers-comments-planning-and-strips-todo-tags ()
  "Book output omits authoring metadata from visible XHTML."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Clean Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: clean-book"
		    ""
		    "* TODO Chapter :draft:"
		    "SCHEDULED: <2026-07-19 Sun>"
		    ":PROPERTIES:"
		    ":CUSTOM_ID: chapter"
		    ":END:"
		    "# hidden comment"
		    "Visible prose.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (chapter (hub/org-epub-test--slurp
			 (expand-file-name "chapter-1.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<h1[^>]*>Chapter</h1>" chapter))
	  (should (string-match-p "Visible prose" chapter))
	  (should-not (string-match-p "TODO\|draft\|SCHEDULED\|PROPERTIES\|hidden comment" chapter))))))))

(ert-deftest hub/org-epub-copies-reader-css-from-etc-epub ()
  "Successful exports copy the tracked reader CSS into the work directory."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (let* ((result (hub/org-epub-export-to-epub))
						     (css (expand-file-name "reader.css" (plist-get result :work-dir))))
						(should (file-exists-p css))
						(should (string-match-p "font-family: serif" (hub/org-epub-test--slurp css)))))))))

(ert-deftest hub/org-epub-xhtml-references-reader-css ()
  "Generated XHTML pages reference the copied reader stylesheet."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (let* ((result (hub/org-epub-export-to-epub))
						     (titlepage (hub/org-epub-test--slurp
								 (expand-file-name "titlepage.xhtml" (plist-get result :work-dir)))))
						(should (string-match-p "rel=\"stylesheet\"" titlepage))
						(should (string-match-p "href=\"reader.css\"" titlepage))))))))

(ert-deftest hub/org-epub-reader-css-uses-generic-fonts-and-page-break-hints ()
  "The default reader CSS stays conservative and page-oriented."
  (let ((css (hub/org-epub-test--slurp
	      (expand-file-name "etc/epub/reader.css" default-directory))))
    (should (string-match-p "font-family: serif" css))
    (should (string-match-p "font-family: monospace" css))
    (should (string-match-p "break-before: page" css))
    (should-not (string-match-p "@font-face" css))))

(ert-deftest hub/org-epub-reader-css-italicizes-quotes-and-unitalicizes-emphasis ()
  "The default reader CSS italicizes quotes but reverses emphasized spans."
  (let ((css (hub/org-epub-test--slurp
	      (expand-file-name "etc/epub/reader.css" default-directory))))
    (should (string-match-p "blockquote[[:ascii:][:nonascii:]]*font-style: italic" css))
    (should (string-match-p "blockquote em[[:ascii:][:nonascii:]]*font-style: normal" css))))

(ert-deftest hub/org-epub-reader-css-keeps-attributed-quote-caption-inside-border ()
  "Attributed quote captions share the quote border and align right."
  (let ((css (hub/org-epub-test--slurp
	      (expand-file-name "etc/epub/reader.css" default-directory))))
    (should (string-match-p "figure\\.quote[[:ascii:][:nonascii:]]*border-left:" css))
    (should (string-match-p "figure\\.quote blockquote[[:ascii:][:nonascii:]]*border-left: 0" css))
    (should (string-match-p "figure\\.quote figcaption[[:ascii:][:nonascii:]]*text-align: right" css))))

(ert-deftest hub/org-epub-reader-css-gives-callouts-background ()
  "The default reader CSS gives callouts a visible background."
  (let ((css (hub/org-epub-test--slurp
	      (expand-file-name "etc/epub/reader.css" default-directory))))
    (should (string-match-p "\\.callout[[:ascii:][:nonascii:]]*background-color:" css))))

(ert-deftest hub/org-epub-reader-css-uses-sans-serif-callouts-and-rules ()
  "The default reader CSS distinguishes callouts with sans-serif text and rules."
  (let ((css (hub/org-epub-test--slurp
	      (expand-file-name "etc/epub/reader.css" default-directory))))
    (should (string-match-p "\\.callout[[:ascii:][:nonascii:]]*font-family: sans-serif" css))
    (should (string-match-p "\\.callout-rule[[:ascii:][:nonascii:]]*border-top:" css))))

(ert-deftest hub/org-epub-reader-css-styles-callout-labels-bold-small-caps ()
  "The default reader CSS makes callout labels bold pseudo-small-caps."
  (let ((css (hub/org-epub-test--slurp
	      (expand-file-name "etc/epub/reader.css" default-directory))))
    (should (string-match-p "\\.callout-label[[:ascii:][:nonascii:]]*font-weight: bold" css))
    (should (string-match-p "\\.callout-label[[:ascii:][:nonascii:]]*letter-spacing: 0.04em" css))
    (should (string-match-p "\\.callout-label[[:ascii:][:nonascii:]]*text-transform: uppercase" css))
    (should-not (string-match-p "font-variant: small-caps" css))))

(ert-deftest hub/org-epub-generated-xhtml-avoids-primary-inline-styles ()
  "Generated XHTML relies on classes and CSS, not primary inline style attrs."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (let* ((result (hub/org-epub-export-to-epub))
						     (titlepage (hub/org-epub-test--slurp
								 (expand-file-name "titlepage.xhtml" (plist-get result :work-dir)))))
						(should-not (string-match-p "style=\"" titlepage))))))))

(ert-deftest hub/org-epub-callout-renders-aside-with-localized-label-title ()
  "Callout blocks render as semantic asides with type and title."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Callout Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: callout-book"
		    ""
		    "#+ATTR_CALLOUT: :type warning :title \"Heads up\""
		    "#+begin_callout"
		    "Careful now."
		    "#+end_callout")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<aside class=\"callout callout-warning\"" body))
	  (should (string-match-p "<hr class=\"callout-rule callout-rule-before\" />" body))
	  (should (string-match-p "<p class=\"callout-label\"><strong>WARNING: HEADS UP</strong></p>" body))
	  (should (string-match-p "Careful now" body))
	  (should (string-match-p "<hr class=\"callout-rule callout-rule-after\" />" body))))))))

(ert-deftest hub/org-epub-note-callout-title-omits-type ()
  "Note callouts with titles render only the title in the label."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Note Callout Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: note-callout-book"
		    ""
		    "#+ATTR_CALLOUT: :type note :title \"Remember\""
		    "#+begin_callout"
		    "Keep this."
		    "#+end_callout")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<p class=\"callout-label\"><strong>REMEMBER</strong></p>" body))
	  (should-not (string-match-p "<p class=\"callout-label\"><strong>NOTE" body))))))))

(ert-deftest hub/org-epub-french-callout-label-uses-space-before-colon ()
  "French non-note callouts put a space before the colon."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Avertissement"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: avertissement"
		    "#+LANGUAGE: fr"
		    ""
		    "#+ATTR_CALLOUT: :type warning :title \"Attention\""
		    "#+begin_callout"
		    "Prudence."
		    "#+end_callout")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<p class=\"callout-label\"><strong>ATTENTION : ATTENTION</strong></p>" body))))))))

(ert-deftest hub/org-epub-french-tip-callout-label-is-localized ()
  "French tip callouts use the localized Astuce type label."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Astuce"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: astuce"
		    "#+LANGUAGE: fr"
		    ""
		    "#+ATTR_CALLOUT: :type tip :title \"À retenir\""
		    "#+begin_callout"
		    "Prudence."
		    "#+end_callout")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<p class=\"callout-label\"><strong>ASTUCE : À RETENIR</strong></p>" body))))))))

(ert-deftest hub/org-epub-unknown-callout-type-fails-strict ()
  "Unknown callout types fail strict export."
  (hub/org-epub-test--with-export-buffer
   (string-join '("#+TITLE: Bad Callout"
		  "#+AUTHOR: Ada Lovelace"
		  "#+EPUB_IDENTIFIER: bad-callout"
		  ""
		  "#+ATTR_CALLOUT: :type danger"
		  "#+begin_callout"
		  "Nope."
		  "#+end_callout")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error))))

(ert-deftest hub/org-epub-attributed-quote-renders-figure ()
  "Attributed quotes render as figure, blockquote, and figcaption."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Quote Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: quote-book"
		    ""
		    "#+ATTR_QUOTE: :author \"Grace Hopper\""
		    "#+begin_quote"
		    "The quote."
		    "#+end_quote")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<figure class=\"quote\">" body))
	  (should (string-match-p "<figcaption>— Grace Hopper</figcaption>" body))))))))

(ert-deftest hub/org-epub-metric-value-prefers-attr-epub-with-latex-options-fallback ()
  "Metric blocks render values from ATTR_EPUB or legacy ATTR_LATEX options."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Metric Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: metric-book"
		    ""
		    "#+ATTR_EPUB: :value \"230+\""
		    "#+begin_metric"
		    "Regions covered"
		    "#+end_metric"
		    ""
		    "#+ATTR_LATEX: :options [98%]"
		    "#+begin_metric"
		    "Accuracy"
		    "#+end_metric")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "<aside class=\"metric\">" body))
	  (should (string-match-p "metric-value\">230+" body))
	  (should (string-match-p "metric-value\">98%" body))))))))

(ert-deftest hub/org-epub-graph-image-fallback-packages-figure ()
  "Graph blocks with ATTR_EPUB image fallbacks render as packaged figures."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Graph Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: graph-book"
		    ""
		    "#+ATTR_EPUB: :image graph.png :alt \"Growth chart\""
		    "#+begin_graph"
		    "Graph description."
		    "#+end_graph")
		  "\n")
     (with-temp-file (expand-file-name "graph.png" default-directory)
       (insert "fake png"))
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (file-exists-p (expand-file-name "graph.png" (plist-get result :work-dir))))
	  (should (string-match-p "<figure class=\"graph\">" body))
	  (should (string-match-p "alt=\"Growth chart\"" body))))))))

(ert-deftest hub/org-epub-latex-only-graph-fails-strict ()
  "LaTeX-only graph blocks need an EPUB fallback image."
  (hub/org-epub-test--with-export-buffer
   (string-join '("#+TITLE: Latex Graph"
		  "#+AUTHOR: Ada Lovelace"
		  "#+EPUB_IDENTIFIER: latex-graph"
		  ""
		  "#+begin_graph"
		  "#+begin_export latex"
		  "\\begin{tikzpicture}\\end{tikzpicture}"
		  "#+end_export"
		  "#+end_graph")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error))))

(ert-deftest hub/org-epub-packages-local-standalone-and-captioned-images ()
  "Local standalone and captioned images are copied and embedded."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Image Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: image-book"
		    ""
		    "[[./plain.png]]"
		    ""
		    "#+CAPTION: Architecture overview"
		    "[[./captioned.png]]")
		  "\n")
     (with-temp-file (expand-file-name "plain.png" default-directory)
       (insert "plain"))
     (with-temp-file (expand-file-name "captioned.png" default-directory)
       (insert "captioned"))
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (work-dir (plist-get result :work-dir))
	       (body (hub/org-epub-test--slurp (expand-file-name "body.xhtml" work-dir))))
	  (should (file-exists-p (expand-file-name "plain.png" work-dir)))
	  (should (file-exists-p (expand-file-name "captioned.png" work-dir)))
	  (should (string-match-p "src=\"plain.png\"" body))
	  (should (string-match-p "src=\"captioned.png\"" body))
	  (should (string-match-p "Architecture overview" body))))))))

(ert-deftest hub/org-epub-described-image-link-stays-hyperlink ()
  "Described image links are hyperlinks, not embedded figures."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Image Link Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: image-link-book"
		    ""
		    "[[./diagram.png][Open diagram]]")
		  "\n")
     (with-temp-file (expand-file-name "diagram.png" default-directory)
       (insert "diagram"))
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (work-dir (plist-get result :work-dir))
	       (body (hub/org-epub-test--slurp (expand-file-name "body.xhtml" work-dir))))
	  (should (string-match-p "<a href=\"./diagram.png\">Open diagram</a>" body))
	  (should-not (file-exists-p (expand-file-name "diagram.png" work-dir)))))))))

(ert-deftest hub/org-epub-alt-priority-uses-attr-caption-filename-report ()
  "Image alt text prefers ATTR_EPUB alt, then caption, then filename fallback."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Alt Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: alt-book"
		    ""
		    "#+ATTR_EPUB: :alt \"Explicit alt\""
		    "[[./explicit.png]]"
		    ""
		    "#+CAPTION: Caption alt"
		    "[[./caption.png]]"
		    ""
		    "[[./fallback.png]]")
		  "\n")
     (dolist (file '("explicit.png" "caption.png" "fallback.png"))
       (with-temp-file (expand-file-name file default-directory)
	 (insert file)))
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "alt=\"Explicit alt\"" body))
	  (should (string-match-p "alt=\"Caption alt\"" body))
	  (should (string-match-p "alt=\"fallback.png\"" body))))))))

(ert-deftest hub/org-epub-remote-standalone-image-fails-strict ()
  "Standalone remote image URLs fail strict export."
  (hub/org-epub-test--with-export-buffer
   (string-join '("#+TITLE: Remote Image"
		  "#+AUTHOR: Ada Lovelace"
		  "#+EPUB_IDENTIFIER: remote-image"
		  ""
		  "[[https://example.com/image.png]]")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error))))

(ert-deftest hub/org-epub-local-non-image-and-audio-links-fail-strict ()
  "Local non-image and audio links fail strict export."
  (hub/org-epub-test--with-export-buffer
   (string-join '("#+TITLE: Unsupported Links"
		  "#+AUTHOR: Ada Lovelace"
		  "#+EPUB_IDENTIFIER: unsupported-links"
		  ""
		  "[[./appendix.pdf][Appendix]]"
		  "[[./audio.mp3][Audio]]")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error))))

(ert-deftest hub/org-epub-ordinary-footnotes-render-linked-endnotes ()
  "Ordinary Org footnotes render as linked EPUB endnotes."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Notes Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: notes-book"
		    ""
		    "A claim[fn:one]."
		    ""
		    "[fn:one] Supporting note.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "id=\"noteref-one\"" body))
	  (should (string-match-p "href=\"#note-one\"" body))
	  (should (string-match-p "<section class=\"endnotes\" epub:type=\"endnotes\">" body))
	  (should (string-match-p "id=\"note-one\"" body))
	  (should (string-match-p "Supporting note" body))))))))

(ert-deftest hub/org-epub-forced-footnotes-render-linked-endnotes ()
  "HUB_NOTE_KIND footnote definitions render as linked EPUB endnotes."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Forced Note Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: forced-note-book"
		    ""
		    "A claim[fn:forced]."
		    ""
		    "[fn:forced] Forced note."
		    ":HUB_NOTE_KIND: footnote")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "id=\"noteref-forced\"" body))
	  (should (string-match-p "Forced note" body))
	  (should-not (string-match-p "HUB_NOTE_KIND" body))))))))

(ert-deftest hub/org-epub-marginalia-renders-endnote-with-degradation-report ()
  "Marginalia notes degrade to linked endnotes with a report item."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Marginalia Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: marginalia-book"
		    ""
		    "A margin thought[fn:margin]."
		    ""
		    "[fn:margin] Marginal note."
		    ":HUB_NOTE_KIND: marginalia")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (work-dir (plist-get result :work-dir))
	       (body (hub/org-epub-test--slurp (expand-file-name "body.xhtml" work-dir)))
	       (report (hub/org-epub-test--slurp (expand-file-name "notes-report.json" work-dir))))
	  (should (string-match-p "Marginal note" body))
	  (should (string-match-p "marginalia" report))
	  (should (string-match-p "degraded-to-endnote" report))))))))

(ert-deftest hub/org-epub-footnote-definition-is-single-inline-paragraph ()
  "Generated EPUB endnote definitions keep marker and body in one paragraph."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Inline Note Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: inline-note-book"
		    ""
		    "A claim[fn:one]."
		    ""
		    "[fn:one] Supporting note.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p
		   "<p class=\"footdef\"><sup><a[^>]+>1</a></sup> Supporting note.</p>"
		   body))
	  (should-not (string-match-p "class=\"footpara\"" body))))))))

(ert-deftest hub/org-epub-notes-include-backlinks ()
  "Generated EPUB endnotes include backlinks to note references."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Backlinks Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: backlinks-book"
		    ""
		    "A claim[fn:one]."
		    ""
		    "[fn:one] Supporting note.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (body (hub/org-epub-test--slurp
		      (expand-file-name "body.xhtml" (plist-get result :work-dir)))))
	  (should (string-match-p "href=\"#noteref-one\"" body))
	  (should (string-match-p "class=\"note-backref\"" body))))))))

(ert-deftest hub/org-epub-cross-chapter-footnote-definition-is-available ()
  "Chapter exports can resolve footnote definitions kept later in the file."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Late Footnote Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: late-footnote-book"
		    ""
		    "* First"
		    "A claim[fn:1]."
		    ""
		    "* Second"
		    "Later chapter."
		    ""
		    "[fn:1] Late note definition.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (work-dir (plist-get result :work-dir))
	       (first (hub/org-epub-test--slurp (expand-file-name "chapter-1.xhtml" work-dir))))
	  (should (string-match-p "Late note definition" first))
	  (should (string-match-p "id=\"note-1\"" first))))))))

(ert-deftest hub/org-epub-prefers-chapter-local-endnotes ()
  "Chapter exports keep endnotes local to the chapter XHTML file."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Chapter Notes Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: chapter-notes-book"
		    ""
		    "* First"
		    "A claim[fn:first]."
		    ""
		    "[fn:first] First chapter note."
		    ""
		    "* Second"
		    "No note here.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (work-dir (plist-get result :work-dir))
	       (first (hub/org-epub-test--slurp (expand-file-name "chapter-1.xhtml" work-dir)))
	       (second (hub/org-epub-test--slurp (expand-file-name "chapter-2.xhtml" work-dir))))
	  (should (string-match-p "First chapter note" first))
	  (should (string-match-p "<section class=\"endnotes\"" first))
	  (should-not (string-match-p "First chapter note" second))))))))

(ert-deftest hub/org-epub-strict-failure-opens-human-preflight-buffer ()
  "Strict preflight failures render an actionable human report buffer."
  (hub/org-epub-test--with-export-buffer
   (string-join '("#+TITLE: Strict Failure Book"
		  "#+AUTHOR: Ada Lovelace"
		  "#+EPUB_IDENTIFIER: strict-failure-book"
		  ""
		  "[[https://example.com/image.png]]")
		"\n")
   (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
	 (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
	 (hub/org-epub-pandoc-executable "pandoc"))
     (should-error (hub/org-epub-export-to-epub) :type 'user-error)
     (with-current-buffer "*Org EPUB Preflight*"
       (should (string-match-p "Org EPUB Preflight" (buffer-string)))
       (should (string-match-p "standalone remote images" (buffer-string)))))))

(ert-deftest hub/org-epub-permissive-export-continues-with-degradations ()
  "Permissive export continues after degradable preflight items."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Permissive Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: permissive-book"
		    ""
		    "[[https://example.com/image.png]]")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub t))
	       (work-dir (plist-get result :work-dir))
	       (body (hub/org-epub-test--slurp (expand-file-name "body.xhtml" work-dir)))
	       (report (hub/org-epub-test--slurp (expand-file-name "preflight.json" work-dir))))
	  (should (string-match-p "<a href=\"https://example.com/image.png\">" body))
	  (should (string-match-p "remote-image-degraded-to-link" report))))))))

(ert-deftest hub/org-epub-batch-writes-preflight-json ()
  "Batch exports write structured preflight JSON into the work directory."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (let* ((result (hub/org-epub-export-to-epub))
						     (report-file (expand-file-name "preflight.json" (plist-get result :work-dir))))
						(should (file-exists-p report-file))))))))

(ert-deftest hub/org-epub-report-contains-status-counts-and-items ()
  "Preflight JSON contains status, counts, and item details."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Report Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: report-book"
		    ""
		    "A thought[fn:margin]."
		    ""
		    "[fn:margin] Marginal note."
		    ":HUB_NOTE_KIND: marginalia")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(let* ((result (hub/org-epub-export-to-epub))
	       (report (hub/org-epub-test--read-json-file
			(expand-file-name "preflight.json" (plist-get result :work-dir))))
	       (counts (alist-get 'counts report))
	       (items (alist-get 'items report)))
	  (should (equal "degraded" (alist-get 'status report)))
	  (should (= 1 (alist-get 'warning counts)))
	  (should (string= "marginalia-degraded-to-endnote"
			   (alist-get 'code (car items))))))))))

(ert-deftest hub/org-epub-success-message-includes-final-path-only ()
  "Successful export message only announces the final EPUB path."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t))
	messages)
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable "pandoc"))
					     (hub/org-epub-test--stub-pandoc
					      (cl-letf (((symbol-function 'message)
							 (lambda (format-string &rest args)
							   (push (apply #'format format-string args) messages))))
						(let* ((result (hub/org-epub-export-to-epub))
						       (final-message (car messages)))
						  (should (equal (format "Exported EPUB: %s" (plist-get result :epub-file))
								 final-message))
						  (should-not (string-match-p (regexp-quote (plist-get result :work-dir))
									      final-message)))))))))

(ert-deftest hub/org-epub-missing-pandoc-errors-actionably ()
  "Missing Pandoc produces an actionable user error."
  (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					 (let ((hub/org-epub-output-root (make-temp-file "hub-org-epub-output-" t))
					       (hub/org-epub-work-root (make-temp-file "hub-org-epub-work-" t))
					       (hub/org-epub-pandoc-executable nil))
					   (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
					     (should-error (hub/org-epub-export-to-epub) :type 'user-error)))))

(ert-deftest hub/org-epub-pandoc-receives-spine-in-order ()
  "Pandoc receives generated spine XHTML files in reading order."
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Ordered Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: ordered-book"
		    ""
		    "* First"
		    "One."
		    ""
		    "* Second"
		    "Two.")
		  "\n")
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable "pandoc"))
       (hub/org-epub-test--stub-pandoc
	(hub/org-epub-export-to-epub)
	(let* ((args (plist-get (car hub/org-epub-test--pandoc-calls) :args))
	       (xhtmls (seq-filter (lambda (arg) (string-match-p "\\.xhtml\\'" arg)) args)))
	  (should (equal (mapcar #'file-name-nondirectory xhtmls)
			 '("titlepage.xhtml" "toc.xhtml" "chapter-1.xhtml" "chapter-2.xhtml")))))))))

(ert-deftest hub/org-epub-real-pandoc-test-skips-when-pandoc-missing ()
  "Real Pandoc smoke test is skipped when Pandoc is not available."
  (skip-unless (executable-find "pandoc"))
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer (hub/org-epub-test--minimal-org)
					   (let ((hub/org-epub-output-root output-root)
						 (hub/org-epub-work-root work-root)
						 (hub/org-epub-pandoc-executable nil))
					     (let* ((result (hub/org-epub-export-to-epub))
						    (epub-file (plist-get result :epub-file)))
					       (should (file-exists-p epub-file))
					       (should (> (file-attribute-size (file-attributes epub-file)) 0)))))))

(ert-deftest hub/org-epub-real-package-contains-css-cover-xhtml-metadata-nav ()
  "Real Pandoc package contains EPUB resources produced by the exporter."
  (skip-unless (and (executable-find "pandoc") (executable-find "unzip")))
  (let ((output-root (make-temp-file "hub-org-epub-output-" t))
	(work-root (make-temp-file "hub-org-epub-work-" t)))
    (hub/org-epub-test--with-export-buffer
     (string-join '("#+TITLE: Real Package Book"
		    "#+AUTHOR: Ada Lovelace"
		    "#+EPUB_IDENTIFIER: real-package-book"
		    "#+EPUB_COVER: cover.png"
		    ""
		    "* First"
		    "Body."
		    ""
		    "* Second"
		    "More body.")
		  "\n")
     (with-temp-file (expand-file-name "cover.png" default-directory)
       (set-buffer-file-coding-system 'binary)
       (insert (base64-decode-string
		"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/p9sAAAAASUVORK5CYII=")))
     (let ((hub/org-epub-output-root output-root)
	   (hub/org-epub-work-root work-root)
	   (hub/org-epub-pandoc-executable nil))
       (let* ((result (hub/org-epub-export-to-epub))
	      (listing (with-temp-buffer
			 (process-file "unzip" nil t nil "-l" (plist-get result :epub-file))
			 (buffer-string)))
	      (visible-toc (with-temp-buffer
			     (process-file "unzip" nil t nil "-p" (plist-get result :epub-file) "EPUB/text/ch002.xhtml")
			     (buffer-string))))
	 (should (string-match-p "EPUB/styles/.*\\.css" listing))
	 (should (string-match-p "EPUB/media/.*\\.png" listing))
	 (should (string-match-p "content.opf" listing))
	 (should (string-match-p "nav.xhtml" listing))
	 (should (string-match-p "xhtml" listing))
	 (should (string-match-p "href=\"ch003.xhtml#first\"" visible-toc))
	 (should (string-match-p "href=\"ch004.xhtml#second\"" visible-toc))
	 (should-not (string-match-p "chapter-1.xhtml" visible-toc)))))))

(ert-deftest hub/org-epub-backend-has-dispatch-menu ()
  "EPUB export registers an Org export backend menu entry."
  (should (org-export-get-backend 'hub-epub))
  (let ((menu (org-export-backend-menu (org-export-get-backend 'hub-epub))))
    (should (equal (car menu) ?E))
    (should (string-match-p "EPUB" (cadr menu)))))

(provide 'org-epub-export-test)
;;; org-epub-export-test.el ends here
