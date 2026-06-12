;;; org-confluence-export-test.el --- Tests for Org Confluence export -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavior tests for the Org -> Confluence Storage Format backend.

;;; Code:

(require 'ert)
(require 'org)
(require 'ox)

;; Ensure repo modules are reachable for isolated batch test runners.
(let ((root (file-name-as-directory
	     (locate-dominating-file (or load-file-name buffer-file-name)
				     "domains.yaml"))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root))
  (add-to-list 'load-path (expand-file-name "packages/org-confluence" root))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "core" root)))

(load "export" nil 'nomessage)

(defun hub/org-confluence-test--export (org)
  "Return Confluence XHTML exported from ORG."
  (with-temp-buffer
    (insert org)
    (org-mode)
    (org-confluence-export)))

(defun hub/org-confluence-test--with-file-buffer (path contents thunk)
  "Run THUNK in an Org buffer visiting PATH with CONTENTS."
  (with-current-buffer (find-file-noselect path)
    (unwind-protect
	(progn
	  (erase-buffer)
	  (insert contents)
	  (save-buffer)
	  (org-mode)
	  (funcall thunk))
      (set-buffer-modified-p nil)
      (kill-buffer))))

(ert-deftest hub/org-confluence-export-paragraph ()
  "Export a plain paragraph as XHTML."
  (should (equal (hub/org-confluence-test--export "Hello world")
		 "<p>Hello world</p>")))

(ert-deftest hub/org-confluence-export-paragraph-normalizes-soft-wraps ()
  "Export hard-wrapped Org source paragraphs as flowing XHTML text."
  (should (equal (hub/org-confluence-test--export "This is a hard-wrapped\nparagraph with *inline markup*.")
		 "<p>This is a hard-wrapped paragraph with <strong>inline markup</strong>.</p>")))

(ert-deftest hub/org-confluence-export-heading-h1 ()
  "Export a level-one heading as XHTML."
  (should (equal (hub/org-confluence-test--export "* Title")
		 "<h1>Title</h1>")))

(ert-deftest hub/org-confluence-export-heading-h2 ()
  "Export a level-two heading as XHTML."
  (should (equal (hub/org-confluence-test--export "** Subtitle")
		 "<h2>Subtitle</h2>")))

(ert-deftest hub/org-confluence-export-heading-h3 ()
  "Export a level-three heading as XHTML."
  (should (equal (hub/org-confluence-test--export "*** Subsub")
		 "<h3>Subsub</h3>")))

(ert-deftest hub/org-confluence-export-heading-h4 ()
  "Export a level-four heading as XHTML."
  (should (equal (hub/org-confluence-test--export "**** Deep")
		 "<h4>Deep</h4>")))

(ert-deftest hub/org-confluence-export-mixed ()
  "Export headings and paragraphs in document order."
  (should (equal (hub/org-confluence-test--export "* Title\nHello world")
		 "<h1>Title</h1>\n<p>Hello world</p>")))

(ert-deftest hub/org-confluence-export-empty ()
  "Export an empty Org buffer as an empty string."
  (should (equal (hub/org-confluence-test--export "") "")))

(ert-deftest hub/org-confluence-export-backend-has-dispatch-menu ()
  "Register Confluence in the normal Org export dispatcher."
  (let ((menu (org-export-backend-menu (org-export-get-backend 'confluence))))
    (should (equal (car menu) ?C))
    (should (string-match-p "Confluence" (cadr menu)))
    (should (assoc ?O (caddr menu)))))

(ert-deftest hub/org-confluence-export-as-xhtml-subtree-buffer ()
  "Export a subtree to a temporary XHTML buffer through Org export plumbing."
  (with-temp-buffer
    (insert "* One\nFirst body\n* Two\nSecond body")
    (org-mode)
    (goto-char (point-min))
    (search-forward "Two")
    (org-back-to-heading)
    (let ((buffer (org-confluence-export-as-xhtml nil t nil t nil)))
      (unwind-protect
	  (with-current-buffer buffer
	    (should (equal (string-trim (buffer-string))
			   "<p>Second body</p>")))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(ert-deftest hub/org-confluence-export-bold ()
  "Export bold inline markup as XHTML."
  (should (equal (hub/org-confluence-test--export "*bold*")
		 "<p><strong>bold</strong></p>")))

(ert-deftest hub/org-confluence-export-italic ()
  "Export italic inline markup as XHTML."
  (should (equal (hub/org-confluence-test--export "/italic/")
		 "<p><em>italic</em></p>")))

(ert-deftest hub/org-confluence-export-inline-code ()
  "Export inline code markup as XHTML."
  (should (equal (hub/org-confluence-test--export "~code~")
		 "<p><code>code</code></p>")))

(ert-deftest hub/org-confluence-export-strikethrough ()
  "Export strike-through inline markup as XHTML."
  (should (equal (hub/org-confluence-test--export "+strike+")
		 "<p><strike>strike</strike></p>")))

(ert-deftest hub/org-confluence-export-link-external ()
  "Export a described external link as XHTML."
  (should (equal (hub/org-confluence-test--export "[[https://x.com][text]]")
		 "<p><a href=\"https://x.com\">text</a></p>")))

(ert-deftest hub/org-confluence-export-link-plain ()
  "Export a plain external link as XHTML."
  (should (equal (hub/org-confluence-test--export "[[https://x.com]]")
		 "<p><a href=\"https://x.com\">https://x.com</a></p>")))

(ert-deftest hub/org-confluence-export-status-link ()
  "Export a Confluence status Org link as a status macro."
  (should (equal (hub/org-confluence-test--export "[[confluence-status:Purple][Medium]]")
		 "<p><ac:structured-macro ac:name=\"status\" ac:schema-version=\"1\"><ac:parameter ac:name=\"title\">Medium</ac:parameter><ac:parameter ac:name=\"colour\">Purple</ac:parameter></ac:structured-macro></p>")))

(ert-deftest hub/org-confluence-export-mixed-inline ()
  "Export nested and adjacent inline markup as XHTML."
  (should (equal (hub/org-confluence-test--export "A /lean/ and *bold* [[https://x.com][*link*]].")
		 "<p>A <em>lean</em> and <strong>bold</strong> <a href=\"https://x.com\"><strong>link</strong></a>.</p>")))

(ert-deftest hub/org-confluence-export-bullet-list ()
  "Export a bullet list as XHTML."
  (should (equal (hub/org-confluence-test--export "- item")
		 "<ul><li>item</li></ul>")))

(ert-deftest hub/org-confluence-export-bullet-list-multi ()
  "Export a multi-item bullet list as XHTML."
  (should (equal (hub/org-confluence-test--export "- one\n- two")
		 "<ul><li>one</li><li>two</li></ul>")))

(ert-deftest hub/org-confluence-export-ordered-list ()
  "Export an ordered list as XHTML."
  (should (equal (hub/org-confluence-test--export "1. item")
		 "<ol><li>item</li></ol>")))

(ert-deftest hub/org-confluence-export-ordered-list-multi ()
  "Export a multi-item ordered list as XHTML."
  (should (equal (hub/org-confluence-test--export "1. one\n2. two")
		 "<ol><li>one</li><li>two</li></ol>")))

(ert-deftest hub/org-confluence-export-list-nested ()
  "Export a nested bullet list as XHTML."
  (should (equal (hub/org-confluence-test--export "- parent\n  - child")
		 "<ul><li>parent<ul><li>child</li></ul></li></ul>")))

(ert-deftest hub/org-confluence-export-horizontal-rule ()
  "Export a horizontal rule as XHTML."
  (should (equal (hub/org-confluence-test--export "-----")
		 "<hr/>")))

(ert-deftest hub/org-confluence-export-list-preserves-spaces ()
  "Export list item text without removing internal spaces."
  (should (equal (hub/org-confluence-test--export "- First bullet\n- Second bullet")
		 "<ul><li>First bullet</li><li>Second bullet</li></ul>")))

(ert-deftest hub/org-confluence-export-mixed-list-types ()
  "Export adjacent bullet and ordered items as separate XHTML lists."
  (should (equal (hub/org-confluence-test--export "- First bullet\n- Second bullet\n  - Nested bullet\n\n1. First ordered item\n2. Second ordered item")
		 "<ul><li>First bullet</li><li>Second bullet<ul><li>Nested bullet</li></ul></li></ul><ol><li>First ordered item</li><li>Second ordered item</li></ol>")))

(ert-deftest hub/org-confluence-export-underline ()
  "Export underline inline markup as XHTML."
  (should (equal (hub/org-confluence-test--export "_under_")
		 "<p><u>under</u></p>")))

(ert-deftest hub/org-confluence-export-table ()
  "Export an Org table as XHTML."
  (should (equal (hub/org-confluence-test--export "| Name | Score |\n|------+-------|\n| Ada  | 10    |")
		 "<table><tbody><tr><th><p>Name</p></th><th><p>Score</p></th></tr><tr><td><p>Ada</p></td><td><p>10</p></td></tr></tbody></table>")))

(ert-deftest hub/org-confluence-export-table-inline-bold ()
  "Export bold Org markup inside table cells as XHTML strong tags."
  (should (equal (hub/org-confluence-test--export "| *T ime* | Detail |\n|---------+--------|\n| *R esults* | x |")
		 "<table><tbody><tr><th><p><strong>T ime</strong></p></th><th><p>Detail</p></th></tr><tr><td><p><strong>R esults</strong></p></td><td><p>x</p></td></tr></tbody></table>")))

(ert-deftest hub/org-confluence-export-table-repairs-emoji-adjacent-bold ()
  "Repair literal bold markers after emoji in imported table cells."
  (should (equal (hub/org-confluence-test--export "| 📆*T ime* |\n|---|")
		 "<table><tbody><tr><th><p>📆<strong>T ime</strong></p></th></tr></tbody></table>")))

(ert-deftest hub/org-confluence-export-blockquote ()
  "Export a quote block as XHTML."
  (should (equal (hub/org-confluence-test--export "#+begin_quote\nQuoted text\n#+end_quote")
		 "<blockquote><p>Quoted text</p></blockquote>")))

(ert-deftest hub/org-confluence-export-source-block ()
  "Export a source block as a Confluence code macro."
  (should (equal (hub/org-confluence-test--export "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src")
		 "<ac:structured-macro ac:name=\"code\" ac:schema-version=\"1\"><ac:parameter ac:name=\"language\">emacs-lisp</ac:parameter><ac:plain-text-body><![CDATA[(message \"hi\")]]></ac:plain-text-body></ac:structured-macro>")))

(ert-deftest hub/org-confluence-export-standalone-image ()
  "Export a standalone plain local image link as a Confluence image."
  (should (equal (hub/org-confluence-test--export "[[./img/foo.png]]")
		 "<ac:image ac:width=\"760\" ac:style=\"max-width: 100%; height: auto;\"><ri:attachment ri:filename=\"foo.png\"/></ac:image>")))

(ert-deftest hub/org-confluence-export-captioned-image ()
  "Export an Org image caption as alt text and visible caption text."
  (should (equal (hub/org-confluence-test--export "#+CAPTION: Architecture overview\n[[./img/architecture.png]]")
		 "<ac:image ac:width=\"760\" ac:style=\"max-width: 100%; height: auto;\" ac:alt=\"Architecture overview\"><ri:attachment ri:filename=\"architecture.png\"/></ac:image>\n<p><em>Architecture overview</em></p>")))

(ert-deftest hub/org-confluence-export-image-uses-publish-filename-map ()
  "Export image attachment references through the publish filename map."
  (with-temp-buffer
    (insert "[[./img/foo.png]]")
    (org-mode)
    (should (equal (org-confluence-export nil nil nil nil
					  '(:confluence-image-filenames (("./img/foo.png" . "foo-hash.png"))))
		   "<ac:image ac:width=\"760\" ac:style=\"max-width: 100%; height: auto;\"><ri:attachment ri:filename=\"foo-hash.png\"/></ac:image>"))))

(ert-deftest hub/org-confluence-export-described-image-link ()
  "Keep described local image links as normal links."
  (should (equal (hub/org-confluence-test--export "[[./img/foo.png][Open image]]")
		 "<p><a href=\"./img/foo.png\">Open image</a></p>")))

(ert-deftest hub/org-confluence-export-remote-image-url ()
  "Keep remote image URLs as normal links."
  (should (equal (hub/org-confluence-test--export "[[https://example.com/foo.png]]")
		 "<p><a href=\"https://example.com/foo.png\">https://example.com/foo.png</a></p>")))

(ert-deftest hub/org-confluence-export-non-image-file-link ()
  "Keep local non-image file links as normal links."
  (should (equal (hub/org-confluence-test--export "[[./files/report.pdf]]")
		 "<p><a href=\"./files/report.pdf\">./files/report.pdf</a></p>")))

(defun hub/org-confluence-test--image-asset (path source-link)
  "Return expected image asset for PATH and SOURCE-LINK."
  (list :path path
	:source-path path
	:source-link source-link
	:filename (org-confluence--hashed-image-filename path)))

(ert-deftest hub/org-confluence-image-hashed-filename-is-xml-safe ()
  "Build hashed attachment filenames from hex digest text, not raw bytes."
  (let ((filename (org-confluence--hashed-image-filename "/tmp/foo.png")))
    (should (string-match-p "\\`foo-[[:xdigit:]]\\{12\\}\\.png\\'" filename))))

(ert-deftest hub/org-confluence-image-resolves-relative-path ()
  "Resolve relative image paths from the Org buffer file directory."
  (let* ((root (make-temp-file "org-confluence-images-" t))
	 (org-file (expand-file-name "page.org" root))
	 (image-dir (expand-file-name "img" root))
	 (image-file (expand-file-name "foo.png" image-dir)))
    (unwind-protect
	(progn
	  (make-directory image-dir)
	  (with-temp-file image-file (insert "png"))
	  (hub/org-confluence-test--with-file-buffer
	   org-file "[[./img/foo.png]]"
	   (lambda ()
	     (should (equal (org-confluence-image-assets)
			    (list (hub/org-confluence-test--image-asset image-file "./img/foo.png")))))))
      (delete-directory root t))))

(ert-deftest hub/org-confluence-image-resolves-absolute-path ()
  "Accept absolute image paths for upload."
  (let* ((root (make-temp-file "org-confluence-images-" t))
	 (org-file (expand-file-name "page.org" root))
	 (image-file (expand-file-name "foo.png" root)))
    (unwind-protect
	(progn
	  (with-temp-file image-file (insert "png"))
	  (hub/org-confluence-test--with-file-buffer
	   org-file (format "[[%s]]" image-file)
	   (lambda ()
	     (should (equal (org-confluence-image-assets)
			    (list (hub/org-confluence-test--image-asset image-file image-file)))))))
      (delete-directory root t))))

(ert-deftest hub/org-confluence-image-missing-file-errors ()
  "Signal an error for missing local image files."
  (let* ((root (make-temp-file "org-confluence-images-" t))
	 (org-file (expand-file-name "page.org" root)))
    (unwind-protect
	(hub/org-confluence-test--with-file-buffer
	 org-file "[[./img/missing.png]]"
	 (lambda ()
	   (should-error (org-confluence-image-assets) :type 'user-error)))
      (delete-directory root t))))

(ert-deftest hub/org-confluence-image-unsaved-relative-buffer-errors ()
  "Signal an error for relative image paths in unsaved buffers."
  (with-temp-buffer
    (insert "[[./img/foo.png]]")
    (org-mode)
    (should-error (org-confluence-image-assets) :type 'user-error)))

(ert-deftest hub/org-confluence-image-duplicate-reference-is-allowed ()
  "Allow the same local image to be referenced more than once."
  (let* ((root (make-temp-file "org-confluence-images-" t))
	 (org-file (expand-file-name "page.org" root))
	 (image-file (expand-file-name "foo.png" root)))
    (unwind-protect
	(progn
	  (with-temp-file image-file (insert "png"))
	  (hub/org-confluence-test--with-file-buffer
	   org-file "[[./foo.png]]\n\n[[./foo.png]]"
	   (lambda ()
	     (should (equal (org-confluence-image-assets)
			    (list (hub/org-confluence-test--image-asset image-file "./foo.png")
				  (hub/org-confluence-test--image-asset image-file "./foo.png")))))))
      (delete-directory root t))))

(ert-deftest hub/org-confluence-image-duplicate-basenames-use-hashed-filenames ()
  "Allow duplicate source basenames by hashing attachment filenames."
  (let* ((root (make-temp-file "org-confluence-images-" t))
	 (org-file (expand-file-name "page.org" root))
	 (one-dir (expand-file-name "one" root))
	 (two-dir (expand-file-name "two" root))
	 (one-file (expand-file-name "same.png" one-dir))
	 (two-file (expand-file-name "same.png" two-dir)))
    (unwind-protect
	(progn
	  (make-directory one-dir)
	  (make-directory two-dir)
	  (with-temp-file one-file (insert "one"))
	  (with-temp-file two-file (insert "two"))
	  (hub/org-confluence-test--with-file-buffer
	   org-file "[[./one/same.png]]\n\n[[./two/same.png]]"
	   (lambda ()
	     (should (equal (org-confluence-image-assets)
			    (list (hub/org-confluence-test--image-asset one-file "./one/same.png")
				  (hub/org-confluence-test--image-asset two-file "./two/same.png")))))))
      (delete-directory root t))))

(ert-deftest hub/org-confluence-image-collects-only-standalone-local-images ()
  "Collect only standalone plain local image links for upload."
  (let* ((root (make-temp-file "org-confluence-images-" t))
	 (org-file (expand-file-name "page.org" root))
	 (image-file (expand-file-name "foo.png" root)))
    (unwind-protect
	(progn
	  (with-temp-file image-file (insert "png"))
	  (hub/org-confluence-test--with-file-buffer
	   org-file "[[./foo.png]]\n\n[[./foo.png][Open image]]\n\n[[https://example.com/foo.png]]\n\n[[./report.pdf]]"
	   (lambda ()
	     (should (equal (org-confluence-image-assets)
			    (list (hub/org-confluence-test--image-asset image-file "./foo.png")))))))
      (delete-directory root t))))

(ert-deftest hub/org-confluence-export-definition-list ()
  "Export an Org descriptive list as a Confluence-safe unordered list."
  (should (equal (hub/org-confluence-test--export "- Term :: Definition")
		 "<ul><li><strong>Term:</strong> Definition</li></ul>")))

(ert-deftest hub/org-confluence-export-definition-list-multi ()
  "Export multiple descriptive list items as a Confluence-safe unordered list."
  (should (equal (hub/org-confluence-test--export "- First :: One\n- Second :: Two")
		 "<ul><li><strong>First:</strong> One</li><li><strong>Second:</strong> Two</li></ul>")))

(ert-deftest hub/org-confluence-export-footnote ()
  "Export footnotes as clickable Confluence anchor links."
  (should (equal (hub/org-confluence-test--export "Text[fn:1]\n\n[fn:1] Note body.")
		 "<p>Text<ac:structured-macro ac:name=\"anchor\" ac:schema-version=\"1\"><ac:default-parameter>fnref-1</ac:default-parameter></ac:structured-macro><sup><ac:link ac:anchor=\"fn-1\"><ac:plain-text-link-body><![CDATA[1]]></ac:plain-text-link-body></ac:link></sup></p>\n<p><ac:structured-macro ac:name=\"anchor\" ac:schema-version=\"1\"><ac:default-parameter>fn-1</ac:default-parameter></ac:structured-macro><strong>1.</strong> Note body. <ac:link ac:anchor=\"fnref-1\"><ac:plain-text-link-body><![CDATA[↩]]></ac:plain-text-link-body></ac:link></p>")))

(ert-deftest hub/org-confluence-export-callout-default ()
  "Export a callout block as a Confluence info panel macro."
  (should (equal (hub/org-confluence-test--export "#+begin_callout\nHeads up\n#+end_callout")
		 "<ac:structured-macro ac:name=\"info\" ac:schema-version=\"1\"><ac:rich-text-body><p>Heads up</p></ac:rich-text-body></ac:structured-macro>")))

(ert-deftest hub/org-confluence-export-callout-title ()
  "Export a semantic callout title as a Confluence panel title."
  (should (equal (hub/org-confluence-test--export "#+ATTR_CALLOUT: :type warning :title \"Heads up\"\n#+begin_callout\nCareful\n#+end_callout")
		 "<ac:structured-macro ac:name=\"warning\" ac:schema-version=\"1\"><ac:parameter ac:name=\"title\">Heads up</ac:parameter><ac:rich-text-body><p>Careful</p></ac:rich-text-body></ac:structured-macro>")))

(provide 'org-confluence-export-test)
;;; org-confluence-export-test.el ends here
