;;; org-confluence-export-test.el --- Tests for Org Confluence export -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavior tests for the Org -> Confluence Storage Format backend.

;;; Code:

(require 'ert)
(require 'org)
(require 'ox)

;; Ensure repo modules are reachable for isolated batch test runners.
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root))
  (add-to-list 'load-path (expand-file-name "modules/org/export-confluence" root))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "core" root)))

(load "export" nil 'nomessage)

(defun hub/org-confluence-test--export (org)
  "Return Confluence XHTML exported from ORG."
  (with-temp-buffer
    (insert org)
    (org-mode)
    (org-confluence-export)))

(ert-deftest hub/org-confluence-export-paragraph ()
  "Export a plain paragraph as XHTML."
  (should (equal (hub/org-confluence-test--export "Hello world")
		 "<p>Hello world</p>")))

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

(ert-deftest hub/org-confluence-export-blockquote ()
  "Export a quote block as XHTML."
  (should (equal (hub/org-confluence-test--export "#+begin_quote\nQuoted text\n#+end_quote")
		 "<blockquote><p>Quoted text</p></blockquote>")))

(ert-deftest hub/org-confluence-export-source-block ()
  "Export a source block as a Confluence code macro."
  (should (equal (hub/org-confluence-test--export "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src")
		 "<ac:structured-macro ac:name=\"code\" ac:schema-version=\"1\"><ac:parameter ac:name=\"language\">emacs-lisp</ac:parameter><ac:plain-text-body><![CDATA[(message \"hi\")]]></ac:plain-text-body></ac:structured-macro>")))

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
