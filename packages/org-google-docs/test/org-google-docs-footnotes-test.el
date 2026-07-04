;;; org-google-docs-footnotes-test.el --- Google Docs footnote plan tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for extracting an Org footnote push plan before mutating Google Docs.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-google-docs-footnotes)

(defmacro org-google-docs-footnotes-test--with-buffer (contents &rest body)
  "Run BODY in an Org buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defun org-google-docs-footnotes-test--diagnostic-codes (plan)
  "Return diagnostic codes from PLAN."
  (mapcar (lambda (diagnostic) (plist-get diagnostic :code))
	  (plist-get plan :diagnostics)))

(defun org-google-docs-footnotes-test--degradation-codes (plan)
  "Return degradation codes from PLAN."
  (mapcar (lambda (diagnostic) (plist-get diagnostic :code))
	  (plist-get plan :degradations)))

(ert-deftest org-google-docs-footnotes-plans-named-footnotes ()
  "Extract named Org footnotes from a conventional Footnotes section."
  (org-google-docs-footnotes-test--with-buffer
   "* Body\nText[fn:one].\n\n* Footnotes\n\n[fn:one] This is the note.\n"
   (let* ((plan (org-google-docs-footnotes-plan-buffer))
	  (reference (car (plist-get plan :references)))
	  (section (plist-get plan :definitions-section)))
     (should-not (plist-get plan :diagnostics))
     (should (plist-get plan :ready-p))
     (should (equal "one" (plist-get reference :label)))
     (should (= 1 (plist-get reference :ordinal)))
     (should (equal "This is the note." (plist-get reference :body)))
     (should (equal "Footnotes" (plist-get section :heading))))))

(ert-deftest org-google-docs-footnotes-duplicates-repeated-reference-bodies ()
  "Repeated Org references become separate native footnote plan entries."
  (org-google-docs-footnotes-test--with-buffer
   "* Body\nFirst[fn:shared] and second[fn:shared].\n\n* Footnotes\n\n[fn:shared] Shared body.\n"
   (let ((plan (org-google-docs-footnotes-plan-buffer)))
     (should (plist-get plan :ready-p))
     (should (= 2 (length (plist-get plan :references))))
     (should (equal '("Shared body." "Shared body.")
		    (mapcar (lambda (reference) (plist-get reference :body))
			    (plist-get plan :references))))
     (should (member :repeated-reference
		     (org-google-docs-footnotes-test--degradation-codes plan))))))

(ert-deftest org-google-docs-footnotes-reports-rich-body-degradation ()
  "Rich inline markup in footnote bodies is a v1 degradation, not a blocker."
  (org-google-docs-footnotes-test--with-buffer
   "* Body\nText[fn:rich].\n\n* Footnotes\n\n[fn:rich] This has /italic/ and [[https://example.com][a link]].\n"
   (let ((plan (org-google-docs-footnotes-plan-buffer)))
     (should (plist-get plan :ready-p))
     (should (equal "This has italic and a link."
		    (plist-get (car (plist-get plan :references)) :body)))
     (should (member :rich-inline-formatting
		     (org-google-docs-footnotes-test--degradation-codes plan))))))

(ert-deftest org-google-docs-footnotes-blocks-unsupported-forms ()
  "Unsupported footnote forms block native Google Docs footnote push."
  (org-google-docs-footnotes-test--with-buffer
   "* Body\nAnonymous[fn:: inline note] missing[fn:missing] marginal[fn:margin].\n\n* Footnotes\n\n[fn:margin]\n:PROPERTIES:\n:HUB_NOTE_KIND: marginalia\n:END:\nMarginal body.\n"
   (let ((plan (org-google-docs-footnotes-plan-buffer)))
     (should-not (plist-get plan :ready-p))
     (should (member :anonymous-footnote
		     (org-google-docs-footnotes-test--diagnostic-codes plan)))
     (should (member :missing-definition
		     (org-google-docs-footnotes-test--diagnostic-codes plan)))
     (should (member :unsupported-marginalia
		     (org-google-docs-footnotes-test--diagnostic-codes plan))))))

(ert-deftest org-google-docs-footnotes-requires-conventional-section ()
  "Definitions outside a conventional footnotes section block v1 push."
  (org-google-docs-footnotes-test--with-buffer
   "* Body\nText[fn:one].\n\n[fn:one] This is outside a conventional section.\n"
   (let ((plan (org-google-docs-footnotes-plan-buffer)))
     (should-not (plist-get plan :ready-p))
     (should (member :definition-outside-footnotes-section
		     (org-google-docs-footnotes-test--diagnostic-codes plan))))))

(ert-deftest org-google-docs-footnotes-blocks-mixed-section-content ()
  "Authored content in the conventional Footnotes section blocks v1 push."
  (org-google-docs-footnotes-test--with-buffer
   "* Body\nText[fn:one].\n\n* Footnotes\n\nThis should not be here.\n\n[fn:one] Body.\n"
   (let ((plan (org-google-docs-footnotes-plan-buffer)))
     (should-not (plist-get plan :ready-p))
     (should (member :mixed-footnotes-section-content
		     (org-google-docs-footnotes-test--diagnostic-codes plan))))))

(ert-deftest org-google-docs-footnotes-accepts-french-section-name ()
  "Default conventional section names include French footnotes heading."
  (org-google-docs-footnotes-test--with-buffer
   "* Body\nTexte[fn:une].\n\n* Notes de bas de page\n\n[fn:une] Une note.\n"
   (let ((plan (org-google-docs-footnotes-plan-buffer)))
     (should (plist-get plan :ready-p))
     (should (equal "Notes de bas de page"
		    (plist-get (plist-get plan :definitions-section) :heading))))))

(provide 'org-google-docs-footnotes-test)
;;; org-google-docs-footnotes-test.el ends here
