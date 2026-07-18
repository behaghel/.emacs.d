;;; org-typographic-semantics-test.el --- Tests for typographic semantics inventory -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(let ((root (file-name-as-directory
	     (locate-dominating-file (or load-file-name buffer-file-name)
				     "domains.yaml"))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root)))

(require 'org/typographic-semantics)

(defconst hub/org-typographic-semantics-test--specimen
  (expand-file-name "modules/org/specimens/typographic-semantics.org"
		    (file-name-as-directory
		     (locate-dominating-file (or load-file-name buffer-file-name)
					     "domains.yaml")))
  "Path to the typographic semantics specimen.")

(defun hub/org-typographic-semantics-test--count (audit category semantic)
  "Return AUDIT count for SEMANTIC under CATEGORY."
  (or (alist-get semantic (plist-get audit category)) 0))

(ert-deftest hub/org-typographic-semantics-audits-specimen-inline-semantics ()
  "Audit the specimen's inline typographic semantics."
  (let ((audit (hub/org-typographic-semantics-audit-file
		hub/org-typographic-semantics-test--specimen)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'bold)))
    (should (= 2 (hub/org-typographic-semantics-test--count audit :inline 'italic)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'underline)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'strike-through)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'code)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'verbatim)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'external-links)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'person-links)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'status-links)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'active-dates)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :inline 'inactive-dates)))
    (should (= 3 (hub/org-typographic-semantics-test--count audit :inline 'footnote-references)))))

(ert-deftest hub/org-typographic-semantics-audits-specimen-notes ()
  "Audit ordinary, forced-bottom, and marginalia note semantics."
  (let ((audit (hub/org-typographic-semantics-audit-file
		hub/org-typographic-semantics-test--specimen)))
    (should (= 3 (hub/org-typographic-semantics-test--count audit :notes 'definitions)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :notes 'ordinary)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :notes 'footnote)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :notes 'marginalia)))))

(ert-deftest hub/org-typographic-semantics-audits-specimen-blocks-structure-media ()
  "Audit specimen block, structure, and media semantics."
  (let ((audit (hub/org-typographic-semantics-audit-file
		hub/org-typographic-semantics-test--specimen)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :blocks 'source)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :blocks 'example)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :blocks 'quote)))
    (should (= 2 (hub/org-typographic-semantics-test--count audit :blocks 'callout)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :blocks 'standfirst)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :blocks 'metric)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :blocks 'graph)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :structure 'tables)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :structure 'section-breaks)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :structure 'ordered-lists)))
    (should (= 2 (hub/org-typographic-semantics-test--count audit :structure 'unordered-lists)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :structure 'definition-lists)))
    (should (= 3 (hub/org-typographic-semantics-test--count audit :structure 'checkboxes)))
    (should (= 2 (hub/org-typographic-semantics-test--count audit :media 'standalone-images)))
    (should (= 2 (hub/org-typographic-semantics-test--count audit :media 'captioned-figures)))
    (should (= 1 (hub/org-typographic-semantics-test--count audit :media 'described-image-links)))))

;;; org-typographic-semantics-test.el ends here
