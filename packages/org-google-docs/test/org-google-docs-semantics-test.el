;;; org-google-docs-semantics-test.el --- Google Docs semantic support tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for classifying typographic semantics against current Google Docs
;; adapter capabilities.

;;; Code:

(require 'ert)
(require 'org)

(let ((root (file-name-as-directory
	     (locate-dominating-file (or load-file-name buffer-file-name)
				     "domains.yaml"))))
  (add-to-list 'load-path (expand-file-name "packages/org-google-docs" root))
  (add-to-list 'load-path (expand-file-name "modules" root)))

(require 'org-google-docs-semantics)
(require 'org/typographic-semantics)

(defconst org-google-docs-semantics-test--specimen
  (expand-file-name "modules/org/specimens/typographic-semantics.org"
		    (file-name-as-directory
		     (locate-dominating-file (or load-file-name buffer-file-name)
					     "domains.yaml")))
  "Path to the typographic semantics specimen.")

(defun org-google-docs-semantics-test--names (classification bucket)
  "Return semantic names from CLASSIFICATION BUCKET."
  (mapcar (lambda (entry) (plist-get entry :name))
	  (plist-get classification bucket)))

(defun org-google-docs-semantics-test--entry (classification bucket name)
  "Return CLASSIFICATION entry named NAME from BUCKET."
  (seq-find (lambda (entry) (eq (plist-get entry :name) name))
	    (plist-get classification bucket)))

(ert-deftest org-google-docs-semantics-classifies-specimen-current-support ()
  "Classify the typographic specimen against current Google Docs support."
  (let* ((audit (hub/org-typographic-semantics-audit-file
		 org-google-docs-semantics-test--specimen))
	 (classification (org-google-docs-semantics-classify-audit audit)))
    (should (equal (plist-get classification :provider) 'google-docs))
    (should (member 'inline-emphasis
		    (org-google-docs-semantics-test--names classification :supported)))
    (should (member 'lists
		    (org-google-docs-semantics-test--names classification :supported)))
    (should (member 'tables
		    (org-google-docs-semantics-test--names classification :supported)))
    (should (member 'source-blocks
		    (org-google-docs-semantics-test--names classification :supported)))
    (should (member 'dates
		    (org-google-docs-semantics-test--names classification :degraded)))
    (should (member 'footnotes
		    (org-google-docs-semantics-test--names classification :unsupported)))
    (should (member 'standalone-images
		    (org-google-docs-semantics-test--names classification :unsupported)))
    (should (member 'quote-blocks
		    (org-google-docs-semantics-test--names classification :deferred)))
    (should (member 'person-links
		    (org-google-docs-semantics-test--names classification :deferred)))
    (should (member 'status-links
		    (org-google-docs-semantics-test--names classification :deferred)))))

(ert-deftest org-google-docs-semantics-preserves-counts-and-reasons ()
  "Support classification entries retain counts and actionable reasons."
  (let* ((audit (hub/org-typographic-semantics-audit-file
		 org-google-docs-semantics-test--specimen))
	 (classification (org-google-docs-semantics-classify-audit audit))
	 (footnotes (org-google-docs-semantics-test--entry
		     classification :unsupported 'footnotes))
	 (dates (org-google-docs-semantics-test--entry
		 classification :degraded 'dates))
	 (marginalia (org-google-docs-semantics-test--entry
		      classification :unsupported 'marginalia)))
    (should (= 3 (plist-get footnotes :count)))
    (should (string-match-p "native Google Docs footnotes"
			    (plist-get footnotes :reason)))
    (should (= 2 (plist-get dates :count)))
    (should (string-match-p "inactive Org timestamps"
			    (plist-get dates :target)))
    (should (= 1 (plist-get marginalia :count)))
    (should (string-match-p "marginalia" (plist-get marginalia :reason)))))

(ert-deftest org-google-docs-semantics-formats-provider-report ()
  "Format a concise Google Docs semantic support report."
  (let* ((audit (hub/org-typographic-semantics-audit-file
		 org-google-docs-semantics-test--specimen))
	 (classification (org-google-docs-semantics-classify-audit audit))
	 (report (org-google-docs-semantics-format-report classification)))
    (should (string-match-p "Google Docs semantic support" report))
    (should (string-match-p "Unsupported:" report))
    (should (string-match-p "footnotes (3)" report))
    (should (string-match-p "Deferred:" report))
    (should (string-match-p "person links (1)" report))
    (should (string-match-p "status links (1)" report))))

(provide 'org-google-docs-semantics-test)
;;; org-google-docs-semantics-test.el ends here
