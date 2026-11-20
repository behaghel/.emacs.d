;;; org-google-docs-classification-test.el --- Plain audit contract tests -*- lexical-binding: t; -*-

;;; Commentary:
;; The reusable classifier consumes data, not the personal auditor or specimen.
;; Producer/consumer specimen integration is verified by the consumer repository.

;;; Code:

(require 'ert)
(require 'org-google-docs-semantics)

(defconst org-google-docs-classification-test--audit
  '(:inline ((bold . 1) (italic . 1) (external-links . 1)
	     (active-dates . 1) (inactive-dates . 1) (footnote-references . 3)
	     (person-links . 1) (status-links . 1))
	    :structure ((ordered-lists . 1) (unordered-lists . 1) (tables . 1))
	    :blocks ((source . 1) (quote . 1))
	    :media ((standalone-images . 1)) :notes ((marginalia . 1)))
  "Plain typographic audit data accepted by the classification boundary.")

(ert-deftest org-google-docs-classification-consumes-plain-audit ()
  "Classification preserves semantic group counts without loading an auditor."
  (let* ((result (org-google-docs-semantics-classify-audit
		  org-google-docs-classification-test--audit))
	 (dates (seq-find (lambda (entry) (eq (plist-get entry :name) 'dates))
			  (plist-get result :degraded))))
    (should (eq (plist-get result :provider) 'google-docs))
    (should (= 2 (plist-get dates :count)))
    (should (string-match-p "inactive Org timestamps" (plist-get dates :target)))
    (should (plist-get result :supported))
    (should (plist-get result :unsupported))
    (should (seq-some (lambda (entry) (eq (plist-get entry :name) 'person-links))
		      (plist-get result :degraded)))))

(ert-deftest org-google-docs-classification-formats-local-report ()
  "A plain fixture produces an actionable provider report."
  (let ((report (org-google-docs-semantics-format-report
		 (org-google-docs-semantics-classify-audit
		  org-google-docs-classification-test--audit))))
    (should (string-match-p "Google Docs semantic support" report))
    (should (string-match-p "footnotes (3)" report))
    (should (string-match-p "person links (1)" report))))

(ert-deftest org-google-docs-classification-reflects-accepted-capabilities ()
  "The support report no longer calls implemented semantic groups unsupported."
  (let* ((audit '(:media ((standalone-images . 1) (captioned-figures . 1) (described-image-links . 1))
			 :blocks ((quote . 1) (callout . 1) (example . 1))
			 :inline ((status-links . 1))))
	 (result (org-google-docs-semantics-classify-audit audit))
	 (supported (mapcar (lambda (entry) (plist-get entry :name)) (plist-get result :supported))))
    (dolist (name '(standalone-images captioned-figures quote-blocks callouts example-blocks status-links))
      (should (memq name supported)))
    (should-not (plist-get result :unclassified))))

(ert-deftest org-google-docs-classification-unmapped-counters-are-visible-and-unsupported ()
  "New or omitted audit categories cannot disappear from the support report."
  (let* ((result (org-google-docs-semantics-classify-audit '(:blocks ((future-block . 2)))))
	 (entry (car (plist-get result :unclassified))))
    (should entry)
    (should (= (plist-get entry :count) 2))
    (should (memq entry (plist-get result :unsupported)))
    (should (string-match-p "No declared" (plist-get entry :reason)))))

(provide 'org-google-docs-classification-test)
;;; org-google-docs-classification-test.el ends here
