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
    (should (plist-get result :deferred))))

(ert-deftest org-google-docs-classification-formats-local-report ()
  "A plain fixture produces an actionable provider report."
  (let ((report (org-google-docs-semantics-format-report
		 (org-google-docs-semantics-classify-audit
		  org-google-docs-classification-test--audit))))
    (should (string-match-p "Google Docs semantic support" report))
    (should (string-match-p "footnotes (3)" report))
    (should (string-match-p "person links (1)" report))))

(provide 'org-google-docs-classification-test)
;;; org-google-docs-classification-test.el ends here
