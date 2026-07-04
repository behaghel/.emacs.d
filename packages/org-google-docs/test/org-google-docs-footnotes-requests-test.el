;;; org-google-docs-footnotes-requests-test.el --- Footnote request planning tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for native Google Docs footnote request planning.  These tests cover
;; the API choreography after a robust upstream seam has provided document
;; indices for planned Org footnote references.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-google-docs-footnotes)

(ert-deftest org-google-docs-footnotes-builds-create-requests-at-doc-indices ()
  "Create native footnote requests from reference document indices."
  (let* ((references (list (list :label "one" :ordinal 1 :body "First body." :doc-index 12)
			   (list :label "two" :ordinal 2 :body "Second body." :doc-index 34)))
	 (requests (org-google-docs-footnotes-create-requests references)))
    (should (equal requests
		   '(((createFootnote . ((location . ((index . 12))))))
		     ((createFootnote . ((location . ((index . 34)))))))))))

(ert-deftest org-google-docs-footnotes-rejects-create-request-without-doc-index ()
  "Native footnote creation requires explicit document indices."
  (should-error
   (org-google-docs-footnotes-create-requests
    (list (list :label "one" :ordinal 1 :body "Body without an index.")))
   :type 'user-error))

(ert-deftest org-google-docs-footnotes-builds-body-insert-requests-from-replies ()
  "Insert footnote body text into created footnote segments."
  (let* ((references (list (list :label "one" :ordinal 1 :body "First body." :doc-index 12)
			   (list :label "two" :ordinal 2 :body "Second body." :doc-index 34)))
	 (response '((replies . [((createFootnote . ((footnoteId . "fn-a"))))
				 ((createFootnote . ((footnoteId . "fn-b"))))])))
	 (requests (org-google-docs-footnotes-body-insert-requests references response)))
    (should (equal requests
		   '(((insertText . ((text . "First body.")
				     (location . ((segmentId . "fn-a")
						  (index . 1))))))
		     ((insertText . ((text . "Second body.")
				     (location . ((segmentId . "fn-b")
						  (index . 1)))))))))))

(ert-deftest org-google-docs-footnotes-body-insert-rejects-mismatched-replies ()
  "Footnote body insertion requires one createFootnote reply per reference."
  (let ((references (list (list :label "one" :ordinal 1 :body "First body." :doc-index 12)))
	(response '((replies . []))))
    (should-error
     (org-google-docs-footnotes-body-insert-requests references response)
     :type 'user-error)))

(provide 'org-google-docs-footnotes-requests-test)
;;; org-google-docs-footnotes-requests-test.el ends here
