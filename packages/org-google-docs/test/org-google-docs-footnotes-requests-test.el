;;; org-google-docs-footnotes-requests-test.el --- Footnote request planning tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for native Google Docs footnote request planning.  These tests cover
;; the API choreography after a robust upstream seam has provided document
;; indices for planned Org footnote references.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-google-docs-footnotes)

(ert-deftest org-google-docs-footnotes-filters-org-only-footnotes-from-ir ()
  "Native footnote push removes Org-only definition section from body IR."
  (let ((ir (list (list :type 'paragraph
			:style 'normal
			:contents (list (list :text "Text.")))
		  (list :type 'paragraph
			:style 'heading-1
			:contents (list (list :text "Footnotes")))
		  (list :type 'footnote
			:label "one"
			:contents (list (list :text "Body.")))
		  (list :type 'paragraph
			:style 'normal
			:contents (list (list :text "After."))))))
    (should (equal (mapcar (lambda (element)
			     (org-google-docs-footnotes--runs-plain-text
			      (plist-get element :contents)))
			   (org-google-docs-footnotes-filter-native-footnote-ir ir))
		   '("Text." "After.")))))

(ert-deftest org-google-docs-footnotes-conversion-advice-filters-only-during-session ()
  "Conversion advice filters footnote IR only while a push session is active."
  (let ((ir (list (list :type 'paragraph
			:style 'heading-1
			:contents (list (list :text "Footnotes")))
		  (list :type 'footnote :label "one"))))
    (cl-labels ((orig () ir))
      (let ((org-google-docs-footnotes--push-session nil))
	(should (equal (org-google-docs-footnotes--around-org-buffer-to-ir #'orig)
		       ir)))
      (let ((org-google-docs-footnotes--push-session (list :references [])))
	(should-not (org-google-docs-footnotes--around-org-buffer-to-ir #'orig))))))

(ert-deftest org-google-docs-footnotes-builds-create-requests-at-doc-indices ()
  "Create native footnote requests from reference document indices."
  (let* ((references (list (list :label "one" :ordinal 1 :body "First body." :doc-index 12)
			   (list :label "two" :ordinal 2 :body "Second body." :doc-index 34)))
	 (requests (org-google-docs-footnotes-create-requests references)))
    (should (equal requests
		   '(((createFootnote . ((location . ((index . 12))))))
		     ((createFootnote . ((location . ((index . 34)))))))))))

(ert-deftest org-google-docs-footnotes-sorts-create-requests-descending ()
  "Sort native footnote requests from high to low indices for stable mutation."
  (let* ((references (list (list :label "one" :ordinal 1 :body "First body." :doc-index 12)
			   (list :label "two" :ordinal 2 :body "Second body." :doc-index 34)))
	 (sorted (org-google-docs-footnotes--sort-references-for-mutation references))
	 (requests (org-google-docs-footnotes-create-requests sorted)))
    (should (equal (mapcar (lambda (reference) (plist-get reference :label)) sorted)
		   '("two" "one")))
    (should (equal requests
		   '(((createFootnote . ((location . ((index . 34))))))
		     ((createFootnote . ((location . ((index . 12)))))))))))

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

(ert-deftest org-google-docs-footnotes-batch-advice-skips-unindexed-references ()
  "Batch advice still pushes body changes when no footnote index was emitted."
  (let* ((session (list :references (vconcat (list (list :label "one" :body "Body.")))
			:cursor 0
			:previous-handler nil))
	 (org-google-docs-footnotes--push-session session)
	 (calls nil))
    (cl-labels ((fake-batch
		  (_document-id requests callback &optional _account _on-error)
		  (push requests calls)
		  (funcall callback '((replies . [])))))
      (org-google-docs-footnotes--around-batch-update
       #'fake-batch "doc" '((deleteContentRange . ((range . ((startIndex . 3))))))
       (lambda (_response) (push :callback calls)))
      (setq calls (nreverse calls))
      (should (= 2 (length calls)))
      (should (equal (car calls)
		     '((deleteContentRange . ((range . ((startIndex . 3))))))))
      (should (eq (cadr calls) :callback))
      (should-not org-google-docs-footnotes--push-session))))

(ert-deftest org-google-docs-footnotes-batch-advice-mutates-in-descending-index-order ()
  "Batch advice keeps create replies aligned with descending mutation order."
  (let* ((session (list :references (vconcat (list (list :label "one"
							 :ordinal 1
							 :body "First body."
							 :doc-index 12)
						   (list :label "two"
							 :ordinal 2
							 :body "Second body."
							 :doc-index 34)))
			:cursor 0
			:previous-handler nil))
	 (org-google-docs-footnotes--push-session session)
	 calls)
    (cl-labels ((orig (_document-id requests callback &optional _account _on-error)
		  (push requests calls)
		  (funcall callback
			   (if (seq-find (lambda (request)
					   (alist-get 'createFootnote request))
					 requests)
			       '((replies . [nil
					     ((createFootnote . ((footnoteId . "fn-two"))))
					     ((createFootnote . ((footnoteId . "fn-one"))))]))
			     '((replies . []))))))
      (org-google-docs-footnotes--around-batch-update
       #'orig "doc-1" '(((insertText . ((text . "Body")))))
       #'ignore)
      (should (equal (cadr calls)
		     '(((insertText . ((text . "Body"))))
		       ((createFootnote . ((location . ((index . 34))))))
		       ((createFootnote . ((location . ((index . 12)))))))))
      (should (equal (car calls)
		     '(((insertText . ((text . "Second body.")
				       (location . ((segmentId . "fn-two")
						    (index . 1))))))
		       ((insertText . ((text . "First body.")
				       (location . ((segmentId . "fn-one")
						    (index . 1))))))))))))

(ert-deftest org-google-docs-footnotes-batch-advice-runs-two-phase-update ()
  "Batch advice appends createFootnote and inserts bodies before main callback."
  (let* ((session (list :references (vconcat (list (list :label "one"
							 :ordinal 1
							 :body "First body."
							 :doc-index 12)))
			:cursor 0
			:previous-handler nil))
	 (org-google-docs-footnotes--push-session session)
	 calls callback-ran)
    (cl-labels ((orig (_document-id requests callback &optional _account _on-error)
		  (push requests calls)
		  (funcall callback
			   (if (seq-find (lambda (request)
					   (alist-get 'createFootnote request))
					 requests)
			       '((replies . [nil ((createFootnote . ((footnoteId . "fn-a"))))]))
			     '((replies . []))))))
      (org-google-docs-footnotes--around-batch-update
       #'orig "doc-1" '(((insertText . ((text . "Body")))))
       (lambda (_response) (setq callback-ran t)))
      (should callback-ran)
      (should (= 2 (length calls)))
      (should (seq-find (lambda (request) (alist-get 'createFootnote request))
			(cadr calls)))
      (should (equal (car calls)
		     '(((insertText . ((text . "First body.")
				       (location . ((segmentId . "fn-a")
						    (index . 1))))))))))))

(provide 'org-google-docs-footnotes-requests-test)
;;; org-google-docs-footnotes-requests-test.el ends here
