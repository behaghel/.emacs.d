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
(add-to-list 'load-path (expand-file-name "../../org-sync"
					  (file-name-directory load-file-name)))

(require 'org-google-docs-footnotes)

(defun org-google-docs-footnotes-test--document-with-text (start content)
  "Return minimal Google Docs JSON with text run CONTENT at START."
  (let* ((text-element (list (cons 'startIndex start)
			     (cons 'textRun (list (cons 'content content)))))
	 (paragraph (list (cons 'paragraph
				(list (cons 'elements (vector text-element)))))))
    (list (cons 'body (list (cons 'content (vector paragraph)))))))

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

(ert-deftest org-google-docs-footnotes-locates-sentinels-in-doc-json ()
  "Sentinel lookup returns exact UTF-16 document indices."
  (let* ((text-element (list (cons 'startIndex 10)
			     (cons 'textRun
				   '((content . "A 😀 ⟦GDOCS_FN:1:one⟧.")))))
	 (paragraph (list (cons 'paragraph
				(list (cons 'elements (vector text-element))))))
	 (document (list (cons 'body
			       (list (cons 'content (vector paragraph))))))
	 (location (org-google-docs-footnotes--locate-sentinel
		    document "⟦GDOCS_FN:1:one⟧")))
    (should (equal location '(:start 15 :end 31)))))

(ert-deftest org-google-docs-footnotes-builds-sentinel-create-requests ()
  "Sentinel footnote creation deletes anchor text before creating footnote."
  (let ((requests (org-google-docs-footnotes-sentinel-create-requests
		   (list (list :label "one"
			       :sentinel-start 12
			       :sentinel-end 30)))))
    (should (equal requests
		   '(((deleteContentRange
		       . ((range . ((startIndex . 12) (endIndex . 30))))))
		     ((createFootnote . ((location . ((index . 12)))))))))))

(ert-deftest org-google-docs-footnotes-batch-advice-mutates-in-descending-index-order ()
  "Batch advice locates sentinels and creates footnotes high to low."
  (let* ((session (list :references (vconcat (list (list :label "one"
							 :ordinal 1
							 :body "First body."
							 :sentinel "⟦GDOCS_FN:1:one⟧")
						   (list :label "two"
							 :ordinal 2
							 :body "Second body."
							 :sentinel "⟦GDOCS_FN:2:two⟧")))
			:cursor 0
			:previous-handler nil))
	 (org-google-docs-footnotes--push-session session)
	 (document (org-google-docs-footnotes-test--document-with-text
		    1 "A ⟦GDOCS_FN:1:one⟧ B ⟦GDOCS_FN:2:two⟧."))
	 calls)
    (cl-labels ((orig (_document-id requests callback &optional _account _on-error)
		  (push requests calls)
		  (funcall callback
			   (if (seq-find (lambda (request)
					   (alist-get 'createFootnote request))
					 requests)
			       '((replies . [nil
					     ((createFootnote . ((footnoteId . "fn-two"))))
					     nil
					     ((createFootnote . ((footnoteId . "fn-one"))))]))
			     '((replies . []))))))
      (cl-letf (((symbol-function 'gdocs-api-get-document)
		 (lambda (_document-id callback &optional _account _on-error)
		   (funcall callback document))))
	(org-google-docs-footnotes--around-batch-update
	 #'orig "doc-1" '(((insertText . ((text . "Body")))))
	 #'ignore))
      (should (equal (nth 1 calls)
		     '(((deleteContentRange
			 . ((range . ((startIndex . 22) (endIndex . 38))))))
		       ((createFootnote . ((location . ((index . 22))))))
		       ((deleteContentRange
			 . ((range . ((startIndex . 3) (endIndex . 19))))))
		       ((createFootnote . ((location . ((index . 3)))))))))
      (should (equal (car calls)
		     '(((insertText . ((text . "Second body.")
				       (location . ((segmentId . "fn-two")
						    (index . 1))))))
		       ((insertText . ((text . "First body.")
				       (location . ((segmentId . "fn-one")
						    (index . 1))))))))))))

(ert-deftest org-google-docs-footnotes-batch-advice-runs-post-body-update ()
  "Batch advice replaces sentinels and inserts bodies before main callback."
  (let* ((session (list :references (vconcat (list (list :label "one"
							 :ordinal 1
							 :body "First body."
							 :sentinel "⟦GDOCS_FN:1:one⟧")))
			:cursor 0
			:previous-handler nil))
	 (org-google-docs-footnotes--push-session session)
	 (document (org-google-docs-footnotes-test--document-with-text
		    1 "Body ⟦GDOCS_FN:1:one⟧"))
	 calls callback-ran)
    (cl-labels ((orig (_document-id requests callback &optional _account _on-error)
		  (push requests calls)
		  (funcall callback
			   (if (seq-find (lambda (request)
					   (alist-get 'createFootnote request))
					 requests)
			       '((replies . [nil ((createFootnote . ((footnoteId . "fn-a"))))]))
			     '((replies . []))))))
      (cl-letf (((symbol-function 'gdocs-api-get-document)
		 (lambda (_document-id callback &optional _account _on-error)
		   (funcall callback document))))
	(org-google-docs-footnotes--around-batch-update
	 #'orig "doc-1" '(((insertText . ((text . "Body")))))
	 (lambda (_response) (setq callback-ran t))))
      (should callback-ran)
      (should (= 3 (length calls)))
      (should (equal (car calls)
		     '(((insertText . ((text . "First body.")
				       (location . ((segmentId . "fn-a")
						    (index . 1))))))))))))

(provide 'org-google-docs-footnotes-requests-test)
;;; org-google-docs-footnotes-requests-test.el ends here
