;;; org-marginalia-test.el --- Org marginalia tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org marginalia collection and layout.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'hub-org-marginalia)

(defmacro hub/org-marginalia-test--with-buffer (contents &rest body)
  "Run BODY in a temporary Org buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(ert-deftest hub/org-marginalia-collects-native-footnote ()
  "A native Org footnote reference becomes a sidenote record."
  (hub/org-marginalia-test--with-buffer "Text[fn:one]\n\n[fn:one] Note body.\n"
					(let ((notes (hub/org-marginalia-collect)))
					  (should (= 1 (length notes)))
					  (should (equal "one" (plist-get (car notes) :id)))
					  (should (eq 'sidenote (plist-get (car notes) :kind)))
					  (should (equal "Note body." (plist-get (car notes) :body)))
					  (should (= 1 (plist-get (car notes) :anchor-line))))))

(ert-deftest hub/org-marginalia-parses-note-properties ()
  "HUB_NOTE_* properties under a footnote definition enrich the note record."
  (hub/org-marginalia-test--with-buffer "Text[fn:review]\n\n[fn:review]\n:PROPERTIES:\n:HUB_NOTE_KIND: footnote\n:HUB_NOTE_STATUS: open\n:HUB_NOTE_SOURCE: confluence\n:HUB_NOTE_REMOTE_ID: 123\n:END:\nPlease revise.\n"
					(let ((note (car (hub/org-marginalia-collect))))
					  (should (eq 'footnote (plist-get note :kind)))
					  (should (equal "open" (plist-get note :status)))
					  (should (equal "confluence" (plist-get note :source)))
					  (should (equal "123" (plist-get note :remote-id)))
					  (should (equal "Please revise." (plist-get note :body))))))

(ert-deftest hub/org-marginalia-strips-definition-colon-and-metadata ()
  "Footnote definitions with colon separators hide metadata from note body."
  (hub/org-marginalia-test--with-buffer "Text[fn:review]\n\n[fn:review]:\n:PROPERTIES:\n:HUB_NOTE_KIND: footnote\n:END:\nTraditional footnote.\n"
					(let ((note (car (hub/org-marginalia-collect))))
					  (should (eq 'footnote (plist-get note :kind)))
					  (should (equal "Traditional footnote." (plist-get note :body))))))

(ert-deftest hub/org-marginalia-preserves-reference-order ()
  "Marginalia records follow reference order, not definition order."
  (hub/org-marginalia-test--with-buffer "First[fn:b]\nSecond[fn:a]\n\n[fn:a] A.\n\n[fn:b] B.\n"
					(let ((notes (hub/org-marginalia-collect)))
					  (should (equal '("b" "a") (mapcar (lambda (note) (plist-get note :id)) notes))))))

(ert-deftest hub/org-marginalia-accepts-indented-definitions ()
  "Indented footnote definitions are definitions, not extra references."
  (hub/org-marginalia-test--with-buffer "Text[fn:one]\n\n  [fn:one] Note body.\n"
					(let ((notes (hub/org-marginalia-collect)))
					  (should (= 1 (length notes)))
					  (should (equal "Note body." (plist-get (car notes) :body))))))

(ert-deftest hub/org-marginalia-layout-pushes-overlaps-down ()
  "Close marginalia boxes are pushed downward while preserving order."
  (let* ((notes (list (list :id "a" :anchor-line 10 :height 3)
		      (list :id "b" :anchor-line 11 :height 2)
		      (list :id "c" :anchor-line 20 :height 1)))
	 (laid-out (hub/org-marginalia-layout notes)))
    (should (equal '(10 14 20)
		   (mapcar (lambda (note) (plist-get note :display-line)) laid-out)))
    (should-not (plist-get (nth 0 laid-out) :displaced))
    (should (plist-get (nth 1 laid-out) :displaced))
    (should-not (plist-get (nth 2 laid-out) :displaced))))

(provide 'org-marginalia-test)
;;; org-marginalia-test.el ends here
