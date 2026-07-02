;;; org-marginalia-test.el --- Tests for org-marginalia -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org marginalia collection and layout.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-marginalia)

(ert-deftest org-marginalia-collects-native-footnotes ()
  "Collect native Org footnotes with reference and definition positions."
  (with-temp-buffer
    (org-mode)
    (insert "Text[fn:one]\n\n[fn:one] Note body.\n")
    (let ((note (car (org-marginalia-collect))))
      (should (equal (plist-get note :id) "one"))
      (should (equal (plist-get note :body) "Note body."))
      (should (integerp (plist-get note :reference-pos)))
      (should (integerp (plist-get note :definition-pos))))))

(ert-deftest org-marginalia-preserves-note-kind-property ()
  "Collect `HUB_NOTE_KIND' from footnote property drawers."
  (with-temp-buffer
    (org-mode)
    (insert "Text[fn:one]\n\n[fn:one]\n:PROPERTIES:\n:HUB_NOTE_KIND: footnote\n:END:\n\nBody.\n")
    (let ((note (car (org-marginalia-collect))))
      (should (eq (plist-get note :kind) 'footnote)))))

(ert-deftest org-marginalia-layout-displaces-overlapping-notes ()
  "Layout moves overlapping notes to non-overlapping display lines."
  (let* ((notes '((:id "one" :anchor-line 1 :height 3)
		  (:id "two" :anchor-line 2 :height 1)))
	 (laid-out (org-marginalia-layout notes 1)))
    (should (= (plist-get (nth 0 laid-out) :display-line) 1))
    (should (= (plist-get (nth 1 laid-out) :display-line) 5))
    (should (plist-get (nth 1 laid-out) :displaced))))

(provide 'org-marginalia-test)
;;; org-marginalia-test.el ends here
