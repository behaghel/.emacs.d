;;; org-comments-anchors-test.el --- Anchor tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for anchor matching helpers extracted from the legacy implementation.

;;; Code:

(require 'ert)
(require 'org-comments-anchors)

(ert-deftest org-comments-anchors-finds-normalized-target-match ()
  "Anchor matching finds normalized text in a source buffer."
  (with-temp-buffer
    (insert "Alpha selected\n   text omega")
    (let ((matches (org-comments--anchor-matches-for-text
		    (current-buffer) "selected text")))
      (should (= 1 (length matches)))
      (should (equal (buffer-substring-no-properties
		      (caar matches)
		      (cdar matches))
		     "selected\n   text")))))

(ert-deftest org-comments-anchors-similarity-identical-text-is-one ()
  "Text similarity returns one for identical strings."
  (should (= 1.0 (org-comments--similarity "Alpha" "Alpha"))))

(provide 'org-comments-anchors-test)
;;; org-comments-anchors-test.el ends here
