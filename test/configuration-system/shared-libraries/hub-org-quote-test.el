;;; hub-org-quote-test.el --- Tests for Org quote metadata helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies canonical quote attribute parsing for shared Org helpers.

;;; Code:

(require 'ert)
(require 'org)
(require 'ox)
(require 'hub-org-quote)

(defun hub-org-quote-test--first-quote (contents)
  "Return the first quote block parsed from CONTENTS."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'quote-block #'identity nil t)))

(ert-deftest hub/org-quote-author-reads-attr-quote-author ()
  "Quote author metadata comes from `#+ATTR_QUOTE: :author'."
  (let ((quote (hub-org-quote-test--first-quote
		"#+ATTR_QUOTE: :author \"A & B\"\n#+begin_quote\nText\n#+end_quote")))
    (should (equal (hub/org-quote-author quote) "A & B"))))

(ert-deftest hub/org-quote-author-ignores-empty-author ()
  "Empty quote author metadata is treated as absent."
  (let ((quote (hub-org-quote-test--first-quote
		"#+ATTR_QUOTE: :author \"\"\n#+begin_quote\nText\n#+end_quote")))
    (should-not (hub/org-quote-author quote))))

(provide 'hub-org-quote-test)
;;; hub-org-quote-test.el ends here
