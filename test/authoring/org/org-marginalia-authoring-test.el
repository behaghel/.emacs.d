;;; org-marginalia-authoring-test.el --- Org marginalia authoring tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for kind-specific Org footnote/marginalia authoring helpers.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'org/authoring)

(defmacro hub/org-marginalia-authoring-test--with-buffer (&rest body)
  "Run BODY in a temporary Org buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (org-mode)
     (insert "Text")
     (goto-char (point-max))
     ,@body))

(defun hub/org-marginalia-authoring-test--insert-with-body (command body)
  "Call COMMAND while stubbing minibuffer note BODY."
  (cl-letf (((symbol-function 'org-footnote-unique-label) (lambda (&rest _args) "one"))
	    ((symbol-function 'hub/org-yas-ready-p) (lambda () nil))
	    ((symbol-function 'read-string) (lambda (&rest _args) body)))
    (call-interactively command)))

(ert-deftest hub/org-insert-forced-footnote-template-adds-kind-property ()
  "The traditional footnote helper writes HUB_NOTE_KIND footnote metadata."
  (hub/org-marginalia-authoring-test--with-buffer
   (hub/org-marginalia-authoring-test--insert-with-body
    #'hub/org-insert-traditional-footnote-template "Legal note.")
   (should (string-match-p (regexp-quote "Text[fn:one]") (buffer-string)))
   (should (string-match-p (regexp-quote ":HUB_NOTE_KIND: footnote") (buffer-string)))
   (should (string-match-p (regexp-quote "Legal note.") (buffer-string)))))

(ert-deftest hub/org-tempo-completes-kind-specific-footnote-shortcuts ()
  "The <ft shortcut expands to a traditional footnote helper."
  (hub/org-marginalia-authoring-test--with-buffer
   (insert " <ft")
   (cl-letf (((symbol-function 'hub/org-insert-traditional-footnote-template)
	      (lambda () (insert "TRADITIONAL"))))
     (should (hub/org-tempo-complete-traditional-footnote))
     (should (string-suffix-p " TRADITIONAL" (buffer-string))))))

(provide 'org-marginalia-authoring-test)
;;; org-marginalia-authoring-test.el ends here
