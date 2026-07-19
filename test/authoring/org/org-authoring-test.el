;;; org-authoring-test.el --- Org authoring helper tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for interactive Org authoring templates and Tempo integrations.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'test-helpers)
(require 'org/authoring)

(defmacro hub-org-authoring-test--with-buffer (&rest body)
  "Run BODY in a temporary Org buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (org-mode)
     ,@body))

(ert-deftest hub/org-insert-quote-template-prompts-for-optional-author ()
  "The quote template inserts attribution metadata when an author is provided."
  (hub-org-authoring-test--with-buffer
   (cl-letf (((symbol-function 'hub/org-yas-ready-p) (lambda () nil))
	     ((symbol-function 'read-string) (lambda (&rest _args) "Ada Lovelace")))
     (hub/org-insert-quote-template))
   (should (equal (buffer-string)
		  "#+ATTR_QUOTE: :author \"Ada Lovelace\"\n#+begin_quote\n\n#+end_quote"))
   (should (equal (buffer-substring-no-properties (point) (point-max))
		  "\n#+end_quote"))))

(ert-deftest hub/org-insert-quote-template-allows-plain-quote ()
  "An empty quote author prompt keeps the standard quote shape."
  (hub-org-authoring-test--with-buffer
   (cl-letf (((symbol-function 'hub/org-yas-ready-p) (lambda () nil))
	     ((symbol-function 'read-string) (lambda (&rest _args) "")))
     (hub/org-insert-quote-template))
   (should (equal (buffer-string) "#+begin_quote\n\n#+end_quote"))))

(ert-deftest hub/org-tempo-complete-quote-expands-q-shortcut ()
  "The `<q' shortcut expands through the semantic quote template."
  (hub-org-authoring-test--with-buffer
   (insert "<q")
   (cl-letf (((symbol-function 'hub/org-insert-quote-template)
	      (lambda () (insert "QUOTE"))))
     (should (hub/org-tempo-complete-quote))
     (should (equal (buffer-string) "QUOTE")))))

(provide 'org-authoring-test)
;;; org-authoring-test.el ends here
