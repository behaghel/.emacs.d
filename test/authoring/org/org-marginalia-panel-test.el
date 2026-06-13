;;; org-marginalia-panel-test.el --- Org marginalia panel tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the interactive Org marginalia panel renderer.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'org/marginalia)

(defmacro hub/org-marginalia-panel-test--with-source (contents &rest body)
  "Run BODY in a temporary Org source buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(ert-deftest hub/org-marginalia-panel-renders-side-notes ()
  "The panel renderer inserts note bodies in display-line order."
  (hub/org-marginalia-panel-test--with-source "Text[fn:one]\n\n[fn:one] Note body.\n"
					      (let ((panel (generate-new-buffer " *hub marginalia test*")))
						(unwind-protect
						    (let ((source (current-buffer)))
						      (hub/org-marginalia-render-buffer source panel)
						      (with-current-buffer panel
							(should (equal source hub/org-marginalia-panel-source-buffer))
							(should buffer-read-only)
							(should (search-forward "Note body." nil t))))
						  (kill-buffer panel)))))

(ert-deftest hub/org-marginalia-note-with-viewport-anchor-replaces-anchor ()
  "Viewport anchoring must replace, not append after, the original anchor line."
  (let ((note (hub/org-marginalia--note-with-viewport-anchor
	       '(:id "one" :anchor-line 486 :body "x") 21)))
    (should (= 21 (plist-get note :anchor-line)))
    (should (= 486 (plist-get note :logical-anchor-line)))))

(ert-deftest hub/org-marginalia-panel-records-jump-targets ()
  "Rendered notes carry their source footnote definition position."
  (hub/org-marginalia-panel-test--with-source "Text[fn:one]\n\n[fn:one] Note body.\n"
					      (let ((panel (generate-new-buffer " *hub marginalia target test*")))
						(unwind-protect
						    (progn
						      (hub/org-marginalia-render-buffer (current-buffer) panel)
						      (with-current-buffer panel
							(goto-char (point-min))
							(search-forward "Note body.")
							(let ((note (get-text-property (point) 'hub-org-marginalia-note)))
							  (should (equal "one" (plist-get note :id)))
							  (should (integerp (plist-get note :definition-pos))))))
						  (kill-buffer panel)))))

(provide 'org-marginalia-panel-test)
;;; org-marginalia-panel-test.el ends here
