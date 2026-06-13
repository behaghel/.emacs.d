;;; org-context-panel-test.el --- Org context panel tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the interactive Org context panel renderer.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'org/context-panel)

(defmacro hub/org-context-panel-test--with-source (contents &rest body)
  "Run BODY in a temporary Org source buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(ert-deftest hub/org-context-panel-renders-side-notes ()
  "The panel renderer inserts note bodies in display-line order."
  (hub/org-context-panel-test--with-source "Text[fn:one]\n\n[fn:one] Note body.\n"
					   (let ((panel (generate-new-buffer " *hub context panel test*")))
					     (unwind-protect
						 (let ((source (current-buffer)))
						   (hub/org-context-panel-render-buffer source panel)
						   (with-current-buffer panel
						     (should (equal source hub/org-context-panel-source-buffer))
						     (should buffer-read-only)
						     (should (search-forward "Note body." nil t))))
					       (kill-buffer panel)))))

(ert-deftest hub/org-context-panel-marginalia-with-viewport-anchor-replaces-anchor ()
  "Viewport anchoring must replace, not append after, the original anchor line."
  (let ((note (hub/org-context-panel--marginalia-with-viewport-anchor
	       '(:id "one" :anchor-line 486 :body "x") 21)))
    (should (= 21 (plist-get note :anchor-line)))
    (should (= 486 (plist-get note :logical-anchor-line)))))

(ert-deftest hub/org-context-panel-records-jump-targets ()
  "Rendered notes carry their source footnote definition position."
  (hub/org-context-panel-test--with-source "Text[fn:one]\n\n[fn:one] Note body.\n"
					   (let ((panel (generate-new-buffer " *hub context panel target test*")))
					     (unwind-protect
						 (progn
						   (hub/org-context-panel-render-buffer (current-buffer) panel)
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (search-forward "Note body.")
						     (let ((note (get-text-property (point) 'hub-org-context-panel-item)))
						       (should (equal "one" (plist-get note :id)))
						       (should (integerp (plist-get note :definition-pos))))))
					       (kill-buffer panel)))))

(provide 'org-context-panel-test)
;;; org-context-panel-test.el ends here
