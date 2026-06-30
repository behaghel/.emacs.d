;;; prose-wrapping-test.el --- Tests for visual prose wrapping -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify that document authoring uses visual wrapping without inserting hard
;; line breaks through auto-fill.

;;; Code:

(require 'ert)
(require 'org)
(require 'hub-prose)

(unless (fboundp 'adaptive-wrap-prefix-mode)
  (define-minor-mode adaptive-wrap-prefix-mode
    "Test stub for `adaptive-wrap-prefix-mode'."))
(unless (featurep 'adaptive-wrap)
  (provide 'adaptive-wrap))

(unless (fboundp 'visual-fill-column-mode)
  (define-minor-mode visual-fill-column-mode
    "Test stub for `visual-fill-column-mode'."))
(unless (featurep 'visual-fill-column)
  (provide 'visual-fill-column))

(unless (fboundp 'org-phscroll-mode)
  (define-minor-mode org-phscroll-mode
    "Test stub for `org-phscroll-mode'."))
(unless (featurep 'org-phscroll)
  (provide 'org-phscroll))

(ert-deftest hub/prose-wrapping-is-visual-not-hard-fill ()
  "Verify prose wrapping is visual and leaves hard auto-fill disabled."
  (with-temp-buffer
    (text-mode)
    (auto-fill-mode 1)
    (let ((hub/prose-visual-fill-column 73))
      (hub/prose-visual-fill-mode))
    (should-not auto-fill-function)
    (should visual-line-mode)
    (should-not truncate-lines)
    (should word-wrap)
    (should adaptive-wrap-prefix-mode)
    (should (= adaptive-wrap-extra-indent 0))
    (should visual-fill-column-mode)
    (should (= visual-fill-column-width 73))
    (should visual-fill-column-center-text)))

(ert-deftest hub/org-prose-wrapping-keeps-org-indent-prefixes ()
  "Org virtual indentation owns wrap prefixes when enabled."
  (with-temp-buffer
    (insert "* Heading\nBody line that should inherit heading indentation.\n")
    (let ((org-startup-indented t))
      (org-mode))
    (adaptive-wrap-prefix-mode 1)
    (hub/prose-visual-fill-mode)
    (goto-char (point-min))
    (forward-line 1)
    (should (bound-and-true-p org-indent-mode))
    (should-not adaptive-wrap-prefix-mode)
    (should (get-text-property (point) 'line-prefix))
    (should (get-text-property (point) 'wrap-prefix))
    (should visual-line-mode)
    (should word-wrap)))

(ert-deftest hub/org-prose-wrapping-enables-table-horizontal-scroll ()
  "Org prose wrapping enables table-local horizontal scrolling when available."
  (with-temp-buffer
    (org-mode)
    (let ((hub/prose-org-table-horizontal-scroll t))
      (hub/prose-visual-fill-mode))
    (should org-phscroll-mode)))

(provide 'prose-wrapping-test)
;;; prose-wrapping-test.el ends here
