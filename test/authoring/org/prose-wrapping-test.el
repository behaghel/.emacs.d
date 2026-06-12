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

(ert-deftest hub/org-prose-wrapping-is-visual-not-hard-fill ()
  "Verify Org prose wrapping is visual and leaves hard auto-fill disabled."
  (with-temp-buffer
    (org-mode)
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

(provide 'prose-wrapping-test)
;;; prose-wrapping-test.el ends here
