;;; org-marginalia-smoke-test.el --- Smoke tests for org-marginalia -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke-load tests for the standalone org-marginalia package entry points.

;;; Code:

(require 'ert)

(ert-deftest org-marginalia-smoke-loads-model-and-provider ()
  "The org-marginalia model and context-panel provider load cleanly."
  (should (require 'org-marginalia))
  (should (require 'org-marginalia-context-panel))
  (should (fboundp 'org-marginalia-collect))
  (should (fboundp 'org-marginalia-layout))
  (should (fboundp 'org-marginalia-context-panel-provider))
  (should (fboundp 'org-marginalia-context-panel-mode)))

(provide 'org-marginalia-smoke-test)
;;; org-marginalia-smoke-test.el ends here
