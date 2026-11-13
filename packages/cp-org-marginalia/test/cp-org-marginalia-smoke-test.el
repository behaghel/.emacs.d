;;; cp-org-marginalia-smoke-test.el --- Smoke tests for cp-org-marginalia -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke-load tests for the standalone cp-org-marginalia package entry points.

;;; Code:

(require 'ert)

(ert-deftest cp-org-marginalia-smoke-loads-model-and-provider ()
  "The cp-org-marginalia model and context-panel provider load cleanly."
  (should (require 'cp-org-marginalia))
  (should (require 'cp-org-marginalia-context-panel))
  (should (fboundp 'cp-org-marginalia-collect))
  (should (fboundp 'cp-org-marginalia-layout))
  (should (fboundp 'cp-org-marginalia-context-panel-provider))
  (should (fboundp 'cp-org-marginalia-context-panel-mode)))

(provide 'cp-org-marginalia-smoke-test)
;;; cp-org-marginalia-smoke-test.el ends here
