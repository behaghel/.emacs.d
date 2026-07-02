;;; org-comments-smoke-test.el --- Smoke tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke tests for the initial package extraction bridge.

;;; Code:

(require 'ert)
(require 'org-comments)

(ert-deftest org-comments-smoke-loads-package ()
  "The package entrypoint loads the initial compatibility implementation."
  (should (featurep 'org-comments))
  (should (featurep 'org-comments-core))
  (should (featurep 'org-comments-target))
  (should (featurep 'org-comments-sidecar))
  (should (featurep 'org-comments-store))
  (should (featurep 'org-comments-anchors))
  (should (featurep 'org-comments-links))
  (should (featurep 'org-comments-compose))
  (should (featurep 'org-comments-ui))
  (should (featurep 'org-context-panel))
  (should (featurep 'org-comments-context-panel))
  (should (featurep 'org-comments-overlays))
  (should (featurep 'org-comments-panel-actions))
  (should (featurep 'org-comments-panel-filter))
  (should (featurep 'org-comments-panel-render))
  (should (featurep 'org-comments-panel))
  (should (featurep 'org-comments-page-panel))
  (should (featurep 'org-comments-page))
  (should (featurep 'org-comments-commands))
  (should (featurep 'org-comments-migrate))
  (should (featurep 'org-comments-backend))
  (should (featurep 'org-comments-backend-org))
  (should (featurep 'org-comments-legacy))
  (should (org-comments-backend-capable-p 'org :list-comments))
  (should (fboundp 'org-comments-sidecar-path)))

(provide 'org-comments-smoke-test)
;;; org-comments-smoke-test.el ends here
