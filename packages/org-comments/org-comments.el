;;; org-comments.el --- Org-native comments with backend-ready storage -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-comments

;;; Commentary:
;; Entry point for the reusable Org comments package.  Requiring this feature
;; loads the generic sidecar store, backend protocol, UI adapters, overlays,
;; panels, and `org-comments-mode' activation surface.

;;; Code:

(require 'org-comments-core)
(require 'org-comments-target)
(require 'org-comments-model)
(require 'org-comments-collaboration)
(require 'org-comments-sidecar)
(require 'org-comments-store)
(require 'org-comments-anchors)
(require 'org-comments-links)
(require 'org-comments-backend)
(require 'org-comments-legacy)
(require 'org-comments-backend-org)
(require 'org-comments-compose)
(require 'org-comments-ui)
(require 'org-context-panel)
(require 'org-comments-context-panel)
(require 'org-comments-overlays)
(require 'org-comments-panel-actions)
(require 'org-comments-panel-filter)
(require 'org-comments-panel-render)
(require 'org-comments-panel)
(require 'org-comments-page-panel)
(require 'org-comments-page)
(require 'org-comments-commands)
(require 'org-comments-migrate)

(provide 'org-comments)
;;; org-comments.el ends here
