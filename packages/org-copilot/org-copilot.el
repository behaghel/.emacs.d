;;; org-copilot.el --- Org-native AI authoring assistant -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Org Copilot provides AI authoring assistance as review comments and
;; suggestion workflows for Org buffers.
;;
;; This entry point intentionally does not require optional LLM adapters such as
;; gptel.  Adapter packages can be loaded separately.

;;; Code:

(require 'org)
(require 'org-context-panel)
(require 'org-copilot-model)
(require 'org-copilot-session)
(require 'org-copilot-context-panel)
(require 'org-copilot-diff)
(require 'org-copilot-suggestion)
(require 'org-copilot-llm)
(require 'org-copilot-chat)

(provide 'org-copilot)
;;; org-copilot.el ends here
