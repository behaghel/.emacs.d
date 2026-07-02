;;; org-comments-legacy.el --- Legacy Org sidecar comment storage -*- lexical-binding: t; -*-

;;; Commentary:
;; Plain Org sidecar storage for region-targeted comments.  Source Org buffers
;; remain clean; comments are stored next to them as `article.comments.org'.

;;; Code:

(require 'cl-lib)
(require 'org-comments-core)
(require 'org-comments-model)
(require 'org-comments-sidecar)
(require 'org-comments-store)
(require 'org-comments-links)
(require 'org-comments-target)
(require 'dom)
(require 'org)
(require 'ol)
(require 'seq)
(require 'subr-x)

(require 'org-comments-anchors)

(provide 'org-comments-legacy)
;;; org-comments-legacy.el ends here
