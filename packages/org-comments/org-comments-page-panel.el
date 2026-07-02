;;; org-comments-page-panel.el --- Page panel for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Page-only panel entry point for Org comments.  It reuses the generic panel
;; mode, filters, rendering, and actions, but renders only page-level comments.

;;; Code:

(require 'org)
(require 'org-context-panel)
(require 'org-comments-context-panel)

;;;###autoload
(defun org-comments-page-panel-open ()
  "Open or refresh the standalone page comments panel for the current source."
  (interactive)
  (let ((source-buffer (org-context-panel-current-source-buffer)))
    (with-current-buffer source-buffer
      (org-comments-context-panel-enable)
      (org-context-panel-open-bottom-view 'page-comments source-buffer))))

;;;###autoload
(defun org-comments-page-panel-refresh ()
  "Refresh the standalone Org page comments panel."
  (interactive)
  (org-context-panel-refresh-bottom-view))

(provide 'org-comments-page-panel)
;;; org-comments-page-panel.el ends here
