;;; org-comments-overlays.el --- Persistent overlays for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Public overlay activation facade for Org sidecar comments.  Comments-specific
;; provider glue lives in `org-comments-context-panel'; generic refresh hooks are
;; owned by `org-context-panel-mode'.

;;; Code:

(require 'org-comments-context-panel)

;;;###autoload
(defun org-comments-overlays-delete ()
  "Delete Org comments overlays in the current buffer."
  (org-comments-context-panel-delete-overlays))

(defun org-comments-page-comment-marker-position ()
  "Return page comment marker position in the current Org buffer, or nil."
  (org-comments-context-panel-page-marker-position))

(defun org-comments-page-comment-marker-at-point-p ()
  "Return non-nil when point is at the page comment marker position."
  (org-comments-context-panel-page-marker-at-point-p))

;;;###autoload
(defun org-comments-overlays-refresh ()
  "Refresh persistent sidecar comment overlays in the current Org buffer."
  (interactive)
  (org-comments-context-panel-refresh))

;;;###autoload
(defun org-comments-overlays-enable ()
  "Enable package-owned Org comments overlays in the current buffer."
  (org-comments-context-panel-enable))

;;;###autoload
(defun org-comments-overlays-disable ()
  "Disable package-owned Org comments overlays in the current buffer."
  (org-comments-context-panel-disable))

(provide 'org-comments-overlays)
;;; org-comments-overlays.el ends here
