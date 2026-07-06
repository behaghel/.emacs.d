;;; org-comments-context-panel.el --- Org comments context-panel provider -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-comments

;;; Commentary:
;; Provider glue between org-comments sidecar/page-comment semantics and the
;; reusable `org-context-panel' primitives.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-context-panel)
(require 'org-comments-panel-actions)
(require 'org-comments-panel-filter)
(require 'org-comments-panel-render)
(require 'org-comments-store)

(declare-function org-comments-page-open "org-comments-page")
(autoload 'org-comments-panel-mode "org-comments-panel")
(defvar org-comments-panel-source-buffer)

(defcustom org-comments-panel-buffer-name "*Org Comments*"
  "Buffer name used for the standalone Org comments panel."
  :type 'string
  :group 'org-comments)

(defcustom org-comments-panel-width 38
  "Width of the standalone Org comments side window."
  :type 'natnum
  :group 'org-comments)

(defcustom org-comments-page-panel-buffer-name "*Org Page Comments*"
  "Buffer name used for the standalone Org page comments panel."
  :type 'string
  :group 'org-comments)

(defface org-comments-region-face
  '((t :inherit highlight :underline t))
  "Face used to mark commented source regions."
  :group 'org-comments)

(defvar-local org-comments-overlays nil
  "Inline comment overlays in the current Org source buffer.")

(defvar-local org-comments-page-comment-overlay nil
  "Page-comment marker overlay in the current Org source buffer.")

(defun org-comments-context-panel--delete-range-overlays ()
  "Delete Org comments range overlays in the current buffer."
  (org-context-panel-delete-overlays org-comments-overlays)
  (setq org-comments-overlays nil))

(defun org-comments-context-panel-delete-overlays ()
  "Delete Org comments context-panel overlays in the current buffer."
  (org-comments-context-panel--delete-range-overlays)
  (org-context-panel-delete-top-markers 'comments)
  (setq org-comments-page-comment-overlay nil))

(defun org-comments-context-panel-cleanup-source-overlays ()
  "Clean up Org comments source overlays in the current buffer."
  (org-comments-context-panel--delete-range-overlays)
  (setq org-comments-page-comment-overlay nil))

(defun org-comments-context-panel-page-marker-position ()
  "Return page comment marker position in the current Org buffer, or nil."
  (org-context-panel-marker-position
   org-comments-page-comment-overlay
   (lambda () (org-comments-collect-page (current-buffer)))))

(defun org-comments-context-panel-page-marker-at-point-p ()
  "Return non-nil when point is at the page comment marker position."
  (org-context-panel-marker-at-point-p
   org-comments-page-comment-overlay
   (lambda () (org-comments-collect-page (current-buffer)))))

(defun org-comments-context-panel--source-comment-p (comment)
  "Return non-nil when COMMENT belongs in source-context side panels."
  (and (eq (plist-get comment :type) 'comment)
       (not (plist-get comment :page-comment))))

(defun org-comments-context-panel--anchored-inline-comment-p (comment)
  "Return non-nil when COMMENT should have a source region overlay."
  (and (org-comments-context-panel--source-comment-p comment)
       (not (eq (plist-get comment :anchor-state) 'stale))))

(defun org-comments-context-panel-collect-side-items (source-buffer)
  "Collect side items for SOURCE-BUFFER."
  (cl-remove-if-not #'org-comments-context-panel--source-comment-p
		    (org-comments-collect source-buffer t)))

(defun org-comments-context-panel-collect-top-markers (source-buffer)
  "Collect top marker descriptors for SOURCE-BUFFER."
  (let ((page-comments (org-comments-collect-page source-buffer)))
    (when page-comments
      (list (list :id 'page-comments
		  :label (format "[%s PAGE comment%s]"
				 (length page-comments)
				 (if (= (length page-comments) 1) "" "s"))
		  :help "Open page comments"
		  :view-id 'page-comments
		  :overlay-variable 'org-comments-page-comment-overlay
		  :items page-comments)))))

(defun org-comments-context-panel-render-side-item (_source-buffer item)
  "Render one Org comments side-panel ITEM."
  (org-comments-panel-render-insert-comment item))

(defun org-comments-context-panel-jump-side-item (source-buffer item)
  "Jump from context-panel ITEM to its source or sidecar target.
SOURCE-BUFFER is the Org source buffer associated with ITEM."
  (let ((position (or (plist-get item :target-start)
		      (plist-get item :anchor-pos))))
    (if (org-comments-panel--sidecar-jump-comment-p item)
	(pop-to-buffer (org-comments-panel--goto-sidecar-heading item))
      (unless (and (buffer-live-p source-buffer) position)
	(user-error "Comment has no source location"))
      (pop-to-buffer source-buffer)
      (goto-char position))))

(defun org-comments-context-panel-render-side-panel (source-buffer _items)
  "Render the Org comments side panel for SOURCE-BUFFER.
ITEMS is accepted for the generic context-panel provider protocol; the current
comments renderer keeps using the existing source-buffer-scoped filter pipeline."
  (setq org-comments-panel-source-buffer source-buffer)
  (org-comments-panel-render-buffer
   source-buffer
   (org-comments-panel-filter-apply
    (with-current-buffer source-buffer
      (org-comments-collect source-buffer t))
    (org-comments-filter-state source-buffer))
   (org-comments-panel-filter-apply
    (with-current-buffer source-buffer
      (org-comments-collect-page source-buffer))
    (org-comments-filter-state source-buffer))))

(defun org-comments-context-panel-render-page-view (source-buffer _view)
  "Render the page-comments bottom VIEW for SOURCE-BUFFER."
  (setq org-comments-panel-source-buffer source-buffer)
  (org-comments-panel-render-buffer
   source-buffer
   nil
   (org-comments-panel-filter-apply
    (with-current-buffer source-buffer
      (org-comments-collect-page source-buffer))
    (org-comments-filter-state source-buffer))))

(defun org-comments-context-panel-collect-bottom-views (_source-buffer)
  "Collect Org comments bottom view descriptors."
  (list (list :id 'page-comments
	      :buffer-name org-comments-page-panel-buffer-name
	      :mode #'org-comments-panel-mode
	      :render #'org-comments-context-panel-render-page-view)))

(defun org-comments-context-panel-follow-source-p (source-buffer)
  "Return non-nil when SOURCE-BUFFER is an Org comments source buffer."
  (with-current-buffer source-buffer
    (not (or (and buffer-file-name
		  (string-suffix-p ".comments.org" buffer-file-name))
	     (string-match-p "\\.comments\\.org\\(?:<.*>\\)?\\'"
			     (buffer-name))))))

(defun org-comments-context-panel-provider ()
  "Return the org-comments context-panel provider descriptor."
  (list :name 'comments
	:collect-side-items #'org-comments-context-panel-collect-side-items
	:collect-top-markers #'org-comments-context-panel-collect-top-markers
	:collect-bottom-views #'org-comments-context-panel-collect-bottom-views
	:render-side-panel #'org-comments-context-panel-render-side-panel
	:render-side-item #'org-comments-context-panel-render-side-item
	:jump-side-item #'org-comments-context-panel-jump-side-item
	:priority 10
	:side-panel-mode #'org-comments-panel-mode
	:side-panel-buffer-name org-comments-panel-buffer-name
	:side-panel-width org-comments-panel-width
	:follow-source-p #'org-comments-context-panel-follow-source-p
	:refresh-source-overlays #'org-comments-context-panel-refresh-source-overlays
	:cleanup-source-overlays #'org-comments-context-panel-cleanup-source-overlays))

(defun org-comments-context-panel-refresh-source-overlays ()
  "Refresh Org comments source range overlays in the current Org buffer."
  (org-comments-context-panel--delete-range-overlays)
  (dolist (comment (cl-remove-if-not
		    #'org-comments-context-panel--anchored-inline-comment-p
		    (org-comments-context-panel-collect-side-items (current-buffer))))
    (let ((start (plist-get comment :target-start))
	  (end (plist-get comment :target-end)))
      (when-let* ((overlay (org-context-panel-make-range-overlay
			    start end
			    :face 'org-comments-region-face
			    :properties (list 'org-comments-comment comment))))
	(push overlay org-comments-overlays)))))

(defun org-comments-context-panel-enable ()
  "Enable Org comments as a context-panel provider in the current buffer."
  (org-context-panel-register-provider (org-comments-context-panel-provider))
  (org-context-panel-mode 1))

(defun org-comments-context-panel-disable ()
  "Disable Org comments as a context-panel provider in the current buffer."
  (org-comments-context-panel-delete-overlays)
  (org-context-panel-unregister-provider 'comments)
  (unless (org-context-panel-registered-providers)
    (org-context-panel-mode -1)))

(defun org-comments-context-panel-refresh ()
  "Refresh Org comments context-panel overlays in the current Org buffer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comment overlays only work in Org buffers"))
  (org-comments-context-panel-enable)
  (org-context-panel-refresh-source-overlays))

(provide 'org-comments-context-panel)
;;; org-comments-context-panel.el ends here
