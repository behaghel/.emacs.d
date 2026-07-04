;;; org-copilot-suggestion.el --- Suggestion previews for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Preview and anchoring helpers for document and section-level Org Copilot
;; suggestions.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'org-context-panel)
(require 'org-copilot-model)
(require 'org-copilot-session)

(defcustom org-copilot-suggestion-buffer-name "*Org Copilot Suggestion*"
  "Buffer name used for Org Copilot document/section suggestion previews."
  :type 'string
  :group 'org-copilot)

(defvar-local org-copilot-suggestion-source-buffer nil
  "Source buffer associated with the current suggestion preview.")

(defvar-local org-copilot-suggestion-comment-id nil
  "Comment id associated with the current suggestion preview, or nil.")

(defvar-local org-copilot-suggestion-kind nil
  "Kind of suggestion displayed in the current preview buffer.")

(define-derived-mode org-copilot-suggestion-mode org-mode "Org-Copilot-Suggestion"
  "Major mode for read-only Org Copilot suggestion previews."
  (setq buffer-read-only t))

(defun org-copilot-suggestion--protect-window (window panel-buffer source-buffer)
  "Protect WINDOW for PANEL-BUFFER and SOURCE-BUFFER when supported."
  (when (and (window-live-p window)
	     (fboundp 'org-context-panel-protect-window))
    (org-context-panel-protect-window window panel-buffer source-buffer)))

(defun org-copilot-suggestion-normalize-section-body (suggestion)
  "Normalize section body SUGGESTION for insertion."
  (concat (string-trim suggestion) "\n"))

(defun org-copilot-suggestion--status-label (kind)
  "Return preview status label for suggestion KIND."
  (pcase kind
    ('section "can accept")
    (_ "preview only")))

(defun org-copilot-suggestion-open (source-buffer title content &optional kind comment-id select)
  "Open a left-side suggestion preview for SOURCE-BUFFER.
TITLE labels the preview and CONTENT is rendered read-only.  KIND and
COMMENT-ID are stored as preview metadata.  When SELECT is non-nil, select the
preview window; otherwise preserve the current selected window."
  (let ((buffer (get-buffer-create org-copilot-suggestion-buffer-name))
	(selected-window (selected-window)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(org-copilot-suggestion-mode)
	(setq org-copilot-suggestion-source-buffer source-buffer)
	(setq org-copilot-suggestion-kind kind)
	(setq org-copilot-suggestion-comment-id comment-id)
	(erase-buffer)
	(insert (format "Org Copilot Suggestion — %s · %s\n\n"
			title
			(org-copilot-suggestion--status-label kind)))
	(insert content)
	(unless (string-suffix-p "\n" content)
	  (insert "\n"))
	(goto-char (point-min))))
    (let ((window (display-buffer-in-side-window
		   buffer
		   '((side . left)
		     (slot . 1)
		     (window-width . 0.38)
		     (window-parameters
		      . ((no-other-window . t)
			 (no-delete-other-windows . t)))))))
      (org-copilot-suggestion--protect-window window buffer source-buffer)
      (cond
       (select (select-window window))
       ((window-live-p selected-window) (select-window selected-window))))
    buffer))

(defun org-copilot-suggestion--heading-line ()
  "Return the current Org heading line without text properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun org-copilot-suggestion--section-bounds-at-heading ()
  "Return section bounds at current heading as a plist."
  (let* ((heading-start (point))
	 (heading-line (org-copilot-suggestion--heading-line))
	 (section-title (org-get-heading t t t t))
	 (section-path (org-get-outline-path t t))
	 (body-start (save-excursion
		       (forward-line 1)
		       (point)))
	 (end (save-excursion
		(org-end-of-subtree t t)
		(point))))
    (list :heading-start heading-start
	  :heading-line heading-line
	  :section-title section-title
	  :section-path section-path
	  :body-start body-start
	  :end end)))

(defun org-copilot-suggestion--all-section-bounds ()
  "Return all Org section bounds in the current buffer."
  (let (sections)
    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward org-heading-regexp nil t)
	 (beginning-of-line)
	 (push (org-copilot-suggestion--section-bounds-at-heading) sections)
	 (org-end-of-subtree t t))))
    (nreverse sections)))

(defun org-copilot-suggestion--section-path-equal-p (left right)
  "Return non-nil when outline paths LEFT and RIGHT are equal."
  (and (listp left)
       (listp right)
       (equal left right)))

(defun org-copilot-suggestion-resolve-section (source-buffer suggestion context)
  "Resolve section anchor for SUGGESTION in SOURCE-BUFFER using CONTEXT.
SUGGESTION and CONTEXT are plists.  Return section bounds or nil."
  (with-current-buffer source-buffer
    (let* ((heading-line (plist-get suggestion :heading-line))
	   (section-title (plist-get suggestion :section-title))
	   (section-path (plist-get suggestion :section-path))
	   (sections (org-copilot-suggestion--all-section-bounds)))
      (cond
       (heading-line
	(cl-find heading-line sections
		 :key (lambda (section) (plist-get section :heading-line))
		 :test #'string=))
       (section-path
	(cl-find section-path sections
		 :key (lambda (section) (plist-get section :section-path))
		 :test #'org-copilot-suggestion--section-path-equal-p))
       (section-title
	(let ((matches (cl-remove-if-not
			(lambda (section)
			  (string= section-title
				   (plist-get section :section-title)))
			sections)))
	  (and (= (length matches) 1) (car matches))))
       ((eq (plist-get context :type) 'section)
	(let ((marker (plist-get context :heading-marker)))
	  (and (markerp marker)
	       (marker-buffer marker)
	       (save-excursion
		 (goto-char marker)
		 (org-copilot-suggestion--section-bounds-at-heading)))))))))

(defun org-copilot-suggestion--next-id ()
  "Return a session-local suggestion comment id."
  (format "ai-section-%d" (1+ (length (org-copilot-comments)))))

(defun org-copilot-suggestion--source-buffer ()
  "Return source buffer for suggestion commands."
  (cond
   ((buffer-live-p org-copilot-suggestion-source-buffer)
    org-copilot-suggestion-source-buffer)
   ((buffer-live-p org-context-panel-source-buffer)
    org-context-panel-source-buffer)
   ((derived-mode-p 'org-mode)
    (current-buffer))
   (t
    (org-context-panel-current-source-buffer))))

(defun org-copilot-suggestion-section-comment-p (comment)
  "Return non-nil when COMMENT is an anchored section suggestion."
  (and (plist-get comment :suggestion)
       (or (plist-get comment :section-title)
	   (plist-get comment :heading-line)
	   (plist-get comment :section-path))))

;;;###autoload
(defun org-copilot-view-suggestion-at-point (&optional select)
  "Open left-side preview for the section suggestion at point.
With prefix argument SELECT, select the preview window.  Interactively, the
preview window is selected by default."
  (interactive (list t))
  (let* ((source-buffer (org-copilot-suggestion--source-buffer))
	 (item (or (org-context-panel-item-at-point)
		   (user-error "No AI suggestion at point")))
	 (comment (or (and (org-copilot-comment-id item)
			   (with-current-buffer source-buffer
			     (org-copilot-find-comment
			      (org-copilot-comment-id item))))
		      item)))
    (unless (org-copilot-suggestion-section-comment-p comment)
      (user-error "No section suggestion preview at point"))
    (org-copilot-suggestion-open
     source-buffer
     (format "§ Section: %s"
	     (or (plist-get comment :section-title)
		 (plist-get comment :heading-line)
		 "current"))
     (plist-get comment :suggestion)
     'section
     (org-copilot-comment-id comment)
     select)))

(defun org-copilot-suggestion-install-section
    (source-buffer suggestion context &optional message)
  "Install anchored section SUGGESTION in SOURCE-BUFFER using CONTEXT.
MESSAGE is used as the comment body when non-nil.  Return the installed comment
or nil when the section cannot be resolved."
  (when-let* ((section (org-copilot-suggestion-resolve-section
			source-buffer suggestion context))
	      (replacement (plist-get suggestion :suggestion)))
    (with-current-buffer source-buffer
      (let* ((body-start (plist-get section :body-start))
	     (end (plist-get section :end))
	     (target (buffer-substring-no-properties body-start end))
	     (normalized (org-copilot-suggestion-normalize-section-body
			  replacement))
	     (comment (org-copilot-add-comment
		       (list :id (org-copilot-suggestion--next-id)
			     :type 'scope
			     :status 'active
			     :source-start body-start
			     :source-end end
			     :target-text target
			     :suggestion normalized
			     :summary (or (plist-get suggestion :summary)
					  "Section suggestion")
			     :body (or message
				       (plist-get suggestion :message)
				       "Section suggestion")
			     :heading-line (plist-get section :heading-line)
			     :section-title (plist-get section :section-title)
			     :section-path (plist-get section :section-path)))))
	(org-copilot-suggestion-open
	 source-buffer
	 (format "§ Section: %s" (plist-get section :section-title))
	 normalized 'section (org-copilot-comment-id comment))
	comment))))

(provide 'org-copilot-suggestion)
;;; org-copilot-suggestion.el ends here
