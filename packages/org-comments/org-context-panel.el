;;; org-context-panel.el --- Org context panel primitives -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-comments

;;; Commentary:
;; Reusable Org context panel primitives.
;;
;; This file is intentionally a package-in-package inside org-comments.  It
;; provides generic source overlay and top-marker mechanics; providers such as
;; org-comments own item semantics and rendering.

;;; Code:

(require 'cl-lib)
(require 'org)

(defgroup org-context-panel nil
  "Reusable Org context panel mechanics."
  :group 'org)

(defcustom org-context-panel-buffer-name "*Org Context Panel*"
  "Buffer name used for generic Org context side panels."
  :type 'string
  :group 'org-context-panel)

(defcustom org-context-panel-width 38
  "Width of generic Org context side panels."
  :type 'natnum
  :group 'org-context-panel)

(defcustom org-context-panel-bottom-buffer-name "*Org Context View*"
  "Fallback buffer name used for generic Org context bottom views."
  :type 'string
  :group 'org-context-panel)

(defcustom org-context-panel-bottom-height 12
  "Height of generic Org context bottom views."
  :type 'natnum
  :group 'org-context-panel)

(defface org-context-panel-marker-face
  '((t :inherit link))
  "Default face for Org context top markers."
  :group 'org-context-panel)

(defvar-local org-context-panel-providers nil
  "Provider descriptors registered for the current Org source buffer.")

(defvar-local org-context-panel-top-marker-overlays nil
  "Top marker overlays rendered for registered providers.")

(defvar-local org-context-panel-source-buffer nil
  "Org source buffer associated with the current context panel buffer.")

(defvar-local org-context-panel-side-panel-buffer nil
  "Side panel buffer associated with the current Org source buffer.")

(defvar-local org-context-panel-bottom-panel-buffer nil
  "Bottom view buffer associated with the current Org source buffer.")

(defvar-local org-context-panel-view-id nil
  "Provider view id rendered by the current context-panel buffer.")

(defvar org-context-panel--following nil
  "Non-nil while `org-context-panel' is following selected windows.")

(defvar org-context-panel--repairing-window nil
  "Non-nil while restoring a protected context-panel window.")

(defvar org-context-panel-current-source-buffer nil
  "Source buffer dynamically bound while running side-panel lifecycle hooks.")

(defvar org-context-panel-current-source-window nil
  "Source window dynamically bound while running side-panel lifecycle hooks.")

(defvar org-context-panel-current-panel-buffer nil
  "Panel buffer dynamically bound while running side-panel lifecycle hooks.")

(defvar org-context-panel-current-panel-window nil
  "Panel window dynamically bound while running side-panel lifecycle hooks.")

(defvar org-context-panel-after-side-panel-open-hook nil
  "Hook run after opening or refreshing a side context panel.")

(defvar org-context-panel-before-side-panel-close-hook nil
  "Hook run before closing a side context panel.")

(defvar org-context-panel-auxiliary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'org-context-panel-close-current-window)
    map)
  "Shared keymap for dismissible Org context-panel auxiliary views.")

(define-derived-mode org-context-panel-buffer-mode special-mode "Org-Context"
  "Major mode for generic Org context side panel buffers.")

(defun org-context-panel-close-current-window ()
  "Close the current context-panel auxiliary window."
  (interactive)
  (quit-window t))

(defun org-context-panel--refresh-after-save ()
  "Refresh context-panel source overlays after saving the current buffer."
  (when org-context-panel-mode
    (org-context-panel-refresh-source-overlays)))

(defun org-context-panel--provider-name (provider)
  "Return PROVIDER's name, or signal an error when missing."
  (let ((name (plist-get provider :name)))
    (unless name
      (error "Org context panel provider is missing :name"))
    name))

(defun org-context-panel-register-provider (provider)
  "Register PROVIDER for the current buffer and return it.
PROVIDER is a plist descriptor.  Registering another provider with the same
`:name' replaces the previous descriptor while preserving registration order."
  (let ((name (org-context-panel--provider-name provider)))
    (setq org-context-panel-providers
	  (append (cl-remove name org-context-panel-providers
			     :key (lambda (entry) (plist-get entry :name))
			     :test #'eq)
		  (list provider)))
    provider))

(defun org-context-panel-unregister-provider (name)
  "Unregister provider NAME from the current buffer."
  (setq org-context-panel-providers
	(cl-remove name org-context-panel-providers
		   :key (lambda (entry) (plist-get entry :name))
		   :test #'eq)))

(defun org-context-panel-registered-provider (name)
  "Return registered provider NAME for the current buffer, or nil."
  (cl-find name org-context-panel-providers
	   :key (lambda (entry) (plist-get entry :name))
	   :test #'eq))

(defun org-context-panel-registered-providers ()
  "Return registered context-panel providers for the current buffer."
  (copy-sequence org-context-panel-providers))

(defun org-context-panel--item-with-provider (item provider)
  "Return ITEM copied with PROVIDER ownership metadata."
  (let ((copy (copy-sequence item)))
    (plist-put copy :provider (plist-get provider :name))))

(defun org-context-panel-collect-side-items (&optional source-buffer)
  "Collect side item descriptors for SOURCE-BUFFER.
When SOURCE-BUFFER is nil, use the current buffer.  Each returned descriptor is
copied and annotated with its provider name in `:provider'."
  (let ((buffer (or source-buffer (current-buffer)))
	items)
    (with-current-buffer buffer
      (dolist (provider org-context-panel-providers)
	(when-let* ((function (plist-get provider :collect-side-items)))
	  (dolist (item (funcall function buffer))
	    (push (org-context-panel--item-with-provider item provider) items)))))
    (nreverse items)))

(defun org-context-panel-item-anchor-position (item)
  "Return source buffer anchor position for ITEM, or nil."
  (or (plist-get item :source-start)
      (plist-get item :target-start)
      (plist-get item :reference-pos)
      (plist-get item :anchor-pos)))

(defun org-context-panel-position-hidden-p (position)
  "Return non-nil when POSITION is hidden by text invisibility."
  (and position (invisible-p position)))

(defun org-context-panel-position-row (position window)
  "Return visible row for POSITION in WINDOW, or nil.
The primary path uses redisplay coordinates so visual wrapping and partial
scrolling follow the live window.  A logical-line fallback keeps the helper
usable in batch tests and non-redisplayed windows."
  (when (and (window-live-p window)
	     position
	     (not (org-context-panel-position-hidden-p position))
	     (<= (window-start window) position)
	     (< position (window-end window t)))
    (or (when-let* ((posn (posn-at-point position window))
		    (row (cdr (posn-col-row posn))))
	  (max 1 row))
	(with-current-buffer (window-buffer window)
	  (save-excursion
	    (goto-char (window-start window))
	    (1+ (count-screen-lines (point) position)))))))

(defun org-context-panel-item-with-viewport-row (item row)
  "Return ITEM copied with viewport ROW metadata."
  (let ((copy (copy-sequence item)))
    (setq copy (plist-put copy :logical-anchor-line
			  (or (plist-get item :logical-anchor-line)
			      (plist-get item :anchor-line))))
    (plist-put copy :anchor-line row)))

(defun org-context-panel-items-for-window (window &optional source-buffer)
  "Return provider side items visible in WINDOW.
When SOURCE-BUFFER is nil, use WINDOW's buffer.  Returned items are copied and
annotated with `:anchor-line' viewport rows."
  (let ((buffer (or source-buffer (window-buffer window))))
    (with-current-buffer buffer
      (delq nil
	    (mapcar
	     (lambda (item)
	       (let ((position (org-context-panel-item-anchor-position item)))
		 (if position
		     (when-let* ((row (org-context-panel-position-row
				       position window)))
		       (org-context-panel-item-with-viewport-row item row))
		   item)))
	     (org-context-panel-collect-side-items buffer))))))

(defun org-context-panel--source-buffer ()
  "Return the Org source buffer for context-panel commands."
  (cond
   ((buffer-live-p org-context-panel-source-buffer)
    org-context-panel-source-buffer)
   ((derived-mode-p 'org-context-panel-buffer-mode)
    (user-error "No source buffer associated with this context panel"))
   ((derived-mode-p 'org-mode)
    (current-buffer))
   (t
    (user-error "Org context panel needs an Org source buffer"))))

(defun org-context-panel-current-source-buffer ()
  "Return the Org source buffer associated with the current context."
  (org-context-panel--source-buffer))

(defun org-context-panel-item-key (item)
  "Return a stable context-panel identity key for ITEM."
  (or (plist-get item :id)
      (plist-get item :definition-pos)
      (plist-get item :jump-pos)
      (plist-get item :anchor-pos)
      (org-context-panel-item-anchor-position item)))

(defun org-context-panel-item-at-position (position)
  "Return context-panel item at POSITION, or nil."
  (get-text-property position 'org-context-panel-item))

(defun org-context-panel-item-at-point ()
  "Return context-panel item at point or just before point."
  (or (org-context-panel-item-at-position (point))
      (org-context-panel-item-at-position (max (point-min) (1- (point))))))

(defun org-context-panel-exact-item-at-position (position)
  "Return context-panel item starting at POSITION, or nil."
  (let ((item (org-context-panel-item-at-position position)))
    (when (and item
	       (or (= position (point-min))
		   (not (equal item (org-context-panel-item-at-position
				     (1- position))))))
      item)))

(defun org-context-panel-item-starts ()
  "Return positions where context-panel rows start in the current buffer."
  (let (positions)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(when (org-context-panel-exact-item-at-position (point))
	  (push (point) positions))
	(let ((next (next-single-property-change
		     (point) 'org-context-panel-item nil (point-max))))
	  (goto-char (if (and next (> next (point))) next (1+ (point)))))))
    (nreverse positions)))

(defun org-context-panel-goto-item-key (key)
  "Move point to the context-panel item identified by KEY.
Return the destination position, or nil when KEY is not visible."
  (let ((found nil))
    (goto-char (point-min))
    (while (and (not found) (< (point) (point-max)))
      (let ((item (org-context-panel-item-at-position (point))))
	(when (and item (equal key (org-context-panel-item-key item)))
	  (setq found (point)))
	(goto-char (or (next-single-property-change
			(point) 'org-context-panel-item nil (point-max))
		       (point-max)))))
    (when found
      (goto-char found))))

(defun org-context-panel-item-provider (item &optional source-buffer)
  "Return provider descriptor for ITEM in SOURCE-BUFFER."
  (let ((source (or source-buffer (org-context-panel--source-buffer))))
    (with-current-buffer source
      (org-context-panel-registered-provider (plist-get item :provider)))))

(defun org-context-panel-jump-to-item (item &optional source-buffer)
  "Jump to context-panel ITEM using its provider.
When SOURCE-BUFFER is nil, infer it from the current context."
  (let* ((source (or source-buffer (org-context-panel--source-buffer)))
	 (provider (or (org-context-panel-item-provider item source)
		       (user-error "No provider for context-panel item")))
	 (jump (or (plist-get provider :jump-side-item)
		   (user-error "Provider %s does not support row jumps"
			       (plist-get provider :name)))))
    (funcall jump source item)))

;;;###autoload
(defun org-context-panel-jump-at-point ()
  "Jump from the current context-panel row using its provider."
  (interactive)
  (let ((item (or (org-context-panel-item-at-point)
		  (user-error "No context-panel item at point"))))
    (org-context-panel-jump-to-item item)))

(defun org-context-panel--source-navigation-items (source-buffer)
  "Return navigable context-panel items for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (let ((providers (org-context-panel-registered-providers)))
      (org-context-panel--sort-items
       (cl-remove-if-not
	(lambda (item)
	  (let ((position (org-context-panel-item-anchor-position item)))
	    (and position
		 (not (org-context-panel-position-hidden-p position)))))
	(org-context-panel-collect-side-items source-buffer))
       providers))))

(defun org-context-panel--sync-visible-side-panel (source-buffer item)
  "Refresh SOURCE-BUFFER's visible side panel and focus ITEM when possible."
  (when-let* ((panel-buffer (buffer-local-value
			     'org-context-panel-side-panel-buffer source-buffer))
	      (panel-window (and (buffer-live-p panel-buffer)
				 (get-buffer-window panel-buffer t))))
    (with-current-buffer panel-buffer
      (org-context-panel-render-side-panel
       source-buffer (get-buffer-window source-buffer t))
      (when-let* ((key (org-context-panel-item-key item)))
	(org-context-panel-goto-item-key key)))
    (set-window-buffer panel-window panel-buffer)))

(defun org-context-panel--goto-source-navigation-item (item source-buffer)
  "Jump to source navigation ITEM in SOURCE-BUFFER and refresh panel state."
  (org-context-panel-jump-to-item item source-buffer)
  (org-context-panel--sync-visible-side-panel source-buffer item)
  item)

(defun org-context-panel--source-navigation-step (step)
  "Move to the next context-panel source item by STEP."
  (let* ((source (org-context-panel--source-buffer))
	 (position (with-current-buffer source (point)))
	 (items (org-context-panel--source-navigation-items source))
	 (ordered (if (> step 0) items (reverse items)))
	 (next (or (cl-find-if
		    (lambda (item)
		      (let ((anchor (org-context-panel-item-anchor-position item)))
			(if (> step 0)
			    (> anchor position)
			  (< anchor position))))
		    ordered)
		   (car ordered))))
    (unless next
      (user-error "No context-panel items in this buffer"))
    (org-context-panel--goto-source-navigation-item next source)))

;;;###autoload
(defun org-context-panel-next-item ()
  "Jump to the next context-panel item in the source buffer."
  (interactive)
  (org-context-panel--source-navigation-step 1))

;;;###autoload
(defun org-context-panel-previous-item ()
  "Jump to the previous context-panel item in the source buffer."
  (interactive)
  (org-context-panel--source-navigation-step -1))

(defun org-context-panel--provider-items (provider items)
  "Return ITEMS owned by PROVIDER."
  (cl-remove-if-not
   (lambda (item)
     (eq (plist-get item :provider) (plist-get provider :name)))
   items))

(defun org-context-panel--provider-priority (provider)
  "Return numeric display priority for PROVIDER."
  (or (plist-get provider :priority) 100))

(defun org-context-panel--item-provider (item providers)
  "Return provider descriptor for ITEM from PROVIDERS."
  (cl-find (plist-get item :provider) providers
	   :key (lambda (provider) (plist-get provider :name))
	   :test #'eq))

(defun org-context-panel--item-display-line (item)
  "Return display line key for ITEM."
  (or (plist-get item :display-line)
      (plist-get item :anchor-line)
      most-positive-fixnum))

(defun org-context-panel--item-source-position (item)
  "Return source position sort key for ITEM."
  (or (org-context-panel-item-anchor-position item)
      most-positive-fixnum))

(defun org-context-panel--sort-items (items providers)
  "Return ITEMS sorted by viewport/source position and provider priority."
  (sort (copy-sequence items)
	(lambda (left right)
	  (let* ((left-provider (org-context-panel--item-provider left providers))
		 (right-provider (org-context-panel--item-provider right providers))
		 (left-key (list (org-context-panel--item-display-line left)
				 (org-context-panel--item-source-position left)
				 (org-context-panel--provider-priority left-provider)))
		 (right-key (list (org-context-panel--item-display-line right)
				  (org-context-panel--item-source-position right)
				  (org-context-panel--provider-priority right-provider))))
	    (cl-loop for left-part in left-key
		     for right-part in right-key
		     thereis (< left-part right-part)
		     until (/= left-part right-part))))))

(defun org-context-panel--default-render-side-panel (source-buffer items)
  "Render fallback side panel contents for SOURCE-BUFFER and ITEMS."
  (insert (format "Context for: %s\n\n" (buffer-name source-buffer)))
  (if items
      (dolist (item items)
	(insert (format "- %s\n" (or (plist-get item :label)
				     (plist-get item :id)
				     (plist-get item :provider)
				     "item"))))
    (insert "No visible context items.\n")))

(defun org-context-panel--side-panel-property (source-buffer property)
  "Return provider-selected side panel PROPERTY for SOURCE-BUFFER, or nil."
  (with-current-buffer source-buffer
    (cl-loop for provider in org-context-panel-providers
	     for value = (plist-get provider property)
	     when value return value)))

(defun org-context-panel--side-panel-mode (source-buffer)
  "Return provider-selected side panel mode for SOURCE-BUFFER, or nil."
  (org-context-panel--side-panel-property source-buffer :side-panel-mode))

(defun org-context-panel--side-panel-value (source-buffer property fallback)
  "Return provider-selected side panel PROPERTY or FALLBACK for SOURCE-BUFFER."
  (let ((value (org-context-panel--side-panel-property source-buffer property)))
    (cond
     ((functionp value) (funcall value source-buffer))
     (value)
     (t fallback))))

(defun org-context-panel--side-panel-buffer-name (source-buffer)
  "Return provider-selected side panel buffer name for SOURCE-BUFFER."
  (org-context-panel--side-panel-value
   source-buffer :side-panel-buffer-name org-context-panel-buffer-name))

(defun org-context-panel--side-panel-width (source-buffer)
  "Return provider-selected side panel width for SOURCE-BUFFER."
  (org-context-panel--side-panel-value
   source-buffer :side-panel-width org-context-panel-width))

(defun org-context-panel--run-side-panel-hook
    (hook source-buffer panel-buffer &optional source-window panel-window)
  "Run side-panel lifecycle HOOK with panel context dynamically bound."
  (let ((org-context-panel-current-source-buffer source-buffer)
	(org-context-panel-current-source-window source-window)
	(org-context-panel-current-panel-buffer panel-buffer)
	(org-context-panel-current-panel-window panel-window))
    (run-hooks hook)))

(defun org-context-panel--pad-to-viewport-row (row)
  "Insert blank lines until point is at viewport ROW."
  (when (and (integerp row) (> row 1))
    (while (< (line-number-at-pos) row)
      (insert "\n"))))

(defun org-context-panel-protect-window (window panel-buffer source-buffer)
  "Protect WINDOW as a context panel for PANEL-BUFFER and SOURCE-BUFFER.
Protected context-panel windows are skipped by normal window navigation,
resist `delete-other-windows', and are repaired if a document buffer replaces
their panel buffer."
  (when (window-live-p window)
    (set-window-parameter window 'org-context-panel-protected t)
    (set-window-parameter window 'org-context-panel-panel-buffer panel-buffer)
    (set-window-parameter window 'org-context-panel-source-buffer source-buffer)
    (set-window-parameter window 'no-other-window t)
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-dedicated-p window t)))

(defun org-context-panel--protected-window-p (window)
  "Return non-nil when WINDOW is a protected context-panel window."
  (and (window-live-p window)
       (window-parameter window 'org-context-panel-protected)))

(defun org-context-panel--repair-protected-window (window)
  "Restore protected context-panel WINDOW if its buffer was replaced."
  (when (and (not org-context-panel--repairing-window)
	     (org-context-panel--protected-window-p window))
    (let* ((expected (window-parameter window 'org-context-panel-panel-buffer))
	   (source (window-parameter window 'org-context-panel-source-buffer))
	   (actual (window-buffer window)))
      (when (and (buffer-live-p expected)
		 (not (eq actual expected)))
	(let ((org-context-panel--repairing-window t)
	      (source-window (and (buffer-live-p source)
				  (get-buffer-window source t))))
	  (set-window-dedicated-p window nil)
	  (set-window-buffer window expected)
	  (org-context-panel-protect-window window expected source)
	  (when (and (buffer-live-p actual)
		     (window-live-p source-window)
		     (not (eq source-window window)))
	    (set-window-buffer source-window actual)))))))

(defun org-context-panel--window-buffer-changed (&rest _args)
  "Repair protected context-panel windows after buffer changes."
  (walk-windows #'org-context-panel--repair-protected-window nil t))

(add-hook 'window-buffer-change-functions
	  #'org-context-panel--window-buffer-changed)

(defun org-context-panel--render-composed-side-panel (source-buffer items providers)
  "Render merged provider ITEMS for SOURCE-BUFFER using PROVIDERS."
  (dolist (item (org-context-panel--sort-items items providers))
    (when-let* ((provider (org-context-panel--item-provider item providers))
		(function (plist-get provider :render-side-item)))
      (when (plist-get item :logical-anchor-line)
	(org-context-panel--pad-to-viewport-row
	 (plist-get item :anchor-line)))
      (let ((start (point)))
	(funcall function source-buffer item)
	(add-text-properties start (point)
			     `(org-context-panel-item ,item))))))

(defun org-context-panel--render-provider-side-panels (source-buffer items providers)
  "Render SOURCE-BUFFER using provider whole-panel renderers."
  (dolist (provider providers)
    (let ((provider-items (org-context-panel--provider-items provider items)))
      (when-let* ((function (plist-get provider :render-side-panel)))
	(funcall function source-buffer provider-items)))))

(defun org-context-panel-render-side-panel (source-buffer &optional source-window)
  "Render side panel contents for SOURCE-BUFFER.
When SOURCE-WINDOW is non-nil, provider items are projected onto that window's
visible viewport rows."
  (let* ((items (if source-window
		    (org-context-panel-items-for-window source-window source-buffer)
		  (org-context-panel-collect-side-items source-buffer)))
	 (providers (with-current-buffer source-buffer
		      (org-context-panel-registered-providers)))
	 (composable (and providers
			  (cl-every (lambda (provider)
				      (plist-get provider :render-side-item))
				    providers)
			  (or (> (length providers) 1)
			      (not (plist-get (car providers) :render-side-panel))))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if composable
	  (org-context-panel--render-composed-side-panel source-buffer items providers)
	(org-context-panel--render-provider-side-panels source-buffer items providers))
      (when (= (buffer-size) 0)
	(org-context-panel--default-render-side-panel source-buffer items))
      (goto-char (point-min)))))

;;;###autoload
(defun org-context-panel-open (&optional source-buffer)
  "Open or refresh a generic side context panel for SOURCE-BUFFER."
  (interactive)
  (let* ((source (or source-buffer (org-context-panel--source-buffer)))
	 (source-window (get-buffer-window source))
	 (buffer-name (org-context-panel--side-panel-buffer-name source))
	 (width (org-context-panel--side-panel-width source))
	 (panel-buffer (get-buffer-create buffer-name)))
    (with-current-buffer panel-buffer
      (let ((mode (org-context-panel--side-panel-mode source)))
	(cond
	 ((and mode (not (derived-mode-p mode)))
	  (funcall mode))
	 ((and (not mode) (not (derived-mode-p 'org-context-panel-buffer-mode)))
	  (org-context-panel-buffer-mode))))
      (setq org-context-panel-source-buffer source)
      (org-context-panel-render-side-panel source source-window))
    (with-current-buffer source
      (setq org-context-panel-side-panel-buffer panel-buffer))
    (let ((panel-window (display-buffer-in-side-window
			 panel-buffer
			 `((side . right)
			   (slot . 1)
			   (window-width . ,width)
			   (window-parameters
			    . ((no-other-window . t)
			       (no-delete-other-windows . t)))))))
      (org-context-panel-protect-window panel-window panel-buffer source)
      (org-context-panel--run-side-panel-hook
       'org-context-panel-after-side-panel-open-hook
       source panel-buffer source-window panel-window))
    panel-buffer))

;;;###autoload
(defun org-context-panel-refresh ()
  "Refresh the generic side context panel."
  (interactive)
  (let* ((source (org-context-panel--source-buffer))
	 (source-window (get-buffer-window source))
	 (buffer-name (org-context-panel--side-panel-buffer-name source)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((mode (org-context-panel--side-panel-mode source)))
	(cond
	 ((and mode (not (derived-mode-p mode)))
	  (funcall mode))
	 ((and (not mode) (not (derived-mode-p 'org-context-panel-buffer-mode)))
	  (org-context-panel-buffer-mode))))
      (setq org-context-panel-source-buffer source)
      (org-context-panel-render-side-panel source source-window))))

(defun org-context-panel--close-side-panel-buffer (panel-buffer)
  "Close side PANEL-BUFFER and clear its source association."
  (when (buffer-live-p panel-buffer)
    (let ((source (buffer-local-value 'org-context-panel-source-buffer
				      panel-buffer))
	  (panel-window (get-buffer-window panel-buffer)))
      (org-context-panel--run-side-panel-hook
       'org-context-panel-before-side-panel-close-hook
       source panel-buffer
       (and (buffer-live-p source) (get-buffer-window source t))
       panel-window)
      (when (window-live-p panel-window)
	(delete-window panel-window))
      (kill-buffer panel-buffer)
      (when (buffer-live-p source)
	(with-current-buffer source
	  (setq org-context-panel-side-panel-buffer nil))))))

;;;###autoload
(defun org-context-panel-close ()
  "Close the generic side context panel for the current source."
  (interactive)
  (let* ((source (org-context-panel--source-buffer))
	 (buffer-name (org-context-panel--side-panel-buffer-name source))
	 (panel-buffer (or (and (buffer-live-p org-context-panel-side-panel-buffer)
				org-context-panel-side-panel-buffer)
			   (get-buffer buffer-name))))
    (org-context-panel--close-side-panel-buffer panel-buffer)))

(defun org-context-panel--bottom-view-descriptors (source-buffer)
  "Return bottom view descriptors registered for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (cl-loop for provider in org-context-panel-providers
	     for function = (plist-get provider :collect-bottom-views)
	     when function append
	     (mapcar (lambda (view)
		       (let ((copy (copy-sequence view)))
			 (plist-put copy :provider (plist-get provider :name))))
		     (funcall function source-buffer)))))

(defun org-context-panel-bottom-view (view-id &optional source-buffer)
  "Return bottom view descriptor VIEW-ID for SOURCE-BUFFER, or nil."
  (let ((source (or source-buffer (org-context-panel--source-buffer))))
    (cl-find view-id (org-context-panel--bottom-view-descriptors source)
	     :key (lambda (view) (plist-get view :id))
	     :test #'eq)))

(defun org-context-panel--bottom-view-mode (view)
  "Enable VIEW's buffer mode, or the generic context-panel buffer mode."
  (let ((mode (plist-get view :mode)))
    (cond
     ((and mode (not (derived-mode-p mode)))
      (funcall mode))
     ((and (not mode) (not (derived-mode-p 'org-context-panel-buffer-mode)))
      (org-context-panel-buffer-mode)))))

(defun org-context-panel-render-bottom-view (source-buffer view)
  "Render bottom VIEW for SOURCE-BUFFER in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when-let* ((function (plist-get view :render)))
      (funcall function source-buffer view))
    (when (= (buffer-size) 0)
      (insert (format "Context view: %s\n" (plist-get view :id))))
    (goto-char (point-min))))

(defun org-context-panel--side-panel-window ()
  "Return a visible `org-context-panel' side-panel window, or nil."
  (cl-find-if
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (and (buffer-live-p org-context-panel-source-buffer)
	    (not org-context-panel-view-id))))
   (window-list nil 'no-minibuf)))

(defun org-context-panel--panel-buffer-p (buffer)
  "Return non-nil when BUFFER is an `org-context-panel' panel buffer."
  (with-current-buffer buffer
    (and (buffer-live-p org-context-panel-source-buffer)
	 t)))

(defun org-context-panel--provider-follow-source-p (provider source-buffer)
  "Return non-nil when PROVIDER should follow SOURCE-BUFFER."
  (if-let* ((predicate (plist-get provider :follow-source-p)))
      (funcall predicate source-buffer)
    t))

(defun org-context-panel--followable-source-p (buffer)
  "Return non-nil when BUFFER should drive a visible context side panel."
  (with-current-buffer buffer
    (and (derived-mode-p 'org-mode)
	 org-context-panel-mode
	 (cl-some (lambda (provider)
		    (org-context-panel--provider-follow-source-p provider buffer))
		  org-context-panel-providers))))

(defun org-context-panel--follow-selected-buffer ()
  "Keep a visible side context panel bound to the selected Org buffer."
  (unless (or org-context-panel--following
	      (active-minibuffer-window)
	      (minibufferp (window-buffer (selected-window)))
	      (org-context-panel--panel-buffer-p (window-buffer (selected-window))))
    (let ((org-context-panel--following t))
      (when-let* ((panel-window (org-context-panel--side-panel-window)))
	(let* ((source-window (selected-window))
	       (source-buffer (window-buffer source-window))
	       (old-panel-buffer (window-buffer panel-window)))
	  (if (org-context-panel--followable-source-p source-buffer)
	      (let ((panel-buffer (org-context-panel-open source-buffer)))
		(unless (eq (get-buffer-window panel-buffer) panel-window)
		  (set-window-dedicated-p panel-window nil)
		  (set-window-buffer panel-window panel-buffer)
		  (org-context-panel-protect-window
		   panel-window panel-buffer source-buffer))
		(unless (eq panel-buffer old-panel-buffer)
		  (org-context-panel--close-side-panel-buffer old-panel-buffer)))
	    (org-context-panel--close-side-panel-buffer old-panel-buffer)))))))

(add-hook 'post-command-hook #'org-context-panel--follow-selected-buffer)

;;;###autoload
(defun org-context-panel-open-bottom-view (view-id &optional source-buffer)
  "Open bottom context view VIEW-ID for SOURCE-BUFFER."
  (interactive)
  (let* ((source (or source-buffer (org-context-panel--source-buffer)))
	 (view (or (org-context-panel-bottom-view view-id source)
		   (user-error "No Org context bottom view: %s" view-id)))
	 (buffer-name (or (plist-get view :buffer-name)
			  org-context-panel-bottom-buffer-name))
	 (height (or (plist-get view :height)
		     org-context-panel-bottom-height))
	 (panel-buffer (get-buffer-create buffer-name)))
    (with-current-buffer panel-buffer
      (org-context-panel--bottom-view-mode view)
      (setq org-context-panel-source-buffer source)
      (setq org-context-panel-view-id view-id)
      (org-context-panel-render-bottom-view source view))
    (with-current-buffer source
      (setq org-context-panel-bottom-panel-buffer panel-buffer))
    (org-context-panel-protect-window
     (display-buffer-in-side-window
      panel-buffer
      `((side . bottom)
	(slot . 1)
	(window-height . ,height)
	(window-parameters
	 . ((no-other-window . t)
	    (no-delete-other-windows . t)))))
     panel-buffer source)
    panel-buffer))

;;;###autoload
(defun org-context-panel-refresh-bottom-view ()
  "Refresh the current generic bottom context view."
  (interactive)
  (let* ((source (org-context-panel--source-buffer))
	 (view-id (or org-context-panel-view-id
		      (user-error "No context bottom view associated with buffer"))))
    (org-context-panel-open-bottom-view view-id source)))

;;;###autoload
(defun org-context-panel-close-bottom-view ()
  "Close the generic bottom context view for the current source."
  (interactive)
  (let* ((source (org-context-panel--source-buffer))
	 (panel-buffer (or (and (buffer-live-p org-context-panel-bottom-panel-buffer)
				org-context-panel-bottom-panel-buffer)
			   (get-buffer org-context-panel-bottom-buffer-name))))
    (when (buffer-live-p panel-buffer)
      (when-let* ((window (get-buffer-window panel-buffer)))
	(delete-window window))
      (kill-buffer panel-buffer))
    (when (buffer-live-p source)
      (with-current-buffer source
	(setq org-context-panel-bottom-panel-buffer nil)))))

(defun org-context-panel-cleanup-source-overlays ()
  "Clean up source overlays for registered providers in the current buffer."
  (dolist (provider org-context-panel-providers)
    (when-let* ((function (plist-get provider :cleanup-source-overlays)))
      (funcall function)))
  (org-context-panel-delete-top-markers))

(defun org-context-panel-refresh-source-overlays ()
  "Refresh source overlays for registered providers in the current buffer."
  (dolist (provider org-context-panel-providers)
    (when-let* ((function (plist-get provider :refresh-source-overlays)))
      (funcall function)))
  (org-context-panel-refresh-top-markers))

;;;###autoload
(define-minor-mode org-context-panel-mode
  "Enable reusable Org context-panel mechanics for the current buffer.
This mode is intentionally non-invasive: it refreshes provider-owned source
markers and overlays, but it does not open side or bottom panel windows."
  :lighter " Ctx"
  (if org-context-panel-mode
      (progn
	(add-hook 'after-save-hook #'org-context-panel--refresh-after-save nil t)
	(org-context-panel-refresh-source-overlays))
    (remove-hook 'after-save-hook #'org-context-panel--refresh-after-save t)
    (org-context-panel-cleanup-source-overlays)))

(defun org-context-panel-metadata-end-position ()
  "Return position after leading Org metadata keywords."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at-p "^[	 ]*#\\+[^:
]+:.*$")
      (forward-line 1))
    (point)))

(defun org-context-panel-valid-region-p (start end)
  "Return non-nil when START and END are valid overlay bounds."
  (and start end (<= (point-min) start) (<= start end) (<= end (point-max))))

(defun org-context-panel-delete-overlay (overlay)
  "Delete OVERLAY when it is live."
  (when (overlayp overlay)
    (delete-overlay overlay)))

(defun org-context-panel-delete-overlays (overlays)
  "Delete each overlay in OVERLAYS."
  (mapc #'org-context-panel-delete-overlay overlays))

(defun org-context-panel-delete-top-markers (&optional provider-name)
  "Delete context-panel top marker overlays in the current buffer.
When PROVIDER-NAME is non-nil, delete only markers owned by that provider."
  (let (kept-overlays)
    (dolist (overlay org-context-panel-top-marker-overlays)
      (if (and provider-name
	       (not (eq (overlay-get overlay 'org-context-panel-provider)
			provider-name)))
	  (push overlay kept-overlays)
	(when-let* ((marker (and (overlayp overlay)
				 (overlay-get overlay 'org-context-panel-marker)))
		    (overlay-variable (plist-get marker :overlay-variable)))
	  (set overlay-variable nil))
	(org-context-panel-delete-overlay overlay)))
    (setq org-context-panel-top-marker-overlays (nreverse kept-overlays))))

(cl-defun org-context-panel-make-range-overlay
    (start end &key face properties evaporate)
  "Create a source range overlay from START to END.
FACE is applied as the overlay face when non-nil.  PROPERTIES is a plist of
additional overlay properties.  When EVAPORATE is non-nil, set the overlay's
`evaporate' property.  Return nil when START and END are not valid in the
current buffer."
  (when (org-context-panel-valid-region-p start end)
    (let ((overlay (make-overlay start end nil t nil)))
      (when face
	(overlay-put overlay 'face face))
      (when evaporate
	(overlay-put overlay 'evaporate t))
      (while properties
	(overlay-put overlay (pop properties) (pop properties)))
      overlay)))

(cl-defun org-context-panel-make-top-marker
    (&key label keymap help-echo position face mouse-face priority properties)
  "Create a top marker overlay and return it.
LABEL is rendered in the marker's `after-string'.  KEYMAP is applied to the
visible marker text and to the overlay itself.  HELP-ECHO, POSITION, FACE,
MOUSE-FACE, PRIORITY, and PROPERTIES customize the marker.  POSITION defaults to
`org-context-panel-metadata-end-position'."
  (let* ((marker-position (or position (org-context-panel-metadata-end-position)))
	 (overlay (make-overlay marker-position marker-position nil t nil))
	 (marker-face (or face 'org-context-panel-marker-face))
	 (marker-mouse-face (or mouse-face 'highlight)))
    (when priority
      (overlay-put overlay 'priority priority))
    (when keymap
      (overlay-put overlay 'keymap keymap))
    (while properties
      (overlay-put overlay (pop properties) (pop properties)))
    (overlay-put overlay 'after-string
		 (concat (propertize label
				     'face marker-face
				     'mouse-face marker-mouse-face
				     'help-echo help-echo
				     'keymap keymap)
			 "\n"))
    overlay))

(defun org-context-panel--marker-at-point ()
  "Return context-panel top marker descriptor at point, or nil."
  (or (cl-loop for overlay in (overlays-at (point))
	       for marker = (overlay-get overlay 'org-context-panel-marker)
	       when marker return marker)
      (cl-loop for overlay in org-context-panel-top-marker-overlays
	       when (and (overlayp overlay)
			 (= (overlay-start overlay) (point)))
	       return (overlay-get overlay 'org-context-panel-marker))))

(defun org-context-panel-open-marker-view ()
  "Open the bottom view associated with the top marker at point."
  (interactive)
  (let* ((marker (org-context-panel--marker-at-point))
	 (view-id (plist-get marker :view-id)))
    (unless view-id
      (user-error "No context view associated with marker"))
    (org-context-panel-open-bottom-view view-id)))

(defun org-context-panel--top-marker-keymap (marker provider)
  "Return the keymap for MARKER from PROVIDER."
  (let ((keymap (plist-get marker :keymap))
	(action (plist-get marker :action))
	(view-id (plist-get marker :view-id)))
    (cond
     (keymap keymap)
     (action
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "RET") action)
	(define-key map [mouse-1] action)
	map))
     (view-id
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "RET") #'org-context-panel-open-marker-view)
	(define-key map [mouse-1] #'org-context-panel-open-marker-view)
	map))
     ((plist-get provider :open-top-marker)
      (let ((map (make-sparse-keymap))
	    (function (plist-get provider :open-top-marker)))
	(define-key map (kbd "RET") function)
	(define-key map [mouse-1] function)
	map)))))

(defun org-context-panel--render-top-marker (marker provider)
  "Render MARKER descriptor for PROVIDER and return its overlay."
  (let* ((overlay-variable (plist-get marker :overlay-variable))
	 (overlay (org-context-panel-make-top-marker
		   :label (plist-get marker :label)
		   :keymap (org-context-panel--top-marker-keymap marker provider)
		   :help-echo (plist-get marker :help)
		   :position (plist-get marker :position)
		   :face (plist-get marker :face)
		   :mouse-face (plist-get marker :mouse-face)
		   :priority (plist-get marker :priority)
		   :properties (append (list 'org-context-panel-provider
					     (plist-get provider :name)
					     'org-context-panel-marker marker)
				       (plist-get marker :properties)))))
    (when overlay-variable
      (set overlay-variable overlay))
    overlay))

(defun org-context-panel-refresh-top-markers ()
  "Refresh top markers from registered provider descriptors."
  (org-context-panel-delete-top-markers)
  (dolist (provider org-context-panel-providers)
    (when-let* ((function (plist-get provider :collect-top-markers)))
      (dolist (marker (funcall function (current-buffer)))
	(push (org-context-panel--render-top-marker marker provider)
	      org-context-panel-top-marker-overlays))))
  (setq org-context-panel-top-marker-overlays
	(nreverse org-context-panel-top-marker-overlays)))

(defun org-context-panel-marker-position (overlay &optional fallback-predicate)
  "Return top marker OVERLAY position, or fallback metadata position.
When OVERLAY is live, return its start position.  Otherwise, if
FALLBACK-PREDICATE returns non-nil, return the buffer metadata end
position."
  (cond
   ((overlayp overlay)
    (overlay-start overlay))
   ((and fallback-predicate (funcall fallback-predicate))
    (org-context-panel-metadata-end-position))))

(defun org-context-panel-marker-at-point-p (overlay &optional fallback-predicate)
  "Return non-nil when point is at marker OVERLAY or fallback position."
  (and (derived-mode-p 'org-mode)
       (equal (point) (org-context-panel-marker-position overlay fallback-predicate))))

(provide 'org-context-panel)
;;; org-context-panel.el ends here
