;;; context-panel.el --- Org mode context side panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only right-side panel for contextual Org authoring records.  Native Org
;; footnotes are currently the first data source; sidecar comments will follow.

;;; Code:

(require 'hub-org-comments)
(require 'hub-org-marginalia)
(require 'org)
(require 'subr-x)

(defgroup hub/org-context-panel nil
  "Interactive Org context panel."
  :group 'org)

(defcustom hub/org-context-panel-buffer-name "*Org Context*"
  "Buffer name used for the Org context side panel."
  :type 'string
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-width 38
  "Width of the Org context side panel."
  :type 'natnum
  :group 'hub/org-context-panel)

(defvar-local hub/org-context-panel-source-buffer nil
  "Org source buffer rendered by the current context panel.")

(defvar-local hub/org-context-panel--panel-buffer nil
  "Panel buffer associated with the current Org source buffer.")

(defvar-local hub/org-context-panel--source-buffer nil
  "Org source buffer associated with the current panel buffer.")

(defvar hub/org-context-panel-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hub/org-context-panel-jump-to-definition)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap used in Org context panel buffers.")

(define-derived-mode hub/org-context-panel-buffer-mode special-mode "Org-Context"
  "Major mode for read-only Org context panel buffers."
  (setq-local truncate-lines nil))

(defun hub/org-context-panel--panel-buffer ()
  "Return the panel buffer for the current Org source buffer."
  (or (and (buffer-live-p hub/org-context-panel--panel-buffer)
	   hub/org-context-panel--panel-buffer)
      (setq hub/org-context-panel--panel-buffer
	    (get-buffer-create hub/org-context-panel-buffer-name))))

(defun hub/org-context-panel--marginalia-kind-label (kind)
  "Return a compact display label for marginalia KIND."
  (pcase kind
    ('footnote "FOOTNOTE")
    (_ "NOTE")))

(defun hub/org-context-panel--position-row (position window)
  "Return one-based visible row for POSITION in WINDOW, or nil.
This uses Emacs' redisplay coordinates instead of counting buffer lines so
wrapped lines, visual filling, and partial scrolling follow the live window."
  (when-let* ((posn (posn-at-point position window))
	      (row (cdr (posn-col-row posn))))
    (1+ row)))

(defun hub/org-context-panel--marginalia-with-viewport-anchor (note row)
  "Return NOTE copied with its anchor line replaced by viewport ROW."
  (let ((copy (copy-sequence note)))
    (setq copy (plist-put copy :logical-anchor-line (plist-get note :anchor-line)))
    (setq copy (plist-put copy :anchor-line row))
    copy))

(defun hub/org-context-panel--item-anchor-position (item)
  "Return source buffer anchor position for ITEM."
  (or (plist-get item :reference-pos)
      (plist-get item :anchor-pos)))

(defun hub/org-context-panel--item-with-viewport-anchor (item row)
  "Return ITEM copied with its anchor line replaced by viewport ROW."
  (hub/org-context-panel--marginalia-with-viewport-anchor item row))

(defun hub/org-context-panel--items-for-window (source-window items)
  "Return context ITEMS laid out for SOURCE-WINDOW viewport lines."
  (hub/org-marginalia-layout
   (delq nil
	 (mapcar
	  (lambda (item)
	    (when-let* ((row (hub/org-context-panel--position-row
			      (hub/org-context-panel--item-anchor-position item)
			      source-window)))
	      (hub/org-context-panel--item-with-viewport-anchor item row)))
	  items))))

(defun hub/org-context-panel--post-command-refresh ()
  "Refresh a visible context panel after source-buffer commands."
  (when hub/org-context-panel-mode
    (hub/org-context-panel-refresh)))

(defun hub/org-context-panel--truncate (text length)
  "Return TEXT truncated to LENGTH characters with an ellipsis."
  (if (> (length text) length)
      (concat (substring text 0 length) "…")
    text))

(defun hub/org-context-panel--insert-marginalia (note)
  "Insert marginalia NOTE into the current panel buffer."
  (let ((kind (plist-get note :kind))
	(body (or (plist-get note :body) "")))
    (insert (format "%s %s\n" (hub/org-context-panel--marginalia-kind-label kind) (plist-get note :id)))
    (unless (string-empty-p body)
      (insert body "\n"))))

(defun hub/org-context-panel--insert-comment (comment)
  "Insert COMMENT into the current panel buffer."
  (let ((status (or (plist-get comment :status) "open"))
	(target (or (plist-get comment :target-text) ""))
	(body (or (plist-get comment :body) "")))
    (insert (format "COMMENT %s\n" status))
    (unless (string-empty-p target)
      (insert "“" (hub/org-context-panel--truncate target 80) "”\n\n"))
    (unless (string-empty-p body)
      (insert body "\n"))))

(defun hub/org-context-panel--insert-item (item)
  "Insert context ITEM into the current panel buffer."
  (let ((start (point)))
    (pcase (plist-get item :type)
      ('comment (hub/org-context-panel--insert-comment item))
      (_ (hub/org-context-panel--insert-marginalia item)))
    (when (plist-get item :displaced)
      (insert "↳ shifted down from nearby text\n"))
    (add-text-properties start (point) `(hub-org-context-panel-item ,item))))

;;;###autoload
(defun hub/org-context-panel-render-buffer (source-buffer panel-buffer &optional source-window)
  "Render SOURCE-BUFFER context items into PANEL-BUFFER.
When SOURCE-WINDOW is non-nil, align notes to visible lines in that window."
  (let* ((all-items (with-current-buffer source-buffer
		      (sort (append (hub/org-marginalia-collect)
				    (hub/org-comment-collect source-buffer))
			    (lambda (left right)
			      (< (or (plist-get left :anchor-line) 1)
				 (or (plist-get right :anchor-line) 1))))))
	 (items (if (and source-window (window-live-p source-window))
		    (hub/org-context-panel--items-for-window source-window all-items)
		  (hub/org-marginalia-layout all-items))))
    (with-current-buffer panel-buffer
      (let ((inhibit-read-only t))
	(hub/org-context-panel-buffer-mode)
	(setq-local hub/org-context-panel-source-buffer source-buffer)
	(setq-local hub/org-context-panel--source-buffer source-buffer)
	(erase-buffer)
	(cond
	 (items
	  (let ((current-line 1))
	    (dolist (item items)
	      (let ((display-line (or (plist-get item :display-line) current-line)))
		(while (< current-line display-line)
		  (insert "\n")
		  (setq current-line (1+ current-line)))
		(hub/org-context-panel--insert-item item)
		(setq current-line (line-number-at-pos (point) t))))))
	 (all-items
	  ;; The current viewport simply has no context anchors.
	  (insert ""))
	 (t
	  (insert "No context items in this buffer.\n")))
	(goto-char (point-min))
	(setq buffer-read-only t)))
    (when-let* ((window (get-buffer-window panel-buffer t)))
      (set-window-start window (with-current-buffer panel-buffer (point-min))))
    panel-buffer))

;;;###autoload
(defun hub/org-context-panel-refresh ()
  "Refresh the context panel for the current Org source buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (hub/org-context-panel-render-buffer
   (current-buffer)
   (hub/org-context-panel--panel-buffer)
   (get-buffer-window (current-buffer) t)))

;;;###autoload
(defun hub/org-context-panel-open ()
  "Open or refresh the Org context side panel."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (let* ((source-buffer (current-buffer))
	 (source-window (selected-window))
	 (panel (hub/org-context-panel--panel-buffer)))
    (display-buffer-in-side-window
     panel
     `((side . right)
       (slot . 1)
       (window-width . ,hub/org-context-panel-width)))
    ;; Render after displaying the side window: the source window width changes,
    ;; so position coordinates must use the final narrowed source window.
    (redisplay t)
    (hub/org-context-panel-render-buffer source-buffer panel source-window)
    (with-current-buffer source-buffer
      (setq hub/org-context-panel--panel-buffer panel))
    panel))

;;;###autoload
(defun hub/org-context-panel-close ()
  "Close the context side panel associated with the current buffer."
  (interactive)
  (when-let* ((panel (and (boundp 'hub/org-context-panel--panel-buffer)
			  hub/org-context-panel--panel-buffer))
	      (window (get-buffer-window panel t)))
    (quit-window nil window)))

;;;###autoload
(defun hub/org-context-panel-jump-to-definition ()
  "Jump from a rendered context item to its Org footnote definition."
  (interactive)
  (let* ((note (or (get-text-property (point) 'hub-org-context-panel-item)
		   (get-text-property (max (point-min) (1- (point))) 'hub-org-context-panel-item)))
	 (source hub/org-context-panel-source-buffer)
	 (position (or (plist-get note :definition-pos)
		       (plist-get note :jump-pos))))
    (unless (and note (buffer-live-p source) position)
      (user-error "No context item at point"))
    (pop-to-buffer source)
    (goto-char position)))

;;;###autoload
(define-minor-mode hub/org-context-panel-mode
  "Toggle an Org context side panel for the current buffer."
  :lighter " Ctx"
  (if hub/org-context-panel-mode
      (progn
	(add-hook 'post-command-hook #'hub/org-context-panel--post-command-refresh nil t)
	(hub/org-context-panel-open))
    (remove-hook 'post-command-hook #'hub/org-context-panel--post-command-refresh t)
    (hub/org-context-panel-close)))

(provide 'org/context-panel)
;;; context-panel.el ends here
