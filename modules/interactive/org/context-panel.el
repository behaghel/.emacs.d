;;; context-panel.el --- Org mode context side panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only right-side panel for contextual Org authoring records.  Native Org
;; footnotes are currently the first data source; sidecar comments will follow.

;;; Code:

(require 'cl-lib)
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

(defcustom hub/org-context-panel-dock-prose t
  "Whether opening the context panel docks visually filled prose toward it."
  :type 'boolean
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-target-preview-length 20
  "Maximum length of quoted comment target previews in the context panel."
  :type 'natnum
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-overview-comment-lines 2
  "Maximum number of comment body lines shown in context panel overview mode."
  :type 'natnum
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-overview-comment-line-length 34
  "Maximum length of each comment body line in context panel overview mode."
  :type 'natnum
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-open-face
  '((t :inherit default :foreground "#FFFFFF" :background "#0052CC" :box (:line-width (1 . -1) :color "#0052CC")))
  "Face for open comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-todo-face
  '((t :inherit default :foreground "#172B4D" :background "#FFAB00" :box (:line-width (1 . -1) :color "#FFAB00")))
  "Face for todo comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-resolved-face
  '((t :inherit default :foreground "#FFFFFF" :background "#00875A" :box (:line-width (1 . -1) :color "#00875A")))
  "Face for resolved comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-default-face
  '((t :inherit default :foreground "#172B4D" :background "#DFE1E6" :box (:line-width (1 . -1) :color "#DFE1E6")))
  "Face for default comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-icon-face
  '((t :inherit shadow))
  "Face used for compact context panel icons."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-target-face
  '((t :inherit shadow :slant italic))
  "Face used for comment target previews."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-comment-region-face
  '((t :inherit highlight :underline t))
  "Face used to mark commented source regions."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-current-item-face
  '((t :inherit highlight))
  "Face used for the context item at point in the source buffer."
  :group 'hub/org-context-panel)

(defvar-local hub/org-context-panel-source-buffer nil
  "Org source buffer rendered by the current context panel.")

(defvar-local hub/org-context-panel--panel-buffer nil
  "Panel buffer associated with the current Org source buffer.")

(defvar-local hub/org-context-panel--source-buffer nil
  "Org source buffer associated with the current panel buffer.")

(defvar-local hub/org-context-panel--comment-overlays nil
  "Comment target overlays in the current Org source buffer.")

(defvar-local hub/org-context-panel--visual-fill-state nil
  "Saved visual-fill-column state while context panel docks prose.")

(defvar hub/org-context-panel--following nil
  "Non-nil while the context panel follow hook is refreshing.")

(defvar hub/org-context-panel-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hub/org-context-panel-jump-to-definition)
    (define-key map (kbd "q") #'hub/org-context-panel-close)
    map)
  "Keymap used in Org context panel buffers.")

(with-eval-after-load 'evil
  (evil-define-key 'normal hub/org-context-panel-buffer-mode-map
		   (kbd "RET") #'hub/org-context-panel-jump-to-definition
		   (kbd "q") #'hub/org-context-panel-close
		   (kbd "]c") #'hub/org-context-panel-next-item
		   (kbd "[c") #'hub/org-context-panel-previous-item))

(define-derived-mode hub/org-context-panel-buffer-mode special-mode "Org-Context"
  "Major mode for read-only Org context panel buffers."
  (setq-local truncate-lines nil
	      word-wrap t)
  (visual-line-mode 1))

(defun hub/org-context-panel--panel-buffer ()
  "Return the panel buffer for the current Org source buffer."
  (or (and (buffer-live-p hub/org-context-panel--panel-buffer)
	   hub/org-context-panel--panel-buffer)
      (setq hub/org-context-panel--panel-buffer
	    (get-buffer-create hub/org-context-panel-buffer-name))))

(defun hub/org-context-panel--marginalia-kind-icon (kind)
  "Return a compact display icon for marginalia KIND."
  (pcase kind
    ('footnote "†")
    (_ "✣")))

(defun hub/org-context-panel--comment-status-face (status)
  "Return status chip face for comment STATUS."
  (pcase (downcase (or status ""))
    ("open" 'hub/org-context-panel-status-open-face)
    ("todo" 'hub/org-context-panel-status-todo-face)
    ("resolved" 'hub/org-context-panel-status-resolved-face)
    (_ 'hub/org-context-panel-status-default-face)))

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
  (let ((stale-items (cl-remove-if-not #'hub/org-context-panel--stale-comment-p items))
	(anchored-items (cl-remove-if #'hub/org-context-panel--stale-comment-p items)))
    (append
     (hub/org-marginalia-layout
      (delq nil
	    (mapcar
	     (lambda (item)
	       (if source-window
		   (when-let* ((row (hub/org-context-panel--position-row
				     (hub/org-context-panel--item-anchor-position item)
				     source-window)))
		     (hub/org-context-panel--item-with-viewport-anchor item row))
		 item))
	     anchored-items)))
     stale-items)))

(defun hub/org-context-panel--delete-comment-overlays ()
  "Delete context-panel comment overlays in the current buffer."
  (mapc #'delete-overlay hub/org-context-panel--comment-overlays)
  (setq hub/org-context-panel--comment-overlays nil))

(defun hub/org-context-panel--comment-item-p (item)
  "Return non-nil when ITEM is a sidecar comment."
  (eq (plist-get item :type) 'comment))

(defun hub/org-context-panel--stale-comment-p (item)
  "Return non-nil when ITEM is an unanchored stale sidecar comment."
  (and (hub/org-context-panel--comment-item-p item)
       (eq (plist-get item :anchor-state) 'stale)))

(defun hub/org-context-panel--source-point-in-item-p (item source-point)
  "Return non-nil when SOURCE-POINT is inside ITEM's target region."
  (and source-point
       (hub/org-context-panel--comment-item-p item)
       (not (hub/org-context-panel--stale-comment-p item))
       (let ((start (plist-get item :target-start))
	     (end (plist-get item :target-end)))
	 (and start end (<= start source-point) (<= source-point end)))))

(defun hub/org-context-panel--item-with-current-state (item source-point)
  "Return ITEM copied with current-state metadata for SOURCE-POINT."
  (if (hub/org-context-panel--source-point-in-item-p item source-point)
      (plist-put (copy-sequence item) :current t)
    item))

(defun hub/org-context-panel--overview-comment-lines (body)
  "Return compact overview lines for comment BODY."
  (let ((text (string-trim (replace-regexp-in-string "[[:space:]]+" " " (or body ""))))
	(lines nil)
	(limit hub/org-context-panel-overview-comment-line-length))
    (while (and (not (string-empty-p text))
		(< (length lines) hub/org-context-panel-overview-comment-lines))
      (if (<= (length text) limit)
	  (setq lines (append lines (list text))
		text "")
	(let ((chunk (substring text 0 limit)))
	  (setq lines (append lines (list chunk))
		text (string-trim-left (substring text limit))))))
    (when (and lines (not (string-empty-p text)))
      (let* ((last-index (1- (length lines)))
	     (last-line (nth last-index lines)))
	(setf (nth last-index lines)
	      (if (> (length last-line) 1)
		  (concat (substring last-line 0 (1- (length last-line))) "…")
		"…"))))
    lines))

(defun hub/org-context-panel--item-with-overview-height (item)
  "Return ITEM copied with compact overview height where appropriate."
  (if (and (hub/org-context-panel--comment-item-p item)
	   (not (plist-get item :current)))
      (plist-put (copy-sequence item) :height
		 (+ 1 (length (hub/org-context-panel--overview-comment-lines
			       (plist-get item :body)))))
    item))

(defun hub/org-context-panel--focused-comment (items)
  "Return the current focused comment from ITEMS, or nil."
  (cl-find-if
   (lambda (item)
     (and (hub/org-context-panel--comment-item-p item)
	  (plist-get item :current)))
   items))

(defun hub/org-context-panel--refresh-comment-overlays (source-buffer items)
  "Refresh source comment overlays for comment ITEMS in SOURCE-BUFFER."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (hub/org-context-panel--delete-comment-overlays)
      (dolist (item items)
	(when (and (hub/org-context-panel--comment-item-p item)
		   (not (hub/org-context-panel--stale-comment-p item)))
	  (let ((start (plist-get item :target-start))
		(end (plist-get item :target-end)))
	    (when (and start end (<= (point-min) start) (<= start end) (<= end (point-max)))
	      (let ((overlay (make-overlay start end nil t nil)))
		(overlay-put overlay 'face 'hub/org-context-panel-comment-region-face)
		(overlay-put overlay 'hub-org-context-panel-item item)
		(push overlay hub/org-context-panel--comment-overlays)))))))))

;;;###autoload
(defun hub/org-comment-overlays-refresh ()
  "Refresh persistent sidecar comment overlays in the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comment overlays only work in Org buffers"))
  (hub/org-context-panel--refresh-comment-overlays
   (current-buffer)
   (hub/org-comment-collect (current-buffer))))

;;;###autoload
(define-minor-mode hub/org-comment-overlays-mode
  "Toggle persistent sidecar comment overlays in the current Org buffer."
  :lighter " Cmnt"
  (if hub/org-comment-overlays-mode
      (progn
	(add-hook 'after-save-hook #'hub/org-comment-overlays-refresh nil t)
	(hub/org-comment-overlays-refresh))
    (remove-hook 'after-save-hook #'hub/org-comment-overlays-refresh t)
    (hub/org-context-panel--delete-comment-overlays)))

(defun hub/org-context-panel--visual-fill-total-margin (source-window)
  "Return total visual-fill-column margin for SOURCE-WINDOW."
  (let* ((width (or (and (boundp 'visual-fill-column-width)
			 (numberp visual-fill-column-width)
			 visual-fill-column-width)
		    fill-column))
	 (total-width (if (fboundp 'visual-fill-column--window-max-text-width)
			  (visual-fill-column--window-max-text-width source-window)
			(window-body-width source-window))))
    (max 0 (- total-width width))))

(defun hub/org-context-panel--dock-prose (source-window)
  "Dock visually filled prose in SOURCE-WINDOW toward the context panel."
  (when (and hub/org-context-panel-dock-prose
	     (window-live-p source-window)
	     (require 'visual-fill-column nil t))
    (with-current-buffer (window-buffer source-window)
      (when (bound-and-true-p visual-fill-column-mode)
	(unless hub/org-context-panel--visual-fill-state
	  (setq hub/org-context-panel--visual-fill-state
		(list :center (and (boundp 'visual-fill-column-center-text)
				   visual-fill-column-center-text)
		      :extra (and (boundp 'visual-fill-column-extra-text-width)
				  visual-fill-column-extra-text-width))))
	(let* ((margin (hub/org-context-panel--visual-fill-total-margin source-window))
	       (half (/ margin 2)))
	  ;; `visual-fill-column' can only left-dock or center text directly.  Keep
	  ;; centering enabled, then shift the centered margins right by expanding the
	  ;; left margin and collapsing the right margin.
	  (setq-local visual-fill-column-center-text t
		      visual-fill-column-extra-text-width (cons (- half) half))
	  (when (fboundp 'visual-fill-column--set-margins)
	    (visual-fill-column--set-margins source-window)))))))

(defun hub/org-context-panel--restore-prose-docking (source-buffer)
  "Restore visual-fill-column state saved for SOURCE-BUFFER."
  (when (and (buffer-live-p source-buffer)
	     (require 'visual-fill-column nil t))
    (with-current-buffer source-buffer
      (when hub/org-context-panel--visual-fill-state
	(setq-local visual-fill-column-center-text
		    (plist-get hub/org-context-panel--visual-fill-state :center)
		    visual-fill-column-extra-text-width
		    (plist-get hub/org-context-panel--visual-fill-state :extra)
		    hub/org-context-panel--visual-fill-state nil)
	(when-let* ((window (get-buffer-window source-buffer t)))
	  (when (fboundp 'visual-fill-column--set-margins)
	    (visual-fill-column--set-margins window)))))))

(defun hub/org-context-panel--post-command-refresh ()
  "Refresh a visible context panel after source-buffer commands."
  (when hub/org-context-panel-mode
    (hub/org-context-panel-refresh)))

(defun hub/org-context-panel--visible-window ()
  "Return the visible context panel window, or nil."
  (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
    (get-buffer-window panel t)))

(defun hub/org-context-panel--panel-source-buffer ()
  "Return the source buffer currently rendered by the context panel."
  (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
    (with-current-buffer panel
      hub/org-context-panel-source-buffer)))

(defun hub/org-context-panel--minibuffer-active-p ()
  "Return non-nil while minibuffer interaction is active."
  (or (active-minibuffer-window)
      (minibufferp (window-buffer (selected-window)))))

(defun hub/org-context-panel--enable-follow ()
  "Enable automatic context panel following."
  (add-hook 'buffer-list-update-hook #'hub/org-context-panel--follow-selected-buffer))

(defun hub/org-context-panel--disable-follow ()
  "Disable automatic context panel following."
  (remove-hook 'buffer-list-update-hook #'hub/org-context-panel--follow-selected-buffer))

(defun hub/org-context-panel--follow-selected-buffer ()
  "Keep a visible context panel bound to the selected Org buffer."
  (unless (or hub/org-context-panel--following
	      (hub/org-context-panel--minibuffer-active-p))
    (let ((hub/org-context-panel--following t))
      (when-let* ((panel-window (hub/org-context-panel--visible-window))
		  (source-window (selected-window)))
	(unless (eq source-window panel-window)
	  (let ((source-buffer (window-buffer source-window)))
	    (if (with-current-buffer source-buffer (derived-mode-p 'org-mode))
		(let ((previous-source (hub/org-context-panel--panel-source-buffer))
		      (panel-buffer (window-buffer panel-window)))
		  (unless (eq previous-source source-buffer)
		    (hub/org-context-panel--restore-prose-docking previous-source))
		  (hub/org-context-panel--dock-prose source-window)
		  (hub/org-context-panel-render-buffer source-buffer panel-buffer source-window)
		  (with-current-buffer source-buffer
		    (setq hub/org-context-panel--panel-buffer panel-buffer)))
	      (hub/org-context-panel-close))))))))


(defun hub/org-context-panel--truncate (text length)
  "Return TEXT truncated to LENGTH characters with an ellipsis."
  (if (> (length text) length)
      (concat (substring text 0 length) "…")
    text))

(defun hub/org-context-panel--insert-icon (icon)
  "Insert context panel ICON with its face."
  (insert (propertize icon 'face 'hub/org-context-panel-icon-face)))

(defun hub/org-context-panel--insert-status-chip (status)
  "Insert comment STATUS as a chip."
  (let ((label (upcase (or status "unknown"))))
    (insert (propertize (concat " " label " ")
			'face (hub/org-context-panel--comment-status-face status)))))

(defun hub/org-context-panel--insert-marginalia (note)
  "Insert marginalia NOTE into the current panel buffer."
  (let ((kind (plist-get note :kind))
	(body (or (plist-get note :body) "")))
    (hub/org-context-panel--insert-icon (hub/org-context-panel--marginalia-kind-icon kind))
    (insert (format " %s\n" (plist-get note :id)))
    (unless (string-empty-p body)
      (insert body "\n"))))

(defun hub/org-context-panel--insert-comment (comment)
  "Insert COMMENT into the current panel buffer."
  (let ((status (or (plist-get comment :status) "open"))
	(target (or (plist-get comment :target-text) ""))
	(body (or (plist-get comment :body) ""))
	(stale (hub/org-context-panel--stale-comment-p comment)))
    (hub/org-context-panel--insert-icon (if stale "⚠" "💬"))
    (insert " ")
    (hub/org-context-panel--insert-status-chip status)
    (unless (string-empty-p target)
      (insert " " (propertize
		   (concat "“" (hub/org-context-panel--truncate
				target hub/org-context-panel-target-preview-length) "”")
		   'face 'hub/org-context-panel-target-face)))
    (insert "\n")
    (when stale
      (insert (propertize "Anchor no longer matches source text.\n" 'face 'warning)))
    (if (plist-get comment :current)
	(unless (string-empty-p body)
	  (insert body "\n"))
      (dolist (line (hub/org-context-panel--overview-comment-lines body))
	(insert line "\n")))))

(defun hub/org-context-panel--insert-item (item)
  "Insert context ITEM into the current panel buffer."
  (let ((start (point)))
    (pcase (plist-get item :type)
      ('comment (hub/org-context-panel--insert-comment item))
      (_ (hub/org-context-panel--insert-marginalia item)))
    (add-text-properties start (point) `(hub-org-context-panel-item ,item))
    (when (plist-get item :current)
      (add-face-text-property start (point) 'hub/org-context-panel-current-item-face t))))

(defun hub/org-context-panel--item-key (item)
  "Return a stable panel identity key for ITEM."
  (or (plist-get item :id)
      (plist-get item :definition-pos)
      (plist-get item :jump-pos)
      (plist-get item :anchor-pos)))

(defun hub/org-context-panel--item-at-point ()
  "Return context item at point or just before point."
  (or (get-text-property (point) 'hub-org-context-panel-item)
      (get-text-property (max (point-min) (1- (point))) 'hub-org-context-panel-item)))

(defun hub/org-context-panel--item-starts ()
  "Return positions where context panel items start."
  (let ((position (point-min))
	starts)
    (while (< position (point-max))
      (let ((item (get-text-property position 'hub-org-context-panel-item)))
	(when (and item
		   (or (= position (point-min))
		       (not (equal item (get-text-property (1- position)
							   'hub-org-context-panel-item)))))
	  (push position starts))
	(setq position (or (next-single-property-change
			    position 'hub-org-context-panel-item nil (point-max))
			   (point-max)))))
    (nreverse starts)))

(defun hub/org-context-panel--goto-item-key (key)
  "Move point to the context panel item identified by KEY."
  (let ((found nil))
    (goto-char (point-min))
    (while (and (not found) (< (point) (point-max)))
      (let ((item (get-text-property (point) 'hub-org-context-panel-item)))
	(when (and item (equal key (hub/org-context-panel--item-key item)))
	  (setq found (point)))
	(goto-char (or (next-single-property-change
			(point) 'hub-org-context-panel-item nil (point-max))
		       (point-max)))))
    (when found
      (goto-char found))))

;;;###autoload
(defun hub/org-context-panel-render-buffer (source-buffer panel-buffer &optional source-window)
  "Render SOURCE-BUFFER context items into PANEL-BUFFER.
When SOURCE-WINDOW is non-nil, align notes to visible lines in that window."
  (let* ((source-point (with-current-buffer source-buffer (point)))
	 (all-items (with-current-buffer source-buffer
		      (sort (mapcar
			     (lambda (item)
			       (hub/org-context-panel--item-with-overview-height
				(hub/org-context-panel--item-with-current-state item source-point)))
			     (append (hub/org-marginalia-collect)
				     (hub/org-comment-collect source-buffer t)))
			    (lambda (left right)
			      (< (or (plist-get left :anchor-line) 1)
				 (or (plist-get right :anchor-line) 1))))))
	 (focused-comment (hub/org-context-panel--focused-comment all-items))
	 (items (cond
		 (focused-comment (list focused-comment))
		 ((and source-window (window-live-p source-window))
		  (hub/org-context-panel--items-for-window source-window all-items))
		 (t (hub/org-context-panel--items-for-window nil all-items)))))
    (hub/org-context-panel--refresh-comment-overlays source-buffer all-items)
    (with-current-buffer panel-buffer
      (let ((inhibit-read-only t)
	    (preserved-key (hub/org-context-panel--item-key
			    (hub/org-context-panel--item-at-point))))
	(hub/org-context-panel-buffer-mode)
	(setq-local hub/org-context-panel-source-buffer source-buffer)
	(setq-local hub/org-context-panel--source-buffer source-buffer)
	(erase-buffer)
	(cond
	 (items
	  (let ((current-line 1))
	    (dolist (item items)
	      (when (and (hub/org-context-panel--stale-comment-p item)
			 (> current-line 1))
		(insert "\n\n")
		(setq current-line (+ current-line 2)))
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
	(if preserved-key
	    (or (hub/org-context-panel--goto-item-key preserved-key)
		(goto-char (point-min)))
	  (goto-char (point-min)))
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
    (hub/org-context-panel--dock-prose source-window)
    (hub/org-context-panel-render-buffer source-buffer panel source-window)
    (with-current-buffer source-buffer
      (setq hub/org-context-panel--panel-buffer panel))
    (hub/org-context-panel--enable-follow)
    panel))

;;;###autoload
(defun hub/org-context-panel-close ()
  "Close the context side panel associated with the current buffer."
  (interactive)
  (hub/org-context-panel--disable-follow)
  (let* ((panel-window (hub/org-context-panel--visible-window))
	 (source-buffer (cond
			 ((derived-mode-p 'hub/org-context-panel-buffer-mode)
			  hub/org-context-panel-source-buffer)
			 ((bound-and-true-p hub/org-context-panel--panel-buffer)
			  (current-buffer))
			 ((hub/org-context-panel--panel-source-buffer))
			 (t (current-buffer)))))
    (when (buffer-live-p source-buffer)
      (hub/org-context-panel--restore-prose-docking source-buffer)
      (with-current-buffer source-buffer
	(unless hub/org-comment-overlays-mode
	  (hub/org-context-panel--delete-comment-overlays))))
    (when (window-live-p panel-window)
      (quit-window nil panel-window))))

;;;###autoload
(defun hub/org-context-panel--jump-to-sidecar-comment (comment)
  "Jump to COMMENT heading in its sidecar file."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(id (plist-get comment :id)))
    (unless (and sidecar-file id)
      (user-error "Comment record is missing sidecar metadata"))
    (find-file sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal id (org-entry-get nil "HUB_COMMENT_ID"))
		     return t
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" id))))

(defun hub/org-context-panel-next-item ()
  "Move point to the next context panel item."
  (interactive)
  (let* ((starts (hub/org-context-panel--item-starts))
	 (next (or (cl-find-if (lambda (position) (> position (point))) starts)
		   (car starts))))
    (unless next
      (user-error "No context items"))
    (goto-char next)))

(defun hub/org-context-panel-previous-item ()
  "Move point to the previous context panel item."
  (interactive)
  (let* ((starts (reverse (hub/org-context-panel--item-starts)))
	 (previous (or (cl-find-if (lambda (position) (< position (point))) starts)
		       (car starts))))
    (unless previous
      (user-error "No context items"))
    (goto-char previous)))

(defun hub/org-context-panel--comment-at-point ()
  "Return sidecar comment at point in the current source buffer, or nil."
  (let ((point (point)))
    (cl-find-if
     (lambda (comment)
       (let ((start (plist-get comment :target-start))
	     (end (plist-get comment :target-end)))
	 (and start end (<= start point) (<= point end))))
     (hub/org-comment-collect (current-buffer)))))

;;;###autoload
(defun hub/org-context-panel-jump-to-item-at-point ()
  "Jump from source point to the related item in the context panel."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (let ((comment (hub/org-context-panel--comment-at-point)))
    (unless comment
      (user-error "No context item at point"))
    (hub/org-context-panel-open)
    (when-let* ((window (hub/org-context-panel--visible-window)))
      (select-window window)
      (hub/org-context-panel--goto-item-key (hub/org-context-panel--item-key comment)))))

(defun hub/org-context-panel-jump-to-definition ()
  "Jump from a rendered context item to its source or sidecar definition."
  (interactive)
  (let* ((note (hub/org-context-panel--item-at-point))
	 (source hub/org-context-panel-source-buffer)
	 (position (or (plist-get note :definition-pos)
		       (plist-get note :jump-pos))))
    (cond
     ((and note (hub/org-context-panel--stale-comment-p note))
      (hub/org-context-panel--jump-to-sidecar-comment note))
     ((and note (buffer-live-p source) position)
      (pop-to-buffer source)
      (goto-char position))
     (t
      (user-error "No context item at point")))))

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
