;;; marginalia.el --- Org mode marginalia panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only right-side panel for native Org footnotes and future marginalia
;; metadata.  The Org buffer remains the canonical editable source.

;;; Code:

(require 'hub-org-marginalia)
(require 'org)
(require 'subr-x)

(defgroup hub/org-marginalia-panel nil
  "Interactive Org marginalia panel."
  :group 'hub/org-marginalia)

(defcustom hub/org-marginalia-panel-buffer-name "*Org Marginalia*"
  "Buffer name used for the Org marginalia side panel."
  :type 'string
  :group 'hub/org-marginalia-panel)

(defcustom hub/org-marginalia-panel-width 38
  "Width of the Org marginalia side panel."
  :type 'natnum
  :group 'hub/org-marginalia-panel)

(defvar-local hub/org-marginalia-panel-source-buffer nil
  "Org source buffer rendered by the current marginalia panel.")

(defvar-local hub/org-marginalia--panel-buffer nil
  "Panel buffer associated with the current Org source buffer.")

(defvar-local hub/org-marginalia--source-buffer nil
  "Org source buffer associated with the current panel buffer.")

(defvar hub/org-marginalia-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hub/org-marginalia-jump-to-definition)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap used in Org marginalia panel buffers.")

(define-derived-mode hub/org-marginalia-panel-mode special-mode "Org-Marginalia"
  "Major mode for read-only Org marginalia panel buffers."
  (setq-local truncate-lines nil))

(defun hub/org-marginalia--panel-buffer ()
  "Return the panel buffer for the current Org source buffer."
  (or (and (buffer-live-p hub/org-marginalia--panel-buffer)
	   hub/org-marginalia--panel-buffer)
      (setq hub/org-marginalia--panel-buffer
	    (get-buffer-create hub/org-marginalia-panel-buffer-name))))

(defun hub/org-marginalia--kind-label (kind)
  "Return a compact display label for marginalia KIND."
  (pcase kind
    ('footnote "FOOTNOTE")
    (_ "NOTE")))

(defun hub/org-marginalia--position-row (position window)
  "Return one-based visible row for POSITION in WINDOW, or nil.
This uses Emacs' redisplay coordinates instead of counting buffer lines so
wrapped lines, visual filling, and partial scrolling follow the live window."
  (when-let* ((posn (posn-at-point position window))
	      (row (cdr (posn-col-row posn))))
    (1+ row)))

(defun hub/org-marginalia--note-with-viewport-anchor (note row)
  "Return NOTE copied with its anchor line replaced by viewport ROW."
  (let ((copy (copy-sequence note)))
    (setq copy (plist-put copy :logical-anchor-line (plist-get note :anchor-line)))
    (setq copy (plist-put copy :anchor-line row))
    copy))

(defun hub/org-marginalia--notes-for-window (source-window)
  "Return marginalia records laid out for SOURCE-WINDOW viewport lines."
  (with-current-buffer (window-buffer source-window)
    (hub/org-marginalia-layout
     (delq nil
	   (mapcar
	    (lambda (note)
	      (when-let* ((row (hub/org-marginalia--position-row
				(plist-get note :reference-pos) source-window)))
		(hub/org-marginalia--note-with-viewport-anchor note row)))
	    (hub/org-marginalia-collect))))))

(defun hub/org-marginalia--post-command-refresh ()
  "Refresh a visible marginalia panel after source-buffer commands."
  (when hub/org-marginalia-mode
    (hub/org-marginalia-refresh)))

(defun hub/org-marginalia--insert-note (note)
  "Insert NOTE into the current panel buffer."
  (let ((start (point))
	(kind (plist-get note :kind))
	(body (or (plist-get note :body) "")))
    (insert (format "%s %s\n" (hub/org-marginalia--kind-label kind) (plist-get note :id)))
    (unless (string-empty-p body)
      (insert body "\n"))
    (when (plist-get note :displaced)
      (insert "↳ shifted down from nearby text\n"))
    (add-text-properties start (point) `(hub-org-marginalia-note ,note))))

;;;###autoload
(defun hub/org-marginalia-render-buffer (source-buffer panel-buffer &optional source-window)
  "Render SOURCE-BUFFER marginalia into PANEL-BUFFER.
When SOURCE-WINDOW is non-nil, align notes to visible lines in that window."
  (let* ((all-notes (with-current-buffer source-buffer
		      (hub/org-marginalia-collect)))
	 (notes (if (and source-window (window-live-p source-window))
		    (hub/org-marginalia--notes-for-window source-window)
		  (hub/org-marginalia-layout all-notes))))
    (with-current-buffer panel-buffer
      (let ((inhibit-read-only t))
	(hub/org-marginalia-panel-mode)
	(setq-local hub/org-marginalia-panel-source-buffer source-buffer)
	(setq-local hub/org-marginalia--source-buffer source-buffer)
	(erase-buffer)
	(cond
	 (notes
	  (let ((current-line 1))
	    (dolist (note notes)
	      (let ((display-line (or (plist-get note :display-line) current-line)))
		(while (< current-line display-line)
		  (insert "\n")
		  (setq current-line (1+ current-line)))
		(hub/org-marginalia--insert-note note)
		(setq current-line (line-number-at-pos (point) t))))))
	 (all-notes
	  ;; The current viewport simply has no marginalia anchors.
	  (insert ""))
	 (t
	  (insert "No marginalia in this buffer.\n")))
	(goto-char (point-min))
	(setq buffer-read-only t)))
    (when-let* ((window (get-buffer-window panel-buffer t)))
      (set-window-start window (with-current-buffer panel-buffer (point-min))))
    panel-buffer))

;;;###autoload
(defun hub/org-marginalia-refresh ()
  "Refresh the marginalia panel for the current Org source buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org marginalia only works in Org buffers"))
  (hub/org-marginalia-render-buffer
   (current-buffer)
   (hub/org-marginalia--panel-buffer)
   (get-buffer-window (current-buffer) t)))

;;;###autoload
(defun hub/org-marginalia-open ()
  "Open or refresh the Org marginalia side panel."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org marginalia only works in Org buffers"))
  (let* ((source-buffer (current-buffer))
	 (source-window (selected-window))
	 (panel (hub/org-marginalia--panel-buffer)))
    (display-buffer-in-side-window
     panel
     `((side . right)
       (slot . 1)
       (window-width . ,hub/org-marginalia-panel-width)))
    ;; Render after displaying the side window: the source window width changes,
    ;; so position coordinates must use the final narrowed source window.
    (redisplay t)
    (hub/org-marginalia-render-buffer source-buffer panel source-window)
    (with-current-buffer source-buffer
      (setq hub/org-marginalia--panel-buffer panel))
    panel))

;;;###autoload
(defun hub/org-marginalia-close ()
  "Close the marginalia side panel associated with the current buffer."
  (interactive)
  (when-let* ((panel (and (boundp 'hub/org-marginalia--panel-buffer)
			  hub/org-marginalia--panel-buffer))
	      (window (get-buffer-window panel t)))
    (quit-window nil window)))

;;;###autoload
(defun hub/org-marginalia-jump-to-definition ()
  "Jump from a rendered marginalia note to its Org footnote definition."
  (interactive)
  (let* ((note (or (get-text-property (point) 'hub-org-marginalia-note)
		   (get-text-property (max (point-min) (1- (point))) 'hub-org-marginalia-note)))
	 (source hub/org-marginalia-panel-source-buffer)
	 (position (plist-get note :definition-pos)))
    (unless (and note (buffer-live-p source) position)
      (user-error "No marginalia note at point"))
    (pop-to-buffer source)
    (goto-char position)))

;;;###autoload
(define-minor-mode hub/org-marginalia-mode
  "Toggle an Org marginalia side panel for the current buffer."
  :lighter " Marg"
  (if hub/org-marginalia-mode
      (progn
	(add-hook 'post-command-hook #'hub/org-marginalia--post-command-refresh nil t)
	(hub/org-marginalia-open))
    (remove-hook 'post-command-hook #'hub/org-marginalia--post-command-refresh t)
    (hub/org-marginalia-close)))

(provide 'org/marginalia)
;;; marginalia.el ends here
