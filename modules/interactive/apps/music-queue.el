;;; music-queue.el --- Shared listing display on the real EMMS queue -*- lexical-binding: t; -*-

;;; Commentary:
;; Replace only the display of existing EMMS track spans.  Native text,
;; properties, undo, selection markers and editing commands remain authoritative.

;;; Code:
(require 'hub-utils)
(require 'apps/music-row)
(require 'emms-playlist-mode)
(require 'cl-lib)

(defvar hub/music-queue-listing-mode)
(defvar-local hub/music-queue--overlays nil "Presentation overlays, in native occurrence order.")
(defvar-local hub/music-queue--timer nil "Pending coalesced display refresh.")
(defvar-local hub/music-queue--key nil "Last painted model and layout fingerprint.")

(defun hub/music-queue--width ()
  "Return the visible queue's body width without displaying it."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (window-body-width window) (frame-width)))

(defun hub/music-queue--clear ()
  "Remove this buffer's listing overlays and pending timer only."
  (when (timerp hub/music-queue--timer) (cancel-timer hub/music-queue--timer))
  (setq hub/music-queue--timer nil hub/music-queue--key nil)
  (mapc #'delete-overlay hub/music-queue--overlays)
  (setq hub/music-queue--overlays nil))

(defun hub/music-queue--refresh (&optional force)
  "Repaint native queue spans if changed, or unconditionally with FORCE.
Never rewrite playlist text or move point, selection or undo history."
  (when hub/music-queue-listing-mode
    (let* ((library (hub/music-loaded-library-buffer))
	   (width (hub/music-queue--width))
	   (key (list (buffer-chars-modified-tick) width emms-playlist-buffer emms-player-playing-p emms-player-paused-p
		      (hub/music-playback-state)
		      (when (markerp emms-playlist-selected-marker)
			(marker-position emms-playlist-selected-marker))
		      library (when library (buffer-local-value 'hub/spotify--generation library))
		      (when library (length (buffer-local-value 'hub/spotify--tracks library))))))
      (when (or force (not (equal key hub/music-queue--key)))
	(let ((rows (car (hub/music-row-combine (hub/music-row-queue (current-buffer))
						(hub/music-row-liked library)))))
	  (mapc #'delete-overlay hub/music-queue--overlays)
	  (setq hub/music-queue--overlays nil)
	  (save-excursion
	    (save-restriction
	      (widen)
	      (dolist (row rows)
		(let* ((position (hub/music-row-position row))
		       (end (cdr (emms-property-region position 'emms-track)))
		       (overlay (make-overlay position end nil nil nil)))
		  (overlay-put overlay 'display
			       (hub/music-row-plain-display (hub/music-row-display row width)))
		  (overlay-put overlay 'evaporate t)
		  (push overlay hub/music-queue--overlays)))))
	  (setq hub/music-queue--overlays (nreverse hub/music-queue--overlays)
		hub/music-queue--key key))))))

(defun hub/music-queue--run-scheduled (buffer)
  "Refresh an already visible queue BUFFER after native changes settle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq hub/music-queue--timer nil)
      (when (and hub/music-queue-listing-mode (get-buffer-window buffer t))
	(hub/music-queue--refresh t)))))

(defun hub/music-queue--schedule (&rest _changes)
  "Coalesce presentation updates after native edits without touching the queue."
  (setq hub/music-queue--key nil)
  (unless (timerp hub/music-queue--timer)
    (setq hub/music-queue--timer
	  (run-at-time 0 nil #'hub/music-queue--run-scheduled (current-buffer)))))

(defun hub/music-queue--refresh-visible (&rest _events)
  "Schedule passive repainting of already enabled, visible queue buffers."
  (dolist (buffer (buffer-list))
    (when (and (buffer-local-value 'hub/music-queue-listing-mode buffer)
	       (get-buffer-window buffer t))
      (with-current-buffer buffer (hub/music-queue--schedule)))))

(defun hub/music-queue--disable ()
  "Disable listing presentation before changing this buffer's major mode."
  (hub/music-queue-listing-mode -1))

;;;###autoload
(define-minor-mode hub/music-queue-listing-mode
  "Show shared music columns on existing, editable EMMS track spans.
Disabling the mode removes only presentation; no queue text is regenerated."
  :lighter nil
  :group 'hub/music
  (if hub/music-queue-listing-mode
      (progn
	(unless (derived-mode-p 'emms-playlist-mode)
	  (setq hub/music-queue-listing-mode nil)
	  (user-error "Music queue columns require an existing EMMS playlist"))
	(add-hook 'after-change-functions #'hub/music-queue--schedule nil t)
	(add-hook 'post-command-hook #'hub/music-queue--refresh nil t)
	(add-hook 'kill-buffer-hook #'hub/music-queue--clear nil t)
	(add-hook 'change-major-mode-hook #'hub/music-queue--disable nil t)
	(hub/music-queue--refresh t))
    (remove-hook 'after-change-functions #'hub/music-queue--schedule t)
    (remove-hook 'post-command-hook #'hub/music-queue--refresh t)
    (remove-hook 'kill-buffer-hook #'hub/music-queue--clear t)
    (remove-hook 'change-major-mode-hook #'hub/music-queue--disable t)
    (hub/music-queue--clear)))

(dolist (hook '(emms-player-started-hook emms-player-paused-hook emms-player-stopped-hook
					 emms-player-finished-hook emms-playlist-selection-changed-hook
					 hub/spotify-browser-player-status-hook hub/music-library-changed-hook
					 window-size-change-functions window-buffer-change-functions))
  (add-hook hook #'hub/music-queue--refresh-visible t))

(provide 'apps/music-queue)
;;; music-queue.el ends here
