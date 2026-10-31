;;; emms-spectrum.el --- Opt-in system-output spectrum -*- lexical-binding: t; -*-

;;; Commentary:
;; CAVA supplies frequency amplitudes, not recorded audio, over a private pipe.
;; Capture starts only on an explicit command and stops when the player is hidden.

;;; Code:

(require 'hub-utils)
(require 'apps/emms-player)
(require 'cl-lib)
(require 'subr-x)
(require 'svg)

(defgroup hub/emms-spectrum nil
  "Opt-in system-output visualization in the EMMS player."
  :group 'hub/emms-player)

(defcustom hub/emms-spectrum-program "cava"
  "CAVA executable with system-output capture support.
Launch Emacs from devenv or supply the executable's absolute path."
  :type 'string
  :group 'hub/emms-spectrum)

(defcustom hub/emms-spectrum-bars 32
  "Number of frequency bars, from 4 through 128."
  :type 'integer
  :group 'hub/emms-spectrum)

(defcustom hub/emms-spectrum-height 80
  "Pixel height of the spectrum, from 32 through 256."
  :type 'integer
  :group 'hub/emms-spectrum)

(defconst hub/emms-spectrum--frame-rate 20
  "Maximum capture and animation frame rate.")

(defvar hub/emms-spectrum--process nil "Current explicitly started capture process.")
(defvar hub/emms-spectrum--timer nil "Animation and visibility-check timer.")
(defvar hub/emms-spectrum--config-file nil "Private temporary settings file, never audio.")
(defvar hub/emms-spectrum--stderr nil "Bounded diagnostic buffer for CAVA errors.")
(defvar hub/emms-spectrum--pending "" "Incomplete protocol frame, held only in memory.")
(defvar hub/emms-spectrum--values nil "Latest complete spectrum vector, held only in memory.")
(defvar hub/emms-spectrum--diagnostic nil "Actionable explanation of the last capture failure.")
(defvar hub/emms-spectrum--overlay nil "Image overlay owned by the current player buffer.")
(defvar hub/emms-spectrum--image nil "Current SVG image, flushed when replaced.")
(defvar hub/emms-spectrum--frame nil "Frame in which capture was explicitly enabled.")
(defvar hub/emms-spectrum--dirty nil "Non-nil when a newer complete frame awaits display.")

(defun hub/emms-spectrum--configuration ()
  "Return fixed raw-output settings with an explicit system-output source."
  (concat
   (format "[general]\nbars = %d\nframerate = %d\nsleep_timer = 0\n\n"
	   hub/emms-spectrum-bars hub/emms-spectrum--frame-rate)
   (pcase system-type
     ('darwin "[input]\nmethod = coreaudio\nsource = tap\n\n")
     ('gnu/linux "[input]\nmethod = pipewire\nsource = auto\n\n")
     (_ (user-error "System-output spectrum is supported on macOS and PipeWire Linux")))
   "[output]\nmethod = raw\nraw_target = /dev/stdout\ndata_format = ascii\n"
   "channels = mono\nascii_max_range = 1000\nbar_delimiter = 59\nframe_delimiter = 10\n"))

(defun hub/emms-spectrum--consume (chunk)
  "Consume ASCII spectrum CHUNK, preserving only the newest complete frame.
Reject malformed, out-of-range, or excessively long partial frames."
  (setq hub/emms-spectrum--pending (concat hub/emms-spectrum--pending chunk))
  (let (end)
    (while (setq end (string-match "\n" hub/emms-spectrum--pending))
      (let* ((line (substring hub/emms-spectrum--pending 0 end))
	     (fields (split-string (string-remove-suffix ";" line) ";")))
	(unless (and (< (length line) 8192)
		     (string-match-p "\\`[0-9]+\\(?:;[0-9]+\\)*;?\\'" line)
		     (= (length fields) hub/emms-spectrum-bars))
	  (error "Unexpected CAVA spectrum frame; check the CAVA version"))
	(let ((values (mapcar #'string-to-number fields)))
	  (unless (cl-every (lambda (value) (<= 0 value 1000)) values)
	    (error "Out-of-range CAVA spectrum frame"))
	  (setq hub/emms-spectrum--values (vconcat values)
		hub/emms-spectrum--dirty t)))
      (setq hub/emms-spectrum--pending (substring hub/emms-spectrum--pending (1+ end)))))
  (when (> (length hub/emms-spectrum--pending) 8192)
    (error "CAVA sent an oversized incomplete spectrum frame")))

(defun hub/emms-spectrum--svg (values width height)
  "Build a pixel-based SVG from amplitude VALUES with WIDTH and HEIGHT."
  (let* ((svg (svg-create width height))
	 (step (/ (float width) (length values)))
	 (bar-width (max 1 (- step 3))))
    (svg-rectangle svg 0 0 width height :fill "#101410")
    (cl-loop for value across values for index from 0
	     for pixels = (round (* height (/ (max 0 (min 1000 value)) 1000.0)))
	     do (svg-rectangle svg (* index step) (- height pixels) bar-width pixels
			       :class "spectrum-bar"
			       :fill (cond ((> value 850) "#e06c75")
					   ((> value 600) "#e5c07b")
					   (t "#55d755"))))
    (cl-loop for y from 8 below height by 8
	     do (svg-line svg 0 y width y :stroke "#101410" :stroke-width 2))
    svg))

(defun hub/emms-spectrum--visible-window ()
  "Return the player's visible window in the frame that enabled capture."
  (let ((buffer (get-buffer hub/emms-player-buffer-name))
	(frame (or hub/emms-spectrum--frame (selected-frame))))
    (and buffer (frame-live-p frame) (get-buffer-window buffer frame))))

(defun hub/emms-spectrum--forget-image ()
  "Release the previous SVG's image-cache entry."
  (when (and hub/emms-spectrum--image (display-graphic-p))
    (image-flush hub/emms-spectrum--image))
  (setq hub/emms-spectrum--image nil))

(defun hub/emms-spectrum--draw ()
  "Update only the spectrum overlay, without editing text or changing focus."
  (when-let* ((window (hub/emms-spectrum--visible-window)))
    (when (overlay-buffer hub/emms-spectrum--overlay)
      (let* ((width (max 160 (min 960 (window-body-width window t))))
	     (values (or hub/emms-spectrum--values (make-vector hub/emms-spectrum-bars 0)))
	     (image (svg-image (hub/emms-spectrum--svg values width hub/emms-spectrum-height)
			       :ascent 'center)))
	(hub/emms-spectrum--forget-image)
	(setq hub/emms-spectrum--image image hub/emms-spectrum--dirty nil)
	(overlay-put hub/emms-spectrum--overlay 'display image)))))

(defun hub/emms-spectrum--insert ()
  "Insert spectrum status and an image placeholder in a player render."
  (cond
   ((and hub/emms-spectrum--process (process-live-p hub/emms-spectrum--process))
    (insert "Spectrum: system output · v stop\n")
    (when (overlayp hub/emms-spectrum--overlay) (delete-overlay hub/emms-spectrum--overlay))
    (let ((start (point)))
      (insert " \n\n")
      (setq hub/emms-spectrum--overlay (make-overlay start (1+ start)))
      (overlay-put hub/emms-spectrum--overlay 'evaporate t)
      (hub/emms-spectrum--draw)))
   (hub/emms-spectrum--diagnostic
    (insert (propertize (concat "Spectrum: unavailable — " hub/emms-spectrum--diagnostic "\n\n")
			'face 'warning)))))

(defun hub/emms-spectrum-stop (&optional diagnostic)
  "Stop capture, timers and temporary resources; never stop playback.
Retain DIAGNOSTIC as an explanation when stopping because of a failure."
  (interactive)
  (let ((process hub/emms-spectrum--process))
    ;; Clear ownership before deleting: a stale sentinel must not change new state.
    (setq hub/emms-spectrum--process nil)
    (when (processp process) (delete-process process)))
  (when (timerp hub/emms-spectrum--timer) (cancel-timer hub/emms-spectrum--timer))
  (setq hub/emms-spectrum--timer nil)
  (when (overlayp hub/emms-spectrum--overlay) (delete-overlay hub/emms-spectrum--overlay))
  (setq hub/emms-spectrum--overlay nil)
  (hub/emms-spectrum--forget-image)
  (when (and hub/emms-spectrum--config-file (file-exists-p hub/emms-spectrum--config-file))
    (delete-file hub/emms-spectrum--config-file))
  (when (buffer-live-p hub/emms-spectrum--stderr)
    (when-let* ((process (get-buffer-process hub/emms-spectrum--stderr)))
      (set-process-query-on-exit-flag process nil)
      (delete-process process))
    (kill-buffer hub/emms-spectrum--stderr))
  (setq hub/emms-spectrum--config-file nil hub/emms-spectrum--stderr nil
	hub/emms-spectrum--pending "" hub/emms-spectrum--values nil
	hub/emms-spectrum--frame nil hub/emms-spectrum--dirty nil
	hub/emms-spectrum--diagnostic diagnostic))

(defun hub/emms-spectrum--fail (reason)
  "Stop capture and show bounded, actionable REASON without disrupting playback."
  (let ((diagnostic (truncate-string-to-width
		     (replace-regexp-in-string "[\n\r\t]+" " " reason) 700 nil nil "…")))
    (hub/emms-spectrum-stop diagnostic)
    (hub/emms-player--refresh)
    (message "EMMS spectrum: %s" diagnostic)))

(defun hub/emms-spectrum--check-visible (&rest _args)
  "Stop capture as soon as its player is no longer visible."
  (when (and hub/emms-spectrum--process (not (hub/emms-spectrum--visible-window)))
    (hub/emms-spectrum-stop)))

(defun hub/emms-spectrum--tick ()
  "Observe visibility, bound diagnostics, and render at most one new frame."
  (hub/emms-spectrum--check-visible)
  (when hub/emms-spectrum--process
    (when (buffer-live-p hub/emms-spectrum--stderr)
      (with-current-buffer hub/emms-spectrum--stderr
	(when (> (buffer-size) 4096)
	  (delete-region (point-min) (- (point-max) 4096)))))
    (when (and hub/emms-spectrum--dirty (overlayp hub/emms-spectrum--overlay))
      (condition-case err (hub/emms-spectrum--draw)
	(error (hub/emms-spectrum--fail (error-message-string err)))))))

(defun hub/emms-spectrum--filter (process chunk)
  "Accept spectrum CHUNK only from the currently owned PROCESS."
  (when (eq process hub/emms-spectrum--process)
    (hub/emms-spectrum--check-visible)
    (when hub/emms-spectrum--process
      (condition-case err (hub/emms-spectrum--consume chunk)
	(error (hub/emms-spectrum--fail (error-message-string err)))))))

(defun hub/emms-spectrum--sentinel (process event)
  "Handle an owned PROCESS exiting with EVENT; never automatically restart it."
  (when (and (eq process hub/emms-spectrum--process)
	     (memq (process-status process) '(exit signal failed)))
    (let ((details (when (buffer-live-p hub/emms-spectrum--stderr)
		     (with-current-buffer hub/emms-spectrum--stderr
		       (buffer-substring-no-properties (point-min) (min (point-max) 1025))))))
      (hub/emms-spectrum--fail
       (concat "CAVA " (string-trim event) ". "
	       (when (eq system-type 'darwin)
		 "Check System Audio Recording permission for Emacs/your terminal in System Settings. ")
	       "Press v to retry. " details)))))

(defun hub/emms-spectrum--fit ()
  "Fit the visible player after an explicit toggle, never during animation."
  (when-let* ((window (hub/emms-spectrum--visible-window)))
    (save-selected-window (fit-window-to-buffer window 20 11))))

(defun hub/emms-spectrum-toggle ()
  "Explicitly toggle system-output analysis in the visible EMMS player.
This analyzes other audible applications too, never the microphone."
  (interactive)
  (if hub/emms-spectrum--process
      (progn (hub/emms-spectrum-stop) (hub/emms-player--refresh) (hub/emms-spectrum--fit))
    (condition-case err
	(progn
	  (unless (and (hub/emms-spectrum--visible-window)
		       (display-graphic-p) (image-type-available-p 'svg))
	    (user-error "Open the player with 'mm in graphical Emacs with SVG support"))
	  (unless (and (integerp hub/emms-spectrum-bars) (<= 4 hub/emms-spectrum-bars 128)
		       (integerp hub/emms-spectrum-height) (<= 32 hub/emms-spectrum-height 256))
	    (user-error "Spectrum requires 4–128 bars and a 32–256 pixel height"))
	  (let* ((program (or (executable-find hub/emms-spectrum-program)
			      (user-error "CAVA is missing; launch Emacs from devenv or set hub/emms-spectrum-program")))
		 (configuration (hub/emms-spectrum--configuration))
		 (directory (expand-file-name "var/emms-spectrum/" user-emacs-directory)))
	    (make-directory directory t)
	    (set-file-modes directory #o700)
	    (setq hub/emms-spectrum--config-file (make-temp-file (expand-file-name "cava-" directory) nil ".conf")
		  hub/emms-spectrum--stderr (generate-new-buffer " *EMMS spectrum diagnostics*")
		  hub/emms-spectrum--frame (selected-frame)
		  hub/emms-spectrum--diagnostic nil
		  hub/emms-spectrum--pending "")
	    (set-file-modes hub/emms-spectrum--config-file #o600)
	    (with-temp-file hub/emms-spectrum--config-file (insert configuration))
	    (setq hub/emms-spectrum--process
		  (make-process :name "EMMS spectrum" :buffer nil
				:command (list program "-p" hub/emms-spectrum--config-file)
				:connection-type 'pipe :coding 'utf-8-unix :noquery t
				:stderr hub/emms-spectrum--stderr
				:filter #'hub/emms-spectrum--filter
				:sentinel #'hub/emms-spectrum--sentinel))
	    (setq hub/emms-spectrum--timer
		  (run-at-time 0.05 (/ 1.0 hub/emms-spectrum--frame-rate) #'hub/emms-spectrum--tick))
	    (hub/emms-player--refresh)
	    (hub/emms-spectrum--fit)))
      (error (hub/emms-spectrum--fail (error-message-string err))))))

(add-hook 'hub/emms-player-render-hook #'hub/emms-spectrum--insert)
(add-hook 'hub/emms-player-close-hook #'hub/emms-spectrum-stop)
(defun hub/emms-spectrum--prepare-player ()
  "Tie capture cleanup to the current player buffer's lifetime."
  (add-hook 'kill-buffer-hook #'hub/emms-spectrum-stop nil t))

(add-hook 'hub/emms-player-mode-hook #'hub/emms-spectrum--prepare-player)
;; The player may have existed before the analyzer was loaded by its first toggle.
(when-let* ((buffer (get-buffer hub/emms-player-buffer-name)))
  (with-current-buffer buffer (hub/emms-spectrum--prepare-player)))
(add-hook 'window-configuration-change-hook #'hub/emms-spectrum--check-visible)
(add-hook 'window-buffer-change-functions #'hub/emms-spectrum--check-visible)
(add-hook 'delete-frame-functions #'hub/emms-spectrum--check-visible)
(add-hook 'kill-emacs-hook #'hub/emms-spectrum-stop)

(provide 'apps/emms-spectrum)
;;; emms-spectrum.el ends here
