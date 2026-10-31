;;; emms-player.el --- Docked EMMS playback view -*- lexical-binding: t; -*-

;;; Commentary:
;; A read-only player projection in the shared context-panels bottom slot.
;; EMMS owns the queue and playback; closing this view only dismisses its UI.

;;; Code:

(require 'hub-utils)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defconst hub/emms-audio-file-regexp
  (concat "\\."
	  (regexp-opt '("mp3" "wav" "wave" "ogg" "oga" "opus" "flac"
			"m4a" "aac" "aiff" "aif" "wma"))
	  "\\'")
  "File name regexp for audio links opened with EMMS.")

(defun hub/emms-open-file (file &optional _link)
  "Play FILE with EMMS, ignoring Org's original LINK argument."
  (emms-play-file file))

(defun hub/emms-seek-backward-10 ()
  "Seek backward ten seconds in the current EMMS track."
  (interactive)
  (emms-seek -10))

(defun hub/emms-seek-forward-30 ()
  "Seek forward thirty seconds in the current EMMS track."
  (interactive)
  (emms-seek 30))

(with-eval-after-load 'org
  (add-to-list 'org-file-apps
	       (cons hub/emms-audio-file-regexp #'hub/emms-open-file)))

(defgroup hub/emms-player nil
  "Docked EMMS playback display."
  :group 'multimedia)

(defcustom hub/emms-player-buffer-name "*EMMS Player*"
  "Buffer name for the docked playback view."
  :type 'string
  :group 'hub/emms-player)

(defvar hub/emms-player--timer nil
  "Refresh timer, active only while the playback view is visible.")

(defvar emms-playlist-buffer nil)
(defvar emms-playlist-selected-marker)
(defvar emms-player-playing-p nil)
(defvar emms-player-paused-p nil)
(defvar emms-playing-time 0)
(defvar context-panels-source-buffer)

(declare-function emms-track-get "emms" (track name))
(declare-function emms-playlist-tracks-in-region "emms" (beg end))
(declare-function emms-playlist-selected-track "emms" ())
(declare-function emms-pause "emms" ())
(declare-function emms-next "emms" ())
(declare-function emms-previous "emms" ())
(declare-function context-panels-mode "context-panels" (&optional arg))
(declare-function context-panels-register-provider "context-panels" (provider))
(declare-function context-panels-open-bottom-view "context-panels" (id &optional source select))
(declare-function context-panels-open-docked-view "context-panels" (buffer &optional select))
(declare-function context-panels-close-bottom-view "context-panels" ())
(declare-function emms-play-file "emms-source-file" (file))
(declare-function emms-seek "emms" (duration))

(defun hub/emms-player--duration (track)
  "Return TRACK's known positive duration in seconds, or nil."
  (when track
    (let ((duration (emms-track-get track 'info-playing-time)))
      (and (numberp duration) (> duration 0) duration))))

(defun hub/emms-player--time (seconds)
  "Format SECONDS as a playing time; nil means unknown."
  (if (not (numberp seconds)) "--:--"
    (let ((seconds (max 0 (floor seconds))))
      (if (>= seconds 3600)
	  (format "%d:%02d:%02d" (/ seconds 3600) (% (/ seconds 60) 60) (% seconds 60))
	(format "%d:%02d" (/ seconds 60) (% seconds 60))))))

(defun hub/emms-player--title (track)
  "Return a single-line title for TRACK, falling back to its file name."
  (if (not track) "No track"
    (replace-regexp-in-string
     "[\n\r\t]+" " "
     (or (emms-track-get track 'info-title)
	 (file-name-nondirectory (emms-track-get track 'name))
	 "Untitled"))))

(defun hub/emms-player--snapshot ()
  "Read playlist state without modifying its text, selection, or point."
  (let (tracks current (index 0))
    (when (buffer-live-p emms-playlist-buffer)
      (with-current-buffer emms-playlist-buffer
	(save-excursion
	  (save-restriction
	    (widen)
	    (setq tracks (nreverse (emms-playlist-tracks-in-region (point-min) (point-max)))
		  current (emms-playlist-selected-track))
	    (when current
	      (let* ((position (marker-position emms-playlist-selected-marker))
		     (start (if (and (> position (point-min))
				     (eq (get-text-property (1- position) 'emms-track) current))
				(previous-single-property-change
				 position 'emms-track nil (point-min))
			      position)))
		(setq index (if (= start (point-min)) 0
			      (length (emms-playlist-tracks-in-region
				       (point-min) start))))))))))
    (list :tracks tracks :current current :index index
	  :elapsed (if emms-player-playing-p emms-playing-time 0)
	  :state (cond (emms-player-paused-p "Paused")
		       (emms-player-playing-p "Playing")
		       (t "Stopped")))))

(defun hub/emms-player--cancel-timer ()
  "Stop refreshing the playback view."
  (when (timerp hub/emms-player--timer)
    (cancel-timer hub/emms-player--timer))
  (setq hub/emms-player--timer nil))

(defun hub/emms-player--render (_source _view)
  "Render the current playback state, independent of SOURCE and VIEW."
  (let* ((snapshot (hub/emms-player--snapshot))
	 (tracks (plist-get snapshot :tracks))
	 (current (plist-get snapshot :current))
	 (duration (hub/emms-player--duration current))
	 (elapsed (max 0 (or (plist-get snapshot :elapsed) 0)))
	 (filled (if duration (min 30 (floor (* 30 (/ elapsed (float duration))))) 0))
	 (total 0) (unknown 0)
	 (index (plist-get snapshot :index)))
    (dolist (track tracks)
      (if-let* ((seconds (hub/emms-player--duration track)))
	  (setq total (+ total seconds))
	(cl-incf unknown)))
    (insert (propertize (format "%s — %s\n" (plist-get snapshot :state)
				(hub/emms-player--title current)) 'face 'bold))
    (insert (format "[%s%s] %s / %s%s\n"
		    (make-string filled ?=) (make-string (- 30 filled) ?-)
		    (hub/emms-player--time elapsed) (hub/emms-player--time duration)
		    (if (and current (not duration)) " (unknown)" "")))
    (insert (format "Playlist: %s%s · %d tracks\n\n"
		    (hub/emms-player--time total)
		    (if (> unknown 0) (format " + %d unknown" unknown) "")
		    (length tracks)))
    (if (not tracks)
	(insert "No tracks. Use M-x emms-play-file to load audio.\n")
      (cl-loop for track in (seq-take (nthcdr index tracks) 4)
	       for number from (1+ index)
	       for selected = (and current (= number (1+ index)))
	       for row = (format "%s %d. %s  %s\n" (if selected "▶" " ")
				 number (hub/emms-player--title track)
				 (hub/emms-player--time (hub/emms-player--duration track)))
	       do (insert (if selected (propertize row 'face 'highlight) row))))
    (insert "\nSPC play/pause · c -10s · r +30s · t next · s previous · q close\n"))
  (unless (timerp hub/emms-player--timer)
    (setq hub/emms-player--timer (run-at-time 1 1 #'hub/emms-player--refresh))))

(defun hub/emms-player--refresh (&rest _args)
  "Refresh only a visible player, never selecting windows or reopening its slot."
  (let ((buffer (get-buffer hub/emms-player-buffer-name)))
    (if (not (and buffer (get-buffer-window buffer t)))
	(hub/emms-player--cancel-timer)
      (condition-case err
	  (with-current-buffer buffer
	    (let ((inhibit-read-only t))
	      (save-excursion
		(erase-buffer)
		(hub/emms-player--render nil nil))))
	(error
	 (hub/emms-player--cancel-timer)
	 (message "EMMS player refresh failed: %s" (error-message-string err)))))))

(define-derived-mode hub/emms-player-mode special-mode "EMMS Player"
  "Read-only docked playback display with normal-state Bepo controls."
  (setq-local truncate-lines t)
  (add-hook 'kill-buffer-hook #'hub/emms-player--cancel-timer nil t))

(defun hub/emms-player--source ()
  "Return a visible ordinary source buffer suitable for opening the player."
  (let ((window
	 (cl-find-if
	  (lambda (window)
	    (and (not (window-minibuffer-p window))
		 (not (window-parameter window 'window-side))
		 (not (window-parameter window 'context-panels-protected))))
	  (cons (selected-window) (window-list nil 'no-minibuf)))))
    (unless window (user-error "The EMMS player needs an ordinary editing window"))
    (window-buffer window)))

(defun hub/emms-player-open ()
  "Open and focus the docked EMMS player without starting playback."
  (interactive)
  (require 'context-panels)
  (unless (fboundp 'context-panels-open-docked-view)
    (user-error "Update context-panels for docking; select its checkout with CONTEXT_PANELS_PATH"))
  (require 'emms)
  (let ((source (hub/emms-player--source)))
    (with-current-buffer source
      (context-panels-register-provider
       (list :name 'emms :icon "♫"
	     :collect-bottom-views
	     (lambda (_source)
	       (list (list :id 'player :label "EMMS Player" :docked t :height 11
			   :buffer-name hub/emms-player-buffer-name
			   :mode #'hub/emms-player-mode
			   :render #'hub/emms-player--render)))))
      (context-panels-mode 1))
    (context-panels-open-bottom-view '(emms . player) source t)))

(defun hub/emms-player-close ()
  "Dismiss the shared bottom player without stopping playback."
  (interactive)
  (context-panels-close-bottom-view)
  (hub/emms-player--refresh))

(defun hub/emms-player--command (command)
  "Load EMMS and invoke playback COMMAND, then refresh a visible player."
  (require 'emms)
  (funcall command)
  (hub/emms-player--refresh))

(defun hub/emms-player-toggle ()
  "Start or toggle playback without changing the current window."
  (interactive)
  (hub/emms-player--command #'emms-pause))

(defun hub/emms-player-next ()
  "Play the next track without changing the current window."
  (interactive)
  (hub/emms-player--command #'emms-next))

(defun hub/emms-player-previous ()
  "Play the previous track without changing the current window."
  (interactive)
  (hub/emms-player--command #'emms-previous))

(defconst hub/emms-player--bindings
  '(("SPC" . hub/emms-player-toggle)
    ("c" . hub/emms-seek-backward-10)
    ("r" . hub/emms-seek-forward-30)
    ("t" . hub/emms-player-next)
    ("s" . hub/emms-player-previous)
    ("q" . hub/emms-player-close))
  "Playback controls shared by ordinary and Evil player keymaps.")

(dolist (binding hub/emms-player--bindings)
  (define-key hub/emms-player-mode-map (kbd (car binding)) (cdr binding)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'hub/emms-player-mode 'normal)
  (dolist (binding hub/emms-player--bindings)
    (evil-define-key* '(normal motion) hub/emms-player-mode-map
		      (kbd (car binding)) (cdr binding)))
  (evil-make-intercept-map hub/emms-player-mode-map 'normal)
  (evil-make-intercept-map hub/emms-player-mode-map 'motion)
  (dolist (binding '(("'mm" . hub/emms-player-open)
		     ("'m SPC" . hub/emms-player-toggle)
		     ("'mp" . hub/emms-player-toggle)
		     ("'mc" . hub/emms-seek-backward-10)
		     ("'mr" . hub/emms-seek-forward-30)
		     ("'mt" . hub/emms-player-next)
		     ("'ms" . hub/emms-player-previous)))
    (define-key evil-normal-state-map (kbd (car binding)) (cdr binding))))

(with-eval-after-load 'emms
  (dolist (hook '(emms-player-started-hook emms-player-stopped-hook
					   emms-player-paused-hook emms-player-finished-hook
					   emms-player-seeked-functions emms-track-updated-functions
					   emms-playlist-selection-changed-hook emms-playlist-source-inserted-hook
					   emms-playlist-cleared-hook))
    (add-hook hook #'hub/emms-player--refresh)))

(provide 'apps/emms-player)
;;; emms-player.el ends here
