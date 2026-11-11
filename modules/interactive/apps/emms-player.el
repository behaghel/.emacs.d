;;; emms-player.el --- Docked EMMS playback view -*- lexical-binding: t; -*-

;;; Commentary:
;; A read-only player projection in the shared context-panels bottom slot.
;; EMMS owns the mixed queue and dispatches tracks to their playback backends.
;; Closing this projection only dismisses its UI; it never stops audio.

;;; Code:

(require 'hub-utils)
(require 'apps/music)
(require 'apps/music-footer)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defvar org-file-apps)
(defvar evil-normal-state-map)
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-make-intercept-map "evil-core" (keymap &optional state aux))

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
  "Seek backward ten seconds in the selected player's current track."
  (interactive)
  (hub/emms-player--seek -10))

(defun hub/emms-seek-forward-30 ()
  "Seek forward thirty seconds in the selected player's current track."
  (interactive)
  (hub/emms-player--seek 30))

(with-eval-after-load 'org
  (add-to-list 'org-file-apps
	       (cons hub/emms-audio-file-regexp #'hub/emms-open-file)))

(defgroup hub/emms-player nil
  "Docked music playback display."
  :group 'multimedia)

(defcustom hub/emms-player-buffer-name "*EMMS Player*"
  "Buffer name for the docked playback view."
  :type 'string
  :group 'hub/emms-player)

(defun hub/emms-player--seek (seconds)
  "Seek SECONDS through EMMS's currently selected backend."
  (emms-seek seconds))

(defvar hub/emms-player--timer nil
  "Refresh timer, active only while the playback view is visible.")

(defvar hub/emms-player-render-hook nil
  "Hook inserting optional content between the player summary and queue.")

(defvar hub/emms-player-close-hook nil
  "Hook stopping optional resources before the player is dismissed.")

(autoload 'hub/emms-spectrum-toggle "apps/emms-spectrum"
  "Explicitly toggle system-output spectrum analysis in the visible player." t)

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
(declare-function emms-stop "emms" ())
(declare-function emms-next "emms" ())
(declare-function emms-previous "emms" ())
(declare-function context-panels-mode "context-panels" (&optional arg))
(declare-function context-panels-register-provider "context-panels" (provider))
(declare-function context-panels-open-bottom-view "context-panels" (id &optional source select))
(declare-function context-panels-open-docked-view "context-panels" (buffer &optional select))
(declare-function context-panels-close-bottom-view "context-panels" ())
(declare-function context-panels-footer-update "context-panels-footer" (&optional buffer))
(declare-function emms-play-file "emms-source-file" (file))
(declare-function emms-seek "emms" (duration))
(declare-function hub/music-console-render "apps/music-console" (snapshot))
(declare-function hub/music-console-prepare-artwork "apps/music-console" (snapshot))
(declare-function hub/music-console-cancel-artwork "apps/music-console" ())
(declare-function hub/spotify-browser-player-position "apps/spotify-browser-player" ())

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
    (let ((state (hub/music-playback-state)))
      (list :tracks tracks :current current :index index :paused emms-player-paused-p
	    :elapsed (cond ((eq state 'connecting) 0)
			   ((eq state 'seeking) (hub/spotify-browser-player-position))
			   (emms-player-playing-p emms-playing-time)
			   (t 0))
	    :state (pcase state ('connecting "Waiting for audio") ('seeking "Seeking")
			  ('paused "Paused") ('playing "Playing") (_ "Stopped"))))))

(defun hub/emms-player--cancel-timer ()
  "Stop refreshing the playback view."
  (when (timerp hub/emms-player--timer)
    (cancel-timer hub/emms-player--timer))
  (setq hub/emms-player--timer nil)
  (when (fboundp 'hub/music-console-cancel-artwork)
    (hub/music-console-cancel-artwork)))

(defun hub/emms-player--render (_source _view)
  "Render the current playback state, independent of SOURCE and VIEW."
  (require 'apps/music-console)
  (let ((snapshot (hub/emms-player--snapshot)))
    (hub/music-console-prepare-artwork snapshot)
    (hub/music-console-render snapshot))
  (when (fboundp 'context-panels-footer-update) (context-panels-footer-update (current-buffer)))
  (setq-local mode-name "Music")
  (unless (timerp hub/emms-player--timer)
    (setq hub/emms-player--timer (run-at-time 1 1 #'hub/emms-player--refresh))))

(defun hub/emms-player--refresh (&rest _args)
  "Refresh only a visible player, never selecting windows or reopening its slot."
  (let ((buffer (get-buffer hub/emms-player-buffer-name)))
    (if (not (and buffer (get-buffer-window buffer t)))
	(hub/emms-player--cancel-timer)
      (condition-case err
	  (with-current-buffer buffer
	    (let ((inhibit-read-only t)
		  (position (point))
		  (windows (mapcar (lambda (window)
				     (list window (window-point window) (window-start window)))
				   (get-buffer-window-list buffer nil t))))
	      (erase-buffer)
	      (hub/emms-player--render nil nil)
	      (goto-char (min position (point-max)))
	      (dolist (state windows)
		(when (window-live-p (car state))
		  (set-window-point (car state) (min (cadr state) (point-max)))
		  (set-window-start (car state) (min (caddr state) (point-max)) t)))))
	(error
	 (hub/emms-player--cancel-timer)
	 (message "EMMS player refresh failed: %s" (error-message-string err)))))))

(define-derived-mode hub/emms-player-mode special-mode "EMMS Player"
  "Read-only docked playback display with normal-state Bepo controls."
  (setq-local truncate-lines t)
  (setq-local line-spacing 0)
  (setq-local display-line-numbers nil)
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

(defun hub/emms-player--register-view (source)
  "Offer the player for SOURCE without opening it or initializing playback."
  (with-current-buffer source
    (context-panels-register-provider
     (list :name 'emms :icon "♫" :collect-bottom-views
	   (lambda (_source)
	     (list (list :id 'player :label "Player" :docked t
			 :height (if (display-graphic-p)
				     (max 11 (ceiling (/ 320.0 (frame-char-height)))) 11)
			 :buffer-name hub/emms-player-buffer-name
			 :mode #'hub/emms-player-mode :footer #'hub/music-player-footer
			 :render #'hub/emms-player--render)))))))

(defun hub/emms-player-open ()
  "Open and focus the docked music player without starting playback."
  (interactive)
  (require 'context-panels)
  (unless (fboundp 'context-panels-open-docked-view)
    (user-error "Update context-panels for docking; select its checkout with CONTEXT_PANELS_PATH"))
  (require 'emms)
  (let ((source (hub/emms-player--source)))
    (with-current-buffer source
      (hub/emms-player--register-view source)
      (hub/music--register-queue-view source)
      (context-panels-mode 1))
    (context-panels-open-bottom-view '(emms . player) source t)
    ;; The initial panel render precedes window creation. Prepare covers only
    ;; after the view is actually visible; hidden projections perform no I/O.
    (hub/emms-player--refresh)))

(defun hub/emms-player-close ()
  "Dismiss the shared bottom player without stopping playback."
  (interactive)
  (run-hooks 'hub/emms-player-close-hook)
  (context-panels-close-bottom-view)
  (hub/emms-player--refresh))

(defun hub/emms-player--command (command)
  "Dispatch playback COMMAND through EMMS, then refresh its view."
  (require 'emms)
  (funcall command)
  (hub/emms-player--refresh))

(defun hub/emms-player-toggle ()
  "Start or toggle playback without changing the current window."
  (interactive)
  (hub/emms-player--command
   (if (eq (hub/music-playback-state) 'connecting) #'emms-stop #'emms-pause)))

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
    ("v" . hub/emms-spectrum-toggle)
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
  ;; The lazy music access module owns the global 'm prefix.
  (hub/music--local-help))

(with-eval-after-load 'emms
  (dolist (hook '(emms-player-started-hook emms-player-stopped-hook
					   emms-player-paused-hook emms-player-finished-hook
					   emms-player-seeked-functions emms-track-updated-functions
					   emms-playlist-selection-changed-hook emms-playlist-source-inserted-hook
					   emms-playlist-cleared-hook))
    (add-hook hook #'hub/emms-player--refresh)))

(with-eval-after-load 'apps/spotify-browser-player
  (add-hook 'hub/spotify-browser-player-status-hook #'hub/emms-player--refresh))

(provide 'apps/emms-player)
;;; emms-player.el ends here
