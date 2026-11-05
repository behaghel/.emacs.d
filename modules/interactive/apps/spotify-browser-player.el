;;; spotify-browser-player.el --- Official SDK backend for the EMMS playlist -*- lexical-binding: t; -*-

;;; Commentary:
;; EMMS owns ordering and repeat. This adapter owns one occurrence identity and
;; accepts completion only for that occurrence's current acknowledged controls.

;;; Code:
(require 'hub-utils)
(require 'emms)
(require 'apps/spotify-browser)
(require 'cl-lib)

(cl-defstruct (hub/spotify-browser-player--occurrence (:constructor hub/spotify-browser-player--create))
  "One EMMS selection, including separate selections of the same URI."
  id seq track timer finish-timer pending armed ending near)
(defvar hub/spotify-browser-player--run nil "Current occurrence, never a second playlist.")
(defvar hub/spotify-browser-player--serial 0 "Monotonic selection identity.")
(defvar hub/spotify-player-last-error nil "Safe backend failure metadata.")

(defun hub/spotify-browser-player--current-p (run)
  "Return non-nil for the active owned RUN."
  (and run (eq run hub/spotify-browser-player--run)
       (eq emms-player-playing-p 'emms-player-spotify-browser)))

(defun hub/spotify-browser-player--release (run)
  "Invalidate RUN before allowing callbacks or another backend to proceed."
  (when (eq run hub/spotify-browser-player--run) (setq hub/spotify-browser-player--run nil))
  (dolist (timer (list (hub/spotify-browser-player--occurrence-timer run)
		       (hub/spotify-browser-player--occurrence-finish-timer run)))
    (when (timerp timer) (cancel-timer timer))))

(defun hub/spotify-browser-player--stopped ()
  "Release EMMS clocks without invoking natural queue advancement."
  (when (eq emms-player-playing-p 'emms-player-spotify-browser)
    (let ((emms-player-stopped-p t)) (emms-player-stopped))))

(defun hub/spotify-browser-player--stop ()
  "Invalidate the occurrence and require browser silence before returning."
  (when-let* ((run hub/spotify-browser-player--run))
    (hub/spotify-browser-player--release run)
    (unwind-protect
	(hub/spotify-browser-stop (hub/spotify-browser-player--occurrence-id run))
      (hub/spotify-browser-player--stopped))))

(defun hub/spotify-browser-player--lost ()
  "Stop here when the owned browser or authorization disappears."
  (when-let* ((run hub/spotify-browser-player--run))
    (hub/spotify-browser-player--release run)
    (setq hub/spotify-player-last-error '(:stage browser :reason receiver-lost))
    (hub/spotify-browser-player--stopped)
    (message "Spotify receiver lost; queue not advanced. M-x hub/spotify-browser-connect")))

(defun hub/spotify-browser-player--fail (run &optional kind status)
  "Stop RUN with finite KIND and HTTP STATUS metadata, never raw messages."
  (when (hub/spotify-browser-player--current-p run)
    (setq hub/spotify-player-last-error
	  (list :stage 'browser :reason
		(if (member kind '("initialization_error" "authentication_error" "account_error" "playback_error"
				   "autoplay_failed" "play" "control" "state" "transport" "unexpected_track"
				   "receiver_queue" "stop" "timeout")) (intern kind) 'failure)
		:http-status (and (integerp status) (<= 100 status 599) status)))
    (message "Spotify stopped: %s. Queue not advanced. Use C-h v hub/spotify-player-last-error"
	     (plist-get hub/spotify-player-last-error :reason))
    (condition-case nil (hub/spotify-browser-player--stop)
      (error (hub/spotify-browser-player--stopped)))))

(defun hub/spotify-browser-player--deadline (run seconds)
  "Bound startup or control acknowledgement for RUN to SECONDS."
  (when (timerp (hub/spotify-browser-player--occurrence-timer run))
    (cancel-timer (hub/spotify-browser-player--occurrence-timer run)))
  (setf (hub/spotify-browser-player--occurrence-timer run)
	(run-at-time seconds nil (lambda () (hub/spotify-browser-player--fail run "timeout")))))

(defun hub/spotify-browser-player--start (track)
  "Play TRACK through the explicitly activated official SDK receiver."
  (unless (hub/spotify-browser-ready-p)
    (user-error "Connect and Enable audio first: M-x hub/spotify-browser-connect"))
  (when-let* ((duration (emms-track-get track 'info-playing-time)) (_ (numberp duration)))
    (emms-track-set track 'info-playing-time (max 0 (floor duration))))
  (let ((run (hub/spotify-browser-player--create :id (cl-incf hub/spotify-browser-player--serial) :track track)))
    (setq hub/spotify-browser-player--run run hub/spotify-player-last-error nil)
    (emms-player-started 'emms-player-spotify-browser)
    (hub/spotify-browser-player--deadline run 30)
    (condition-case nil
	(setf (hub/spotify-browser-player--occurrence-seq run)
	      (hub/spotify-browser-send "play" (hub/spotify-browser-player--occurrence-id run)
					`((uri . ,(emms-track-get track 'name)))))
      (error (hub/spotify-browser-player--fail run "transport")))))

(defun hub/spotify-browser-player--control (kind &optional data)
  "Send KIND and DATA for the current occurrence, invalidating older endings."
  (when-let* ((run hub/spotify-browser-player--run))
    (setf (hub/spotify-browser-player--occurrence-pending run) t
	  (hub/spotify-browser-player--occurrence-ending run) nil
	  (hub/spotify-browser-player--occurrence-near run) nil)
    (hub/spotify-browser-player--deadline run 10)
    (condition-case nil
	(setf (hub/spotify-browser-player--occurrence-seq run)
	      (hub/spotify-browser-send kind (hub/spotify-browser-player--occurrence-id run) data))
      (error (hub/spotify-browser-player--fail run "transport")))))

(defun hub/spotify-browser-player--finish (run)
  "Advance once only after a current, confirmed ending and acknowledged stop."
  (when (and (hub/spotify-browser-player--current-p run)
	     (hub/spotify-browser-player--occurrence-ending run)
	     (not (hub/spotify-browser-player--occurrence-pending run))
	     (not emms-player-paused-p))
    (hub/spotify-browser-player--release run)
    (condition-case nil
	(progn
	  (hub/spotify-browser-stop (hub/spotify-browser-player--occurrence-id run))
	  (let ((emms-player-stopped-p nil)) (emms-player-stopped)))
      (error
       (setq hub/spotify-player-last-error '(:stage browser :reason stop-unconfirmed))
       (hub/spotify-browser-player--stopped)
       (message "Spotify stop unconfirmed; queue transition blocked")))))

(defun hub/spotify-browser-player--event (json)
  "Project safe browser JSON into the current EMMS occurrence only."
  (when-let* ((run hub/spotify-browser-player--run)
	      (_ (hub/spotify-browser-player--current-p run))
	      (_ (eql (alist-get 'run json) (hub/spotify-browser-player--occurrence-id run))))
    (let ((type (alist-get 'type json)))
      (cond
       ((equal type "error")
	(run-at-time 0 nil (lambda () (hub/spotify-browser-player--fail run (alist-get 'kind json) (alist-get 'status json)))))
       ((eql (alist-get 'seq json) (hub/spotify-browser-player--occurrence-seq run))
	(pcase type
	  ("ack"
	   (setf (hub/spotify-browser-player--occurrence-pending run) nil)
	   (when (hub/spotify-browser-player--occurrence-armed run)
	     (cancel-timer (hub/spotify-browser-player--occurrence-timer run))))
	  ("state"
	   (let ((position (alist-get 'position json)) (duration (alist-get 'duration json)))
	     (when (and (integerp position) (>= position 0) (integerp duration) (> duration 0))
	       (emms-track-set (hub/spotify-browser-player--occurrence-track run) 'info-playing-time (floor duration 1000))
	       (run-hook-with-args 'emms-player-time-set-functions (floor position 1000))
	       (when (>= position 1000)
		 (setf (hub/spotify-browser-player--occurrence-armed run) t)
		 (unless (hub/spotify-browser-player--occurrence-pending run)
		   (cancel-timer (hub/spotify-browser-player--occurrence-timer run))))
	       (when (and (< position duration) (<= (- duration position) 1500))
		 (setf (hub/spotify-browser-player--occurrence-near run) (float-time))))))
	  ("ended"
	   (when (and (hub/spotify-browser-player--occurrence-armed run)
		      (hub/spotify-browser-player--occurrence-near run)
		      (< (- (float-time) (hub/spotify-browser-player--occurrence-near run)) 3)
		      (not (hub/spotify-browser-player--occurrence-pending run)) (not emms-player-paused-p))
	     (setf (hub/spotify-browser-player--occurrence-ending run) t
		   (hub/spotify-browser-player--occurrence-finish-timer run)
		   (run-at-time 0 nil (lambda () (hub/spotify-browser-player--finish run))))))))))))

(defvar emms-player-spotify-browser
  (emms-player #'hub/spotify-browser-player--start #'hub/spotify-browser-player--stop
	       (lambda (track) (and (eq (emms-track-get track 'type) 'url)
				    (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" (emms-track-get track 'name)))))
  "EMMS player for canonical Spotify URIs, with no desktop or headless fallback.")
(emms-player-set emms-player-spotify-browser 'pause (lambda () (hub/spotify-browser-player--control "pause")))
(emms-player-set emms-player-spotify-browser 'resume (lambda () (hub/spotify-browser-player--control "resume")))
(emms-player-set emms-player-spotify-browser 'seek (lambda (seconds) (hub/spotify-browser-player--control "seek" `((seconds . ,seconds)))))
(setq hub/spotify-browser-event-function #'hub/spotify-browser-player--event)
(add-hook 'hub/spotify-browser-lost-hook #'hub/spotify-browser-player--lost)

(provide 'apps/spotify-browser-player)
;;; spotify-browser-player.el ends here
