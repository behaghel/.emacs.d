;;; spotify-player.el --- Spotify EMMS tracks and backend registration -*- lexical-binding: t; -*-

;;; Commentary:
;; EMMS owns the single mixed playlist.  The receiver plays one Spotify URI;
;; only a matching, current completion event may advance the EMMS playlist.

;;; Code:

(require 'hub-utils)
(require 'emms)
(require 'emms-playlist-mode)
(require 'websocket)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(autoload 'hub/spotify-headless-disconnect "apps/spotify-headless" nil t)
(autoload 'hub/spotify-headless-ready-p "apps/spotify-headless")
(autoload 'hub/spotify-headless-request "apps/spotify-headless")
(defvar hub/spotify-headless--port nil)
(defvar hub/spotify-headless--key nil)

(cl-defstruct (hub/spotify-player--instance (:constructor hub/spotify-player--instance-create))
  "One occurrence of a Spotify URI, independent of duplicate track objects."
  track socket armed ready timeout controls control-timer control-busy
  (inflight 0) (stage 'websocket) close-timer)

(defvar hub/spotify-player--run nil
  "Current Spotify occurrence, or nil; never a second playlist.")

(defvar hub/spotify-player-last-error nil
  "Safe failure metadata only: stage, reason, status, condition and known kind.
Never contains response bodies, event messages, credentials or track metadata.")

(defun hub/spotify-player--current-p (run)
  "Return non-nil only for the live RUN owned by the active EMMS backend."
  (and (eq run hub/spotify-player--run) (eq emms-player-playing-p 'emms-player-spotify-headless)))

(defun hub/spotify-player--release (run)
  "Invalidate RUN before releasing callbacks, timers and its event connection."
  (when (eq run hub/spotify-player--run) (setq hub/spotify-player--run nil))
  (dolist (timer (list (hub/spotify-player--instance-timeout run)
		       (hub/spotify-player--instance-control-timer run)
		       (hub/spotify-player--instance-close-timer run)))
    (when (timerp timer) (cancel-timer timer)))
  (when-let* ((socket (hub/spotify-player--instance-socket run)))
    (websocket-close socket)))

(defun hub/spotify-player--fail (run &optional reason status condition kind)
  "Stop RUN with safe REASON, HTTP STATUS, CONDITION symbol and known KIND."
  (when (hub/spotify-player--current-p run)
    (let ((safe-status (and (integerp status) (<= 100 status 599) status))
	  (safe-condition (and (symbolp condition) condition))
	  (safe-kind (and kind (if (member kind '("restricted" "unsupported" "rate_limited"
						  "rejected" "server" "timeout" "network" "unknown"))
				   kind "unknown"))))
      (setq hub/spotify-player-last-error
	    (list :stage (hub/spotify-player--instance-stage run) :reason (or reason 'failure)
		  :http-status safe-status :condition safe-condition :kind safe-kind))
      (message "Headless Spotify stopped: stage=%s, reason=%s%s%s%s. Queue not advanced."
	       (plist-get hub/spotify-player-last-error :stage) (or reason 'failure)
	       (if safe-status (format ", HTTP %d" safe-status) "")
	       (if safe-condition (format ", condition=%s" safe-condition) "")
	       (if safe-kind (concat ", kind=" safe-kind) "")))
    (hub/spotify-player--stop)))

(defun hub/spotify-player--event (run json)
  "Handle a decoded receiver event JSON for this particular RUN."
  (when (hub/spotify-player--current-p run)
    (let* ((type (alist-get 'type json)) (data (alist-get 'data json))
	   (uri (emms-track-get (hub/spotify-player--instance-track run) 'name)))
      (cond
       ((equal type "playback_error")
	(let ((kind (alist-get 'kind data)) (detail (alist-get 'message data)))
	  ;; Recognize only the SDK's fixed refusal signature, never retain text.
	  (hub/spotify-player--fail
	   run (if (and (equal kind "restricted") (stringp detail)
			(string-match-p "failed retrieving aes key with code [0-9]+" detail))
		   'audio-key-refused 'playback)
	   nil nil kind)))
       ((and (equal type "playing") (not (equal (alist-get 'uri data) uri)))
	(hub/spotify-player--fail run 'unexpected-track))
       ((equal (alist-get 'uri data) uri)
	(pcase type
	  ("playing"
	   (if (not (equal (alist-get 'play_origin data) "go-librespot"))
	       (hub/spotify-player--fail run 'unexpected-origin)
	     (setf (hub/spotify-player--instance-armed run) t
		   (hub/spotify-player--instance-stage run) 'playing)
	     (when-let* ((timer (hub/spotify-player--instance-timeout run))) (cancel-timer timer))))
	  ((or "metadata" "seek")
	   (let ((duration (alist-get 'duration data)) (position (alist-get 'position data)))
	     (when (and (numberp duration) (> duration 0))
	       (emms-track-set (hub/spotify-player--instance-track run) 'info-playing-time (floor duration 1000)))
	     (when (numberp position)
	       (run-hook-with-args 'emms-player-time-set-functions (max 0 (floor position 1000))))))
	  ("not_playing"
	   (when (and (hub/spotify-player--instance-armed run)
		      (equal (alist-get 'play_origin data) "go-librespot"))
	     (hub/spotify-player--halt run)
	     (let ((emms-player-stopped-p nil)) (emms-player-stopped))))))))))

(defun hub/spotify-player--prepare (run steps)
  "Apply ordered setup STEPS for RUN before accepting normal controls."
  (when (hub/spotify-player--current-p run)
    (if (not steps)
	(progn (setf (hub/spotify-player--instance-ready run) t
		     (hub/spotify-player--instance-stage run)
		     (if (hub/spotify-player--instance-armed run) 'playing 'awaiting-playback))
	       (hub/spotify-player--drain run))
      (hub/spotify-player--request
       run (caar steps) (cdar steps)
       (lambda (status _json)
	 (if (eq status 200) (hub/spotify-player--prepare run (cdr steps))
	   (hub/spotify-player--fail run 'http status)))))))

(defun hub/spotify-player--start (track)
  "Start TRACK on the authenticated headless receiver, never Spotify desktop."
  (unless (hub/spotify-headless-ready-p)
    (user-error "Connect the receiver first: M-x hub/spotify-headless-connect"))
  ;; EMMS's mode-line clock requires whole seconds, including cached entries.
  (when-let* ((duration (emms-track-get track 'info-playing-time))
	      (_ (numberp duration)))
    (emms-track-set track 'info-playing-time (max 0 (floor duration))))
  (let ((run (hub/spotify-player--instance-create :track track)))
    (setq hub/spotify-player--run run)
    (emms-player-started 'emms-player-spotify-headless)
    (setf (hub/spotify-player--instance-timeout run)
	  (run-at-time 30 nil (lambda () (hub/spotify-player--fail run 'timeout))))
    (setq hub/spotify-player-last-error nil)
    (condition-case err
	(let ((socket
	       (websocket-open
		(format "ws://127.0.0.1:%d/events" hub/spotify-headless--port) :nowait t
		:custom-header-alist (list (cons "X-Emacs-Spotify-Key" hub/spotify-headless--key))
		:on-open
		(lambda (ws)
		  (if (not (hub/spotify-player--current-p run))
		      (websocket-close ws)
		    (setf (hub/spotify-player--instance-socket run) ws)
		    (hub/spotify-player--prepare
		     run `(("/player/repeat_context" . ((repeat_context . :false)))
			   ("/player/repeat_track" . ((repeat_track . :false)))
			   ("/player/shuffle_context" . ((shuffle_context . :false)))
			   ("/player/play" . ((uri . ,(emms-track-get track 'name)) (position . 0)))))))
		:on-message
		(lambda (_ws frame)
		  (condition-case event-error
		      (hub/spotify-player--event
		       run (json-parse-string (websocket-frame-text frame) :object-type 'alist
					      :array-type 'list :null-object nil :false-object nil))
		    (error (hub/spotify-player--fail run 'event-decode nil (car event-error)))))
		:on-close
		(lambda (_ws)
		  ;; websocket.el closes before reporting a handshake error.
		  ;; Let that richer error win over a generic close notification.
		  (when (hub/spotify-player--current-p run)
		    (setf (hub/spotify-player--instance-close-timer run)
			  (run-at-time 0 nil (lambda () (hub/spotify-player--fail run 'websocket-closed))))))
		:on-error
		(lambda (_ws _where condition)
		  (hub/spotify-player--fail
		   run 'websocket-error
		   (and (eq (car-safe condition) 'websocket-received-error-http-response) (cadr condition))
		   (car-safe condition))))))
	  (if (hub/spotify-player--current-p run)
	      (setf (hub/spotify-player--instance-socket run) socket)
	    (websocket-close socket)))
      (error (hub/spotify-player--fail run 'startup nil (car err))))))

(defun hub/spotify-player--request (run path data callback)
  "Count a mutating receiver request for RUN at PATH with DATA and CALLBACK."
  (setf (hub/spotify-player--instance-stage run) path)
  (cl-incf (hub/spotify-player--instance-inflight run))
  (let (completed)
    (condition-case err
	(hub/spotify-headless-request
	 "POST" path data
	 (lambda (status json)
	   (setq completed t)
	   (setf (hub/spotify-player--instance-inflight run)
		 (max 0 (1- (hub/spotify-player--instance-inflight run))))
	   (when (hub/spotify-player--current-p run) (funcall callback status json))))
      (error
       (unless completed (cl-decf (hub/spotify-player--instance-inflight run)))
       (hub/spotify-player--fail run 'http-start nil (car err))))))

(defun hub/spotify-player--halt (run)
  "Cancel RUN and its receiver context before another EMMS backend starts."
  (hub/spotify-player--release run)
  (let ((ack nil) (deadline (+ (float-time) 2)))
    (unwind-protect
	(progn
	  ;; Unlike pause, stop also cancels a pending asynchronous decoder load.
	  (setq ack (memq (car (hub/spotify-headless-request "POST" "/player/stop" nil)) '(200 204)))
	  (while (and ack (> (hub/spotify-player--instance-inflight run) 0)
		      (< (float-time) deadline))
	    (accept-process-output nil 0.01)))
      (unless (and ack (zerop (hub/spotify-player--instance-inflight run)))
	(hub/spotify-headless-disconnect)))))

(defun hub/spotify-player--stop ()
  "Cancel the current occurrence, releasing clocks without advancing EMMS."
  (when-let* ((run hub/spotify-player--run))
    (unwind-protect (hub/spotify-player--halt run)
      (when (eq emms-player-playing-p 'emms-player-spotify-headless)
	(let ((emms-player-stopped-p t)) (emms-player-stopped))))))

(defun hub/spotify-player-receiver-lost ()
  "Release current EMMS state when its owned receiver has already exited."
  (when-let* ((run hub/spotify-player--run))
    (hub/spotify-player--release run)
    (when (eq emms-player-playing-p 'emms-player-spotify-headless)
      (let ((emms-player-stopped-p t)) (emms-player-stopped)))))

(defun hub/spotify-player--drain (run)
  "Send RUN's queued controls serially after startup, not inside EMMS hooks."
  (when-let* ((timer (hub/spotify-player--instance-control-timer run))) (cancel-timer timer))
  (setf (hub/spotify-player--instance-control-timer run) nil)
  (when (and (hub/spotify-player--current-p run) (hub/spotify-player--instance-ready run)
	     (not (hub/spotify-player--instance-control-busy run))
	     (hub/spotify-player--instance-controls run))
    (let ((job (pop (hub/spotify-player--instance-controls run))))
      (setf (hub/spotify-player--instance-control-busy run) t)
      (hub/spotify-player--request
       run (car job) (cdr job)
       (lambda (status _json)
	 (setf (hub/spotify-player--instance-control-busy run) nil)
	 (if (eq status 200) (hub/spotify-player--drain run) (hub/spotify-player--fail run 'http status)))))))

(defun hub/spotify-player--control (path &optional data)
  "Queue PATH and DATA after EMMS has updated its own pause/seek state."
  (when-let* ((run hub/spotify-player--run))
    (setf (hub/spotify-player--instance-controls run)
	  (append (hub/spotify-player--instance-controls run) (list (cons path data))))
    (unless (or (hub/spotify-player--instance-control-timer run)
		(hub/spotify-player--instance-control-busy run))
      (setf (hub/spotify-player--instance-control-timer run)
	    (run-at-time 0 nil (lambda () (hub/spotify-player--drain run)))))))

(defvar emms-player-spotify-headless
  (emms-player #'hub/spotify-player--start #'hub/spotify-player--stop
	       (lambda (track)
		 (and (eq (emms-track-get track 'type) 'url)
		      (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" (emms-track-get track 'name)))))
  "EMMS backend for canonical Spotify track URIs only.")

(emms-player-set emms-player-spotify-headless 'pause (lambda () (hub/spotify-player--control "/player/pause")))
(emms-player-set emms-player-spotify-headless 'resume (lambda () (hub/spotify-player--control "/player/resume")))
(emms-player-set emms-player-spotify-headless 'seek
		 (lambda (seconds) (hub/spotify-player--control "/player/seek"
								`((position . ,(round (* seconds 1000))) (relative . t)))))

(defun hub/spotify-player-register ()
  "Register the official browser backend while preserving local media players."
  (require 'apps/spotify-browser-player)
  (require 'apps/emms-local)
  (hub/emms-register-local-players)
  (setq emms-player-list (delq 'emms-player-spotify-headless emms-player-list))
  (add-to-list 'emms-player-list 'emms-player-spotify-browser))

(defun hub/spotify-player-track (metadata)
  "Make an EMMS URL track from inert Spotify METADATA."
  ;; Spotify already supplies these fields.  EMMS's generic file-info reader
  ;; defers a playlist rewrite even for URLs; during GPG unlock that rewrite
  ;; can invalidate a pending occurrence without any listener queue edit.
  (let* ((emms-track-initialize-functions
	  (remq 'emms-info-initialize-track emms-track-initialize-functions))
	 (track (emms-track 'url (alist-get 'uri metadata)))
	 (duration (alist-get 'duration_ms metadata)))
    (dolist (entry `((info-title . ,(alist-get 'name metadata))
		     (info-artist . ,(mapconcat (lambda (a) (or (alist-get 'name a) ""))
						(alist-get 'artists metadata) ", "))
		     (info-album . ,(alist-get 'name (alist-get 'album metadata)))
		     (info-artwork-url . ,(alist-get 'url (car (alist-get 'images (alist-get 'album metadata)))))))
      (emms-track-set track (car entry) (cdr entry)))
    (when (and (numberp duration) (> duration 0))
      (emms-track-set track 'info-playing-time (floor duration 1000)))
    track))

(defun hub/spotify-player-add (metadata &optional play)
  "Append METADATA to EMMS; optionally PLAY the appended occurrence."
  (hub/spotify-player-register)
  (unless (buffer-live-p emms-playlist-buffer)
    (emms-playlist-set-playlist-buffer (emms-playlist-new)))
  (let ((track (hub/spotify-player-track metadata)))
    (with-current-buffer emms-playlist-buffer
      (save-excursion
	(goto-char (point-max))
	(let ((position (point)))
	  (emms-playlist-insert-track track)
	  (when play
	    (emms-stop)
	    (emms-playlist-select position)))))
    (when play (emms-start)))
  nil)

(hub/spotify-player-register)

(provide 'apps/spotify-player)
;;; spotify-player.el ends here
