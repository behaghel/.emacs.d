;;; spotify-player-test.el --- Mixed EMMS playlist scenarios -*- lexical-binding: t; -*-

;;; Commentary:
;; Retained experimental headless adapter regression tests, explicitly selecting
;; that adapter. Normal configuration uses the separately tested official SDK.
;; Never read credentials, launch Spotify desktop, or stream real audio.

;;; Code:

(require 'ert)
(require 'cl-lib)
(dolist (package '("emms" "websocket" "evil"))
  (add-to-list 'load-path
	       (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
					     (expand-file-name "straight/build" default-directory)))))
(require 'emms)
(require 'emms-playlist-mode)
(require 'emms-playing-time)
(require 'apps/spotify)
(require 'apps/spotify-player)

(defvar hub/spotify-player-test--file-player nil)
(defvar hub/spotify-player-last-error nil)

(defmacro hub/spotify-player-test--with-queue (&rest body)
  "Run BODY with a mixed queue and fake receiver, never a desktop application."
  (declare (indent 0) (debug t))
  `(let* ((emms-playlist-buffer nil) (emms-playlist-buffers nil)
	  (emms-track-initialize-functions nil) (emms-player-playing-p nil)
	  (emms-player-paused-p nil) (emms-player-delay 0)
	  (emms-player-started-hook nil) (emms-player-stopped-hook nil)
	  (emms-player-finished-hook nil) (emms-player-paused-hook nil)
	  (emms-player-next-function #'emms-next-noerror)
	  (emms-repeat-track nil) (emms-single-track nil)
	  (hub/spotify-player--run nil)
	  (hub/spotify-player--control-timer nil)
	  (hub/spotify-headless--port 12345)
	  files requests sockets
	  (hub/spotify-player-test--file-player
	   (emms-player
	    (lambda (track) (push (emms-track-get track 'name) files)
	      (emms-player-started 'hub/spotify-player-test--file-player))
	    #'ignore (lambda (track) (eq (emms-track-get track 'type) 'file))))
	  (emms-player-list (list 'hub/spotify-player-test--file-player))
	  (playlist (emms-playlist-new " *mixed Spotify test*")))
     (setq emms-playlist-buffer playlist)
     (unwind-protect
	 (cl-letf (((symbol-function 'hub/spotify-player-register)
		    (lambda () (add-to-list 'emms-player-list 'emms-player-spotify-headless)))
		   ((symbol-function 'hub/spotify-headless-ready-p) (lambda () t))
		   ((symbol-function 'hub/spotify-headless-request)
		    (lambda (method path data &optional callback)
		      (push (list method path data) requests)
		      (if callback (funcall callback 200 nil) (cons 200 nil))))
		   ((symbol-function 'hub/spotify-player-play)
		    (lambda (&rest _) (ert-fail "Desktop control is forbidden")))
		   ((symbol-function 'websocket-open)
		    (lambda (_url &rest args)
		      (let ((socket (list :id (length sockets))))
			(push socket sockets)
			(funcall (plist-get args :on-open) socket)
			socket)))
		   ((symbol-function 'websocket-close) #'ignore))
	   (hub/spotify-player-register)
	   ,@body)
       (when emms-player-playing-p
	 (cl-letf (((symbol-function 'websocket-close) #'ignore)
		   ((symbol-function 'hub/spotify-headless-request) (lambda (&rest _) (cons 200 nil))))
	   (emms-stop)))
       (when (buffer-live-p playlist) (kill-buffer playlist)))))

(defun hub/spotify-player-test--raw-track (&optional title)
  "Return synthetic Spotify metadata with optional TITLE."
  `((uri . "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
    (name . ,(or title "Liked song"))
    (artists . (((name . "Artist")))) (album . ((name . "Album"))) (duration_ms . 180000)))

(defun hub/spotify-player-test--event (run type)
  "Deliver a synthetic receiver event of TYPE to RUN."
  (hub/spotify-player--event
   run `((type . ,type) (data . ((uri . "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
				 (play_origin . "go-librespot"))))))

(ert-deftest hub/spotify-player-mixed-queue-naturally-advances-through-emms ()
  "AC-2: Spotify -> file -> Spotify dispatches once using EMMS, even for duplicate URIs."
  (hub/spotify-player-test--with-queue
   (with-current-buffer playlist
     (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
     (emms-playlist-insert-track (emms-track 'file "/tmp/local.wav"))
     (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
     (goto-char (point-min)) (emms-playlist-select (point)))
   (emms-start)
   (should (eq emms-player-playing-p 'emms-player-spotify-headless))
   (let ((first hub/spotify-player--run))
     (hub/spotify-player-test--event first "playing")
     (hub/spotify-player-test--event first "not_playing")
     (should (eq emms-player-playing-p 'hub/spotify-player-test--file-player))
     (should (equal files '("/tmp/local.wav")))
     (emms-player-stopped)
     (should (eq emms-player-playing-p 'emms-player-spotify-headless))
     (let ((second hub/spotify-player--run))
       (hub/spotify-player-test--event first "not_playing")
       (should (eq hub/spotify-player--run second))
       (hub/spotify-player-test--event second "playing")
       (hub/spotify-player-test--event second "not_playing")
       (should-not emms-player-playing-p)))
   (should (= (cl-count "/player/play" requests :key #'cadr :test #'equal) 2))
   (should-not (cl-find-if (lambda (r) (member (cadr r) '("/player/next" "/player/prev"))) requests))))

(ert-deftest hub/spotify-player-library-add-and-ret-preserve-existing-queue ()
  "AC-1: Enqueue does not interrupt a file; RET appends and selects the new occurrence."
  (hub/spotify-player-test--with-queue
   (with-current-buffer playlist
     (emms-playlist-insert-track (emms-track 'file "/tmp/local.wav"))
     (goto-char (point-min)) (emms-playlist-select (point)))
   (emms-start)
   (with-temp-buffer
     (hub/spotify-library-mode)
     (hub/spotify--received 0 200 `((total . 1) (items . (((track . ,(hub/spotify-player-test--raw-track)))))) nil)
     (goto-char (point-min))
     (while (and (not (eobp)) (null (tabulated-list-get-id))) (forward-line 1))
     (hub/spotify-add-selected)
     (should (eq emms-player-playing-p 'hub/spotify-player-test--file-player))
     (hub/spotify-play-selected))
   (should (eq emms-player-playing-p 'emms-player-spotify-headless))
   (with-current-buffer playlist
     (should (= (length (emms-playlist-tracks-in-region (point-min) (point-max))) 3))
     (should (= (emms-track-get (emms-playlist-selected-track) 'info-playing-time) 180)))))

(ert-deftest hub/spotify-player-controls-and-manual-skip-stay-in-emms ()
  "Pause/resume and seeks address this track; next changes the EMMS queue."
  (hub/spotify-player-test--with-queue
   (with-current-buffer playlist
     (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
     (emms-playlist-insert-track (emms-track 'file "/tmp/local.wav"))
     (goto-char (point-min)) (emms-playlist-select (point)))
   (emms-start)
   (let ((run hub/spotify-player--run))
     (hub/spotify-player-test--event run "playing")
     (emms-pause)
     (hub/spotify-player--drain run)
     (should emms-player-paused-p)
     (emms-pause)
     (hub/spotify-player--drain run)
     (should-not emms-player-paused-p)
     (emms-seek -10)
     (hub/spotify-player--drain run)
     (should (cl-find-if (lambda (r) (and (equal (cadr r) "/player/seek")
					  (= (alist-get 'position (caddr r)) -10000))) requests))
     (emms-next)
     (hub/spotify-player-test--event run "not_playing")
     (should (eq emms-player-playing-p 'hub/spotify-player-test--file-player))
     (should (= (length files) 1))
     (should (cl-find "/player/stop" requests :key #'cadr :test #'equal)))))

(ert-deftest hub/spotify-player-failure-stops-here-instead-of-skipping ()
  "A restricted or disconnected stream cannot silently advance to the next entry."
  (hub/spotify-player-test--with-queue
   (with-current-buffer playlist
     (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
     (emms-playlist-insert-track (emms-track 'file "/tmp/local.wav"))
     (goto-char (point-min)) (emms-playlist-select (point)))
   (emms-start)
   (let ((run hub/spotify-player--run))
     (hub/spotify-player--event run '((type . "playback_error") (data . ((kind . "restricted")))))
     (should-not emms-player-playing-p)
     (should-not files)
     (hub/spotify-player-test--event run "not_playing")
     (should-not files))))

(ert-deftest hub/spotify-player-stop-cancels-a-pending-load-before-local-start ()
  "Stopping during loading invalidates success callbacks before another player starts."
  (hub/spotify-player-test--with-queue
   (let (pending)
     (cl-letf (((symbol-function 'hub/spotify-headless-request)
		(lambda (method path data &optional callback)
		  (push (list method path data) requests)
		  (cond ((equal path "/player/play") (setq pending callback))
			((equal path "/player/stop") (when pending (funcall pending 409 nil)) (cons 200 nil))
			(callback (funcall callback 200 nil))
			(t (cons 200 nil))))))
       (with-current-buffer playlist
	 (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
	 (emms-playlist-insert-track (emms-track 'file "/tmp/local.wav"))
	 (goto-char (point-min)) (emms-playlist-select (point)))
       (emms-start)
       (let ((run hub/spotify-player--run))
	 (should pending)
	 (emms-next)
	 (funcall pending 200 nil)
	 (hub/spotify-player-test--event run "playing")
	 (hub/spotify-player-test--event run "not_playing")
	 (should-not hub/spotify-player--run)
	 (should (eq emms-player-playing-p 'hub/spotify-player-test--file-player))
	 (should (= (length files) 1)))))))

(ert-deftest hub/spotify-player-unexpected-playing-uri-stops-instead-of-owning-a-second-queue ()
  "An unexpected receiver selection cannot bypass EMMS's current occurrence."
  (hub/spotify-player-test--with-queue
   (with-current-buffer playlist
     (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
     (goto-char (point-min)) (emms-playlist-select (point)))
   (emms-start)
   (hub/spotify-player--event hub/spotify-player--run
			      '((type . "playing") (data . ((uri . "spotify:track:1111111111111111111111")
							    (play_origin . "go-librespot")))))
   (should-not emms-player-playing-p)
   (should-not files)))

(ert-deftest hub/spotify-player-native-websocket-starts-the-control-pipeline ()
  "Exercise real asynchronous WebSocket callbacks rather than an on-open stub."
  (let ((real-open (symbol-function 'websocket-open))
	(real-close (symbol-function 'websocket-close)))
    (hub/spotify-player-test--with-queue
     (let (server)
       (cl-letf (((symbol-function 'websocket-open) real-open)
		 ((symbol-function 'websocket-close) real-close))
	 (unwind-protect
	     (progn
	       (setq server (websocket-server
			     0 :host "127.0.0.1"
			     :on-open (lambda (ws)
					(websocket-send-text ws "{\"type\":\"playing\",\"data\":{\"uri\":\"spotify:track:4uLU6hMCjMI75M1A2tKUQC\",\"play_origin\":\"go-librespot\"}}"))))
	       (let ((hub/spotify-headless--port (process-contact server :service))
		     (hub/spotify-headless--key "synthetic-key")
		     (deadline (+ (float-time) 3)))
		 (with-current-buffer playlist
		   (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
		   (goto-char (point-min)) (emms-playlist-select (point)))
		 (emms-start)
		 (while (and hub/spotify-player--run
			     (not (hub/spotify-player--instance-armed hub/spotify-player--run))
			     (< (float-time) deadline))
		   (accept-process-output nil 0.01))
		 (should hub/spotify-player--run)
		 (should (hub/spotify-player--instance-armed hub/spotify-player--run))
		 (should (cl-find "/player/play" requests :key #'cadr :test #'equal))))
	   (when hub/spotify-player--run (hub/spotify-player--stop))
	   (when server (websocket-server-close server))))))))

(ert-deftest hub/spotify-player-reports-safe-http-failure-context ()
  "HTTP failures identify the stage/status, without retaining a response body."
  (hub/spotify-player-test--with-queue
   (let ((hub/spotify-player-last-error nil))
     (cl-letf (((symbol-function 'hub/spotify-headless-request)
		(lambda (_method path _data &optional callback)
		  (if callback (funcall callback (if (equal path "/player/play") 503 200)
					'((message . "SYNTHETIC-PRIVATE-PAYLOAD")))
		    (cons 200 nil)))))
       (with-current-buffer playlist
	 (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
	 (goto-char (point-min)) (emms-playlist-select (point)))
       (emms-start)
       (should (equal (plist-get hub/spotify-player-last-error :stage) "/player/play"))
       (should (eq (plist-get hub/spotify-player-last-error :http-status) 503))
       (should-not (string-match-p "SYNTHETIC-PRIVATE-PAYLOAD" (format "%S" hub/spotify-player-last-error)))))))

(ert-deftest hub/spotify-player-reports-only-known-playback-error-kinds ()
  "A receiver's safe error enum is useful; its arbitrary message is not logged."
  (hub/spotify-player-test--with-queue
   (let ((hub/spotify-player-last-error nil))
     (with-current-buffer playlist
       (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
       (goto-char (point-min)) (emms-playlist-select (point)))
     (emms-start)
     (hub/spotify-player--event hub/spotify-player--run
				'((type . "playback_error")
				  (data . ((kind . "restricted") (message . "SYNTHETIC-PRIVATE-PAYLOAD")))))
     (should (equal (plist-get hub/spotify-player-last-error :kind) "restricted"))
     (should-not (string-match-p "SYNTHETIC-PRIVATE-PAYLOAD" (format "%S" hub/spotify-player-last-error))))))

(ert-deftest hub/spotify-player-native-websocket-denial-keeps-http-status ()
  "A close-before-error handshake preserves the useful, safe HTTP diagnosis."
  (let ((real-open (symbol-function 'websocket-open))
	(real-close (symbol-function 'websocket-close)))
    (hub/spotify-player-test--with-queue
     (let ((hub/spotify-player-last-error nil) server)
       (cl-letf (((symbol-function 'websocket-open) real-open)
		 ((symbol-function 'websocket-close) real-close))
	 (unwind-protect
	     (progn
	       (setq server (make-network-process
			     :name "Synthetic rejected WebSocket" :server t :family 'ipv4
			     :host "127.0.0.1" :service 0 :noquery t
			     :filter (lambda (client _request)
				       (process-send-string client "HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
				       (delete-process client))))
	       (let ((hub/spotify-headless--port (process-contact server :service))
		     (hub/spotify-headless--key "synthetic-key")
		     (deadline (+ (float-time) 3)))
		 (with-current-buffer playlist
		   (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
		   (goto-char (point-min)) (emms-playlist-select (point)))
		 (emms-start)
		 (while (and hub/spotify-player--run (< (float-time) deadline))
		   (accept-process-output nil 0.01))
		 (should-not hub/spotify-player--run)
		 (should (eq (plist-get hub/spotify-player-last-error :http-status) 403))
		 (should-not (cl-find "/player/play" requests :key #'cadr :test #'equal))))
	   (when hub/spotify-player--run (hub/spotify-player--stop))
	   (when (process-live-p server) (delete-process server))))))))

(ert-deftest hub/spotify-player-retains-artwork-metadata-without-fetching ()
  "Queue construction retains an album image location without authorization or I/O."
  (let* ((emms-track-initialize-functions nil)
	 (metadata (hub/spotify-player-test--raw-track)))
    (setf (alist-get 'album metadata)
	  '((name . "Fixture album") (images . (((url . "https://i.scdn.co/image/cover123") (width . 640))))))
    (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Track creation fetched a cover"))))
      (should (equal (emms-track-get (hub/spotify-player-track metadata) 'info-artwork-url)
		     "https://i.scdn.co/image/cover123")))))

(ert-deftest hub/spotify-player-milliseconds-feed-integer-emms-time ()
  "Library and receiver milliseconds stay compatible with the real EMMS clock."
  (hub/spotify-player-test--with-queue
   (let* ((metadata (hub/spotify-player-test--raw-track))
	  (_ (setf (alist-get 'duration_ms metadata) 194607))
	  (track (hub/spotify-player-track metadata))
	  (emms-playing-time 0) (emms-playing-time-string "")
	  (emms-playing-time-display-mode t) (emms-playing-time-style 'time)
	  (emms-playing-time-display-short-p nil)
	  (emms-player-time-set-functions '(emms-playing-time-set)))
     (with-current-buffer playlist
       (emms-playlist-insert-track track)
       (goto-char (point-min)) (emms-playlist-select (point)))
     (should (integerp (emms-track-get track 'info-playing-time)))
     (emms-start)
     (emms-playing-time-display)
     (should (string-match-p "03:14" emms-playing-time-string))
     (hub/spotify-player--event
      hub/spotify-player--run
      '((type . "metadata") (data . ((uri . "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
				     (duration . 200456) (position . 12678)))))
     (should (eq (emms-track-get track 'info-playing-time) 200))
     (should (eq emms-playing-time 12))
     (emms-playing-time-display)
     (should (string-match-p "03:20" emms-playing-time-string)))))

(ert-deftest hub/spotify-player-normalizes-previously-enqueued-fractional-duration ()
  "An existing queue entry can be retried without keeping the old float bug."
  (hub/spotify-player-test--with-queue
   (let ((track (hub/spotify-player-track (hub/spotify-player-test--raw-track))))
     (emms-track-set track 'info-playing-time 194.607)
     (with-current-buffer playlist
       (emms-playlist-insert-track track)
       (goto-char (point-min)) (emms-playlist-select (point)))
     (emms-start)
     (should (eq (emms-track-get track 'info-playing-time) 194)))))

(ert-deftest hub/spotify-player-recognizes-key-refusal-without-retaining-raw-errors ()
  "Recognize the SDK's fixed refusal signature without exposing its error text."
  (hub/spotify-player-test--with-queue
   (let ((hub/spotify-player-last-error nil))
     (with-current-buffer playlist
       (emms-playlist-insert-track (hub/spotify-player-track (hub/spotify-player-test--raw-track)))
       (goto-char (point-min)) (emms-playlist-select (point)))
     (emms-start)
     (hub/spotify-player--event
      hub/spotify-player--run
      '((type . "playback_error")
	(data . ((kind . "restricted")
		 (message . "failed retrieving audio key: failed retrieving aes key with code 1 SYNTHETIC-PRIVATE-PAYLOAD")))))
     (should (eq (plist-get hub/spotify-player-last-error :reason) 'audio-key-refused))
     (should-not (string-match-p "SYNTHETIC-PRIVATE-PAYLOAD" (format "%S" hub/spotify-player-last-error))))))

(provide 'spotify-player-test)
;;; spotify-player-test.el ends here
