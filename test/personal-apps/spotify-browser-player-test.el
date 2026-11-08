;;; spotify-browser-player-test.el --- Browser-backed mixed queue acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Real EMMS dispatch, synthetic browser acknowledgements, no account access.

;;; Code:
(require 'ert)
(require 'cl-lib)
(dolist (package '("emms" "websocket" "evil"))
  (add-to-list 'load-path (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
							(expand-file-name "straight/build" default-directory)))))
(require 'apps/spotify-player)
(require 'apps/spotify-browser-player)
(defvar hub/spotify-browser-player-test--local-player nil)

(defmacro hub/spotify-browser-player-test--queue (&rest body)
  "Run BODY with real EMMS playlist state and a synthetic browser transport."
  (declare (indent 0))
  `(let* ((emms-playlist-buffer nil) (emms-playlist-buffers nil)
	  (emms-track-initialize-functions nil) (emms-player-playing-p nil)
	  (emms-player-paused-p nil) (emms-player-delay 0) (emms-repeat-track nil) (emms-single-track nil)
	  (emms-player-started-hook nil) (emms-player-stopped-hook nil) (emms-player-finished-hook nil)
	  (emms-player-paused-hook nil) (emms-player-next-function #'emms-next-noerror)
	  (hub/spotify-browser-player--run nil)
	  (hub/spotify-browser-player-status-hook nil) (serial 0) commands stops files
	  (hub/spotify-browser-player-test--local-player (emms-player (lambda (track) (push (emms-track-get track 'name) files)
									(emms-player-started 'hub/spotify-browser-player-test--local-player))
								      #'ignore (lambda (track) (eq (emms-track-get track 'type) 'file))))
	  (emms-player-list '(hub/spotify-browser-player-test--local-player))
	  (playlist (emms-playlist-new " *browser mixed queue*")))
     (setq emms-playlist-buffer playlist)
     (unwind-protect
	 (cl-letf (((symbol-function 'hub/spotify-browser-ready-p) (lambda () t))
		   ((symbol-function 'hub/spotify-browser-send)
		    (lambda (kind run &optional data) (push (list kind run data) commands) (cl-incf serial)))
		   ((symbol-function 'hub/spotify-browser-stop) (lambda (run) (push run stops))))
	   (hub/spotify-player-register)
	   (with-current-buffer playlist
	     (emms-playlist-insert-track (emms-track 'url "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
	     (emms-playlist-insert-track (emms-track 'file "/tmp/local.wav"))
	     (emms-playlist-insert-track (emms-track 'url "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
	     (goto-char (point-min)) (emms-playlist-select (point)))
	   ,@body)
       (when hub/spotify-browser-player--run
	 (cl-letf (((symbol-function 'hub/spotify-browser-stop) #'ignore)) (emms-stop)))
       (when (buffer-live-p playlist) (kill-buffer playlist)))))

(defun hub/spotify-browser-player-test--event (run type &optional data)
  "Deliver TYPE and DATA for the current identity of RUN."
  (hub/spotify-browser-player--event
   (append `((type . ,type) (run . ,(hub/spotify-browser-player--occurrence-id run))
	     (seq . ,(hub/spotify-browser-player--occurrence-seq run))) data)))

(ert-deftest hub/spotify-browser-player-first-play-prepares-and-awaits-audio ()
  "Explicit Play opens the receiver once and starts the same occurrence after activation."
  (hub/spotify-browser-player-test--queue
   (let ((ready nil) (connections 0))
     (cl-letf (((symbol-function 'hub/spotify-browser-ready-p) (lambda () ready))
	       ((symbol-function 'hub/spotify-browser-live-p) (lambda () t))
	       ((symbol-function 'hub/spotify-browser-connect) (lambda (&rest _) (cl-incf connections))))
       (emms-start)
       (should (= connections 1))
       (should (hub/spotify-browser-player-waiting-p))
       (should emms-player-paused-p)
       (should-not commands)
       (let ((run hub/spotify-browser-player--run))
	 (setq ready t)
	 (hub/spotify-browser-player--await-audio run)
	 (hub/spotify-browser-player--await-audio run)
	 (should-not (hub/spotify-browser-player-waiting-p))
	 (should-not emms-player-paused-p)
	 (should (= (length commands) 1))
	 (should (equal (caar commands) "play"))
	 (should (eq run hub/spotify-browser-player--run)))))))

(ert-deftest hub/spotify-browser-player-liked-songs-ret-awaits-audio ()
  "Library RET preserves the queue and plays its appended occurrence once after activation."
  (require 'apps/spotify)
  (require 'emms-info)
  (hub/spotify-browser-player-test--queue
   (let ((ready nil) (connections 0) pending-info
	 (emms-track-initialize-functions '(emms-info-initialize-track))
	 (emms-info-functions nil) (emms-info-asynchronously t)
	 (emms-info-asynchronous-tracks 0)
	 (original (with-current-buffer playlist
		     (nreverse (emms-playlist-tracks-in-region (point-min) (point-max))))))
     (with-current-buffer playlist (emms-playlist-mode))
     (cl-letf (((symbol-function 'hub/spotify-browser-ready-p) (lambda () ready))
	       ((symbol-function 'hub/spotify-browser-live-p) (lambda () t))
	       ((symbol-function 'hub/spotify-browser-connect)
		(lambda (&rest _)
		  (cl-incf connections)
		  ;; GPG yields to EMMS's real deferred metadata callbacks.
		  (dolist (job (reverse pending-info)) (apply (car job) (cdr job)))))
	       ((symbol-function 'emms-later-do) (lambda (&rest job) (push job pending-info))))
       (with-temp-buffer
	 (hub/spotify-library-mode)
	 (hub/spotify--received
	  0 200 '((total . 1)
		  (items . (((track . ((uri . "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
				       (name . "Synthetic liked song") (duration_ms . 180000))))))) nil)
	 (goto-char (point-min))
	 (while (and (not (eobp)) (null (tabulated-list-get-id))) (forward-line 1))
	 (call-interactively (lookup-key hub/spotify-library-mode-map (kbd "RET"))))
       (should (= connections 1))
       (should (hub/spotify-browser-player-waiting-p))
       (should-not commands)
       (should-not pending-info)
       ;; The exclusion is local to Spotify construction, not a global hook edit.
       (let ((local (emms-track 'file "/tmp/synthetic-local.ogg")))
	 (should (eq (caar pending-info) 'emms-info-really-initialize-track))
	 (should (eq (cadar pending-info) local)))
       (let ((run hub/spotify-browser-player--run))
	 (with-current-buffer playlist
	   (let ((tracks (nreverse (emms-playlist-tracks-in-region (point-min) (point-max)))))
	     (should (equal (butlast tracks) original))
	     (should (eq (emms-playlist-selected-track) (car (last tracks))))))
	 (setq ready t)
	 (hub/spotify-browser-player--await-audio run)
	 (hub/spotify-browser-player--await-audio run)
	 (should (eq run hub/spotify-browser-player--run))
	 (should-not (hub/spotify-browser-player-waiting-p))
	 (should-not emms-player-paused-p)
	 (should (= (length commands) 1))
	 (should (equal (caar commands) "play")))))))

(ert-deftest hub/spotify-browser-player-preparation-is-visible-and-toggle-cancels ()
  "The console shows waiting, not playback; its transport toggle cancels preparation."
  (require 'apps/emms-player)
  (require 'emms-playing-time)
  (hub/spotify-browser-player-test--queue
   (let ((emms-playing-time-display-timer nil)
	 (emms-player-started-hook '(emms-playing-time-start))
	 (emms-player-paused-hook '(emms-playing-time-pause))
	 (emms-player-stopped-hook '(emms-playing-time-stop)))
     (unwind-protect
	 (cl-letf (((symbol-function 'hub/spotify-browser-ready-p) (lambda () nil))
		   ((symbol-function 'hub/spotify-browser-live-p) (lambda () t))
		   ((symbol-function 'hub/spotify-browser-connect) #'ignore))
	   (emms-start)
	   (should (eq (hub/music-playback-state) 'connecting))
	   (let ((snapshot (hub/emms-player--snapshot)))
	     (should (equal (plist-get snapshot :state) "Waiting for audio"))
	     (should (= (plist-get snapshot :elapsed) 0)))
	   (should-not emms-playing-time-display-timer)
	   (hub/emms-player-toggle)
	   (should-not hub/spotify-browser-player--run)
	   (should-not emms-player-playing-p)
	   (should-not emms-playing-time-display-timer)
	   (should-not commands))
       (when (timerp emms-playing-time-display-timer)
	 (cancel-timer emms-playing-time-display-timer))))))

(ert-deftest hub/spotify-browser-player-cancelled-preparation-never-plays-later ()
  "Stop, selection, queue edits and playlist replacement invalidate delayed activation."
  (dolist (change '(stop select duplicate edit playlist))
    (hub/spotify-browser-player-test--queue
     (let ((ready nil))
       (cl-letf (((symbol-function 'hub/spotify-browser-ready-p) (lambda () ready))
		 ((symbol-function 'hub/spotify-browser-live-p) (lambda () t))
		 ((symbol-function 'hub/spotify-browser-connect) #'ignore))
	 (emms-start)
	 (let ((run hub/spotify-browser-player--run))
	   (pcase change
	     ('stop (emms-stop))
	     ('select (emms-playlist-current-select-next))
	     ('duplicate (emms-playlist-current-select-next) (emms-playlist-current-select-next))
	     ('edit (with-current-buffer playlist
		      (goto-char (point-max)) (emms-playlist-insert-track (emms-track 'file "/tmp/added.wav"))))
	     ('playlist (setq emms-playlist-buffer nil)))
	   (setq ready t)
	   (hub/spotify-browser-player--await-audio run)
	   (should-not commands)
	   (should-not stops)
	   (should-not hub/spotify-browser-player--run)
	   (should-not emms-player-playing-p)))))))

(ert-deftest hub/spotify-browser-player-preparation-timeout-never-skips ()
  "Expired audio activation cancels the pending occurrence without a stop command or successor."
  (hub/spotify-browser-player-test--queue
   (cl-letf (((symbol-function 'hub/spotify-browser-ready-p) (lambda () nil))
	     ((symbol-function 'hub/spotify-browser-live-p) (lambda () t))
	     ((symbol-function 'hub/spotify-browser-connect) #'ignore))
     (emms-start)
     (let ((run hub/spotify-browser-player--run))
       (setf (hub/spotify-browser-player--occurrence-activation-deadline run) (- (float-time) 1))
       (hub/spotify-browser-player--await-audio run)
       (should-not emms-player-playing-p)
       (should-not hub/spotify-browser-player--run)
       (should-not commands) (should-not stops) (should-not files)))))

(ert-deftest hub/spotify-browser-player-mixed-natural-advancement-and-duplicates ()
  "A confirmed Spotify ending, local ending and duplicate URI use one EMMS queue."
  (hub/spotify-browser-player-test--queue
   (emms-start)
   (should (eq emms-player-playing-p 'emms-player-spotify-browser))
   (let ((first hub/spotify-browser-player--run))
     (hub/spotify-browser-player-test--event first "state" '((position . 179500) (duration . 180000)))
     (hub/spotify-browser-player-test--event first "ended")
     (hub/spotify-browser-player--finish first)
     (should (equal files '("/tmp/local.wav")))
     (should (equal stops (list (hub/spotify-browser-player--occurrence-id first))))
     (emms-player-stopped)
     (let ((second hub/spotify-browser-player--run))
       (should second)
       (hub/spotify-browser-player-test--event first "ended")
       (hub/spotify-browser-player--finish first)
       (should (eq second hub/spotify-browser-player--run))
       (hub/spotify-browser-player-test--event second "state" '((position . 179500) (duration . 180000)))
       (hub/spotify-browser-player-test--event second "ended")
       (hub/spotify-browser-player--finish second)
       (should-not emms-player-playing-p)))
   (should (= (cl-count "play" commands :key #'car :test #'equal) 2))))

(ert-deftest hub/spotify-browser-player-seeking-shows-only-confirmed-position ()
  "Pending seeks expose receiver-confirmed time, not EMMS's optimistic seek offset."
  (require 'apps/emms-player)
  (require 'emms-playing-time)
  (hub/spotify-browser-player-test--queue
   (let ((emms-playing-time 0)
	 (emms-player-time-set-functions '(emms-playing-time-set))
	 (emms-player-seeked-functions '(emms-playing-time-seek))
	 (notifications 0))
     (emms-start)
     (let* ((run hub/spotify-browser-player--run)
	    (old (hub/spotify-browser-player--occurrence-seq run))
	    (hub/spotify-browser-player-status-hook (list (lambda () (cl-incf notifications)))))
       (hub/spotify-browser-player-test--event run "state" '((position . 102000) (duration . 274000)))
       (emms-seek 30)
       (should (= emms-playing-time 132))
       (should (eq (hub/music-playback-state) 'seeking))
       (let ((snapshot (hub/emms-player--snapshot)))
	 (should (equal (plist-get snapshot :state) "Seeking"))
	 (should (= (plist-get snapshot :elapsed) 102)))
       (setq emms-playing-time 140)
       (should (= (plist-get (hub/emms-player--snapshot) :elapsed) 102))
       (hub/spotify-browser-player--event
	`((type . "ack") (run . ,(hub/spotify-browser-player--occurrence-id run)) (seq . ,old)))
       (should (hub/spotify-browser-player-seeking-p))
       (should (zerop notifications))
       (hub/spotify-browser-player-test--event run "state" '((position . 132000) (duration . 274000)))
       (should (= (plist-get (hub/emms-player--snapshot) :elapsed) 132))
       (hub/spotify-browser-player-test--event run "ack")
       (should-not (hub/spotify-browser-player-seeking-p))
       (should (eq (hub/music-playback-state) 'playing))
       (should (= notifications 1))
       (should (= (plist-get (hub/emms-player--snapshot) :elapsed) 132))))))

(ert-deftest hub/spotify-browser-player-seek-keeps-transport-icon-and-action ()
  "Both seek directions keep the real transport icon and action through SDK state and ACK."
  (require 'apps/emms-player)
  (require 'apps/music-console)
  (dolist (paused '(nil t))
    (hub/spotify-browser-player-test--queue
     (emms-start)
     (let ((run hub/spotify-browser-player--run))
       (hub/spotify-browser-player-test--event run "state" '((position . 102000) (duration . 274000)))
       (when paused
	 (emms-pause)
	 (hub/spotify-browser-player-test--event run "ack"))
       (cl-labels ((check-frame
		     (seeking)
		     (should (eq (and emms-player-paused-p t) paused))
		     (let ((snapshot (hub/emms-player--snapshot))
			   (label (hub/music-state-label
				   (if paused 'paused 'playing)
				   (when seeking "Seeking"))))
		       (with-temp-buffer
			 (cl-letf (((symbol-function 'hub/music-console--columns) (lambda () 140)))
			   (hub/music-console-render snapshot))
			 (should (string-match-p (regexp-quote label) (buffer-string)))
			 (should (string-match-p (if paused "SPC Resume" "SPC Pause") (buffer-string)))
			 (should-not (string-match-p "SPC Play" (buffer-string)))))))
	 (dolist (seconds '(30 -10))
	   (check-frame nil)
	   (emms-seek seconds)
	   (check-frame t)
	   (hub/spotify-browser-player-test--event run "state" '((position . 132000) (duration . 274000)))
	   (check-frame t)
	   (hub/spotify-browser-player-test--event run "ack")
	   (check-frame nil)))))))

(ert-deftest hub/spotify-browser-player-seeking-without-a-sample-stays-unknown ()
  "A very early seek never promotes an optimistic clock value to confirmed progress."
  (require 'apps/emms-player)
  (hub/spotify-browser-player-test--queue
   (let ((emms-playing-time 30))
     (emms-start)
     (emms-seek 30)
     (should (eq (hub/music-playback-state) 'seeking))
     (should-not (hub/spotify-browser-player-position))
     (should-not (plist-get (hub/emms-player--snapshot) :elapsed)))))

(ert-deftest hub/spotify-browser-player-pause-during-seek-keeps-pending-feedback ()
  "A later Pause does not hide an unconfirmed seek or allow its older ACK to clear it."
  (require 'apps/emms-player)
  (hub/spotify-browser-player-test--queue
   (emms-start)
   (let ((run hub/spotify-browser-player--run))
     (hub/spotify-browser-player-test--event run "state" '((position . 102000) (duration . 274000)))
     (emms-seek 30)
     (let ((seek-sequence (hub/spotify-browser-player--occurrence-seq run)))
       (emms-pause)
       (should (eq (hub/music-playback-state) 'seeking))
       (should (plist-get (hub/emms-player--snapshot) :paused))
       (hub/spotify-browser-player--event
	`((type . "ack") (run . ,(hub/spotify-browser-player--occurrence-id run)) (seq . ,seek-sequence)))
       (should (hub/spotify-browser-player-seeking-p))
       (hub/spotify-browser-player-test--event run "ack")
       (should (eq (hub/music-playback-state) 'paused)))
     (emms-seek -10)
     (should (eq (hub/music-playback-state) 'seeking))
     (emms-next)
     (hub/spotify-browser-player-test--event run "ack")
     (should-not (hub/spotify-browser-player-seeking-p))
     (should (eq emms-player-playing-p 'hub/spotify-browser-player-test--local-player)))))

(ert-deftest hub/spotify-browser-player-controls-invalidate-old-completion ()
  "Pending pause/seek and older sequence numbers cannot advance the playlist."
  (hub/spotify-browser-player-test--queue
   (emms-start)
   (let* ((run hub/spotify-browser-player--run) (old (hub/spotify-browser-player--occurrence-seq run)))
     (hub/spotify-browser-player-test--event run "state" '((position . 179500) (duration . 180000)))
     (emms-pause)
     (hub/spotify-browser-player--event `((type . "ended") (run . ,(hub/spotify-browser-player--occurrence-id run)) (seq . ,old)))
     (hub/spotify-browser-player--finish run)
     (should-not files)
     (emms-pause) (emms-seek -10)
     (should (cl-find "seek" commands :key #'car :test #'equal))
     (emms-next)
     (should (equal files '("/tmp/local.wav")))
     (hub/spotify-browser-player-test--event run "ended")
     (should (eq emms-player-playing-p 'hub/spotify-browser-player-test--local-player)))))

(ert-deftest hub/spotify-browser-player-unconfirmed-stop-blocks-local-start ()
  "No local audio starts if the browser cannot confirm stopping."
  (hub/spotify-browser-player-test--queue
   (emms-start)
   (cl-letf (((symbol-function 'hub/spotify-browser-stop) (lambda (_) (user-error "Stop unconfirmed"))))
     (should-error (emms-next) :type 'user-error))
   (should-not files)
   (should-not emms-player-playing-p)))

(ert-deftest hub/spotify-browser-player-time-boundary-and-loss-stop-here ()
  "Whole-second EMMS metadata and receiver loss never advance to local media."
  (hub/spotify-browser-player-test--queue
   (emms-start)
   (let* ((run hub/spotify-browser-player--run) seen
	  (emms-player-time-set-functions (list (lambda (seconds) (setq seen seconds)))))
     (hub/spotify-browser-player-test--event run "state" '((position . 12345) (duration . 194607)))
     (should (eq seen 12))
     (should (eq (emms-track-get (hub/spotify-browser-player--occurrence-track run) 'info-playing-time) 194))
     (hub/spotify-browser-player--lost)
     (should-not emms-player-playing-p)
     (should-not files))))

(ert-deftest hub/spotify-browser-engine-node-tests ()
  "Exercise the actual browser occurrence and cancellation state machine."
  (with-temp-buffer
    (should (zerop (process-file "node" nil (current-buffer) nil "--test" "test/personal-apps/spotify-browser.test.cjs")))))

(provide 'spotify-browser-player-test)
;;; spotify-browser-player-test.el ends here
