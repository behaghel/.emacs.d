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
	  (hub/spotify-browser-player--run nil) (serial 0) commands stops files
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
