;;; emms-local-test.el --- Local backend registration regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real EMMS dispatch without starting media players or reading credentials.

;;; Code:
(require 'ert)
(require 'cl-lib)
(dolist (package '("emms" "websocket" "compat"))
  (add-to-list 'load-path
	       (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
					     (expand-file-name "straight/build" default-directory)))))
(require 'apps/spotify-player)
(require 'apps/spotify-browser-player)

(defmacro hub/emms-local-test--passive (&rest body)
  "Run BODY while forbidding playback, media probes, network and credentials."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'executable-find) (lambda (name) (and (equal name "vlc") "/synthetic/vlc")))
	     ((symbol-function 'start-process) (lambda (&rest _) (ert-fail "Started a player or probe")))
	     ((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Started a process")))
	     ((symbol-function 'auth-source-pass-get) (lambda (&rest _) (ert-fail "Read credentials")))
	     ((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Contacted the network"))))
     ,@body))

(ert-deftest hub/emms-local-spotify-only-registration-recovers-wav-dispatch ()
  "A Spotify-only runtime gains its configured VLC backend without losing Spotify."
  (let ((emms-player-list '(emms-player-spotify-browser))
	(emms-track-initialize-functions nil) (emms-cache-get-function nil)
	(emms-cache-set-function nil))
    (hub/emms-local-test--passive
     (hub/spotify-player-register)
     (should (eq (emms-player-for (emms-track 'file "/tmp/recording.wav")) 'emms-player-vlc))
     (should (eq (emms-player-for (emms-track 'url "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
		 'emms-player-spotify-browser)))))

(ert-deftest hub/emms-local-registration-order-is-idempotent-and-preserves-existing-players ()
  "Local-first and Spotify-first initialisation preserve dispatch and prior entries."
  (require 'apps/emms-local)
  (dolist (order '(local-first spotify-first))
    (let ((emms-player-list '(existing-custom-player)))
      (hub/emms-local-test--passive
       (if (eq order 'local-first)
	   (progn (hub/emms-register-local-players) (hub/spotify-player-register))
	 (hub/spotify-player-register) (hub/emms-register-local-players))
       (let ((before (copy-sequence emms-player-list)))
	 (dotimes (_ 3) (hub/emms-register-local-players) (hub/spotify-player-register))
	 (should (equal before emms-player-list))
	 (should (memq 'existing-custom-player emms-player-list))
	 (should (eq (car emms-player-list) 'emms-player-spotify-browser))
	 (should (< (cl-position 'emms-player-vlc emms-player-list)
		    (cl-position 'emms-player-mplayer emms-player-list))))))))

(ert-deftest hub/emms-local-registration-preserves-queue-and-transport-state ()
  "Registration cannot rewrite a queue, move selection, or stop the active player."
  (require 'apps/emms-local)
  (let* ((emms-player-list '(emms-player-spotify-browser))
	 (emms-playlist-buffer nil) (emms-playlist-buffers nil)
	 (emms-player-playing-p nil) (emms-player-paused-p t)
	 (emms-track-initialize-functions nil) (emms-cache-get-function nil)
	 (emms-cache-set-function nil)
	 (playlist (emms-playlist-new " *local registration queue*"))
	 (track (emms-track 'file "/tmp/recording.wav")))
    (unwind-protect
	(with-current-buffer playlist
	  (setq emms-playlist-buffer playlist)
	  (emms-playlist-insert-track track)
	  (emms-playlist-insert-track track)
	  (goto-char (point-min))
	  (emms-playlist-select (point))
	  (forward-line 1)
	  (let ((before (buffer-string)) (revision (buffer-chars-modified-tick))
		(where (point)) (selected (marker-position emms-playlist-selected-marker)))
	    (hub/emms-local-test--passive
	     (cl-letf (((symbol-function 'emms-start) (lambda (&rest _) (ert-fail "Started transport")))
		       ((symbol-function 'emms-stop) (lambda (&rest _) (ert-fail "Stopped transport"))))
	       (hub/emms-register-local-players)
	       (hub/spotify-player-register)))
	    (should (equal-including-properties before (buffer-string)))
	    (should (= revision (buffer-chars-modified-tick)))
	    (should (= where (point)))
	    (should (= selected (marker-position emms-playlist-selected-marker)))
	    (should (eq emms-playlist-buffer playlist))
	    (should-not emms-player-playing-p)
	    (should emms-player-paused-p)))
      (when (buffer-live-p playlist) (kill-buffer playlist)))))

(provide 'emms-local-test)
;;; emms-local-test.el ends here
