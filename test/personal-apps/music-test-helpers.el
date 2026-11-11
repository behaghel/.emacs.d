;;; music-test-helpers.el --- Inert music source fixtures -*- lexical-binding: t; -*-

;;; Commentary:
;; Reusable real EMMS occurrences and synthetic retained library metadata.

;;; Code:
(require 'ert)
(require 'cl-lib)
(dolist (package '("emms" "consult" "compat"))
  (add-to-list 'load-path (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
							(expand-file-name "straight/build" default-directory)))))
(require 'emms)
(require 'emms-playlist-mode)
(autoload 'hub/music-search "apps/music-search" nil t)
(defvar hub/music-search-test--player nil)
(defvar hub/spotify-buffer-name)
(defvar hub/spotify--tracks)
(defvar hub/spotify--generation)
(defvar hub/spotify--total)
(defvar hub/music-search--operation)

(defmacro hub/music-search-test--queue (&rest body)
  "Run BODY with duplicate track objects in a real mixed EMMS playlist."
  (declare (indent 0))
  `(let* ((hub/spotify-buffer-name " *music search absent library*")
	  (emms-playlist-buffer nil) (emms-playlist-buffers nil)
	  (emms-track-initialize-functions nil) (emms-cache-get-function nil)
	  (emms-cache-set-function nil) (emms-cache-modified-function nil)
	  (emms-player-playing-p nil) (emms-player-paused-p nil)
	  (emms-player-started-hook nil) (emms-player-stopped-hook nil)
	  (emms-player-finished-hook nil) (emms-player-paused-hook nil)
	  (playlist (emms-playlist-new " *music search queue*"))
	  (document (generate-new-buffer " *music search document*"))
	  (duplicate (emms-track 'url "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
	  positions started (stopped 0)
	  (hub/music-search-test--player
	   (emms-player
	    (lambda (track)
	      (push (list track (with-current-buffer playlist
				  (marker-position emms-playlist-selected-marker))) started)
	      (emms-player-started 'hub/music-search-test--player))
	    (lambda () (cl-incf stopped) (emms-player-stopped))
	    (lambda (_) t)))
	  (emms-player-list '(hub/music-search-test--player)))
     (setq emms-playlist-buffer playlist)
     (emms-track-set duplicate 'info-title "Low Light")
     (emms-track-set duplicate 'info-artist "Quiet Geometry")
     (emms-track-set duplicate 'info-album "Still / Motion")
     (emms-track-set duplicate 'info-playing-time 274)
     (unwind-protect
	 (save-window-excursion
	   (with-current-buffer playlist
	     (dolist (track (list duplicate (emms-track 'file "/tmp/local.ogg") duplicate))
	       (push (point) positions)
	       (emms-playlist-insert-track track))
	     (setq positions (nreverse positions))
	     (emms-playlist-select (car positions)))
	   (switch-to-buffer document)
	   (insert "An unchanged writing buffer.") (goto-char 4)
	   (cl-letf (((symbol-function 'auth-source-pass-get)
		      (lambda (&rest _) (ert-fail "Search read credentials")))
		     ((symbol-function 'url-retrieve)
		      (lambda (&rest _) (ert-fail "Search fetched metadata")))
		     ((symbol-function 'make-process)
		      (lambda (&rest _) (ert-fail "Search started a process"))))
	     ,@body))
       (when emms-player-playing-p (emms-stop))
       (dolist (buffer (list playlist document))
	 (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defmacro hub/music-search-test--library (&rest body)
  "Run BODY with retained metadata, not an authorized or fetched library."
  (declare (indent 0))
  `(let* ((hub/spotify-buffer-name " *music search loaded library*")
	  (library (generate-new-buffer hub/spotify-buffer-name))
	  (raw '((uri . "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
		 (name . "Retained song") (artists . (((name . "Library artist"))))
		 (album . ((name . "Retained album"))) (duration_ms . 181000)))
	  added)
     (unwind-protect
	 (progn
	   (with-current-buffer library
	     (setq major-mode 'hub/spotify-library-mode)
	     (setq-local hub/spotify--tracks (list (cons 0 raw))
			 hub/spotify--generation 7 hub/spotify--total 1482))
	   (cl-letf (((symbol-function 'hub/spotify-player-add)
		      (lambda (track &optional play) (push (list track play) added))))
	     ,@body))
       (when (buffer-live-p library) (kill-buffer library)))))

(defun hub/music-test--view-text ()
  "Read the current view's native body, header and optional fixed footer."
  (concat (if (stringp header-line-format) header-line-format "")
	  (buffer-string)
	  (when (fboundp 'context-panels-footer-window)
	    (when-let* ((window (context-panels-footer-window (current-buffer))))
	      (with-current-buffer (window-buffer window) (buffer-string))))))

(provide 'music-test-helpers)
;;; music-test-helpers.el ends here
