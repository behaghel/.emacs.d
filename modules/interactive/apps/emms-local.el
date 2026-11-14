;;; emms-local.el --- Idempotent local EMMS backend registration -*- lexical-binding: t; -*-

;;; Commentary:
;; Define the personal local-player preference independently of feed and Spotify
;; UIs.  Registration loads backend definitions, never starts players or probes.

;;; Code:
(require 'hub-utils)
(require 'subr-x)

(defvar emms-player-list)
(defvar emms-playlist-buffer)
(defvar emms-playing-time-mode)
(defvar emms-playing-time-display-mode)
(declare-function emms-track-type "emms" (track))
(declare-function emms-track-name "emms" (track))
(declare-function emms-track-get "emms" (track name))
(declare-function emms-track-set "emms" (track name value))
(declare-function emms-playlist-current-selected-track "emms" ())
(declare-function emms-playing-time-mode "emms-playing-time" (&optional arg))

(defconst hub/emms-local-players
  '(emms-player-vlc-playlist emms-player-vlc emms-player-mplayer
			     emms-player-mpg321 emms-player-ogg123)
  "Configured local media preference, appended without replacing existing players.")

(defun hub/emms-local--read-duration (track)
  "Fill TRACK's missing whole-second WAV duration on explicit playback.
Only regular, absolute local WAV/WAVE paths reach ffprobe.  Do not overwrite
known duration or rewrite playlist text.  Probe failures must not stop audio."
  (let ((file (emms-track-name track))
	(known (emms-track-get track 'info-playing-time))
	(case-fold-search t))
    (when (and (eq (emms-track-type track) 'file) (stringp file)
	       (not (file-remote-p file)) (file-name-absolute-p file)
	       (string-match-p "\\.wave?\\'" file)
	       (not (and (numberp known) (> known 0))) (file-regular-p file))
      (if-let* ((program (executable-find "ffprobe")))
	  (condition-case err
	      (with-temp-buffer
		(let ((default-directory "/") (coding-system-for-read 'utf-8-unix))
		  (when (eq 0 (call-process program nil (list (current-buffer) t) nil
					    "-v" "error" "-protocol_whitelist" "file" "-f" "wav"
					    "-show_entries" "format=duration" "-of"
					    "default=noprint_wrappers=1:nokey=1" "-i" file))
		    (let ((output (string-trim (buffer-string))))
		      (when (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" output)
			(let ((seconds (string-to-number output)))
			  (when (and (>= seconds 1) (< seconds most-positive-fixnum)
				     (equal file (emms-track-name track)))
			    (emms-track-set track 'info-playing-time (floor seconds)))))))))
	    (error (message "Local WAV duration unavailable: %s" (error-message-string err))))
	(message "Local WAV duration unavailable: ffprobe is not on exec-path")))))

(defun hub/emms-local--track-started ()
  "Read missing local WAV duration for the explicitly started EMMS selection."
  (when-let* ((track (and (buffer-live-p emms-playlist-buffer)
			  (condition-case nil (emms-playlist-current-selected-track) (error nil)))))
    (hub/emms-local--read-duration track)))

(defun hub/emms-local--enable-timing ()
  "Prepare EMMS's native clock without resetting active time or mode-line choices."
  (require 'emms-playing-time)
  (unless emms-playing-time-mode
    (let ((global-mode-string global-mode-string)
	  (emms-playing-time-display-mode emms-playing-time-display-mode))
      (emms-playing-time-mode 1)))
  (add-hook 'emms-player-started-hook #'hub/emms-local--track-started))

;;;###autoload
(defun hub/emms-register-local-players ()
  "Register configured local backends without losing Spotify or custom players.
Keep existing ordering, append missing local entries in preference order, and
never start playback, fetch metadata or change queue/transport state."
  ;; Keep requires inside the function: the helper must finish loading before
  ;; EMMS's after-load configuration can call it again during lazy activation.
  (require 'emms)
  (require 'emms-player-vlc)
  (require 'emms-player-mplayer)
  (dolist (player hub/emms-local-players)
    (add-to-list 'emms-player-list player t))
  (hub/emms-local--enable-timing))

(provide 'apps/emms-local)
;;; emms-local.el ends here
