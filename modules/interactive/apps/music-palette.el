;;; music-palette.el --- Native, state-aware music action palette -*- lexical-binding: t; -*-

;;; Commentary:
;; Presentation and existing commands only. Merely opening or refreshing this
;; palette never reads credentials, contacts a service or starts media/capture.

;;; Code:
(require 'hub-utils)
(require 'apps/music)
(require 'transient)
(require 'face-remap)

(defvar hub/music-palette--timer nil "Read-only state refresh while this palette is active.")
(defvar hub/music-palette--last-state nil "Last displayed, non-sensitive availability state.")

(defun hub/music-palette--heading ()
  "Return the music heading and icon-labelled playback state."
  (concat (propertize "Music" 'face 'hub/music-heading) "   "
	  (hub/music-state-label (hub/music-playback-state)) "\n"
	  (hub/music-state-label (if (hub/music-audio-ready-p) 'ready 'unavailable)
				 (if (hub/music-audio-ready-p) "Spotify audio ready" "Spotify audio not connected"))
	  "   ·   "
	  (hub/music-state-label (if (hub/music-library-authorized-p) 'ready 'unknown)
				 (if (hub/music-library-authorized-p) "Library authorized" "Library not checked"))))

(defun hub/music-palette--toggle-description ()
  "Describe the actual action of the established play/pause command."
  (cond ((eq (hub/music-playback-state) 'connecting) (hub/music-state-label 'stopped "Cancel request"))
	(emms-player-paused-p (hub/music-state-label 'playing "Resume"))
	(emms-player-playing-p (hub/music-state-label 'paused "Pause"))
	(t (hub/music-state-label 'playing "Play"))))

(defun hub/music-palette--audio-description ()
  "Describe audio readiness without making a network request."
  (if (hub/music-audio-ready-p) (hub/music-state-label 'ready "Spotify audio ready")
    (hub/music-state-label 'unavailable "Connect Spotify audio")))

(defun hub/music-palette--library-description ()
  "Describe known library authorization without reading saved credentials."
  (if (hub/music-library-authorized-p) (hub/music-state-label 'ready "Library authorized")
    (hub/music-state-label 'unknown "Authorize library")))

(defun hub/music-palette--spectrum-description ()
  "Describe explicit capture state and availability without enabling it."
  (if (hub/music--spectrum-on-p) (hub/music-state-label 'ready "Spectrum on")
    (hub/music-state-label 'empty (if (hub/music--spectrum-available-p) "Spectrum off" "Spectrum: open player"))))

(defun hub/music-palette--active-p ()
  "Return non-nil only while the current transient is the music palette."
  (and transient--prefix (eq (oref transient--prefix command) 'hub/music-palette)))

(defun hub/music-palette--cleanup ()
  "Cancel only this palette's refresh timer when it leaves the screen."
  (when (timerp hub/music-palette--timer) (cancel-timer hub/music-palette--timer))
  (setq hub/music-palette--timer nil hub/music-palette--last-state nil))

(defun hub/music-palette--refresh ()
  "Refresh changed availability without moving focus or calling a backend."
  (if (not (hub/music-palette--active-p)) (hub/music-palette--cleanup)
    (let ((state (list (hub/music-playback-state) emms-player-paused-p
		       (and (hub/music--transport-available-p) t)
		       (hub/music-audio-ready-p) (hub/music-library-authorized-p)
		       (hub/music--spectrum-on-p) (and (hub/music--spectrum-available-p) t)
		       (frame-width))))
      (unless (equal state hub/music-palette--last-state)
	(setq hub/music-palette--last-state state)
	(when (buffer-live-p transient--buffer)
	  (with-current-buffer transient--buffer
	    (setq-local transient-force-single-column (< (frame-width) 70))))
	(transient--refresh-transient)))))

(defun hub/music-palette--style ()
  "Apply music-local spacing, leaving other transient menus and global fonts alone."
  (when (hub/music-palette--active-p)
    (setq-local line-spacing 0.12
		transient-force-single-column (< (frame-width) 70))
    (face-remap-add-relative 'default :height 0.85)))

(add-hook 'transient-setup-buffer-hook #'hub/music-palette--style)
(add-hook 'transient-exit-hook #'hub/music-palette--cleanup)

;; Transient inspects interactive forms while constructing its menu and may load
;; autoloaded commands. These explicit relays keep inspection free of media/auth
;; dependencies; only choosing an action invokes the established target command.
(transient-define-suffix hub/music-palette--liked ()
			 "Open the existing Liked Songs view."
			 (interactive) (call-interactively #'hub/spotify-liked-songs))
(transient-define-suffix hub/music-palette--player ()
			 "Focus the existing docked player."
			 (interactive) (call-interactively #'hub/emms-player-open))
(transient-define-suffix hub/music-palette--toggle ()
			 "Invoke the established play/pause action."
			 (interactive) (call-interactively #'hub/emms-player-toggle))
(transient-define-suffix hub/music-palette--backward ()
			 "Invoke the established backward seek."
			 (interactive) (call-interactively #'hub/emms-seek-backward-10))
(transient-define-suffix hub/music-palette--forward ()
			 "Invoke the established forward seek."
			 (interactive) (call-interactively #'hub/emms-seek-forward-30))
(transient-define-suffix hub/music-palette--previous ()
			 "Invoke the established previous-track action."
			 (interactive) (call-interactively #'hub/emms-player-previous))
(transient-define-suffix hub/music-palette--next ()
			 "Invoke the established next-track action."
			 (interactive) (call-interactively #'hub/emms-player-next))
(transient-define-suffix hub/music-palette--spectrum ()
			 "Explicitly invoke the existing spectrum toggle."
			 (interactive) (call-interactively #'hub/emms-spectrum-toggle))
(transient-define-suffix hub/music-palette--connect-audio ()
			 "Explicitly connect the existing Spotify audio receiver."
			 (interactive) (call-interactively #'hub/spotify-browser-connect))
(transient-define-suffix hub/music-palette--connect-library ()
			 "Explicitly authorize the separate read-only library."
			 (interactive) (call-interactively #'hub/spotify-connect))

;;;###autoload
(transient-define-prefix hub/music-palette ()
			 "Show music actions with honest state, availability and icon-plus-text labels."
			 :refresh-suffixes t
			 [:description hub/music-palette--heading
				       ["GO TO"
					("l" "Liked Songs  'ml" hub/music-palette--liked)
					("Q" "Edit queue   'mq" hub/music-queue)
					("m" "Focus player 'mm" hub/music-palette--player)]
				       ["TRANSPORT"
					("SPC" hub/music-palette--toggle :description hub/music-palette--toggle-description
					 :transient t :inapt-if-not hub/music--transport-available-p)
					("c" "−10s" hub/music-palette--backward :transient t :inapt-if-not hub/music--playing-p)
					("r" "+30s" hub/music-palette--forward :transient t :inapt-if-not hub/music--playing-p)
					("s" "Previous" hub/music-palette--previous :transient t :inapt-if-not hub/music--transport-available-p)
					("t" "Next" hub/music-palette--next :transient t :inapt-if-not hub/music--transport-available-p)]
				       ["CONNECTION / VIEW"
					("v" hub/music-palette--spectrum :description hub/music-palette--spectrum-description
					 :inapt-if-not hub/music--spectrum-available-p)
					("b" hub/music-palette--connect-audio :description hub/music-palette--audio-description :inapt-if hub/music-audio-ready-p)
					("L" hub/music-palette--connect-library :description hub/music-palette--library-description :inapt-if hub/music-library-authorized-p)]]
			 [:class transient-row
				 ("q" "Back to editor" transient-quit-one)
				 ("<escape>" "Back" transient-quit-one)]
			 (interactive)
			 (hub/music-palette--cleanup)
			 (transient-setup 'hub/music-palette)
			 (setq hub/music-palette--timer (run-at-time 0.5 0.5 #'hub/music-palette--refresh)))

(provide 'apps/music-palette)
;;; music-palette.el ends here
