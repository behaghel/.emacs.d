;;; emms-local.el --- Idempotent local EMMS backend registration -*- lexical-binding: t; -*-

;;; Commentary:
;; Define the personal local-player preference independently of feed and Spotify
;; UIs.  Registration loads backend definitions, never starts players or probes.

;;; Code:
(require 'hub-utils)

(defvar emms-player-list)

(defconst hub/emms-local-players
  '(emms-player-vlc-playlist emms-player-vlc emms-player-mplayer
			     emms-player-mpg321 emms-player-ogg123)
  "Configured local media preference, appended without replacing existing players.")

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
    (add-to-list 'emms-player-list player t)))

(provide 'apps/emms-local)
;;; emms-local.el ends here
