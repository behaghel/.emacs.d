;;; music.el --- Lazy music entry points and state labels -*- lexical-binding: t; -*-

;;; Commentary:
;; Register the established normal-state music prefix without loading playback,
;; authorization or capture. UI actions delegate to the existing app commands.

;;; Code:
(require 'hub-utils)
(require 'subr-x)

(defgroup hub/music nil "Native music access and presentation." :group 'multimedia)
(defface hub/music-heading '((t :inherit variable-pitch :height 1.25 :weight bold))
  "Music palette heading, using the editor's existing variable-pitch family." :group 'hub/music)

(defvar evil-normal-state-map)
(defvar hub/emms-player-mode-map)
(defvar hub/spotify-library-mode-map)
(defvar emms-player-playing-p nil)
(defvar emms-player-paused-p nil)
(defvar emms-playlist-buffer nil)
(defvar emms-playlist-mode-center-when-go)
(defvar hub/spotify-auth--context nil)
(defvar hub/emms-spectrum--process nil)
(defvar hub/emms-player-buffer-name "*EMMS Player*")

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function which-key-add-key-based-replacements "which-key" (&rest replacements))
(declare-function emms-playlist-current-selected-track "emms" ())
(declare-function emms-playlist-new "emms" (&optional name))
(declare-function emms-playlist-set-playlist-buffer "emms" (&optional buffer))
(declare-function emms-playlist-mode-go "emms-playlist-mode" ())
(declare-function hub/spotify-browser-ready-p "apps/spotify-browser" ())
(declare-function hub/spotify-browser-player-waiting-p "apps/spotify-browser-player" ())
(declare-function hub/spotify-browser-player-seeking-p "apps/spotify-browser-player" ())
(declare-function hub/spotify-auth--session-p "apps/spotify-auth" (object))
(declare-function hub/spotify-auth--session-access-token "apps/spotify-auth" (context))
(declare-function hub/spotify-auth--session-expires-at "apps/spotify-auth" (context))

(autoload 'hub/music-palette "apps/music-palette" "Show the native music action palette." t)
(autoload 'hub/music-search "apps/music-search" "Search existing EMMS queue occurrences." t)
(autoload 'hub/spotify-liked-songs "apps/spotify" nil t)
(autoload 'hub/spotify-connect "apps/spotify-auth" nil t)
(autoload 'hub/spotify-browser-connect "apps/spotify-browser" nil t)
(autoload 'hub/emms-player-open "apps/emms-player" nil t)
(autoload 'hub/emms-player-toggle "apps/emms-player" nil t)
(autoload 'hub/emms-player-next "apps/emms-player" nil t)
(autoload 'hub/emms-player-previous "apps/emms-player" nil t)
(autoload 'hub/emms-seek-backward-10 "apps/emms-player" nil t)
(autoload 'hub/emms-seek-forward-30 "apps/emms-player" nil t)
(autoload 'hub/emms-spectrum-toggle "apps/emms-spectrum" nil t)

(defconst hub/music--bindings
  '(("'mm" hub/emms-player-open "Player")
    ("'ml" hub/spotify-liked-songs "Liked Songs")
    ("'mq" hub/music-queue "Queue")
    ("'m/" hub/music-search "Search queue")
    ("'m?" hub/music-palette "Music actions")
    ("'m SPC" hub/emms-player-toggle "Play/pause")
    ("'mp" hub/emms-player-toggle "Play/pause")
    ("'mc" hub/emms-seek-backward-10 "Seek -10 seconds")
    ("'mr" hub/emms-seek-forward-30 "Seek +30 seconds")
    ("'mt" hub/emms-player-next "Next")
    ("'ms" hub/emms-player-previous "Previous")
    ("'mv" hub/emms-spectrum-toggle "Spectrum"))
  "Normal-state music bindings and their discovery labels.")

(defun hub/music--install-keys ()
  "Install lazy music keys without changing insert-state typing or other prefixes."
  (dolist (binding hub/music--bindings)
    (define-key evil-normal-state-map (kbd (car binding)) (cadr binding))))

(defun hub/music--local-help ()
  "Install local navigation and palette access in loaded music views only."
  (dolist (variable '(hub/emms-player-mode-map hub/spotify-library-mode-map))
    (when (and (boundp variable) (keymapp (symbol-value variable)))
      (let ((map (symbol-value variable))
	    (bindings (append '(("?" . hub/music-palette))
			      (when (eq variable 'hub/emms-player-mode-map)
				'(("l" . hub/spotify-liked-songs))))))
	(dolist (binding bindings)
	  (define-key map (kbd (car binding)) (cdr binding))
	  (when (featurep 'evil)
	    (evil-define-key* '(normal motion) map (kbd (car binding)) (cdr binding))))))))

(with-eval-after-load 'evil
  (hub/music--install-keys)
  (hub/music--local-help))
(with-eval-after-load 'apps/emms-player (hub/music--local-help))
(with-eval-after-load 'apps/spotify (hub/music--local-help))
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "' m" "Music")
  (dolist (binding hub/music--bindings)
    (which-key-add-key-based-replacements (key-description (kbd (car binding))) (nth 2 binding))))

(defconst hub/music--states
  '((playing "▶" ">" "Playing" success)
    (connecting "○" "o" "Waiting for audio" warning)
    (paused "⏸" "||" "Paused" font-lock-constant-face)
    (seeking "↻" "~" "Seeking" warning)
    (stopped "■" "[]" "Stopped" shadow)
    (empty "○" "o" "Empty" shadow)
    (ready "●" "+" "Ready" success)
    (unavailable "⚠" "!" "Needs connection" warning)
    (unknown "?" "?" "Not checked" shadow))
  "State, monochrome icon, ASCII fallback, accessible label and theme face.")

(defun hub/music-state-label (state &optional label)
  "Return icon and readable text for STATE, optionally overriding LABEL.
Use an ASCII marker if the current font cannot display the icon."
  (let* ((entry (or (assq state hub/music--states) (assq 'unknown hub/music--states)))
	 (icon (if (char-displayable-p (string-to-char (nth 1 entry))) (nth 1 entry) (nth 2 entry)))
	 (face (nth 4 entry)))
    (propertize (concat icon " " (or label (nth 3 entry))) 'face face)))

(defun hub/music--selected-p ()
  "Return a selected track when EMMS is loaded; never load it just for display."
  (when (and (featurep 'emms) (buffer-live-p emms-playlist-buffer))
    ;; An empty playlist intentionally has no selected track.
    (condition-case nil (emms-playlist-current-selected-track) (error nil))))

(defun hub/music-playback-state ()
  "Return known playback state without starting or querying any backend."
  (cond ((and (featurep 'apps/spotify-browser-player)
	      (hub/spotify-browser-player-waiting-p)) 'connecting)
	((and (featurep 'apps/spotify-browser-player)
	      (hub/spotify-browser-player-seeking-p)) 'seeking)
	(emms-player-playing-p (if emms-player-paused-p 'paused 'playing))
	((hub/music--selected-p) 'stopped)
	(t 'empty)))

(defun hub/music--playing-p ()
  "Return non-nil when a live EMMS player can accept seek controls."
  (and emms-player-playing-p (not (eq (hub/music-playback-state) 'connecting)) t))

(defun hub/music--transport-available-p ()
  "Return non-nil when playback can start or navigate an existing selection."
  (or (hub/music--playing-p) (hub/music--selected-p)))

(defun hub/music-audio-ready-p ()
  "Return known Spotify audio readiness without loading its transport."
  (and (featurep 'apps/spotify-browser) (hub/spotify-browser-ready-p)))

(defun hub/music-library-authorized-p ()
  "Return known unexpired in-memory library authorization, without reading pass."
  (and (featurep 'apps/spotify-auth)
       (hub/spotify-auth--session-p hub/spotify-auth--context)
       (hub/spotify-auth--session-access-token hub/spotify-auth--context)
       (> (or (hub/spotify-auth--session-expires-at hub/spotify-auth--context) 0) (float-time))))

(defun hub/music--spectrum-on-p ()
  "Return non-nil for a known live capture process, without loading capture code."
  (and (processp hub/emms-spectrum--process) (process-live-p hub/emms-spectrum--process)))

(defun hub/music--spectrum-available-p ()
  "Return non-nil when explicit capture can act on a visible graphical player."
  (or (hub/music--spectrum-on-p)
      (and (display-graphic-p) (image-type-available-p 'svg)
	   (get-buffer-window hub/emms-player-buffer-name (selected-frame)))))

;;;###autoload
(defun hub/music-queue ()
  "Focus the current editable EMMS playlist without playing or clearing tracks."
  (interactive)
  (require 'emms-playlist-mode)
  (unless (buffer-live-p emms-playlist-buffer)
    (emms-playlist-set-playlist-buffer (emms-playlist-new)))
  (let ((emms-playlist-mode-center-when-go nil))
    (emms-playlist-mode-go)))

(provide 'apps/music)
;;; music.el ends here
