;;; music-test.el --- Music access and native palette acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Real Evil, Transient, EMMS and context-panels; no account or media processes.

;;; Code:
(require 'ert)
(require 'cl-lib)
(dolist (package '("emms" "evil" "transient" "compat" "cond-let"))
  (add-to-list 'load-path (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
							(expand-file-name "straight/build" default-directory)))))
(require 'evil)
(require 'emms)
(require 'emms-playlist-mode)
(require 'context-panels)
(require 'apps/music)

(defconst hub/music-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Stable checkout root for cold subprocesses, independent of earlier tests.")

(defun hub/music-test--cold (forms)
  "Evaluate FORMS in a fresh batch Emacs without loading media configuration."
  (let ((default-directory hub/music-test--root)
	(home (make-temp-file "music-cold-home-" t))
	(process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (setenv "HOME" home)
	  (with-temp-buffer
	    (let ((result (process-file
			   (expand-file-name invocation-name invocation-directory) nil (current-buffer) nil
			   "--batch" "-Q" "-L" "lisp" "-L" (file-name-directory (locate-library "evil"))
			   "-L" (file-name-directory (locate-library "transient"))
			   "-L" (file-name-directory (locate-library "compat"))
			   "-L" (file-name-directory (locate-library "cond-let"))
			   "-l" "test/test-helpers.el" "--eval"
			   (prin1-to-string `(progn (setq transient-save-history nil) ,forms)))))
	      (unless (zerop result) (ert-fail (buffer-string))))
	    (buffer-string)))
      (delete-directory home t))))

(ert-deftest hub/music-shortcuts-are-lazy-and-preserve-insert-typing ()
  "Music entry keys work before and after Evil without loading playback or OAuth."
  (dolist (order '(before after))
    (should
     (string-match-p
      "MUSIC-COLD-OK"
      (hub/music-test--cold
       `(progn
	  ,@(when (eq order 'after) '((require 'evil)))
	  (require 'apps/music)
	  ,@(when (eq order 'before) '((require 'evil)))
	  (dolist (binding '(("'ml" . hub/spotify-liked-songs) ("'mm" . hub/emms-player-open)
			     ("'mq" . hub/music-queue) ("'m/" . hub/music-search) ("'m?" . hub/music-palette)
			     ("'mc" . hub/emms-seek-backward-10) ("'mr" . hub/emms-seek-forward-30)))
	    (unless (eq (lookup-key evil-normal-state-map (kbd (car binding))) (cdr binding))
	      (error "Missing music binding: %s" (car binding))))
	  (when (eq (lookup-key evil-insert-state-map (kbd "'ml")) 'hub/spotify-liked-songs)
	    (error "Insert typing was rebound"))
	  (dolist (feature '(emms apps/spotify apps/spotify-auth apps/emms-spectrum apps/music-search apps/music-palette transient))
	    (when (featurep feature) (error "Unexpected eager dependency: %s" feature)))
	  (princ "MUSIC-COLD-OK")))))))

(ert-deftest hub/music-state-icons-always-retain-text-and-have-ascii-fallbacks ()
  "Playing, paused and related states are recognizable without relying on colour."
  (dolist (state '((playing . "Playing") (paused . "Paused") (seeking . "Seeking")
		   (stopped . "Stopped") (empty . "Empty") (ready . "Ready")
		   (unavailable . "Needs connection") (unknown . "Not checked")))
    (dolist (glyph '(nil t))
      (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) glyph)))
	(let ((label (substring-no-properties (hub/music-state-label (car state)))))
	  (should (string-match-p (cdr state) label))
	  (should-not (equal label (cdr state)))
	  (when (not glyph) (should (string-match-p "\\`[[:ascii:]]+\\'" label))))))))

(ert-deftest hub/music-liked-shortcut-refocuses-without-reloading-or-moving-row ()
  "The real normal-state shortcut reuses loaded Liked Songs and preserves point."
  (require 'apps/spotify)
  (let ((hub/spotify-buffer-name " *music liked access*") (fetches 0)
	(source (generate-new-buffer " *music access document*"))
	(context-panels--last-source-buffer nil)
	(saved-docks (frame-parameter nil 'context-panels-docked-views))
	(saved-closed (frame-parameter nil 'context-panels-bottom-closed)))
    (unwind-protect
	(save-window-excursion
	  (switch-to-buffer source) (evil-local-mode 1) (evil-normal-state)
	  (cl-letf (((symbol-function 'hub/spotify-refresh)
		     (lambda () (interactive) (cl-incf fetches)
		       (hub/spotify--received 0 200
					      '((total . 2) (items . (((track . ((name . "First") (duration_ms . 10000))))
								      ((track . ((name . "Second") (duration_ms . 20000))))))) nil)))
		    ((symbol-function 'auth-source-pass-get) (lambda (&rest _) (ert-fail "Unexpected credentials")))
		    ((symbol-function 'emms-player-start) (lambda (&rest _) (ert-fail "Unexpected playback"))))
	    (execute-kbd-macro (kbd "'ml"))
	    (should (derived-mode-p 'hub/spotify-library-mode))
	    (evil-local-mode 1) (evil-normal-state)
	    (should (eq (key-binding (kbd "'ml")) #'hub/spotify-liked-songs))
	    (goto-char (point-min)) (forward-line 1)
	    (let ((position (point)) (buffer (current-buffer)))
	      (execute-kbd-macro (kbd "'ml"))
	      (should (eq (current-buffer) buffer))
	      (should (= (point) position))
	      (should (= fetches 1)))))
      (dolist (buffer (list source (get-buffer hub/spotify-buffer-name)))
	(when (buffer-live-p buffer) (kill-buffer buffer)))
      (set-frame-parameter nil 'context-panels-docked-views saved-docks)
      (set-frame-parameter nil 'context-panels-bottom-closed saved-closed))))

(ert-deftest hub/music-player-local-l-opens-liked-songs ()
  "The player's normal-state l opens Liked Songs without changing global motions."
  (require 'apps/emms-player)
  (let ((global-l (lookup-key evil-normal-state-map (kbd "l")))
	;; Programmatic transient exits in other tests defer their map cleanup.
	(overriding-terminal-local-map nil)
	(calls 0))
    (save-window-excursion
      (with-temp-buffer
	(switch-to-buffer (current-buffer))
	(hub/emms-player-mode) (evil-local-mode 1) (evil-normal-state)
	(cl-letf (((symbol-function 'hub/spotify-liked-songs)
		   (lambda () (interactive) (cl-incf calls))))
	  (should (eq (key-binding (kbd "l")) #'hub/spotify-liked-songs))
	  (execute-kbd-macro (kbd "l"))
	  (should (= calls 1)))))
    (should (eq (lookup-key evil-normal-state-map (kbd "l")) global-l))))

(ert-deftest hub/music-queue-opens-existing-playlist-without-starting-or-clearing ()
  "Queue navigation uses the actual current EMMS playlist and retains its tracks."
  (let ((emms-playlist-buffer nil) (emms-playlist-buffers nil)
	(emms-track-initialize-functions nil) playlist)
    (unwind-protect
	(save-window-excursion
	  (setq playlist (emms-playlist-new " *music queue access*") emms-playlist-buffer playlist)
	  (with-current-buffer playlist
	    (emms-playlist-insert-track (emms-track 'file "/tmp/one.ogg"))
	    (goto-char (point-min)) (emms-playlist-select (point)))
	  (let ((before (with-current-buffer playlist (buffer-string))))
	    (cl-letf (((symbol-function 'emms-start) (lambda () (ert-fail "Unexpected playback"))))
	      (call-interactively #'hub/music-queue))
	    (should (eq (current-buffer) playlist))
	    (should (equal (buffer-string) before))))
      (when (buffer-live-p playlist) (kill-buffer playlist)))))

(ert-deftest hub/music-palette-cold-open-does-not-load-auth-playback-or-capture ()
  "The native menu can be opened and dismissed before any music package is loaded."
  (should (string-match-p
	   "MUSIC-PALETTE-COLD-OK"
	   (hub/music-test--cold
	    '(progn
	       (require 'apps/music)
	       (call-interactively #'hub/music-palette)
	       (transient--show)
	       (unless (string-match-p "Liked Songs" (with-current-buffer transient--buffer (buffer-string)))
		 (error "Palette not rendered"))
	       (unless (string-match-p "Search queue" (with-current-buffer transient--buffer (buffer-string)))
		 (error "Search missing from palette"))
	       (transient-quit-all)
	       (dolist (feature '(emms apps/spotify apps/spotify-auth apps/emms-spectrum apps/music-search))
		 (when (featurep feature) (error "Palette loaded media: %s" feature)))
	       (princ "MUSIC-PALETTE-COLD-OK"))))))

(ert-deftest hub/music-palette-uses-live-state-without-changing-playback ()
  "Menu state uses icon+text, updates transport descriptions and never plays on open."
  (require 'apps/music-palette)
  (let ((emms-player-playing-p 'synthetic-player) (emms-player-paused-p nil)
	(hub/spotify-auth--context nil)
	(source (generate-new-buffer " *music palette source*")))
    (unwind-protect
	(save-window-excursion
	  (switch-to-buffer source)
	  (cl-letf (((symbol-function 'hub/spotify-browser-ready-p) (lambda () nil))
		    ((symbol-function 'auth-source-pass-get) (lambda (&rest _) (ert-fail "Unexpected credentials")))
		    ((symbol-function 'hub/spotify-auth--request) (lambda (&rest _) (ert-fail "Unexpected network")))
		    ((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Unexpected process"))))
	    (call-interactively #'hub/music-palette)
	    (transient--show)
	    (let ((text (with-current-buffer transient--buffer (buffer-string))))
	      (should (string-match-p "Playing" text))
	      (should (string-match-p "Pause" text))
	      (should (string-match-p "Liked Songs" text))
	      (should (eq (current-buffer) source)))
	    (setq emms-player-paused-p t)
	    (should (string-match-p "Resume" (hub/music-palette--toggle-description)))
	    (should (string-match-p "Paused" (hub/music-palette--heading)))
	    (transient-quit-all)
	    (should emms-player-paused-p)))
      (when transient--prefix (transient-quit-all))
      (kill-buffer source))))

(ert-deftest hub/music-palette-keys-delegate-and-return-to-the-document ()
  "Real prefix and transient keys reuse transport actions without moving document point."
  (require 'apps/music-palette)
  (let ((source (generate-new-buffer " *music keyboard document*"))
	(emms-player-playing-p 'synthetic-player) (emms-player-paused-p nil) actions)
    (unwind-protect
	(save-window-excursion
	  (switch-to-buffer source) (insert "Document stays here.")
	  (evil-local-mode 1) (evil-normal-state) (goto-char 5)
	  (let ((position (point)))
	    (cl-letf (((symbol-function 'hub/emms-player-toggle)
		       (lambda () (interactive) (push 'toggle actions)
			 (setq emms-player-paused-p (not emms-player-paused-p))))
		      ((symbol-function 'hub/emms-seek-backward-10)
		       (lambda () (interactive) (push 'backward actions)))
		      ((symbol-function 'hub/emms-seek-forward-30)
		       (lambda () (interactive) (push 'forward actions))))
	      (execute-kbd-macro (kbd "'m?"))
	      (should (hub/music-palette--active-p))
	      (execute-kbd-macro (kbd "SPC c r"))
	      (should (equal (reverse actions) '(toggle backward forward)))
	      (should (hub/music-palette--active-p))
	      (hub/music-palette--refresh)
	      (should (string-match-p "Paused" (with-current-buffer transient--buffer (buffer-string))))
	      (execute-kbd-macro (kbd "q"))
	      (should-not transient--prefix)
	      (should-not hub/music-palette--timer)
	      (should (eq (current-buffer) source))
	      (should (= (point) position)))))
      (when transient--prefix (transient-quit-all))
      (kill-buffer source))))

(ert-deftest hub/music-palette-adapts-to-narrow-frames ()
  "A narrow palette stacks groups, and widening restores columns without closing it."
  (require 'apps/music-palette)
  (let ((width 60))
    (unwind-protect
	(save-window-excursion
	  (cl-letf (((symbol-function 'frame-width) (lambda (&optional _) width)))
	    (call-interactively #'hub/music-palette) (transient--show)
	    (should (with-current-buffer transient--buffer transient-force-single-column))
	    (setq width 130)
	    (hub/music-palette--refresh)
	    (should-not (with-current-buffer transient--buffer transient-force-single-column))
	    (should (hub/music-palette--active-p))))
      (when transient--prefix (transient-quit-all)))))

(ert-deftest hub/music-palette-pause-change-refreshes-during-pending-seek ()
  "Toggle availability refreshes even while the displayed Seeking state stays unchanged."
  (require 'apps/music-palette)
  (let ((hub/music-palette--last-state nil) (transient--buffer nil)
	(emms-player-playing-p t) (emms-player-paused-p nil) (refreshes 0))
    (cl-letf (((symbol-function 'hub/music-palette--active-p) (lambda () t))
	      ((symbol-function 'hub/music-playback-state) (lambda () 'seeking))
	      ((symbol-function 'transient--refresh-transient) (lambda () (cl-incf refreshes))))
      (hub/music-palette--refresh)
      (should (string-match-p "Pause" (hub/music-palette--toggle-description)))
      (setq emms-player-paused-p t)
      (hub/music-palette--refresh)
      (should (string-match-p "Resume" (hub/music-palette--toggle-description)))
      (should (= refreshes 2)))))

(ert-deftest hub/music-rendered-player-state-includes-icon-and-text ()
  "Native console playback states retain both their icon and readable text."
  (require 'apps/emms-player)
  (dolist (state '("Playing" "Paused" "Stopped"))
    (let* ((hub/emms-player--timer nil) (hub/emms-player-render-hook nil)
	   (emms-track-initialize-functions nil)
	   (track (emms-track 'file "/tmp/music-state.ogg")))
      (unwind-protect
	  (with-temp-buffer
	    (hub/emms-player-mode)
	    (cl-letf (((symbol-function 'hub/emms-player--snapshot)
		       (lambda () (list :state state :tracks (list track) :current track :index 0 :elapsed 0)))
		      ((symbol-function 'char-displayable-p) (lambda (_) t)))
	      (let ((inhibit-read-only t)) (hub/emms-player--render nil nil))
	      (should (string-match-p (regexp-quote (substring-no-properties
						     (hub/music-state-label (intern (downcase state))))) (buffer-string)))))
	(hub/emms-player--cancel-timer)))))

(provide 'music-test)
;;; music-test.el ends here
