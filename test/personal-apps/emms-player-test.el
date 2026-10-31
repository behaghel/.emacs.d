;;; emms-player-test.el --- Docked EMMS player scenarios -*- lexical-binding: t; -*-

;;; Commentary:
;; Executable acceptance scenarios for spec/personal-apps/apps/stories/emms-player.
;; Real EMMS/context-panels/Evil code, but no external playback processes.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst hub/emms-player-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root for the player scenarios.")

;; The generic ERT runner exposes only a small subset of Straight's build paths.
(dolist (package '("emms" "evil"))
  (let ((directory (expand-file-name
		    package
		    (or (getenv "STRAIGHT_BUILD_PATH")
			(expand-file-name "straight/build" hub/emms-player-test--root)))))
    (when (file-directory-p directory) (add-to-list 'load-path directory))))

(require 'emms)
(require 'emms-playlist-mode)
(require 'context-panels)
(require 'evil)
(require 'apps/emms-player)


(defmacro hub/emms-player-test--with-playlist (&rest body)
  "Run BODY with an isolated two-track playlist, windows, and dock history."
  (declare (indent 0) (debug t))
  `(let* ((emms-playlist-buffer nil)
	  (emms-playlist-buffers nil)
	  (emms-track-initialize-functions nil)
	  (emms-player-playing-p t)
	  (emms-player-paused-p nil)
	  (emms-playing-time 60)
	  (hub/emms-player--timer nil)
	  (hub/emms-player-buffer-name " *EMMS player test*")
	  (source (generate-new-buffer " *emms-player-source*"))
	  (other (generate-new-buffer " *emms-player-other*"))
	  (saved-docks (frame-parameter nil 'context-panels-docked-views))
	  (saved-closed (frame-parameter nil 'context-panels-bottom-closed))
	  (context-panels--last-source-buffer nil)
	  (context-panels--desired-side-panel-p nil)
	  (context-panels--desired-bottom-view-id nil)
	  first second playlist)
     (unwind-protect
	 (save-window-excursion
	   (set-frame-parameter nil 'context-panels-docked-views nil)
	   (set-frame-parameter nil 'context-panels-bottom-closed nil)
	   (delete-other-windows)
	   (setq playlist (emms-playlist-new " *emms-player-queue*")
		 emms-playlist-buffer playlist)
	   (setq first (emms-track 'file "/tmp/first.mp3")
		 second (emms-track 'file "/tmp/next.ogg"))
	   (emms-track-set first 'info-title "First song")
	   (emms-track-set first 'info-playing-time 180)
	   (emms-track-set second 'info-title "Next song")
	   (emms-track-set second 'info-playing-time 240)
	   (with-current-buffer playlist
	     (let ((inhibit-read-only t))
	       (emms-playlist-insert-track first)
	       (emms-playlist-insert-track second)
	       (goto-char (point-min))
	       (emms-playlist-select (point))))
	   (switch-to-buffer source)
	   (text-mode)
	   (cl-letf (((symbol-function 'emms-player-start)
		      (lambda (&rest _) (ert-fail "Unexpected external playback"))))
	     ,@body))
       (hub/emms-player--cancel-timer)
       (dolist (buffer (list source other playlist (get-buffer hub/emms-player-buffer-name)))
	 (when (buffer-live-p buffer) (kill-buffer buffer)))
       (set-frame-parameter nil 'context-panels-docked-views saved-docks)
       (set-frame-parameter nil 'context-panels-bottom-closed saved-closed))))

(ert-deftest hub/emms-player-opens-docked-with-progress-and-next-track ()
  "AC-1: Opening focuses the shared slot without modifying the playback queue."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (let ((original (with-current-buffer playlist (buffer-string))))
     (hub/emms-player-open)
     (should (derived-mode-p 'hub/emms-player-mode))
     (should (eq (window-parameter (selected-window) 'window-side) 'bottom))
     (should (string-match-p "Playing — First song" (buffer-string)))
     (should (string-match-p "1:00 / 3:00" (buffer-string)))
     (should (string-match-p "Playlist: 7:00" (buffer-string)))
     (should (string-match-p "Next song" (buffer-string)))
     (should (string-match-p (regexp-quote "[==========--------------------]")
			     (buffer-string)))
     (should (text-property-any (point-min) (point-max) 'face 'highlight))
     (should (equal original (with-current-buffer playlist (buffer-string)))))))

(ert-deftest hub/emms-player-normal-state-controls-match-global-prefix ()
  "AC-2: Local normal-state keys and prefixed global keys invoke the same actions."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (hub/emms-player-open)
   (evil-local-mode 1)
   (evil-normal-state)
   (let (actions)
     (cl-letf (((symbol-function 'emms-pause) (lambda () (push 'toggle actions)))
	       ((symbol-function 'emms-seek) (lambda (seconds) (push seconds actions)))
	       ((symbol-function 'emms-next) (lambda () (push 'next actions)))
	       ((symbol-function 'emms-previous) (lambda () (push 'previous actions))))
       (dolist (keys '("SPC" "c" "r" "t" "s"))
	 (call-interactively (key-binding (kbd keys))))
       (should (equal (reverse actions) '(toggle -10 30 next previous)))
       (setq actions nil)
       (select-window (get-buffer-window source))
       (evil-local-mode 1)
       (evil-normal-state)
       (dolist (keys '("'m SPC" "'mc" "'mr" "'mt" "'ms"))
	 (call-interactively (key-binding (kbd keys))))
       (should (equal (reverse actions) '(toggle -10 30 next previous)))
       (should (eq (key-binding (kbd "'mm")) #'hub/emms-player-open))))))

(ert-deftest hub/emms-player-shares-slot-and-close-does-not-stop-playback ()
  "AC-3: A source view replaces the player; fallback and dismissal leave playback alone."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (hub/emms-player-open)
   (select-window (get-buffer-window source))
   (context-panels-register-provider
    (list :name 'test-inspector :icon "test"
	  :collect-bottom-views
	  (lambda (_source)
	    (list (list :id 'inspect :buffer-name " *emms-player-inspect*"
			:render (lambda (_source _view) (insert "Inspect")))))))
   (unwind-protect
       (progn
	 (context-panels-open-bottom-view 'inspect source)
	 (hub/emms-player--refresh)
	 (should (get-buffer-window " *emms-player-inspect*"))
	 (should-not (get-buffer-window hub/emms-player-buffer-name))
	 (switch-to-buffer other)
	 (context-panels-reconcile-windows)
	 (should (get-buffer-window hub/emms-player-buffer-name))
	 (select-window (get-buffer-window hub/emms-player-buffer-name))
	 (hub/emms-player-close)
	 (hub/emms-player--refresh)
	 (context-panels-reconcile-windows)
	 (should-not (get-buffer-window hub/emms-player-buffer-name))
	 (should emms-player-playing-p)
	 (should-not hub/emms-player--timer))
     (when (get-buffer " *emms-player-inspect*")
       (kill-buffer " *emms-player-inspect*")))))

(ert-deftest hub/emms-player-empty-playlist-does-not-start-a-player ()
  "AC-4: Empty playlists render a useful stopped state without playback."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (let ((emms-playlist-buffer nil)
	 (emms-player-playing-p nil))
     (hub/emms-player-open)
     (should (string-match-p "Stopped — No track" (buffer-string)))
     (should (string-match-p "Playlist: 0:00" (buffer-string)))
     (should (string-match-p "No tracks. Use M-x emms-play-file" (buffer-string))))))

(ert-deftest hub/emms-player-unknown-duration-is-not-counted-as-zero ()
  "AC-5: Unknown metadata does not turn into misleading zero durations."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (emms-track-set first 'info-playing-time nil)
   (hub/emms-player-open)
   (should (string-match-p "1:00 / --:-- (unknown)" (buffer-string)))
   (should (string-match-p (regexp-quote "Playlist: 4:00 + 1 unknown") (buffer-string)))))

(ert-deftest hub/emms-player-progress-refresh-preserves-focus-and-source-point ()
  "AC-6: Progress ticks update only the visible player and preserve editing position."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (hub/emms-player-open)
   (select-window (get-buffer-window source))
   (insert "Editing here")
   (let ((selected (selected-window)) (position (point)))
     (setq emms-playing-time 90)
     (hub/emms-player--refresh)
     (should (eq selected (selected-window)))
     (should (= position (point)))
     (with-current-buffer hub/emms-player-buffer-name
       (should (string-match-p "1:30 / 3:00" (buffer-string)))))))

(ert-deftest hub/emms-player-requires-dock-capability-with-actionable-error ()
  "An older framework fails clearly instead of quietly dropping docking semantics."
  (cl-letf (((symbol-function 'context-panels-open-docked-view) nil))
    (condition-case err
	(progn (hub/emms-player-open) (ert-fail "Expected capability error"))
      (user-error
       (should (string-match-p "CONTEXT_PANELS_PATH" (error-message-string err)))))))

(ert-deftest hub/emms-player-repeated-track-follows-selected-occurrence ()
  "Repeated songs show the selected occurrence rather than the first matching title."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (with-current-buffer playlist
     (let ((inhibit-read-only t))
       (goto-char (point-max))
       (let ((selected (point)))
	 (emms-playlist-insert-track first)
	 (emms-playlist-insert-track second)
	 (emms-playlist-select selected))))
   (hub/emms-player-open)
   (should (string-match-p "▶ 3. First song" (buffer-string)))
   (should (string-match-p "4. Next song" (buffer-string)))
   (should-not (string-match-p "▶ 1." (buffer-string)))))

(ert-deftest hub/emms-player-module-keeps-playback-dependencies-lazy ()
  "Loading personal app configuration does not load playback or panel packages."
  (with-temp-buffer
    (should
     (zerop
      (call-process
       (expand-file-name invocation-name invocation-directory) nil t nil
       "--batch" "-Q"
       "-L" (expand-file-name "lisp" hub/emms-player-test--root)
       "-L" (expand-file-name "modules/interactive" hub/emms-player-test--root)
       "--eval"
       "(progn (require 'apps/emms-player) (when (or (featurep 'emms) (featurep 'context-panels)) (error \"Eager dependency load\")))")))))

(provide 'emms-player-test)
;;; emms-player-test.el ends here
