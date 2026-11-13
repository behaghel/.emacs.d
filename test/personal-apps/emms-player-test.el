;;; emms-player-test.el --- Docked EMMS player scenarios -*- lexical-binding: t; -*-

;;; Commentary:
;; Executable acceptance scenarios for spec/personal-apps/apps/stories/emms-player.
;; Real EMMS/context-panels/Evil code, but no external playback processes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'music-test-helpers
	 (expand-file-name "music-test-helpers" (file-name-directory (or load-file-name buffer-file-name))))

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
(require 'apps/music-artwork)


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
     (should (string-match-p "Playing" (buffer-string)))
     (should (string-match-p "First song" (buffer-string)))
     (should (string-match-p "1:00 / 3:00" (buffer-string)))
     (should (string-match-p "2 tracks · 7:00" (hub/music-test--view-text)))
     (should (string-match-p "Next song" (buffer-string)))
     (should (string-match-p (regexp-quote "[==========--------------------]")
			     (buffer-string)))
     (should (text-property-any (point-min) (point-max) 'hub/music-role 'title))
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
     (should (string-match-p "Empty" (buffer-string)))
     (should (string-match-p "0 tracks · 0:00" (hub/music-test--view-text)))
     (should (string-match-p "Choose Liked Songs or your editable queue" (buffer-string))))))

(ert-deftest hub/emms-player-unknown-duration-is-not-counted-as-zero ()
  "AC-5: Unknown metadata does not turn into misleading zero durations."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (emms-track-set first 'info-playing-time nil)
   (hub/emms-player-open)
   (should (string-match-p "1:00 / --:-- (unknown)" (buffer-string)))
   (should (string-match-p (regexp-quote "2 tracks · 4:00 + 1 unknown") (hub/music-test--view-text)))))

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

(ert-deftest hub/emms-player-refresh-preserves-native-player-position ()
  "Clock and metadata redraws preserve the player's own reading position too."
  (skip-unless (fboundp 'context-panels-open-docked-view))
  (hub/emms-player-test--with-playlist
   (hub/emms-player-open)
   (goto-char (point-min))
   (search-forward "First song")
   (let ((position (point)) (window (selected-window)))
     (hub/emms-player--refresh)
     (should (= position (point)))
     (should (= position (window-point window)))
     (should (eq window (selected-window))))))

(ert-deftest hub/emms-player-artwork-arrival-preserves-focus-and-reading-position ()
  "A public cover replaces its placeholder once, without touching the document or queue."
  (hub/emms-player-test--with-playlist
   (require 'apps/music-artwork)
   (emms-track-set first 'type 'url)
   (emms-track-set first 'name "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
   (emms-track-set first 'info-artwork-url "https://i.scdn.co/image/arrival")
   (let ((hub/music-artwork--cache (make-hash-table :test #'equal)) callback (requests 0))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
	       ((symbol-function 'image-type-available-p) (lambda (_) t))
	       ((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg :data "<svg/>")))
	       ((symbol-function 'image-flush) #'ignore)
	       ((symbol-function 'hub/music-artwork-request)
		(lambda (key done)
		  (cl-incf requests) (setq callback done)
		  (puthash key (hub/music-artwork--make) hub/music-artwork--cache))))
       (hub/emms-player-open)
       (should (= requests 1))
       (goto-char (point-min)) (search-forward "First song")
       (let ((player-point (point))
	     (queue (with-current-buffer playlist (buffer-string))))
	 (select-window (get-buffer-window source))
	 (insert "Keep writing here")
	 (let ((document-point (point)) (window (selected-window)))
	   (let ((entry (hub/music-artwork-cached '(remote . "https://i.scdn.co/image/arrival"))))
	     (setf (hub/music-artwork--entry-state entry) 'ready
		   (hub/music-artwork--entry-data entry) "fixture-raster-bytes"
		   (hub/music-artwork--entry-type entry) 'png))
	   (funcall callback)
	   (hub/emms-player--refresh)
	   (should (= requests 1))
	   (should (eq window (selected-window)))
	   (should (= document-point (point)))
	   (with-current-buffer hub/emms-player-buffer-name
	     (should (= player-point (point)))
	     (should (string-match-p "Album artwork" (buffer-string))))
	   (should (equal queue (with-current-buffer playlist (buffer-string)))))
	 (hub/emms-player-close)
	 (funcall callback)
	 (should-not (get-buffer-window hub/emms-player-buffer-name)))))))

(ert-deftest hub/emms-player-old-artwork-callback-cannot-redraw-new-track ()
  "Selecting another occurrence retires the old cover callback, not playback."
  (hub/emms-player-test--with-playlist
   (emms-track-set first 'type 'url)
   (emms-track-set first 'name "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
   (emms-track-set second 'type 'url)
   (emms-track-set second 'name "spotify:track:1111111111111111111111")
   (emms-track-set first 'info-artwork-url "https://i.scdn.co/image/first")
   (emms-track-set second 'info-artwork-url "https://i.scdn.co/image/second")
   (let ((hub/music-artwork--cache (make-hash-table :test #'equal)) callbacks)
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
	       ((symbol-function 'image-type-available-p) (lambda (_) t))
	       ((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg :data "<svg/>")))
	       ((symbol-function 'hub/music-artwork-request)
		(lambda (key done) (push done callbacks)
		  (puthash key (hub/music-artwork--make) hub/music-artwork--cache))))
       (hub/emms-player-open)
       (let ((old (car callbacks)))
	 (with-current-buffer playlist
	   (goto-char (point-min)) (forward-line 1) (emms-playlist-select (point)))
	 (hub/emms-player--refresh)
	 (should (= (length callbacks) 2))
	 (cl-letf (((symbol-function 'hub/emms-player--refresh)
		    (lambda (&rest _) (ert-fail "Retired cover redrew a different track"))))
	   (funcall old))
	 (should emms-player-playing-p))))))

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
   (should (string-match-p "Track 3 of 4" (buffer-string)))
   (should (string-match-p "4. Next song" (buffer-string)))
   (should-not (string-match-p "Track 1 of 4" (buffer-string)))))

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
