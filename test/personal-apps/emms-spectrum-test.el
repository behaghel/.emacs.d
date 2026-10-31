;;; emms-spectrum-test.el --- Opt-in spectrum scenarios -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic CAVA protocol frames and child processes, never real audio capture.
;; Scenarios: spec/personal-apps/apps/stories/emms-spectrum/scenarios.feature.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dom)
(require 'context-panels)
(require 'apps/emms-player)
(require 'apps/emms-spectrum)

(defmacro hub/emms-spectrum-test--with-player (&rest body)
  "Run BODY with a visible player and a synthetic analyzer process."
  (declare (indent 0) (debug t))
  `(let* ((user-emacs-directory (make-temp-file "emms-spectrum-test-" t))
	  (hub/emms-player-buffer-name " *spectrum-test-player*")
	  (hub/emms-player--timer nil)
	  (emms-playlist-buffer nil)
	  (emms-player-playing-p t)
	  (hub/emms-spectrum-program (expand-file-name invocation-name invocation-directory))
	  (hub/emms-spectrum--process nil)
	  (hub/emms-spectrum--timer nil)
	  (hub/emms-spectrum--config-file nil)
	  (hub/emms-spectrum--stderr nil)
	  (hub/emms-spectrum--pending "")
	  (hub/emms-spectrum--values nil)
	  (hub/emms-spectrum--diagnostic nil)
	  (hub/emms-spectrum--overlay nil)
	  (hub/emms-spectrum--image nil)
	  (hub/emms-spectrum--frame nil)
	  (hub/emms-spectrum--dirty nil)
	  (source (generate-new-buffer " *spectrum-test-source*"))
	  (other (generate-new-buffer " *spectrum-test-other*"))
	  (real-make-process (symbol-function 'make-process))
	  (fake-command (list "/bin/sh" "-c"
			      (format "printf '%%s\\n' '%s;'; exec sleep 30"
				      (mapconcat (lambda (_) "500")
						 (number-sequence 1 32) ";"))))
	  (starts 0))
     (unwind-protect
	 (save-window-excursion
	   (delete-other-windows)
	   (switch-to-buffer source)
	   (context-panels-mode 1)
	   (context-panels-register-provider
	    (list :name 'spectrum-test :icon "test"
		  :collect-bottom-views
		  (lambda (_source)
		    (list (list :id 'player :buffer-name hub/emms-player-buffer-name
				:mode #'hub/emms-player-mode
				:render #'hub/emms-player--render)))))
	   (context-panels-open-bottom-view 'player source t)
	   (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
		     ((symbol-function 'image-type-available-p) (lambda (_type) t))
		     ((symbol-function 'svg-image)
		      (lambda (_svg &rest _) '(image :type svg :data "<svg/>")))
		     ((symbol-function 'image-flush) (lambda (&rest _) nil))
		     ((symbol-function 'fit-window-to-buffer) (lambda (&rest _) nil))
		     ((symbol-function 'make-process)
		      (lambda (&rest args)
			(cl-incf starts)
			(apply real-make-process (plist-put args :command fake-command)))))
	     ,@body))
       (hub/emms-spectrum-stop)
       (hub/emms-player--cancel-timer)
       (dolist (buffer (list source other (get-buffer hub/emms-player-buffer-name)))
	 (when (buffer-live-p buffer) (kill-buffer buffer)))
       (delete-directory user-emacs-directory t))))

(ert-deftest hub/emms-spectrum-opening-player-is-not-consent ()
  "AC-1: Showing and refreshing the player never starts capture."
  (hub/emms-spectrum-test--with-player
   (hub/emms-player--refresh)
   (should (zerop starts))
   (should-not hub/emms-spectrum--process)
   (should (eq (lookup-key hub/emms-player-mode-map (kbd "v"))
	       #'hub/emms-spectrum-toggle))))

(ert-deftest hub/emms-spectrum-opt-in-stream-renders-native-image ()
  "AC-2: An explicit toggle consumes a real child stream and updates an SVG overlay."
  (hub/emms-spectrum-test--with-player
   (hub/emms-spectrum-toggle)
   (should (= starts 1))
   (should (process-live-p hub/emms-spectrum--process))
   (should (file-exists-p hub/emms-spectrum--config-file))
   (should (= (file-modes hub/emms-spectrum--config-file) #o600))
   (select-window (get-buffer-window source))
   (insert "Editing here")
   (let ((selected (selected-window)) (position (point)))
     (cl-loop repeat 40 until hub/emms-spectrum--values
	      do (accept-process-output hub/emms-spectrum--process 0.05))
     (should (= (length hub/emms-spectrum--values) 32))
     (hub/emms-spectrum--tick)
     (should (eq selected (selected-window)))
     (should (= position (point)))
     (should (overlayp hub/emms-spectrum--overlay))
     (should (eq (plist-get (cdr (overlay-get hub/emms-spectrum--overlay 'display)) :type) 'svg))
     (with-current-buffer hub/emms-player-buffer-name
       (should (string-match-p "Spectrum: system output" (buffer-string)))))))

(ert-deftest hub/emms-spectrum-hidden-stops-and-does-not-restart ()
  "AC-3: Hiding the player stops its process, timer and files without stopping music."
  (hub/emms-spectrum-test--with-player
   (hub/emms-spectrum-toggle)
   (let ((process hub/emms-spectrum--process)
	 (config hub/emms-spectrum--config-file))
     (select-window (get-buffer-window source))
     (delete-window (get-buffer-window hub/emms-player-buffer-name))
     (hub/emms-spectrum--tick)
     (should-not (process-live-p process))
     (should-not hub/emms-spectrum--timer)
     (should-not (file-exists-p config))
     (should emms-player-playing-p)
     (context-panels-open-bottom-view 'player source t)
     (should-not hub/emms-spectrum--process)
     (should (= starts 1)))))

(ert-deftest hub/emms-spectrum-close-and-kill-stop-capture ()
  "AC-3: Explicit close and buffer death both release the capture process."
  (dolist (action '(hub/emms-player-close kill-current-buffer))
    (hub/emms-spectrum-test--with-player
     (hub/emms-spectrum-toggle)
     (let ((process hub/emms-spectrum--process))
       (funcall action)
       (should-not (process-live-p process))
       (should-not hub/emms-spectrum--config-file)
       (should emms-player-playing-p)))))

(ert-deftest hub/emms-spectrum-missing-program-and-nongraphical-fail-closed ()
  "AC-4: Missing tools or SVG support do not start capture or disturb playback."
  (hub/emms-spectrum-test--with-player
   (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
     (hub/emms-spectrum-toggle))
   (should (string-match-p "graphical Emacs" hub/emms-spectrum--diagnostic))
   (let ((hub/emms-spectrum-program "missing-cava-test-executable"))
     (hub/emms-spectrum-toggle))
   (should (string-match-p "CAVA" hub/emms-spectrum--diagnostic))
   (should (zerop starts))
   (should-not hub/emms-spectrum--config-file)
   (should emms-player-playing-p)))

(ert-deftest hub/emms-spectrum-child-failure-is-actionable-and-cleans-up ()
  "AC-4: Child failure reports platform-specific recovery and releases capture."
  (dolist (platform '(darwin gnu/linux))
    (let ((system-type platform))
      (hub/emms-spectrum-test--with-player
       (setq fake-command (list hub/emms-spectrum-program "--batch" "-Q" "--eval"
				"(progn (princ \"AudioTap permissions missing\" #'external-debugging-output) (kill-emacs 1))"))
       (hub/emms-spectrum-toggle)
       (cl-loop repeat 40 while hub/emms-spectrum--process
		do (accept-process-output nil 0.05))
       (should-not hub/emms-spectrum--process)
       (should-not hub/emms-spectrum--timer)
       (should-not hub/emms-spectrum--config-file)
       (should (string-match-p "AudioTap permissions missing" hub/emms-spectrum--diagnostic))
       (should (string-match-p "Press v to retry" hub/emms-spectrum--diagnostic))
       (if (eq platform 'darwin)
	   (should (string-match-p "System Audio" hub/emms-spectrum--diagnostic))
	 (should-not (string-match-p "System Audio" hub/emms-spectrum--diagnostic)))
       (should (= starts 1))
       (should emms-player-playing-p)))))

(ert-deftest hub/emms-spectrum-protocol-coalesces-fragmented-frames ()
  "AC-5: Keep the latest complete frame across process-output boundaries."
  (let ((hub/emms-spectrum-bars 4)
	(hub/emms-spectrum--pending "")
	(hub/emms-spectrum--values nil)
	(hub/emms-spectrum--dirty nil))
    (hub/emms-spectrum--consume "0;100;200;")
    (should-not hub/emms-spectrum--values)
    (hub/emms-spectrum--consume "300;\n400;500;600;700;\n800;")
    (should (equal hub/emms-spectrum--values [400 500 600 700]))
    (should (equal hub/emms-spectrum--pending "800;"))
    (should hub/emms-spectrum--dirty)))

(ert-deftest hub/emms-spectrum-protocol-rejects-malformed-and-unbounded-data ()
  "AC-4/5: Wrong dimensions, invalid values and oversized partial frames fail closed."
  (dolist (chunk '("1;2;3;\n" "-1;2;3;4;\n" "1;2;3;1001;\n" "a;2;3;4;\n"))
    (let ((hub/emms-spectrum-bars 4) (hub/emms-spectrum--pending ""))
      (should-error (hub/emms-spectrum--consume chunk))))
  (let ((hub/emms-spectrum--pending ""))
    (should-error (hub/emms-spectrum--consume (make-string 9000 ?1)))))

(ert-deftest hub/emms-spectrum-macos-config-never-falls-back-to-microphone ()
  "The capture contract explicitly requests a system-output tap and raw bar data."
  (let* ((system-type 'darwin) (config (hub/emms-spectrum--configuration)))
    (should (string-match-p "method = coreaudio\nsource = tap" config))
    (should (string-match-p "method = raw" config))
    (should (string-match-p "data_format = ascii" config))
    (should-not (string-match-p "auto_input\\|portaudio" config))))

(ert-deftest hub/emms-spectrum-svg-has-bounded-pixel-bars ()
  "SVG geometry is pixel-based, bounded, and does not use terminal glyph bars."
  (let* ((svg (hub/emms-spectrum--svg [0 250 500 1000] 160 80))
	 (bars (cl-remove-if-not
		(lambda (node) (equal (dom-attr node 'class) "spectrum-bar"))
		(dom-by-tag svg 'rect))))
    (should (= (length bars) 4))
    (dolist (bar bars)
      (should (<= 0 (dom-attr bar 'x) 160))
      (should (<= 0 (dom-attr bar 'y) 80))
      (should (<= 0 (dom-attr bar 'height) 80)))
    (should-not (dom-by-tag svg 'text))))

(ert-deftest hub/emms-spectrum-replacing-bottom-view-stops-capture ()
  "AC-3: A different bottom view stops analysis rather than receiving animation."
  (hub/emms-spectrum-test--with-player
   (hub/emms-spectrum-toggle)
   (select-window (get-buffer-window source))
   (context-panels-register-provider
    (list :name 'other-view :icon "test"
	  :collect-bottom-views
	  (lambda (_source)
	    (list (list :id 'inspect :buffer-name " *spectrum-inspect*"
			:render (lambda (_source _view) (insert "Inspector")))))))
   (unwind-protect
       (let ((process hub/emms-spectrum--process))
	 (context-panels-open-bottom-view 'inspect source)
	 (hub/emms-spectrum--tick)
	 (should (get-buffer-window " *spectrum-inspect*"))
	 (should-not (process-live-p process))
	 (should-not hub/emms-spectrum--timer)
	 (should emms-player-playing-p))
     (when (get-buffer " *spectrum-inspect*") (kill-buffer " *spectrum-inspect*")))))

(ert-deftest hub/emms-spectrum-malformed-output-stops-owned-process ()
  "AC-4: A broken stdout protocol stops capture and clears its temporary settings."
  (hub/emms-spectrum-test--with-player
   (hub/emms-spectrum-toggle)
   (let ((process hub/emms-spectrum--process)
	 (config hub/emms-spectrum--config-file))
     (hub/emms-spectrum--filter process "invalid spectrum\n")
     (should-not (process-live-p process))
     (should-not (file-exists-p config))
     (should (string-match-p "Unexpected CAVA" hub/emms-spectrum--diagnostic))
     (should-not hub/emms-spectrum--values))))

(ert-deftest hub/emms-spectrum-real-cava-zero-input-protocol ()
  "Real CAVA emits compatible raw frames from /dev/zero, never system audio."
  (skip-unless (executable-find "cava"))
  (let* ((directory (make-temp-file "cava-zero-input-" t))
	 (config (expand-file-name "cava.conf" directory))
	 (errors (generate-new-buffer " *cava-zero-errors*"))
	 (hub/emms-spectrum--pending "")
	 (hub/emms-spectrum--values nil)
	 (hub/emms-spectrum--dirty nil)
	 process)
    (unwind-protect
	(progn
	  (with-temp-file config
	    (let ((system-type 'darwin))
	      (insert (replace-regexp-in-string
		       "method = coreaudio\nsource = tap"
		       "method = fifo\nsource = /dev/zero"
		       (hub/emms-spectrum--configuration) t t))))
	  (setq process
		(make-process :name "CAVA synthetic zero input" :buffer nil
			      :command (list (executable-find "cava") "-p" config)
			      :connection-type 'pipe :noquery t :stderr errors
			      :filter (lambda (_process chunk) (hub/emms-spectrum--consume chunk))
			      :sentinel #'ignore))
	  (cl-loop repeat 100 until hub/emms-spectrum--values
		   do (accept-process-output process 0.05))
	  (ert-info ((with-current-buffer errors (buffer-string)))
		    (should (= (length hub/emms-spectrum--values) hub/emms-spectrum-bars))
		    (should (cl-every #'zerop (append hub/emms-spectrum--values nil)))))
      (when (processp process) (delete-process process))
      (when-let* ((process (get-buffer-process errors)))
	(set-process-query-on-exit-flag process nil)
	(delete-process process))
      (kill-buffer errors)
      (delete-directory directory t))))

(provide 'emms-spectrum-test)
;;; emms-spectrum-test.el ends here
