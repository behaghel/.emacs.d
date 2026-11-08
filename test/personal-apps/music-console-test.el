;;; music-console-test.el --- Native Hi-Fi console acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic metadata and native buffer rendering; never real playback or OAuth.

;;; Code:
(require 'ert)
(require 'cl-lib)
(dolist (package '("emms" "evil"))
  (add-to-list 'load-path (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
							(expand-file-name "straight/build" default-directory)))))
(require 'emms)
(require 'apps/emms-player)
(require 'apps/music-console)

(defun hub/music-console-test--snapshot ()
  "Return a mixed queue with fictional metadata and no fetched artwork."
  (let ((emms-track-initialize-functions nil))
    (let ((first (emms-track 'url "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
	  (next (emms-track 'file "/tmp/paper-lanterns.ogg"))
	  (last (emms-track 'url "spotify:track:1111111111111111111111")))
      (dolist (pair `((,first "Low Light" "Quiet Geometry" "Still / Motion" 274)
		      (,next "Paper Lanterns" "Field Recordings" "Evening" 192)
		      (,last "The Long Way Home" "Northbound" "Home" 299)))
	(cl-loop for key in '(info-title info-artist info-album info-playing-time)
		 for value in (cdr pair) do (emms-track-set (car pair) key value)))
      (list :tracks (list first next last) :current first :selected first :index 0
	    :elapsed 102 :state "Playing"))))

(defmacro hub/music-console-test--render (columns snapshot &rest body)
  "Render SNAPSHOT at COLUMNS without network, credential or process side effects."
  (declare (indent 2))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'hub/music-console--columns) (lambda () ,columns))
	       ((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Renderer fetched data")))
	       ((symbol-function 'auth-source-pass-get) (lambda (&rest _) (ert-fail "Renderer read credentials")))
	       ((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Renderer started a process"))))
       (hub/music-console-render ,snapshot)
       ,@body)))

(ert-deftest hub/music-console-native-track-hierarchy-progress-and-up-next ()
  "The native console exposes metadata, attribution, progress and the real successors."
  (let ((snapshot (hub/music-console-test--snapshot)))
    (hub/music-console-test--render 140 snapshot
				    (dolist (text '("Low Light" "Quiet Geometry" "Still / Motion" "Spotify"
						    "1:42 / 4:34" "UP NEXT" "Paper Lanterns" "The Long Way Home" "12:45"))
				      (should (string-match-p (regexp-quote text) (buffer-string))))
				    (should (text-property-any (point-min) (point-max) 'hub/music-role 'title))
				    (should (text-property-any (point-min) (point-max) 'hub/music-role 'progress))
				    (should (next-button (point-min) t))
				    (should (string-match-p "System mix" (buffer-string)))
				    (should (string-match-p "Spectrum off" (buffer-string))))))

(ert-deftest hub/music-console-narrow-and-empty-layouts-remain-usable ()
  "Small windows retain text and actions instead of clipping essential controls."
  (dolist (columns '(48 80))
    (hub/music-console-test--render columns (hub/music-console-test--snapshot)
				    (goto-char (point-min))
				    (while (not (eobp))
				      (should (<= (string-width (buffer-substring-no-properties (line-beginning-position) (line-end-position))) columns))
				      (forward-line 1))))
  (hub/music-console-test--render 48 '(:tracks nil :current nil :elapsed 0 :state "Stopped")
				  (should (string-match-p "Empty" (buffer-string)))
				  (should (string-match-p "Liked Songs" (buffer-string)))
				  (should (string-match-p "Queue" (buffer-string)))
				  (should (string-match-p "No artwork" (buffer-string)))
				  (should-not (string-match-p "Playing" (buffer-string)))))

(ert-deftest hub/music-console-missing-duration-remains-explicit ()
  "Partial queue duration and an unknown current duration are never invented."
  (let ((snapshot (hub/music-console-test--snapshot)))
    (emms-track-set (plist-get snapshot :current) 'info-playing-time nil)
    (hub/music-console-test--render 100 snapshot
				    (should (string-match-p "1:42 / --:--" (buffer-string)))
				    (should (string-match-p (regexp-quote "8:11 + 1 unknown") (buffer-string))))))

(ert-deftest hub/music-console-shortcuts-are-quiet-but-still-actionable ()
  "Transport hints do not look like hyperlinks or change their click actions."
  (with-temp-buffer
    (hub/music-console--controls 140 "Playing")
    (goto-char (point-min))
    (search-forward "SPC Pause")
    (let* ((button (button-at (1- (point)))) (face (button-get button 'face)))
      (should-not (eq face 'link))
      (should-not (face-attribute face :underline nil t)))
    (search-forward "c -10s")
    (let* ((button (button-at (1- (point)))) (face (button-get button 'face)) invoked)
      (should-not (eq face 'link))
      (should-not (face-attribute face :underline nil t))
      (should (equal (face-attribute face :inherit) '(shadow fixed-pitch)))
      (cl-letf (((symbol-function 'hub/emms-seek-backward-10)
		 (lambda () (interactive) (setq invoked t))))
	(button-activate button))
      (should invoked))))

(ert-deftest hub/music-console-key-tokens-add-accent-without-recoloring-labels ()
  "Only key tokens use the navigation accent; primary and neutral labels stay distinct."
  (with-temp-buffer
    (hub/music-console--controls 140 "Playing")
    (goto-char (point-min))
    (search-forward "SPC Pause")
    (should (eq (get-text-property (1- (point)) 'face) 'hub/music-console-primary))
    (search-forward "c -10s")
    (let* ((key (- (point) (length "c -10s")))
	   (faces (get-text-property key 'face)) invoked)
      (should (memq 'hub/music-console-key (if (listp faces) faces (list faces))))
      (should (eq (get-text-property (1- (point)) 'face) 'hub/music-console-control))
      (should-not (face-attribute 'hub/music-console-key :underline nil t))
      (cl-letf (((symbol-function 'hub/emms-seek-backward-10)
		 (lambda () (interactive) (setq invoked t))))
	(button-activate (button-at key)))
      (should invoked))))

(ert-deftest hub/music-console-footer-has-separate-gap-and-padded-surface ()
  "The footer has a 16-pixel outside gap and eight-pixel inside padding, not extra text height."
  (with-temp-buffer
    (insert "Current track\n")
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (hub/music-console--controls 140 "Playing"))
    (let* ((gap (text-property-any (point-min) (point-max) 'hub/music-role 'control-gap))
	   (padding (text-property-any (point-min) (point-max) 'hub/music-role 'control-padding))
	   (surface (cl-find-if (lambda (overlay) (overlay-get overlay 'hub/music-footer))
				(overlays-in (point-min) (point-max)))))
      (should gap)
      (should padding)
      (should (equal (plist-get (cdr (get-text-property gap 'display)) :height) '(16)))
      (should (equal (plist-get (cdr (get-text-property padding 'display)) :height) '(8)))
      (should surface)
      (should (> (overlay-start surface) gap))
      (should (plist-get (overlay-get surface 'face) :background))
      (should-not (plist-get (overlay-get surface 'face) :foreground))
      (erase-buffer)
      (should-not (overlay-buffer surface)))))

(ert-deftest hub/music-console-metrics-without-native-font-api ()
  "Terminal-only builds remain safe when graphical rendering is simulated."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
	    ((symbol-function 'font-info) nil)
	    ((symbol-function 'face-font) (lambda (&rest _) "terminal")))
    (let ((metrics (hub/music-console--metrics 'hub/music-console-meta "Fixture")))
      (should (> (car metrics) 0))
      (should (> (cdr metrics) 0)))))

(ert-deftest hub/music-console-cover-metrics-include-fallback-fonts ()
  "Non-Latin metadata cannot introduce seams through taller fallback font metrics."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
	    ((symbol-function 'hub/music-console--columns) (lambda () 140))
	    ((symbol-function 'face-font)
	     (lambda (_face _frame &optional character) (if (eq character ?夜) "fallback" "base")))
	    ((symbol-function 'font-info)
	     (lambda (font &rest _)
	       (let ((info (make-vector 14 0)))
		 (aset info 8 (if (equal font "fallback") 36 30))
		 (aset info 9 (if (equal font "fallback") 12 13)) info))))
    (should (equal (hub/music-console--metrics 'hub/music-console-title "夜") '(36 . 13)))))

(ert-deftest hub/music-console-cover-slices-preserve-native-baselines ()
  "Image ascent percentages account for Emacs flooring, preventing seams in covers."
  (with-temp-buffer
    (let (slices)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
		((symbol-function 'image-type-available-p) (lambda (_) t))
		((symbol-function 'frame-char-width) (lambda (&rest _) 9))
		((symbol-function 'hub/music-console--columns) (lambda () 140))
		((symbol-function 'hub/music-console--metrics)
		 (lambda (face &optional _text) (if (eq face 'hub/music-console-title) '(30 . 13) '(15 . 3))))
		((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg :data "<svg/>")))
		((symbol-function 'hub/music-console--slice)
		 (lambda (_image _y height ascent &rest _) (push (cons height ascent) slices))))
	(hub/music-console-render (hub/music-console-test--snapshot)))
      (should (= (length slices) 6))
      (dolist (slice slices)
	(should (= (floor (* (car slice) (/ (cdr slice) 100.0)))
		   (pcase (car slice) (18 15) (25 20) (43 30))))))))

(ert-deftest hub/music-console-hidden-artwork-preparation-performs-no-io ()
  "Preparing an off-screen projection cannot start a cover lookup or download."
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
	      ((symbol-function 'image-type-available-p) (lambda (_) t))
	      ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
	      ((symbol-function 'hub/music-artwork-source)
	       (lambda (&rest _) (ert-fail "Hidden artwork inspected a source")))
	      ((symbol-function 'hub/music-artwork-request)
	       (lambda (&rest _) (ert-fail "Hidden artwork started a download"))))
      (hub/music-console-prepare-artwork (hub/music-console-test--snapshot))
      (should-not hub/music-console--art-resolved))))

(ert-deftest hub/music-console-artwork-fits-intact-and-links-without-overlays ()
  "A cached raster is letterboxed, reused across ticks, and linked to its source."
  (let* ((hub/music-artwork--cache (make-hash-table :test #'equal))
	 (source '(remote . "https://i.scdn.co/image/fit"))
	 (track (plist-get (hub/music-console-test--snapshot) :current))
	 svg (builds 0))
    (puthash source (hub/music-artwork--make :state 'ready :data "fixture-raster" :type 'png)
	     hub/music-artwork--cache)
    (with-temp-buffer
      (setq hub/music-console--art-source source)
      (cl-letf (((symbol-function 'svg-image)
		 (lambda (value &rest _) (setq svg value) (cl-incf builds)
		   '(image :type svg :data "<svg/>"))))
	(let ((image (hub/music-console--cover-image 147)))
	  (should (eq image (hub/music-console--cover-image 147)))
	  (should (= builds 1))
	  (hub/music-console--slice image 0 25 80 track)))
      (let ((image (car (dom-by-tag svg 'image))))
	(should (equal (dom-attr image 'preserveAspectRatio) "xMidYMid meet"))
	(should (= (dom-attr image 'width) 147))
	(should (= (dom-attr image 'height) 147))
	(should-not (dom-attr image 'filter)))
      (should-not (dom-by-tag svg 'text))
      (should (equal (get-text-property (point-min) 'hub/music-url)
		     "https://open.spotify.com/track/4uLU6hMCjMI75M1A2tKUQC")))))

(ert-deftest hub/music-console-proportional-metadata-respects-pixel-budget ()
  "Wide native glyphs fit their column while the full metadata remains inspectable."
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
	      ((symbol-function 'string-pixel-width)
	       (lambda (text) (cl-loop for char across text sum (if (memq char '(?W ?…)) 18 2)))))
      (hub/music-console--clip "WWi" 100 'hub/music-console-title 'title 38)
      (should (equal (buffer-string) "WWi"))
      (erase-buffer)
      (hub/music-console--clip "WWWWi" 100 'hub/music-console-title 'title 40)
      (should (<= (string-pixel-width (buffer-string)) 40))
      (should (equal (get-text-property (point-min) 'help-echo) "WWWWi")))))

(ert-deftest hub/music-console-source-and-audio-readiness-are-distinct ()
  "A ready Spotify receiver is not the source or readiness of local playback."
  (let ((snapshot (hub/music-console-test--snapshot)))
    (cl-letf (((symbol-function 'hub/music-audio-ready-p) (lambda () t)))
      (hub/music-console-test--render 100 snapshot
				      (should (string-match-p "Audio ready" (buffer-string))))
      (setq snapshot (plist-put snapshot :current (cadr (plist-get snapshot :tracks))))
      (hub/music-console-test--render 100 snapshot
				      (should (string-match-p "Local file" (buffer-string)))
				      (should-not (string-match-p "Audio ready" (buffer-string)))))))

(ert-deftest hub/music-console-upcoming-spotify-metadata-has-its-own-attribution ()
  "A mixed queue attributes upcoming Spotify content even when local media is current."
  (let* ((snapshot (hub/music-console-test--snapshot))
	 (tracks (plist-get snapshot :tracks)))
    (setq snapshot (plist-put snapshot :current (cadr tracks))
	  snapshot (plist-put snapshot :index 1))
    (hub/music-console-test--render 100 snapshot
				    (goto-char (point-min))
				    (search-forward "UP NEXT")
				    (search-forward "Spotify")
				    (should (equal (get-text-property (1- (point)) 'hub/music-url)
						   "https://open.spotify.com/track/1111111111111111111111")))))

(ert-deftest hub/music-console-spotify-attribution-links-to-the-service ()
  "Spotify metadata has a service link without requiring its desktop application."
  (hub/music-console-test--render 100 (hub/music-console-test--snapshot)
				  (let ((position (text-property-any (point-min) (point-max) 'hub/music-role 'source)))
				    (should position)
				    (should (equal (get-text-property position 'hub/music-url)
						   "https://open.spotify.com/track/4uLU6hMCjMI75M1A2tKUQC")))))

(provide 'music-console-test)
;;; music-console-test.el ends here
