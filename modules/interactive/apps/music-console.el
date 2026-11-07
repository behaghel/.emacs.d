;;; music-console.el --- Native Hi-Fi music presentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Native, selectable metadata and buttons, supplemented by small SVG decorations.
;; This renderer reads snapshots only: no playback, authorization, or capture.

;;; Code:
(require 'hub-utils)
(require 'apps/music)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'svg)
(require 'dom)
(require 'apps/music-artwork)

(defface hub/music-console-title '((t :inherit variable-pitch :height 1.9 :weight regular))
  "Current track title, in the existing reading family." :group 'hub/music)
(defface hub/music-console-meta '((t :inherit fixed-pitch))
  "Native music metadata." :group 'hub/music)
(defface hub/music-console-muted '((t :inherit shadow))
  "Secondary information and quiet decoration." :group 'hub/music)
(defface hub/music-console-control '((t :inherit (shadow fixed-pitch) :underline nil))
  "Quiet, non-underlined transport hints; actions remain native buttons." :group 'hub/music)
(defface hub/music-console-key '((t :inherit (link fixed-pitch) :underline nil))
  "Navigation-coloured key tokens, distinct from neutral action labels." :group 'hub/music)
(defface hub/music-console-primary '((t :inherit (success fixed-pitch) :underline nil))
  "Primary transport action, without hyperlink styling." :group 'hub/music)
(defface hub/music-console-chrome '((t :inherit mode-line-inactive :extend t))
  "Theme-derived console navigation surface." :group 'hub/music)

(defvar hub/emms-player-buffer-name)
(defvar hub/emms-player-render-hook)
(defvar hub/emms-player-close-hook nil)
(defvar-local hub/music-console--art-track nil "Track whose artwork was resolved.")
(defvar-local hub/music-console--art-url nil "Last retained artwork URL.")
(defvar-local hub/music-console--art-library nil "Identity of already-loaded library data.")
(defvar-local hub/music-console--art-resolved nil "Non-nil after resolving the current track.")
(defvar-local hub/music-console--art-source nil "Current public image or local cover key.")
(defvar-local hub/music-console--art-image nil "Current fitted artwork image descriptor.")
(defvar-local hub/music-console--art-image-key nil "Entry, dimensions and background of the fitted image.")
(defvar hub/emms-spectrum--diagnostic nil)
(defvar hub/emms-spectrum--render-width nil)
(defvar hub/emms-spectrum--render-indent nil)

(declare-function emms-track-get "emms" (track name))
(declare-function hub/emms-player--refresh "apps/emms-player" (&rest args))
(declare-function hub/emms-player--duration "apps/emms-player" (track))
(declare-function hub/emms-player--time "apps/emms-player" (seconds))
(declare-function hub/emms-player--title "apps/emms-player" (track))

;; Spotify icon from Simple Icons (CC0); monochrome brand attribution, not a state icon.
(defconst hub/music-console--spotify-path
  "M12 .248C5.509.248.248 5.509.248 12S5.509 23.752 12 23.752 23.752 18.491 23.752 12 18.491.248 12 .248zm5.39 16.949c-.211.346-.659.456-1.004.245-2.75-1.68-6.212-2.06-10.29-1.129-.394.09-.787-.157-.877-.551-.09-.394.156-.787.551-.877 4.463-1.02 8.291-.582 11.378 1.304.345.211.455.659.244 1.008zm1.438-3.2c-.266.432-.828.568-1.259.303-3.148-1.935-7.948-2.496-11.673-1.365-.484.147-.997-.127-1.144-.61-.147-.484.127-.996.61-1.144 4.255-1.291 9.543-.665 13.164 1.56.43.265.567.828.302 1.256zm.123-3.332C15.177 8.424 8.94 8.217 5.332 9.312c-.579.176-1.191-.151-1.367-.73-.175-.58.151-1.191.731-1.367 4.142-1.257 11.029-1.014 15.376 1.566.521.309.692.982.383 1.502-.309.521-.982.692-1.504.383z"
  "Unaltered Spotify brand icon path in a 24 by 24 view box.")

(defun hub/music-console--frame ()
  "Return the console's owning frame without selecting it."
  (window-frame (or (get-buffer-window (current-buffer) t) (selected-window))))

(defun hub/music-console-color (face &optional background)
  "Resolve FACE's theme color, optionally its BACKGROUND, with terminal fallbacks."
  (let* ((frame (hub/music-console--frame))
	 (color (if background (face-background face frame t) (face-foreground face frame t)))
	 (rgb (and (stringp color) (color-defined-p color frame) (color-values color frame))))
    (if rgb (apply #'format "#%02x%02x%02x" (mapcar (lambda (value) (/ value 257)) rgb))
      (if background "#202020" "#b0b0b0"))))

(defun hub/music-console--columns ()
  "Return actual console width, including when another window has focus."
  (window-body-width (or (get-buffer-window (current-buffer) t) (selected-window))))

(defun hub/music-console--text (track property fallback)
  "Read a single-line TRACK PROPERTY or explicit FALLBACK."
  (let ((text (and track (emms-track-get track property))))
    (if (and (stringp text) (not (string-empty-p (string-trim text))))
	(replace-regexp-in-string "[\n\r\t]+" " " text)
      fallback)))

(defun hub/music-console--spotify-p (track)
  "Return non-nil for a canonical Spotify TRACK."
  (and track (eq (emms-track-get track 'type) 'url)
       (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'"
		       (or (emms-track-get track 'name) ""))))

(defun hub/music-console--source (track)
  "Return TRACK's source label independently of any receiver connection."
  (cond ((hub/music-console--spotify-p track) "Spotify")
	((and track (eq (emms-track-get track 'type) 'file)) "Local file")
	(track "Stream") (t "No source")))

(defun hub/music-console--url (track)
  "Return the canonical public Spotify service link for TRACK, if applicable."
  (when (hub/music-console--spotify-p track)
    (concat "https://open.spotify.com/track/" (substring (emms-track-get track 'name) 14))))

(defun hub/music-console--clip (text columns &optional face role pixels)
  "Insert TEXT in COLUMNS with FACE and ROLE; retain its full help text.
Optional PIXELS also bounds proportional typography on graphical displays."
  (let ((clipped (truncate-string-to-width text (max 1 columns) nil nil "…"))
	(face (or face 'hub/music-console-meta)))
    (when (and pixels (display-graphic-p (hub/music-console--frame))
	       (> (string-pixel-width (propertize clipped 'face face)) pixels))
      (let ((low 0) (high (length clipped)))
	(while (< low high)
	  (let* ((middle (ceiling (/ (+ low high) 2.0)))
		 (candidate (concat (substring clipped 0 middle) (if (< middle (length clipped)) "…" ""))))
	    (if (<= (string-pixel-width (propertize candidate 'face face)) pixels)
		(setq low middle) (setq high (1- middle)))))
	(when (< low (length clipped)) (setq clipped (concat (substring clipped 0 low) "…")))))
    (insert (propertize clipped 'face face 'help-echo text 'hub/music-role role))))

(defun hub/music-console--action (label command &optional face)
  "Insert LABEL invoking COMMAND only on activation, optionally styled with FACE."
  (insert-text-button label 'follow-link t 'face (or face 'link)
		      'action (lambda (_button) (call-interactively command))))

(defun hub/music-console--at (x)
  "Align subsequent native text at pixel X without filling the buffer with spaces."
  (insert (propertize " " 'display `(space :align-to (,x)))))

(defun hub/music-console--metrics (face &optional text)
  "Return FACE's ascent/descent, including fallback fonts needed by TEXT."
  (let* ((frame (hub/music-console--frame))
	 (graphic (display-graphic-p frame))
	 (font (and graphic (face-font face frame)))
	 (fonts (list font))
	 (info (and font (font-info font frame)))
	 (ascent (if info (+ (aref info 8) (aref info 4))
		   (max 1 (ceiling (* 0.8 (frame-char-height))))))
	 (descent (if info (- (aref info 9) (aref info 4))
		    (max 1 (floor (* 0.2 (frame-char-height)))))))
    (when (and graphic text)
      ;; Only a displayed prefix matters. ASCII uses the primary face's font;
      ;; non-ASCII names may use, for example, PingFang with a larger baseline.
      (dolist (character (delete-dups (string-to-list
				       (truncate-string-to-width text (hub/music-console--columns)))))
	(when (> character 127)
	  (let ((fallback (face-font face frame character)))
	    (when (and fallback (not (member fallback fonts)))
	      (push fallback fonts)
	      (when-let* ((metrics (font-info fallback frame)))
		(setq ascent (max ascent (+ (aref metrics 8) (aref metrics 4)))
		      descent (max descent (- (aref metrics 9) (aref metrics 4))))))))))
    (cons ascent descent)))

(defun hub/music-console--max-metrics (&rest metrics)
  "Combine METRICS without losing any font's ascent or descent."
  (cons (apply #'max (mapcar #'car metrics)) (apply #'max (mapcar #'cdr metrics))))

(defun hub/music-console--logo ()
  "Return a monochrome, minimum-size Spotify icon for the current theme."
  (let ((svg (svg-create 24 24)))
    (svg-node svg 'path :d hub/music-console--spotify-path
	      :fill (if (eq (frame-parameter (hub/music-console--frame) 'background-mode) 'light) "#000000" "#ffffff"))
    (svg-image svg :ascent 80)))

(defun hub/music-console--placeholder (size)
  "Return an intentional SIZE-pixel artwork placeholder, not fabricated album art."
  (let* ((svg (svg-create size size))
	 (muted (hub/music-console-color 'shadow))
	 (background (hub/music-console-color 'default t)))
    (svg-rectangle svg 1 1 (- size 2) (- size 2) :rx 4 :fill background :stroke muted :stroke-opacity 0.35)
    (svg-circle svg (/ size 2) (* size 0.43) (* size 0.17) :fill "none" :stroke muted :stroke-opacity 0.45)
    (svg-circle svg (/ size 2) (* size 0.43) 3 :fill muted)
    (svg-text svg "No artwork" :x (/ size 2) :y (* size 0.75)
	      :text-anchor "middle" :font-size 12
	      :font-family (face-attribute 'fixed-pitch :family (hub/music-console--frame) t) :fill muted)
    (svg-image svg :scale 1)))

(defun hub/music-console-cancel-artwork ()
  "Cancel this player's pending artwork without touching playback or cached covers."
  (when-let* ((buffer (if (eq major-mode 'hub/emms-player-mode) (current-buffer)
			(get-buffer hub/emms-player-buffer-name))))
    (with-current-buffer buffer
      (hub/music-artwork-cancel hub/music-console--art-source)
      (setq hub/music-console--art-source nil hub/music-console--art-resolved nil))))

(defun hub/music-console-prepare-artwork (snapshot)
  "Resolve SNAPSHOT's cover once, outside the pure renderer.
Only visible graphical artwork starts image I/O."
  (let ((window (get-buffer-window (current-buffer) t)))
    (when (and (display-graphic-p (hub/music-console--frame))
	       (image-type-available-p 'svg) (>= (hub/music-console--columns) 80)
	       window (eq (frame-visible-p (window-frame window)) t))
      (let* ((track (plist-get snapshot :current))
	     (url (and track (emms-track-get track 'info-artwork-url)))
	     (library (hub/music-artwork-library-state)))
	(when (or (not hub/music-console--art-resolved)
		  (not (eq track hub/music-console--art-track))
		  (not (equal url hub/music-console--art-url))
		  (not (eq library hub/music-console--art-library)))
	  (let ((source (condition-case nil (hub/music-artwork-source track) (error nil)))
		(buffer (current-buffer)))
	    (unless (equal source hub/music-console--art-source)
	      (hub/music-artwork-cancel hub/music-console--art-source))
	    (setq hub/music-console--art-track track hub/music-console--art-url url
		  hub/music-console--art-library library hub/music-console--art-resolved t
		  hub/music-console--art-source source)
	    (add-hook 'kill-buffer-hook #'hub/music-console-cancel-artwork nil t)
	    (when source
	      (hub/music-artwork-request
	       source
	       (lambda ()
		 (when (and (buffer-live-p buffer)
			    (eq track (buffer-local-value 'hub/music-console--art-track buffer))
			    (equal source (buffer-local-value 'hub/music-console--art-source buffer)))
		   (when-let* ((window (get-buffer-window buffer t))
			       ((eq (frame-visible-p (window-frame window)) t)))
		     (let ((hub/emms-player-buffer-name (buffer-name buffer)))
		       (hub/emms-player--refresh)))))))))))))

(defun hub/music-console-retry-artwork ()
  "Explicitly retry the visible player's cover, without reconnecting Spotify."
  (interactive)
  (let ((buffer (get-buffer hub/emms-player-buffer-name)))
    (unless (and buffer (get-buffer-window buffer t)) (user-error "Open the music player first"))
    (with-current-buffer buffer
      (hub/music-artwork-forget hub/music-console--art-source)
      (setq hub/music-console--art-resolved nil)
      (hub/emms-player--refresh))))

(defun hub/music-console--cover-image (size)
  "Return cached artwork fitted intact inside SIZE, or an explicit placeholder."
  (let* ((entry (hub/music-artwork-cached hub/music-console--art-source))
	 (background (hub/music-console-color 'default t))
	 (key (list entry size background)))
    (if (not (and entry (eq (hub/music-artwork--entry-state entry) 'ready)))
	(hub/music-console--placeholder size)
      (unless (equal key hub/music-console--art-image-key)
	(when hub/music-console--art-image (image-flush hub/music-console--art-image))
	(let ((svg (svg-create size size)))
	  (svg-rectangle svg 0 0 size size :fill background)
	  (dom-append-child svg
			    (dom-node 'defs nil
				      (dom-node 'clipPath '((id . "cover-corners"))
						(dom-node 'rect `((width . ,size) (height . ,size) (rx . 4))))))
	  (svg-embed svg (hub/music-artwork--entry-data entry)
		     (concat "image/" (symbol-name (hub/music-artwork--entry-type entry))) t
		     :x 0 :y 0 :width size :height size :preserveAspectRatio "xMidYMid meet"
		     :clip-path "url(#cover-corners)")
	  (setq hub/music-console--art-image (svg-image svg :scale 1)
		hub/music-console--art-image-key key)))
      hub/music-console--art-image)))

(defun hub/music-console--slice (image y height ascent &optional track)
  "Insert IMAGE's Y/HEIGHT slice with ASCENT, optionally attributed to TRACK."
  (let* ((slice (cons 'image (plist-put (copy-sequence (cdr image)) :ascent ascent)))
	 (entry (hub/music-artwork-cached hub/music-console--art-source))
	 (ready (and entry (eq (hub/music-artwork--entry-state entry) 'ready)))
	 (url (and ready (hub/music-console--url track)))
	 (start (point)))
    (insert-image slice (if (= y 0) (if ready "Album artwork" "No artwork   ") " ") nil (list 0 y 1.0 height))
    ;; Do not expose image-map's crop/rotate commands for licensed service art.
    (add-text-properties start (point)
			 (list 'keymap nil 'hub/music-role (if ready 'artwork 'artwork-placeholder)
			       'help-echo
			       (cond (ready "Album artwork")
				     ((and entry (eq (hub/music-artwork--entry-state entry) 'pending)) "Loading artwork")
				     ((and entry (eq (hub/music-artwork--entry-state entry) 'failed))
				      "Artwork unavailable; M-x hub/music-console-retry-artwork")
				     (t "No artwork"))))
    (when url
      (make-text-button start (point) 'follow-link t 'face 'default 'keymap button-map
			'hub/music-url url 'action (lambda (_button) (browse-url url))))))

(add-hook 'hub/emms-player-close-hook #'hub/music-console-cancel-artwork)

(defun hub/music-console--brand (track graphic)
  "Insert TRACK's native source attribution, optionally with a GRAPHIC brand mark."
  (let* ((url (hub/music-console--url track))
	 (label (hub/music-console--source track))
	 (start (point)))
    (when (and url graphic) (insert-image (hub/music-console--logo) "○") (insert " "))
    (if (not url) (insert label)
      (insert-text-button label 'follow-link t 'face 'link 'hub/music-url url
			  'action (lambda (_button) (browse-url url))))
    (add-text-properties start (point) (list 'hub/music-role 'source 'hub/music-url url))))

(defun hub/music-console--total (tracks)
  "Format TRACKS' known duration and explicitly account for unknown durations."
  (let ((total 0) (unknown 0))
    (dolist (track tracks)
      (if-let* ((duration (hub/emms-player--duration track))) (cl-incf total duration)
	(cl-incf unknown)))
    (concat (hub/emms-player--time total) (if (> unknown 0) (format " + %d unknown" unknown) ""))))

(defun hub/music-console--bar (elapsed duration width graphic)
  "Insert honest progress from ELAPSED and DURATION, at WIDTH pixels if GRAPHIC."
  (let* ((ratio (if duration (max 0.0 (min 1.0 (/ elapsed (float duration)))) 0.0))
	 (filled (floor (* ratio 30)))
	 (fallback (if duration (format "[%s%s]" (make-string filled ?=) (make-string (- 30 filled) ?-))
		     "Progress unavailable"))
	 (start (point)))
    (insert fallback)
    (add-text-properties start (point) '(hub/music-role progress face hub/music-console-muted))
    (when graphic
      (let ((svg (svg-create width 4)))
	(svg-rectangle svg 0 0 width 4 :rx 2 :fill (hub/music-console-color 'shadow))
	(when duration
	  (svg-rectangle svg 0 0 (* width ratio) 4 :rx 2 :fill (hub/music-console-color 'success)))
	(add-text-properties start (point) (list 'display (svg-image svg :ascent 'center)))))))

(defun hub/music-console--vertical-space (pixels role)
  "Insert PIXELS of native vertical spacing identified by ROLE.
Ignore the newline's font height so small padding is not inflated to a text row."
  (insert (propertize " " 'display `(space :width (1) :height (,pixels) :ascent 100)
		      'hub/music-role role)
	  (propertize "\n" 'line-height t)))

(defun hub/music-console--controls (columns state)
  "Insert native transport buttons that wrap within COLUMNS, respecting STATE."
  (let ((actions `((,(cond ((equal state "Waiting for audio") "SPC Cancel request")
			   ((equal state "Playing") "SPC Pause")
			   ((equal state "Paused") "SPC Resume") (t "SPC Play")) . hub/emms-player-toggle)
		   ("c -10s" . hub/emms-seek-backward-10) ("r +30s" . hub/emms-seek-forward-30)
		   ("s Previous" . hub/emms-player-previous) ("t Next" . hub/emms-player-next)
		   ("v Spectrum" . hub/emms-spectrum-toggle) ("l Liked Songs" . hub/spotify-liked-songs)
		   ("? Actions" . hub/music-palette) ("q Close" . hub/emms-player-close)))
	(used 0)
	(graphic (display-graphic-p (hub/music-console--frame))))
    (if graphic (hub/music-console--vertical-space 16 'control-gap)
      (insert "\n"))
    (let ((start (point)))
      (when graphic (hub/music-console--vertical-space 8 'control-padding))
      (dolist (action actions)
	(when (> (+ used (length (car action)) 3) columns) (insert "\n") (setq used 0))
	(when (> used 0) (insert "   ") (cl-incf used 3))
	(let ((button-start (point)))
	  (hub/music-console--action (car action) (cdr action)
				     (if (eq (cdr action) 'hub/emms-player-toggle)
					 'hub/music-console-primary 'hub/music-console-control))
	  (add-face-text-property button-start (+ button-start (string-match " " (car action)))
				  'hub/music-console-key))
	(cl-incf used (length (car action))))
      (insert "\n")
      (when graphic (hub/music-console--vertical-space 8 'control-padding))
      ;; Apply only a background, so the surface cannot flatten token/label colours.
      (let ((surface (make-overlay start (point))))
	(overlay-put surface 'hub/music-footer t)
	(overlay-put surface 'evaporate t)
	(overlay-put surface 'face `(:background ,(hub/music-console-color 'hub/music-console-chrome t)
						 :extend t))))))

(defun hub/music-console--artist-source (track columns graphic)
  "Insert TRACK's artist and attributed source inside COLUMNS, optionally GRAPHIC."
  (let ((reserved (+ 3 (length (hub/music-console--source track))
		     (if (and graphic (hub/music-console--spotify-p track))
			 (1+ (ceiling (/ 24.0 (frame-char-width (hub/music-console--frame))))) 0))))
    (hub/music-console--clip (hub/music-console--text track 'info-artist "Unknown artist")
			     (max 1 (- columns reserved)) 'hub/music-console-muted)
    (insert " · ")
    (hub/music-console--brand track graphic)))

(defun hub/music-console--up-next (snapshot columns &optional graphic)
  "Insert SNAPSHOT's successors as native text in COLUMNS, optionally GRAPHIC."
  (let* ((tracks (plist-get snapshot :tracks)) (index (or (plist-get snapshot :index) 0))
	 (next (seq-take (nthcdr (1+ index) tracks) 2)))
    (insert (propertize "UP NEXT\n" 'face 'hub/music-console-muted))
    (if (not next) (insert "End of queue\n")
      (cl-loop for track in next for number from (+ index 2) do
	       (hub/music-console--clip (format "%d. %s" number (hub/emms-player--title track)) columns)
	       (insert "\n")
	       (hub/music-console--artist-source track columns graphic)
	       (insert "\n")))
    (hub/music-console--clip (format "Queue %s · %d tracks" (hub/music-console--total tracks) (length tracks)) columns)
    (insert "\n")))

(defun hub/music-console-render (snapshot)
  "Insert SNAPSHOT as a native, responsive Hi-Fi console without external work."
  (let* ((columns (hub/music-console--columns))
	 (graphic (and (display-graphic-p (hub/music-console--frame)) (image-type-available-p 'svg)))
	 (wide (and graphic (>= columns 110)))
	 (art (and graphic (>= columns 80)))
	 (char-width (frame-char-width (hub/music-console--frame)))
	 (width (* columns char-width))
	 (track (plist-get snapshot :current))
	 (tracks (plist-get snapshot :tracks))
	 (index (or (plist-get snapshot :index) 0))
	 (state (if track (plist-get snapshot :state) "Empty"))
	 (duration (hub/emms-player--duration track))
	 (elapsed (max 0 (or (plist-get snapshot :elapsed) 0)))
	 (next (seq-take (nthcdr (1+ index) tracks) 2))
	 (current-title (if track (hub/emms-player--title track) "No track selected"))
	 (metadata (if track (format "%s · %s" (hub/music-console--text track 'info-artist "Unknown artist")
				     (hub/music-console--text track 'info-album "Unknown album"))
		     "Choose Liked Songs or your editable queue."))
	 (state-label (hub/music-state-label
		       (pcase state ("Playing" 'playing) ("Paused" 'paused)
			      ("Waiting for audio" 'connecting) ("Seeking" 'seeking)
			      ("Empty" 'empty) (_ 'stopped))))
	 (normal (hub/music-console--metrics 'hub/music-console-meta))
	 (brand-metrics (hub/music-console--max-metrics normal '(20 . 5)))
	 (metrics
	  (list
	   (hub/music-console--max-metrics
	    (if (hub/music-console--spotify-p track) brand-metrics normal)
	    (hub/music-console--metrics 'hub/music-console-meta state-label))
	   (hub/music-console--max-metrics
	    normal (hub/music-console--metrics 'hub/music-console-title current-title)
	    (hub/music-console--metrics 'hub/music-console-meta
					(if (and wide (car next)) (hub/emms-player--title (car next)) "")))
	   (hub/music-console--max-metrics
	    (if (and wide (hub/music-console--spotify-p (car next))) brand-metrics normal)
	    (hub/music-console--metrics 'hub/music-console-muted metadata)
	    (hub/music-console--metrics 'hub/music-console-muted
					(if wide (hub/music-console--text (car next) 'info-artist "") "")))
	   (hub/music-console--max-metrics
	    normal (hub/music-console--metrics 'hub/music-console-meta
					       (if (and wide (cadr next)) (hub/emms-player--title (cadr next)) "")))
	   (hub/music-console--max-metrics
	    (if (and wide (hub/music-console--spotify-p (cadr next))) brand-metrics normal)
	    (hub/music-console--metrics 'hub/music-console-muted
					(if wide (hub/music-console--text (cadr next) 'info-artist "") "")))
	   normal))
	 (size (apply #'+ (mapcar (lambda (metric) (+ (car metric) (cdr metric))) metrics)))
	 (image (and art (hub/music-console--cover-image size)))
	 (left (if graphic 28 0))
	 (meta-x (if art (+ left size 26) left))
	 (queue-x (- width 360))
	 (meta-width (max 100 (- (if wide (- queue-x 112) (- width 28)) meta-x))))
    ;; Navigation is native text; unimplemented search is deliberately absent.
    (let ((start (point)))
      (insert "MUSIC · Player   ")
      (hub/music-console--action "Liked Songs" #'hub/spotify-liked-songs)
      (insert "   ") (hub/music-console--action "Queue" #'hub/music-queue)
      (insert "   ") (hub/music-console--action "? Actions" #'hub/music-palette)
      (insert "\n")
      (add-face-text-property start (point) 'hub/music-console-chrome t))
    (when graphic (insert (propertize "\n" 'line-height 10)))
    (unless art (insert (propertize (if track "Compact view\n" "No artwork\n")
				    'face 'hub/music-console-muted)))
    (let ((y 0))
      (cl-loop for row from 0 below 6 for metric in metrics
	       for height = (+ (car metric) (cdr metric)) do
	       (when graphic (hub/music-console--at left))
	       (when art
		 (hub/music-console--slice image y height (ceiling (* 100.0 (/ (car metric) (float height)))) track)
		 (cl-incf y height))
	       (when graphic (hub/music-console--at meta-x))
	       (pcase row
		 (0 (hub/music-console--brand track graphic)
		    (insert " · ")
		    (insert state-label))
		 (1 (hub/music-console--clip current-title
					     (- columns 2)
					     'hub/music-console-title 'title (and graphic meta-width)))
		 (2 (hub/music-console--clip
		     metadata
		     (if graphic (floor (/ meta-width char-width)) columns) 'hub/music-console-muted))
		 (3 (hub/music-console--bar elapsed duration meta-width graphic))
		 (4 (hub/music-console--clip
		     (format "%s / %s%s" (hub/emms-player--time elapsed) (hub/emms-player--time duration)
			     (if (and track (not duration)) " (unknown)" "")) columns))
		 (5 (when track
		      (hub/music-console--clip
		       (format "%s · %d / %d%s"
			       (if (equal state "Stopped") "Selected" "Now playing")
			       (1+ index) (length tracks)
			       (if (not (hub/music-console--spotify-p track)) ""
				 (concat " · " (cond ((hub/music-audio-ready-p) "Audio ready")
						     ((equal state "Waiting for audio") "Enable audio in Chrome")
						     ((featurep 'apps/spotify-browser) "Audio needs connection")
						     (t "Audio not checked")))))
		       columns 'hub/music-console-muted nil (and graphic meta-width)))))
	       (when wide
		 (hub/music-console--at queue-x)
		 (let* ((next-track (nth (if (< row 3) 0 1) next))
			(text (pcase row
				(0 "UP NEXT")
				((or 1 3) (if next-track (format "%d. %s" (+ index (if (= row 1) 2 3))
								 (hub/emms-player--title next-track))
					    (if (= row 1) "End of queue" "")))
				((or 2 4) "")
				(5 (format "Queue %s · %d tracks" (hub/music-console--total tracks) (length tracks))))))
		   (if (and (memq row '(2 4)) next-track)
		       (hub/music-console--artist-source next-track (floor (/ 330 char-width)) graphic)
		     (hub/music-console--clip text (floor (/ 330 char-width)) 'hub/music-console-muted))))
	       (insert (propertize "\n" 'line-height t))))
    ;; Optional capture retains its existing explicit lifecycle. Render hooks do
    ;; not start it, and the off state must not resemble a live signal.
    (let ((hub/emms-spectrum--render-width meta-width)
	  (hub/emms-spectrum--render-indent (and graphic meta-x)))
      (run-hooks 'hub/emms-player-render-hook))
    (unless (hub/music--spectrum-on-p)
      (when graphic (hub/music-console--at meta-x))
      (insert (propertize "System mix · Spectrum off · v enable\n" 'face 'hub/music-console-muted)))
    (hub/music-console--controls columns state)
    (unless wide (hub/music-console--up-next snapshot columns graphic))))

(provide 'apps/music-console)
;;; music-console.el ends here
