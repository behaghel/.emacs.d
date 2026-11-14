;;; music-row.el --- Read-only identities and metadata for music rows -*- lexical-binding: t; -*-

;;; Commentary:
;; Rows project EMMS occurrences or retained library records.  They are not a
;; queue, never create playable tracks and never fetch or authorize anything.

;;; Code:
(require 'hub-utils)
(require 'emms)
(require 'button)
(require 'tabulated-list)
(require 'apps/music)
(require 'cl-lib)
(require 'subr-x)

(defvar hub/spotify--generation)
(defvar hub/spotify--tracks)
(defvar hub/spotify--playback-key)
(declare-function hub/spotify--refresh-playback "apps/spotify" ())

(declare-function modus-themes-get-current-theme "modus-themes" ())
(declare-function modus-themes-get-color-value "modus-themes" (color &optional with-overrides theme))

(defconst hub/music-row--faces
  '((hub/music-row-title blue link semibold "Primary track title.")
    (hub/music-row-artist cyan font-lock-type-face normal "Distinct artist accent.")
    (hub/music-row-album magenta font-lock-keyword-face normal "Secondary album accent.")
    (hub/music-row-time yellow warning normal "Distinct duration accent.")
    (hub/music-row-detail fg-dim shadow normal "Quiet listing separators.")
    (hub/music-row-source fg-dim shadow normal "Quiet provenance, separate from playback."))
  "Music face, Modus palette name, generic fallback, weight and documentation.
The active theme owns the colour values, including its palette overrides.")

(defun hub/music-row--apply-faces (&rest _themes)
  "Apply local music defaults on reload or theme changes.
Update defface defaults, not user/theme overrides or global parent faces."
  (dolist (entry hub/music-row--faces)
    (let* ((colour (when (and (fboundp 'modus-themes-get-color-value)
			      (fboundp 'modus-themes-get-current-theme)
			      (car custom-enabled-themes)
			      (eq (car custom-enabled-themes) (modus-themes-get-current-theme)))
		     (modus-themes-get-color-value (nth 1 entry) t)))
	   (spec `((t (:inherit (,(nth 2 entry) fixed-pitch)
				:height 1.0 :weight ,(nth 3 entry) :slant normal :underline nil)))))
      (when (stringp colour) (setf (plist-get (cadar spec) :foreground) colour))
      (custom-declare-face (car entry) spec (nth 4 entry) :group 'hub/music)
      (face-spec-set (car entry) spec 'face-defface-spec))))

(add-hook 'enable-theme-functions #'hub/music-row--apply-faces)
(add-hook 'disable-theme-functions #'hub/music-row--apply-faces)
(hub/music-row--apply-faces)

(defvar hub/music-row-source-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] #'push-button)
    map)
  "Source-link pointer actions; RET remains the table's explicit Play action.")

(defconst hub/music-row-columns
  [("" 3 nil) ("Title" 32 t) ("Artist" 22 t) ("Album" 22 t)
   ("Time" 7 nil :right-align t) ("Source" 11 nil)]
  "Native table layout for shared music rows; copy before modifying widths.")

(defun hub/music-row-layout (&optional columns)
  "Return the shared table schema fitting COLUMNS, with a 53-column minimum.
Shrink long text fields first; preserve a readable time and provenance column."
  (let* ((columns (max 53 (or columns (window-body-width))))
	 (layout (copy-tree hub/music-row-columns t))
	 (deficit (max 0 (- (+ 5 (cl-loop for column across layout sum (nth 1 column))) columns)))
	 (minimum [3 12 10 8 5 10]))
    (while (and (> deficit 0)
		(cl-some (lambda (index) (> (nth 1 (aref layout index)) (aref minimum index))) '(1 2 3)))
      (dolist (index '(1 2 3))
	(when (and (> deficit 0) (> (nth 1 (aref layout index)) (aref minimum index)))
	  (cl-decf (nth 1 (aref layout index))) (cl-decf deficit))))
    (dolist (index '(4 5))
      (let ((shrink (min deficit (- (nth 1 (aref layout index)) (aref minimum index)))))
	(cl-decf (nth 1 (aref layout index)) shrink) (cl-decf deficit shrink)))
    (cl-incf (nth 1 (aref layout 5))
	     (max 0 (- columns (+ 5 (cl-loop for column across layout sum (nth 1 column))))))
    layout))

(cl-defstruct (hub/music-row (:constructor hub/music-row--create))
  "An inert source identity with normalized display metadata."
  origin track metadata playlist position revision index library generation
  uri title artist album seconds playable liked-row)

(defun hub/music-row--text (value fallback)
  "Return single-line VALUE, or FALLBACK when it contains no useful text."
  (if (and (stringp value) (not (string-empty-p (string-trim value))))
      (replace-regexp-in-string "[[:cntrl:]]+" " " value)
    fallback))

(defun hub/music-row-from-queue (track playlist position index)
  "Project TRACK at POSITION and zero-based INDEX in PLAYLIST without changing it."
  (hub/music-row--create
   :origin 'queue :track track :playlist playlist :position position :index index
   :revision (with-current-buffer playlist (buffer-chars-modified-tick))
   :uri (emms-track-name track) :playable t
   :title (hub/music-row--text (emms-track-get track 'info-title)
			       (hub/music-row--text (emms-track-name track) "Untitled track"))
   :artist (hub/music-row--text (emms-track-get track 'info-artist) "Unknown artist")
   :album (hub/music-row--text (emms-track-get track 'info-album) "Unknown album")
   :seconds (emms-track-get track 'info-playing-time)))

(defun hub/music-row-from-liked (track index &optional library)
  "Project retained Spotify TRACK at INDEX, optionally owned by LIBRARY."
  (let ((uri (alist-get 'uri track))
	(duration (alist-get 'duration_ms track)))
    (hub/music-row--create
     :origin 'liked :track track :metadata (copy-tree track) :index index :library library
     :generation (when (buffer-live-p library) (buffer-local-value 'hub/spotify--generation library))
     :uri uri
     :playable (and (stringp uri) (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" uri)
		    (or (not (assq 'is_playable track))
			(not (memq (alist-get 'is_playable track) '(nil :false :json-false)))))
     :title (hub/music-row--text (alist-get 'name track) "Unavailable track")
     :artist (hub/music-row--text
	      (mapconcat (lambda (artist) (hub/music-row--text (alist-get 'name artist) "Unknown artist"))
			 (alist-get 'artists track) ", ") "Unknown artist")
     :album (hub/music-row--text (alist-get 'name (alist-get 'album track)) "Unknown album")
     :seconds (when (numberp duration) (/ duration 1000.0)))))

(defun hub/music-row-time (row)
  "Format ROW's known duration without inventing missing time."
  (let ((seconds (hub/music-row-seconds row)))
    (if (and (numberp seconds) (> seconds 0))
	(format "%d:%02d" (/ (floor seconds) 60) (% (floor seconds) 60)) "--:--")))

(defun hub/music-row-service (row)
  "Return ROW's readable media provenance without inspecting a local file."
  (cond ((or (eq (hub/music-row-origin row) 'liked)
	     (string-prefix-p "spotify:track:" (or (hub/music-row-uri row) ""))) "Spotify")
	((eq (emms-track-type (hub/music-row-track row)) 'file) "Local file")
	(t "Stream")))

(defun hub/music-row--fields (row)
  "Return styled title, artist, album and duration for ROW without changing it."
  (vector (propertize (hub/music-row-title row) 'face 'hub/music-row-title 'hub/music-row-field 'title)
	  (propertize (hub/music-row-artist row) 'face 'hub/music-row-artist 'hub/music-row-field 'artist)
	  (propertize (hub/music-row-album row) 'face 'hub/music-row-album 'hub/music-row-field 'album)
	  (propertize (hub/music-row-time row) 'face 'hub/music-row-time 'hub/music-row-field 'time)))

(defun hub/music-row--provenance (row)
  "Return ROW's searchable provenance without using it as action identity."
  (propertize
   (format "%s #%d%s · %s%s"
	   (if (eq (hub/music-row-origin row) 'queue) "Queue" "Liked")
	   (1+ (hub/music-row-index row)) (if (hub/music-row-liked-row row) " + Liked" "")
	   (hub/music-row-service row) (if (hub/music-row-playable row) "" " · unavailable"))
   'face 'hub/music-row-source 'hub/music-row-field 'source))

(defun hub/music-row-label (row)
  "Return searchable styled fields and provenance, never ROW's action identity."
  (let ((fields (hub/music-row--fields row)))
    (concat (aref fields 0) (propertize " — " 'face 'hub/music-row-detail)
	    (aref fields 1) (propertize " · " 'face 'hub/music-row-detail)
	    (aref fields 2) "  " (aref fields 3) "  "
	    (propertize "[" 'face 'hub/music-row-source)
	    (hub/music-row--provenance row) (propertize "]" 'face 'hub/music-row-source))))

(defun hub/music-row-status (row)
  "Read ROW's actual transport state without changing selection or playback."
  (when (and emms-player-playing-p (buffer-live-p emms-playlist-buffer)
	     (if (eq (hub/music-row-origin row) 'queue)
		 (and (eq (hub/music-row-playlist row) emms-playlist-buffer)
		      (with-current-buffer emms-playlist-buffer
			(and (= (hub/music-row-revision row) (buffer-chars-modified-tick))
			     (equal (hub/music-row-uri row) (emms-track-name (hub/music-row-track row)))
			     (markerp emms-playlist-selected-marker)
			     (equal (marker-position emms-playlist-selected-marker) (hub/music-row-position row))
			     (eq (hub/music--selected-p) (hub/music-row-track row)))))
	       (and (stringp (hub/music-row-uri row))
		    (when-let* ((track (hub/music--selected-p)))
		      (equal (hub/music-row-uri row) (emms-track-name track))))))
    (if (eq (hub/music-playback-state) 'connecting) 'connecting
      (if emms-player-paused-p 'paused 'playing))))

(defun hub/music-row-cells (row)
  "Return native table cells for ROW with a non-selection playback marker."
  (let* ((fields (hub/music-row--fields row))
	 (status (hub/music-row-status row))
	 (label (when status (hub/music-state-label status)))
	 (icon (if label (propertize (car (split-string label " "))
				     'help-echo label 'hub/music-state status) ""))
	 (service (hub/music-row-service row))
	 (uri (hub/music-row-uri row))
	 (source (if (and (equal service "Spotify") (stringp uri)
			  (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" uri))
		     (let ((url (concat "https://open.spotify.com/track/" (substring uri 14))))
		       (list (propertize "Spotify ↗" 'hub/music-row-field 'source) 'face 'hub/music-row-source 'follow-link t
			     'keymap hub/music-row-source-map
			     'help-echo "Open this track on Spotify"
			     'action (lambda (_button) (browse-url url))))
		   (propertize service 'face 'hub/music-row-source 'hub/music-row-field 'source))))
    (vector icon (aref fields 0) (aref fields 1) (aref fields 2) (aref fields 3) source)))

(defun hub/music-row-display (row &optional columns fields)
  "Render ROW with the same native printer and schema as Liked Songs.
Optional FIELDS are highlighted title/artist/album/time/provenance strings.
The full search candidate and its retained identity are never replaced."
  (let* ((layout (hub/music-row-layout columns))
	 (cells (hub/music-row-cells row))
	 (source (or (and fields (aref fields 4)) (hub/music-row--provenance row)))
	 (status (hub/music-row-status row)))
    (when fields
      (dotimes (index 4)
	(when (stringp (aref fields index)) (aset cells (1+ index) (aref fields index)))))
    (when status
      (setq source (concat source " · " (propertize (capitalize (symbol-name status))
						    'face (get-text-property 0 'face (hub/music-state-label status))))))
    (aset cells 5 (truncate-string-to-width source (nth 1 (aref layout 5)) nil nil t t))
    (with-temp-buffer
      (let ((tabulated-list-format layout) (tabulated-list-padding 0))
	(tabulated-list-print-entry (hub/music-row-index row) cells)
	(buffer-substring (point-min) (1- (point-max)))))))

(defun hub/music-row-assert-current (row)
  "Reject ROW if its captured source identity or playable record is obsolete."
  (if (eq (hub/music-row-origin row) 'queue)
      (unless (and (buffer-live-p (hub/music-row-playlist row))
		   (eq (hub/music-row-playlist row) emms-playlist-buffer)
		   (with-current-buffer (hub/music-row-playlist row)
		     (save-restriction
		       (widen)
		       (and (= (hub/music-row-revision row) (buffer-chars-modified-tick))
			    (eq (emms-playlist-track-at (hub/music-row-position row)) (hub/music-row-track row))
			    (equal (emms-track-name (hub/music-row-track row)) (hub/music-row-uri row))))))
	(user-error "Queue changed; search again to select a current occurrence"))
    (unless (and (buffer-live-p (hub/music-row-library row))
		 (with-current-buffer (hub/music-row-library row)
		   (and (derived-mode-p 'hub/spotify-library-mode)
			(= hub/spotify--generation (hub/music-row-generation row))
			(eq (cdr (assoc (hub/music-row-index row) hub/spotify--tracks)) (hub/music-row-track row))
			(equal (hub/music-row-track row) (hub/music-row-metadata row)))))
      (user-error "Loaded Liked Songs changed; search again"))
    (unless (hub/music-row-playable row) (user-error "This row has no playable Spotify track"))))

(defun hub/music-listings-refresh ()
  "Repaint loaded music listings and local face defaults without fetching.
Keep library records, the browsing row and playback unchanged."
  (interactive)
  (hub/music-row--apply-faces)
  (when (and (fboundp 'hub/spotify--refresh-playback) (hub/music-loaded-library-buffer))
    (with-current-buffer (hub/music-loaded-library-buffer)
      (setq-local hub/spotify--playback-key nil))
    (hub/spotify--refresh-playback)))

(provide 'apps/music-row)
;;; music-row.el ends here
