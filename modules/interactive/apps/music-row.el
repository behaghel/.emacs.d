;;; music-row.el --- Read-only identities and metadata for music rows -*- lexical-binding: t; -*-

;;; Commentary:
;; Rows project EMMS occurrences or retained library records.  They are not a
;; queue, never create playable tracks and never fetch or authorize anything.

;;; Code:
(require 'hub-utils)
(require 'emms)
(require 'button)
(require 'apps/music)
(require 'cl-lib)
(require 'subr-x)

(defvar hub/spotify--generation)
(defvar hub/spotify--tracks)

(defface hub/music-row-detail '((t (:inherit (shadow fixed-pitch) :height 0.9)))
  "Secondary music row metadata, relative to the existing editor font."
  :group 'hub/music)

(defvar hub/music-row-source-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] #'push-button)
    map)
  "Source-link pointer actions; RET remains the table's explicit Play action.")

(defconst hub/music-row-columns
  [("" 3 nil) ("Title" 32 t) ("Artist" 22 t) ("Album" 22 t) ("Time" 7 nil) ("Source" 11 nil)]
  "Native table layout for shared music rows; copy before modifying widths.")

(cl-defstruct (hub/music-row (:constructor hub/music-row--create))
  "An inert source identity with normalized display metadata."
  origin track metadata playlist position revision index library generation
  uri title artist album seconds playable)

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

(defun hub/music-row-label (row)
  "Return searchable metadata and provenance for ROW, not its action identity."
  (format "%s — %s · %s  %s  [%s #%d · %s%s]"
	  (hub/music-row-title row) (hub/music-row-artist row) (hub/music-row-album row)
	  (hub/music-row-time row) (if (eq (hub/music-row-origin row) 'queue) "Queue" "Liked")
	  (1+ (hub/music-row-index row)) (hub/music-row-service row)
	  (if (hub/music-row-playable row) "" " · unavailable")))

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
  (let* ((status (hub/music-row-status row))
	 (label (when status (hub/music-state-label status)))
	 (icon (if label (propertize (car (split-string label " "))
				     'help-echo label 'hub/music-state status) ""))
	 (service (hub/music-row-service row))
	 (uri (hub/music-row-uri row))
	 (source (if (and (equal service "Spotify") (stringp uri)
			  (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" uri))
		     (let ((url (concat "https://open.spotify.com/track/" (substring uri 14))))
		       (list "Spotify ↗" 'face 'hub/music-row-detail 'follow-link t
			     'keymap hub/music-row-source-map
			     'help-echo "Open this track on Spotify"
			     'action (lambda (_button) (browse-url url))))
		   (propertize service 'face 'hub/music-row-detail))))
    (vector icon (hub/music-row-title row) (hub/music-row-artist row)
	    (propertize (hub/music-row-album row) 'face 'hub/music-row-detail)
	    (propertize (hub/music-row-time row) 'face 'hub/music-row-detail) source)))

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

(provide 'apps/music-row)
;;; music-row.el ends here
