;;; music-search.el --- Passive native search of EMMS occurrences -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion reads an ephemeral snapshot, not another playlist.  Only explicit
;; acceptance selects and plays an existing EMMS occurrence.  Browsing does not
;; load Spotify configuration, request metadata or enable capture.

;;; Code:
(require 'hub-utils)
(require 'emms)
(require 'cl-lib)
(require 'subr-x)

(defvar hub/music-search--history nil "Native music completion history.")

(cl-defstruct (hub/music-search--occurrence (:constructor hub/music-search--create))
  "One displayed occurrence from an EMMS playlist snapshot."
  label track position)

(defun hub/music-search--text (track property fallback)
  "Read single-line PROPERTY from TRACK, using FALLBACK for absent text."
  (let ((value (emms-track-get track property)))
    (if (and (stringp value) (not (string-empty-p (string-trim value))))
	(replace-regexp-in-string "[[:cntrl:]]+" " " value)
      fallback)))

(defun hub/music-search--queue (playlist)
  "Read PLAYLIST occurrences in EMMS order without moving its point or selection."
  (with-current-buffer playlist
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((index 0) occurrences)
	  (emms-walk-tracks
	   (let* ((track (emms-playlist-track-at (point)))
		  (duration (emms-track-get track 'info-playing-time))
		  (time (if (and (numberp duration) (> duration 0))
			    (format "%d:%02d" (/ (floor duration) 60) (% (floor duration) 60))
			  "--:--")))
	     (push (hub/music-search--create
		    :track track :position (point)
		    :label (format "%s — %s · %s  %s  [Queue #%d]"
				   (hub/music-search--text
				    track 'info-title
				    (hub/music-search--text track 'name "Untitled track"))
				   (hub/music-search--text track 'info-artist "Unknown artist")
				   (hub/music-search--text track 'info-album "Unknown album")
				   time (cl-incf index)))
		   occurrences)))
	  (nreverse occurrences))))))

(defun hub/music-search--table (occurrences)
  "Return passive native completion for OCCURRENCES in playlist order."
  (let ((labels (mapcar #'hub/music-search--occurrence-label occurrences)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
	  '(metadata (category . hub/music-track)
		     (display-sort-function . identity) (cycle-sort-function . identity))
	(complete-with-action action labels string predicate)))))

(defun hub/music-search--minibuffer-setup ()
  "Let spaces insert query text in this completion only."
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "SPC") #'self-insert-command))

(defun hub/music-search--assert-current (playlist revision occurrence)
  "Reject an obsolete OCCURRENCE from PLAYLIST at captured REVISION."
  (unless (and (buffer-live-p playlist) (eq playlist emms-playlist-buffer)
	       (with-current-buffer playlist
		 (save-restriction
		   (widen)
		   (and (= revision (buffer-chars-modified-tick))
			(eq (emms-playlist-track-at (hub/music-search--occurrence-position occurrence))
			    (hub/music-search--occurrence-track occurrence))))))
    (user-error "Queue changed; search again to select a current occurrence")))

;;;###autoload
(defun hub/music-search ()
  "Search the current EMMS queue; RET plays the existing selected occurrence.
Candidate movement is passive.  C-g returns without changing playback."
  (interactive)
  (unless (buffer-live-p emms-playlist-buffer)
    (user-error "The current EMMS queue is empty; add music first"))
  (let* ((playlist emms-playlist-buffer)
	 (occurrences (hub/music-search--queue playlist))
	 (revision (with-current-buffer playlist (buffer-chars-modified-tick))))
    (unless occurrences
      (user-error "The current EMMS queue is empty; add music first"))
    (save-window-excursion
      (let* ((choice (minibuffer-with-setup-hook #'hub/music-search--minibuffer-setup
		       (completing-read
			(format "Music (Queue: %d · queue only) — RET Play, C-g Back: " (length occurrences))
			(hub/music-search--table occurrences) nil t nil 'hub/music-search--history)))
	     (occurrence (cl-find choice occurrences :key #'hub/music-search--occurrence-label :test #'equal)))
	(when occurrence
	  (hub/music-search--assert-current playlist revision occurrence)
	  (emms-stop)
	  (hub/music-search--assert-current playlist revision occurrence)
	  (with-current-buffer playlist
	    (emms-playlist-select (hub/music-search--occurrence-position occurrence)))
	  (emms-start))))))

(provide 'apps/music-search)
;;; music-search.el ends here
