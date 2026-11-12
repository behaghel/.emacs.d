;;; music-search.el --- Passive search of queue and retained library -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult groups inert rows from EMMS and already loaded Liked Songs.  Only
;; explicit acceptance plays or appends; no previews, fetching or authorization.

;;; Code:
(require 'hub-utils)
(require 'apps/music-row)
(require 'consult)
(require 'cl-lib)

(defvar hub/music-search--history nil "Native music completion history.")
(defvar hub/music-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'self-insert-command)
    (define-key map (kbd "M-RET") #'hub/music-search--enqueue)
    map)
  "Music-specific completion keys composed above the native frontend map.")
(defvar hub/music-search--operation 'play "Explicit action in the active search.")
(defvar-local hub/music-search--active nil "Non-nil in a music search minibuffer.")
(defvar hub/spotify--tracks)
(defvar hub/spotify--total)
(defvar +vertico-current-arrow)
(declare-function vertico--exhibit "vertico" ())
(autoload 'hub/spotify-player-add "apps/spotify-player" nil nil)

(defun hub/music-search--queue (playlist)
  "Project PLAYLIST occurrences without moving point or selection."
  (when (buffer-live-p playlist)
    (with-current-buffer playlist
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (let ((index 0) rows)
	    (emms-walk-tracks
	     (push (hub/music-row-from-queue (emms-playlist-track-at (point)) playlist (point) index) rows)
	     (cl-incf index))
	    (nreverse rows)))))))

(defun hub/music-search--liked (library)
  "Project LIBRARY's already retained records; never load or fetch a page."
  (when library
    (with-current-buffer library
      (mapcar (lambda (entry) (hub/music-row-from-liked (cdr entry) (car entry) library)) hub/spotify--tracks))))

(defun hub/music-search--combine (queue liked)
  "Combine QUEUE occurrences with LIKED rows by exact Spotify URI.
Retain each queue occurrence and its matching library identity; remove only
redundant library hits.  Neither source is fetched or changed."
  (let ((by-uri (make-hash-table :test #'equal))
	(represented (make-hash-table :test #'equal)))
    (dolist (row liked)
      (let ((uri (hub/music-row-uri row)))
	(when (and (stringp uri) (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" uri))
	  (puthash uri row by-uri))))
    (dolist (row queue)
      (when-let* ((match (gethash (hub/music-row-uri row) by-uri)))
	(setf (hub/music-row-liked-row row) match)
	(puthash (hub/music-row-uri row) t represented)))
    (cons queue (cl-remove-if (lambda (row) (gethash (hub/music-row-uri row) represented)) liked))))

(defun hub/music-search--target (target)
  "Resolve TARGET's retained row property, never parsing its display text."
  (let ((row (and (stringp target) (> (length target) 0) (get-text-property 0 'hub/music-row target))))
    (unless (hub/music-row-p row) (user-error "Select a current music row"))
    row))

(defun hub/music-search--annotation (target)
  "Read actual playback state for TARGET without previewing it."
  (let ((row (hub/music-search--target target)))
    (if-let* ((status (hub/music-row-status row))) (hub/music-state-label status)
      (if (hub/music-row-playable row) "" "Unavailable"))))

(defun hub/music-search--source (name narrow rows)
  "Build a passive Consult source with NAME, NARROW key and ROWS."
  (list :name name :narrow narrow :category 'hub/music-track :preview-key nil
	:annotate #'hub/music-search--annotation
	:items (mapcar (lambda (row)
			 (let ((label (hub/music-row-label row)))
			   (cons label (propertize label 'hub/music-row row)))) rows)))

(defun hub/music-search--enqueue ()
  "Accept the current completion with the explicit append-only action."
  (interactive)
  (setq hub/music-search--operation 'enqueue)
  (condition-case error
      (progn (call-interactively (key-binding (kbd "RET")))
	     ;; A successful exit throws out of the minibuffer.  Completion-only
	     ;; RET returns normally, and must not change a later RET's action.
	     (setq hub/music-search--operation 'play))
    (error (setq hub/music-search--operation 'play) (signal (car error) (cdr error)))))

(defun hub/music-search--minibuffer-setup ()
  "Keep query input native and distinguish browsing highlight from playback."
  (setq-local hub/music-search--active t)
  (when (boundp '+vertico-current-arrow) (setq-local +vertico-current-arrow nil))
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "SPC") #'self-insert-command)
  (local-set-key (kbd "M-RET") #'hub/music-search--enqueue))

(defun hub/music-search--refresh-status ()
  "Refresh active Vertico annotations without selecting or previewing a track."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (when (and hub/music-search--active (bound-and-true-p vertico-mode) (fboundp 'vertico--exhibit))
	(vertico--exhibit)))))

(defun hub/music-search--act (row operation)
  "Perform explicit OPERATION on validated ROW through existing queue owners."
  (when (and (eq operation 'enqueue) (hub/music-row-liked-row row))
    (setq row (hub/music-row-liked-row row)))
  (hub/music-row-assert-current row)
  (if (eq (hub/music-row-origin row) 'liked)
      (hub/spotify-player-add (hub/music-row-metadata row) (eq operation 'play))
    (unless (eq operation 'play) (user-error "Queue rows are already queued; RET plays that occurrence"))
    (emms-stop)
    (hub/music-row-assert-current row)
    (with-current-buffer (hub/music-row-playlist row) (emms-playlist-select (hub/music-row-position row)))
    (emms-start)))

;;;###autoload
(defun hub/music-search ()
  "Search the queue and loaded Liked Songs without previews or fetching.
RET plays an existing queue occurrence, or appends and plays a library-only
song.
M-RET appends from retained Liked Songs, including combined queue results,
without interruption.  Matching Spotify URIs share provenance.
C-g changes nothing."
  (interactive)
  (let* ((library (hub/music-loaded-library-buffer))
	 (loaded (hub/music-search--liked library))
	 (sources (hub/music-search--combine (hub/music-search--queue emms-playlist-buffer) loaded))
	 (queue (car sources))
	 (liked (cdr sources))
	 (coverage (if library
		       (format "Liked: %d/%d loaded" (length loaded) (buffer-local-value 'hub/spotify--total library))
		     "queue only · Liked not loaded"))
	 (hub/music-search--operation 'play)
	 (timer (run-at-time 0.5 0.5 #'hub/music-search--refresh-status)))
    (unwind-protect
	(progn
	  (unless (or queue liked) (user-error "No music loaded; add to the queue or open Liked Songs first"))
	  (save-window-excursion
	    (let* ((choice
		    (minibuffer-with-setup-hook #'hub/music-search--minibuffer-setup
		      (consult--multi
		       (list (hub/music-search--source "Queue" ?q queue)
			     (hub/music-search--source coverage ?l liked))
		       :prompt (format "Music (Queue: %d · %s) — RET Play%s, C-g Back: "
				       (length queue) coverage (if loaded ", M-RET Enqueue liked" ""))
		       :require-match t :sort nil :keymap hub/music-search-map :history 'hub/music-search--history))))
	      (when (plist-get (cdr choice) :match)
		(hub/music-search--act (hub/music-search--target (car choice)) hub/music-search--operation)))))
      (cancel-timer timer))))

(provide 'apps/music-search)
;;; music-search.el ends here
