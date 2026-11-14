;;; music-queue-actions.el --- Explicit edits to the authoritative EMMS queue -*- lexical-binding: t; -*-

;;; Commentary:
;; Insert copies and remove exact native occurrences.  These commands never
;; maintain a second queue, resolve removals by URI, or move original additions.

;;; Code:
(require 'hub-utils)
(require 'apps/music-row)
(require 'emms-playlist-mode)
(require 'cl-lib)

(defvar hub/spotify--tracks)

(autoload 'hub/spotify-player-add "apps/spotify-player" nil nil)
(autoload 'hub/emms-register-local-players "apps/emms-local" nil nil)

(defun hub/music-queue-insert (track &optional placement play)
  "Insert TRACK into EMMS at PLACEMENT, either `end' or `next'.
Next means after the current selection, or first when none exists.  Preserve
playback and the existing occurrence unless PLAY explicitly requests
playing the new occurrence."
  (unless (memq placement '(nil end next)) (user-error "Unknown queue insertion placement"))
  (unless (buffer-live-p emms-playlist-buffer)
    (emms-playlist-set-playlist-buffer (emms-playlist-new)))
  (let (position)
    (with-current-buffer emms-playlist-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (let ((selected (and (markerp emms-playlist-selected-marker)
			       (marker-position emms-playlist-selected-marker)
			       (emms-playlist-track-at emms-playlist-selected-marker))))
	    (if (eq placement 'next)
		(if selected
		    (progn
		      (goto-char emms-playlist-selected-marker)
		      (condition-case nil (emms-playlist-next) (error (goto-char (point-max)))))
		  (goto-char (point-min)))
	      (goto-char (point-max)))
	    (setq position (point))
	    (emms-playlist-insert-track track)
	    (when play (emms-stop))
	    (when (or play (not selected)) (emms-playlist-select position))))))
    (when play (emms-start))
    position))

(defun hub/music-row-add (row placement)
  "Add one copy of validated ROW at PLACEMENT without interrupting playback."
  (unless (memq placement '(end next)) (user-error "Unknown queue insertion placement"))
  (when (and (eq placement 'end) (hub/music-row-liked-row row))
    (setq row (hub/music-row-liked-row row)))
  (hub/music-row-assert-current row)
  (if (eq (hub/music-row-origin row) 'queue)
      (hub/music-queue-insert (hub/music-row-track row) placement)
    (if (eq placement 'next)
	(hub/spotify-player-add (hub/music-row-metadata row) nil 'next)
      (hub/spotify-player-add (hub/music-row-metadata row) nil))))

(defun hub/music-row-remove (row)
  "Remove ROW's exact queue occurrence, advancing only if it was playing.
Paused and stopped selections select a successor without starting it.  Stop
acknowledgement and source freshness are required before any deletion."
  (unless (eq (hub/music-row-origin row) 'queue)
    (user-error "Removal needs an exact queue occurrence, not a library record"))
  (hub/music-row-assert-current row)
  (let* ((playlist (hub/music-row-playlist row))
	 (position (hub/music-row-position row))
	 (selected (with-current-buffer playlist (emms-playlist-selected-track-at-p position)))
	 (active (and selected emms-player-playing-p))
	 (continue (and active (not emms-player-paused-p)))
	 next)
    (when active
      (emms-stop)
      (hub/music-row-assert-current row)
      (unless (and (not emms-player-playing-p)
		   (with-current-buffer playlist (emms-playlist-selected-track-at-p position)))
	(user-error "Playback selection changed; select the occurrence again")))
    (with-current-buffer playlist
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char position)
	  (emms-playlist-delete-track)
	  ;; Native deletion removes the track property span, not its separator.
	  (when (and (eolp) (not (eobp)))
	    (let ((inhibit-read-only t)) (delete-char 1)))
	  (when selected
	    (setq emms-player-paused-p nil)
	    (let* ((rows (hub/music-row-queue playlist))
		   (successor (cl-find-if (lambda (item) (>= (hub/music-row-position item) position)) rows))
		   (selection (or successor (car (last rows)))))
	      (setq next successor)
	      (if selection
		  (emms-playlist-select (hub/music-row-position selection))
		(when (overlayp emms-playlist-mode-selected-overlay)
		  (delete-overlay emms-playlist-mode-selected-overlay))
		(setq emms-playlist-mode-selected-overlay nil)
		(when (markerp emms-playlist-selected-marker)
		  (set-marker emms-playlist-selected-marker nil))
		(setq emms-playlist-selected-marker nil)))))))
    (when (and continue next)
      (hub/music-row-assert-current next)
      (unless (and (not emms-player-playing-p)
		   (with-current-buffer playlist
		     (emms-playlist-selected-track-at-p (hub/music-row-position next))))
	(user-error "Playback selection changed; play the next occurrence explicitly"))
      (emms-start))))

;;;###autoload
(defun hub/music-remove ()
  "Remove the focused exact queue occurrence, advancing only if it was playing."
  (interactive)
  (hub/music-row-remove (hub/music-queue-action-target)))

(defun hub/music-queue-action-target ()
  "Return the exact music row focused by the current Player, Queue or library."
  (or
   (cond
    ((derived-mode-p 'hub/spotify-library-mode)
     (let* ((id (tabulated-list-get-id)) (raw (cdr (assoc id hub/spotify--tracks))))
       (when raw (hub/music-row-from-liked raw id (current-buffer)))))
    ((derived-mode-p 'emms-playlist-mode)
     (let ((position (car (emms-property-region (line-beginning-position) 'emms-track))))
       (cl-find position (hub/music-row-queue (current-buffer)) :key #'hub/music-row-position)))
    ((derived-mode-p 'hub/emms-player-mode)
     (when (buffer-live-p emms-playlist-buffer)
       (with-current-buffer emms-playlist-buffer
	 (when (markerp emms-playlist-selected-marker)
	   (cl-find (marker-position emms-playlist-selected-marker)
		    (hub/music-row-queue (current-buffer)) :key #'hub/music-row-position))))))
   (user-error "No music item is focused")))

;;;###autoload
(defun hub/music-append ()
  "Append one copy of the focused music item at the end, without playing it."
  (interactive)
  (hub/music-row-add (hub/music-queue-action-target) 'end))

;;;###autoload
(defun hub/music-add-next ()
  "Insert one copy of the focused music item next, without moving or playing it."
  (interactive)
  (hub/music-row-add (hub/music-queue-action-target) 'next))

(defun hub/music-queue--file-candidate-p (file)
  "Keep local directories and FILE names matching registered player extensions.
Inspect declared filename regexps only; never construct tracks or probe media."
  (and (not (file-remote-p file))
       (or (file-directory-p file)
	   (and (file-regular-p file) (file-readable-p file)
		(cl-some (lambda (player)
			   (let ((regexp (emms-player-get player 'regex)))
			     (and (stringp regexp) (string-match-p regexp file))))
			 emms-player-list)))))

;;;###autoload
(defun hub/music-add-file (file)
  "Append the local audio FILE chosen by path, without starting playback."
  (interactive
   (progn
     (hub/emms-register-local-players)
     (list (read-file-name "Append file to queue: " nil nil t nil
			   #'hub/music-queue--file-candidate-p))))
  (setq file (expand-file-name file))
  (unless (and (not (file-remote-p file)) (file-regular-p file) (file-readable-p file))
    (user-error "Choose a readable local file, not a directory or remote path"))
  (hub/emms-register-local-players)
  (let ((track (emms-track 'file file)))
    (unless (emms-player-for track) (user-error "No registered player supports this file"))
    (hub/music-queue-insert track 'end)))

(provide 'apps/music-queue-actions)
;;; music-queue-actions.el ends here
