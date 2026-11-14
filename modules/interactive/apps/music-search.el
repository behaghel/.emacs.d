;;; music-search.el --- Passive search of queue and retained library -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult groups inert rows from EMMS and already loaded Liked Songs.  Only
;; explicit acceptance plays or appends; no previews, fetching or authorization.

;;; Code:
(require 'hub-utils)
(require 'apps/music-row)
(require 'apps/music-queue-actions)
(require 'consult)
(require 'cl-lib)

(defvar hub/music-search--history nil "Native music completion history.")
(defvar hub/music-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'self-insert-command)
    map)
  "Music-specific completion keys composed above the native frontend map.")
(define-key hub/music-search-map (kbd "M-RET") #'hub/music-search--next)
(defvar hub/music-search--operation 'play "Explicit action in the active search.")
(defvar-local hub/music-search--active nil "Non-nil in a music search minibuffer.")
(defvar hub/spotify--tracks)
(defvar hub/spotify--total)
(defvar +vertico-current-arrow)
(defvar vertico--index)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)
(defvar embark-general-map)
(declare-function vertico--exhibit "vertico" ())
(autoload 'hub/spotify-player-add "apps/spotify-player" nil nil)

(defalias 'hub/music-search--queue #'hub/music-row-queue)
(defalias 'hub/music-search--liked #'hub/music-row-liked)
(defalias 'hub/music-search--combine #'hub/music-row-combine)

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

(defun hub/music-search--highlighted-fields (candidate row)
  "Copy CANDIDATE's match faces into ROW's tagged full metadata fields.
Reapply semantic field faces even if a completion style drops input properties."
  (let ((template (hub/music-row-label row)))
    (vconcat
     (mapcar
      (lambda (field)
	(let* ((begin (text-property-any 0 (length template) 'hub/music-row-field field template))
	       (end (next-single-property-change begin 'hub/music-row-field template (length template)))
	       (text (substring template begin end))
	       (position begin))
	  (while (< position (min end (length candidate)))
	    (let* ((next (next-single-property-change position 'face candidate (min end (length candidate))))
		   (face (get-text-property position 'face candidate)))
	      (when face (add-face-text-property (- position begin) (- next begin) face nil text))
	      (setq position next)))
	  text))
      '(title artist album time source)))))

(with-eval-after-load 'vertico
  (cl-defmethod vertico--format-candidate
    (candidate prefix _suffix index _start &context (hub/music-search--active (eql t)))
    "Use native table columns only for retained music candidates."
    (let* ((datum (get-text-property 0 'multi-category candidate))
	   (target (and (eq (car-safe datum) 'hub/music-track) (cdr datum)))
	   (row (and (stringp target) (get-text-property 0 'hub/music-row target))))
      (if (not (hub/music-row-p row)) (cl-call-next-method)
	;; The default normalizer expands space :align-to properties.  Keep
	;; the native table geometry and Vertico's normal selection face.
	(let ((line (concat prefix
			    (hub/music-row-display row (- (window-body-width) (string-width prefix))
						   (hub/music-search--highlighted-fields candidate row))
			    "\n")))
	  (when (= index vertico--index)
	    (add-face-text-property 0 (length line) 'vertico-current 'append line))
	  line)))))

(defun hub/music-search--next ()
  "Accept the current completion with an explicit add-next action."
  (interactive)
  (setq hub/music-search--operation 'next)
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
  (local-set-key (kbd "-") #'self-insert-command)
  (local-set-key (kbd "M-RET") #'hub/music-search--next))

(defun hub/music-search--refresh-status ()
  "Refresh active Vertico annotations without selecting or previewing a track."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (when (and hub/music-search--active (bound-and-true-p vertico-mode) (fboundp 'vertico--exhibit))
	(vertico--exhibit)))))

(defun hub/music-search--act (row operation)
  "Perform explicit OPERATION on validated ROW through existing queue owners."
  (unless (memq operation '(play enqueue next remove)) (user-error "Unknown music action"))
  (cond
   ((eq operation 'remove) (hub/music-row-remove row))
   ((memq operation '(enqueue next))
    (hub/music-row-add row (if (eq operation 'next) 'next 'end)))
   (t
    (hub/music-row-assert-current row)
    (if (eq (hub/music-row-origin row) 'liked)
	(hub/spotify-player-add (hub/music-row-metadata row) t)
      (emms-stop)
      (hub/music-row-assert-current row)
      (with-current-buffer (hub/music-row-playlist row) (emms-playlist-select (hub/music-row-position row)))
      (emms-start)))))

(defun hub/music-search-embark-play (target)
  "Play TARGET's retained occurrence or append and play its retained library row.
This is a non-command Embark action so target properties survive dispatch."
  (hub/music-search--act (hub/music-search--target target) 'play))

(defun hub/music-search-embark-append (target)
  "Append one copy of TARGET's retained queue or Liked item without interruption.
This is a non-command Embark action so target properties survive dispatch."
  (hub/music-search--act (hub/music-search--target target) 'enqueue))

(defun hub/music-search-embark-next (target)
  "Insert TARGET's retained item next, preserving properties through Embark.
This is a non-command action so target identity is not stripped."
  (hub/music-search--act (hub/music-search--target target) 'next))

(defun hub/music-search-embark-remove (target)
  "Remove TARGET's exact retained queue occurrence, never guessing from a URI.
This is a non-command action so target properties survive Embark dispatch."
  (hub/music-search--act (hub/music-search--target target) 'remove))

(defvar hub/music-search-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") '(menu-item "Play" hub/music-search-embark-play))
    (define-key map (kbd "a") '(menu-item "Append one copy" hub/music-search-embark-append))
    map)
  "Music result actions using retained identities, not text input injection.")

(define-key hub/music-search-embark-map (kbd "-") '(menu-item "Remove occurrence" hub/music-search-embark-remove))
(define-key hub/music-search-embark-map (kbd "n") '(menu-item "Add copy next" hub/music-search-embark-next))

(with-eval-after-load 'embark
  (set-keymap-parent hub/music-search-embark-map embark-general-map)
  (setf (alist-get 'hub/music-track embark-keymap-alist) 'hub/music-search-embark-map)
  (setf (alist-get 'hub/music-track embark-default-action-overrides) #'hub/music-search-embark-play))

;;;###autoload
(defun hub/music-search ()
  "Search the queue and loaded Liked Songs without previews or fetching.
RET plays an existing queue occurrence, or appends and plays a library-only
song.
M-RET inserts a copy next from an exact queue occurrence or retained Liked row,
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
		       :prompt (format "Music (Queue: %d · %s) — RET Play%s%s, C-g Back: "
				       (length queue) coverage ", M-RET Next"
				       (if (fboundp 'embark-act) ", C-. Actions" ""))
		       :require-match t :sort nil :keymap hub/music-search-map :history 'hub/music-search--history))))
	      (when (plist-get (cdr choice) :match)
		(hub/music-search--act (hub/music-search--target (car choice)) hub/music-search--operation)))))
      (cancel-timer timer))))

(provide 'apps/music-search)
;;; music-search.el ends here
