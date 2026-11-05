;;; spotify.el --- Read-only Spotify Liked Songs view -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse a paginated Spotify library in the shared bottom slot.  This module
;; appends explicit selections to the mixed EMMS queue, optionally playing the
;; appended occurrence.  It never modifies the remote Spotify library.

;;; Code:

(require 'hub-utils)
(require 'apps/spotify-auth)
(require 'tabulated-list)
(require 'cl-lib)
(require 'subr-x)

(defcustom hub/spotify-buffer-name "*Spotify Liked Songs*"
  "Buffer name for the read-only Spotify library."
  :type 'string
  :group 'hub/spotify)

(autoload 'hub/spotify-player-add "apps/spotify-player"
  "Append Spotify metadata to EMMS, optionally playing that occurrence.")

(defvar-local hub/spotify--tracks nil
  "Raw track metadata indexed by stable table row IDs, kept only in memory.")

(defvar-local hub/spotify--generation 0
  "Generation identifying the current library request.")
(defvar-local hub/spotify--busy nil
  "Non-nil during a library request.")
(defvar-local hub/spotify--total nil
  "Total number of saved songs, or nil before a successful response.")
(defvar-local hub/spotify--offset 0
  "Number of rows fetched so far.")
(defvar-local hub/spotify--retry-at 0
  "Earliest allowed retry after Spotify rate limiting.")
(defvar-local hub/spotify--source nil
  "Editing source of this library view.")

(declare-function evil-define-key "evil-core" (state keymaps &rest bindings))
(declare-function context-panels-mode "context-panels" (&optional arg))
(declare-function context-panels-register-provider "context-panels" (provider))
(declare-function context-panels-adopt-bottom-view-buffer
		  "context-panels" (buffer view-id &optional source-buffer select))
(declare-function context-panels-close-bottom-view "context-panels" ())
(declare-function hub/emms-player--source "apps/emms-player" ())

(defun hub/spotify--text (value fallback)
  "Return VALUE as safe single-line text, or FALLBACK."
  (if (and (stringp value) (not (string-empty-p value)))
      (replace-regexp-in-string "[[:cntrl:]]" " " value) fallback))

(defun hub/spotify--row (item index)
  "Convert saved ITEM into an inert table row identified by INDEX."
  (let* ((track (alist-get 'track item))
	 (duration (alist-get 'duration_ms track))
	 (artists (mapconcat (lambda (artist) (hub/spotify--text (alist-get 'name artist) "Unknown artist"))
			     (alist-get 'artists track) ", ")))
    (list index
	  (vector (hub/spotify--text (alist-get 'name track) "Unavailable track")
		  (if (string-empty-p artists) "Unknown artist" artists)
		  (hub/spotify--text (alist-get 'name (alist-get 'album track)) "Unknown album")
		  (if (and (numberp duration) (> duration 0))
		      (format "%d:%02d" (/ (floor duration) 60000) (% (/ (floor duration) 1000) 60))
		    "--:--")))))

(defun hub/spotify--render ()
  "Render loaded rows without selecting a window."
  (tabulated-list-print t)
  (unless tabulated-list-entries
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (if (equal hub/spotify--total 0) "No Liked Songs yet.\n"
		"No results loaded. Complete authorization in your browser if requested.\n")))))

(defun hub/spotify--received (offset status json retry)
  "Render a page at OFFSET with HTTP STATUS, JSON and RETRY delay."
  (setq hub/spotify--busy nil
	json (and (listp json) json))
  (cond
   ((eq status 429)
    (let ((seconds (max 1 (or retry 30))))
      (setq hub/spotify--retry-at (+ (float-time) seconds)
	    header-line-format (format "Liked Songs — Rate limited; wait %ds, then gr to retry · q close" seconds))))
   ((memq status '(401 403))
    (setq header-line-format "Liked Songs — Access denied; check app user access and M-x hub/spotify-connect to reconnect"))
   ((not (and (eq status 200) (integerp (alist-get 'total json))
	      (>= (alist-get 'total json) 0) (assq 'items json)
	      (listp (alist-get 'items json))))
    (setq header-line-format "Liked Songs — Request failed; gr to retry · q close"))
   (t
    (condition-case nil
	(let* ((items (alist-get 'items json))
	       (rows (cl-loop for item in items for index from offset
			      collect (hub/spotify--row item index)))
	       (tracks (cl-loop for item in items for index from offset
				collect (cons index (alist-get 'track item)))))
	  (setq hub/spotify--tracks (if (zerop offset) tracks (append hub/spotify--tracks tracks))
		tabulated-list-entries (if (zerop offset) rows (append tabulated-list-entries rows))
		hub/spotify--offset (+ offset (length items))
		hub/spotify--total (alist-get 'total json)
		header-line-format
		(format "Liked Songs — %d of %d loaded · a enqueue · RET play · %sgr refresh · q close"
			hub/spotify--offset hub/spotify--total
			(if (< hub/spotify--offset hub/spotify--total) "+ more · " ""))))
      (error (setq header-line-format "Liked Songs — Invalid response; gr to retry · q close")))))
  (hub/spotify--render))

(defun hub/spotify--fetch (offset)
  "Fetch the library page at OFFSET into the current buffer."
  (when (> hub/spotify--retry-at (float-time))
    (user-error "Spotify rate limited; wait %ds before retrying" (ceiling (- hub/spotify--retry-at (float-time)))))
  (let ((buffer (current-buffer)) (generation (cl-incf hub/spotify--generation)))
    (setq hub/spotify--busy t
	  header-line-format "Liked Songs — Connecting / loading… · q close")
    (hub/spotify--render)
    (hub/spotify-auth-with-token
     (lambda (token error)
       (when (and (buffer-live-p buffer)
		  (= generation (buffer-local-value 'hub/spotify--generation buffer)))
	 (if error
	     (with-current-buffer buffer
	       (setq hub/spotify--busy nil header-line-format (concat "Liked Songs — " error)))
	   (hub/spotify-auth--request
	    (format "https://api.spotify.com/v1/me/tracks?limit=50&offset=%d" offset)
	    "GET" `(("Authorization" . ,(concat "Bearer " token))) nil
	    (lambda (status json retry)
	      (when (and (buffer-live-p buffer)
			 (= generation (buffer-local-value 'hub/spotify--generation buffer)))
		(with-current-buffer buffer
		  (hub/spotify--received offset status json retry)))))))))))

(defun hub/spotify-add-selected (&optional play)
  "Append the selected row to EMMS, optionally PLAY it without clearing the queue."
  (interactive)
  (let* ((track (cdr (assoc (tabulated-list-get-id) hub/spotify--tracks)))
	 (uri (alist-get 'uri track)))
    (unless (and (stringp uri)
		 (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" uri)
		 (or (not (assq 'is_playable track)) (alist-get 'is_playable track)))
      (user-error "This row has no playable Spotify track"))
    (hub/spotify-player-add track play))
  nil)

(defun hub/spotify-play-selected ()
  "Append and play the selected row through EMMS and its headless backend."
  (interactive)
  (hub/spotify-add-selected t))

(defun hub/spotify-load-more ()
  "Append the next page of saved songs, only when explicitly requested."
  (interactive)
  (when hub/spotify--busy (user-error "Spotify is still loading"))
  (unless hub/spotify--total (user-error "Refresh Liked Songs first"))
  (unless (< hub/spotify--offset hub/spotify--total) (user-error "All saved songs are loaded"))
  (hub/spotify--fetch hub/spotify--offset))

(defun hub/spotify-refresh ()
  "Refresh the library without changing playback."
  (interactive)
  (hub/spotify--fetch 0))

(defun hub/spotify-close ()
  "Close the library without stopping playback."
  (interactive)
  (context-panels-close-bottom-view))

(define-derived-mode hub/spotify-library-mode tabulated-list-mode "Spotify Library"
  "Read-only library with explicit loading and no playback side effects."
  (setq-local tabulated-list-format [("Title" 32 t) ("Artist" 24 t) ("Album" 24 t) ("Time" 7 nil)]
	      tabulated-list-use-header-line nil
	      truncate-lines t)
  (tabulated-list-init-header))

(define-key hub/spotify-library-mode-map (kbd "a") #'hub/spotify-add-selected)
(define-key hub/spotify-library-mode-map (kbd "RET") #'hub/spotify-play-selected)
(define-key hub/spotify-library-mode-map (kbd "g") #'hub/spotify-refresh)
(define-key hub/spotify-library-mode-map (kbd "q") #'hub/spotify-close)
(define-key hub/spotify-library-mode-map (kbd "+") #'hub/spotify-load-more)
(with-eval-after-load 'evil
  (evil-define-key 'normal hub/spotify-library-mode-map
		   (kbd "a") #'hub/spotify-add-selected
		   (kbd "RET") #'hub/spotify-play-selected
		   (kbd "+") #'hub/spotify-load-more
		   (kbd "gr") #'hub/spotify-refresh
		   (kbd "q") #'hub/spotify-close))

(defun hub/spotify--views (source)
  "Offer the live library to its editing SOURCE."
  (when-let* ((buffer (get-buffer hub/spotify-buffer-name)))
    (when (eq source (buffer-local-value 'hub/spotify--source buffer))
      (list (list :id 'spotify-library :label "Liked Songs"
		  :buffer-name hub/spotify-buffer-name :mode #'hub/spotify-library-mode)))))

;;;###autoload
(defun hub/spotify-liked-songs ()
  "Open Liked Songs in the shared bottom slot, authorizing when necessary."
  (interactive)
  (require 'context-panels)
  (require 'apps/emms-player)
  (let ((source (hub/emms-player--source))
	(buffer (get-buffer-create hub/spotify-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'hub/spotify-library-mode) (hub/spotify-library-mode))
      (setq hub/spotify--source source))
    (with-current-buffer source
      (context-panels-register-provider
       (list :name 'spotify-library :icon "music" :collect-bottom-views #'hub/spotify--views)))
    (context-panels-mode 1)
    (context-panels-adopt-bottom-view-buffer buffer 'spotify-library source t)
    (unless (or hub/spotify--busy
		(and hub/spotify--total
		     (or (zerop hub/spotify--total) hub/spotify--tracks)))
      (hub/spotify-refresh))))

(provide 'apps/spotify)
;;; spotify.el ends here
