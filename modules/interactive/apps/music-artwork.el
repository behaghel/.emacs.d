;;; music-artwork.el --- Bounded, credential-free cover cache -*- lexical-binding: t; -*-

;;; Commentary:
;; Public Spotify image bytes and local cover files only.  No Spotify API,
;; authorization, disk cache, playback, or audio capture.  Failed requests stay
;; failed until explicitly forgotten; paint/clock ticks never retry downloads.

;;; Code:
(require 'hub-utils)
(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'url-http)

(defconst hub/music-artwork--bytes-limit (* 2 1024 1024) "Maximum compressed cover size.")
(defconst hub/music-artwork--entries-limit 12 "Maximum number of memory-cache entries.")
(defconst hub/music-artwork--timeout 10 "Maximum seconds for one image download.")
(defvar hub/music-artwork--cache (make-hash-table :test #'equal) "Volatile cover cache.")

(cl-defstruct (hub/music-artwork--entry (:constructor hub/music-artwork--make))
  "One owned request or inert cached image."
  (state 'pending) data type reason buffer timer waiters (stamp (float-time)))

(declare-function emms-track-get "emms" (track name))
(defvar hub/spotify-buffer-name)
(defvar hub/spotify--tracks)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defun hub/music-artwork-valid-url-p (url)
  "Return non-nil for a credential-free public Spotify image URL."
  (and (stringp url)
       (let ((case-fold-search nil))
	 (string-match-p "\\`https://i\\.scdn\\.co/image/[[:alnum:]]+\\'" url))))

(defun hub/music-artwork-library-state ()
  "Return already-loaded library metadata, without loading or opening its view."
  (when-let* ((buffer (and (boundp 'hub/spotify-buffer-name)
			   (stringp hub/spotify-buffer-name)
			   (get-buffer hub/spotify-buffer-name))))
    (when (local-variable-p 'hub/spotify--tracks buffer)
      (buffer-local-value 'hub/spotify--tracks buffer))))

(defun hub/music-artwork-source (track)
  "Resolve TRACK's cover from retained metadata or known local cover names.
Never fetch metadata, inspect remote files, or change the queue."
  (when track
    (let* ((name (emms-track-get track 'name))
	   (url (or (emms-track-get track 'info-artwork-url)
		    (when (and (stringp name) (string-prefix-p "spotify:track:" name))
		      (let ((raw (cdr (cl-find name (hub/music-artwork-library-state)
					       :key (lambda (entry) (alist-get 'uri (cdr entry)))
					       :test #'equal))))
			(alist-get 'url (car (alist-get 'images (alist-get 'album raw)))))))))
      (cond
       ((and (eq (emms-track-get track 'type) 'url) (stringp name)
	     (string-match-p "\\`spotify:track:[[:alnum:]]\\{22\\}\\'" name)
	     (hub/music-artwork-valid-url-p url))
	(cons 'remote url))
       ((and (eq (emms-track-get track 'type) 'file) (stringp name)
	     (not (file-remote-p name)))
	(when-let* ((file (cl-find-if
			   #'file-regular-p
			   (mapcar (lambda (cover) (expand-file-name cover (file-name-directory (expand-file-name name))))
				   '("cover.jpg" "cover.png" "folder.jpg" "folder.png"
				     "Cover.jpg" "Folder.jpg" "cover_large.jpg" "cover_med.jpg")))))
	  (cons 'file file)))))))

(defun hub/music-artwork-cached (source)
  "Read SOURCE's cache entry without I/O, retries, or cache mutation."
  (gethash source hub/music-artwork--cache))

(defun hub/music-artwork--owned-p (source entry)
  "Return non-nil if pending ENTRY still owns SOURCE."
  (and (eq (hub/music-artwork-cached source) entry)
       (eq (hub/music-artwork--entry-state entry) 'pending)))

(defun hub/music-artwork--cleanup (entry)
  "Release ENTRY's timer and private response buffer, never any audio process."
  (when (timerp (hub/music-artwork--entry-timer entry))
    (cancel-timer (hub/music-artwork--entry-timer entry)))
  (setf (hub/music-artwork--entry-timer entry) nil)
  (when-let* ((buffer (hub/music-artwork--entry-buffer entry)) ((buffer-live-p buffer)))
    (when-let* ((process (get-buffer-process buffer)))
      (set-process-query-on-exit-flag process nil)
      (delete-process process))
    (kill-buffer buffer))
  (setf (hub/music-artwork--entry-buffer entry) nil))

(defun hub/music-artwork-forget (source)
  "Forget SOURCE and cancel its pending download, without invoking subscribers."
  (when-let* ((entry (hub/music-artwork-cached source)))
    (remhash source hub/music-artwork--cache)
    (setf (hub/music-artwork--entry-state entry) 'cancelled
	  (hub/music-artwork--entry-waiters entry) nil)
    (hub/music-artwork--cleanup entry)))

(defun hub/music-artwork-cancel (source)
  "Cancel a pending SOURCE while retaining completed covers for reopening."
  (when-let* ((entry (hub/music-artwork-cached source))
	      ((eq (hub/music-artwork--entry-state entry) 'pending)))
    (hub/music-artwork-forget source)))

(defun hub/music-artwork--finish (source entry bytes reason)
  "Complete owned SOURCE/ENTRY with bounded raster BYTES or fixed REASON."
  (when (hub/music-artwork--owned-p source entry)
    (let (type)
      (condition-case nil
	  (when bytes
	    (unless (<= (string-bytes bytes) hub/music-artwork--bytes-limit) (error "Oversized artwork"))
	    (setq type (image-type-from-data bytes))
	    (unless (memq type '(png jpeg webp)) (error "Unsupported artwork"))
	    (when (display-graphic-p)
	      (let* ((max-image-size 4096) (image (create-image bytes type t)))
		(unwind-protect
		    (let ((size (image-size image t)))
		      (unless (and size (<= (car size) 4096) (<= (cdr size) 4096))
			(error "Oversized raster")))
		  (image-flush image)))))
	(error (setq bytes nil reason "Unsupported or oversized artwork")))
      (setf (hub/music-artwork--entry-state entry) (if bytes 'ready 'failed)
	    (hub/music-artwork--entry-data entry) bytes
	    (hub/music-artwork--entry-type entry) (and bytes type)
	    (hub/music-artwork--entry-reason entry) reason)
      (let ((waiters (hub/music-artwork--entry-waiters entry)))
	(setf (hub/music-artwork--entry-waiters entry) nil)
	(hub/music-artwork--cleanup entry)
	;; Never recurse into a renderer if a transport completes synchronously.
	(dolist (callback waiters) (run-at-time 0 nil callback))))))

(defun hub/music-artwork--received (status source entry)
  "Accept public image response STATUS for owned SOURCE/ENTRY only."
  (when (hub/music-artwork--owned-p source entry)
    (setf (hub/music-artwork--entry-buffer entry) (current-buffer))
    (if (or (plist-get status :error) (plist-get status :redirect)
	    (not (eq url-http-response-status 200)) (not url-http-end-of-headers))
	(hub/music-artwork--finish source entry nil "Artwork download unavailable")
      (let* ((start (save-excursion
		      (goto-char url-http-end-of-headers)
		      ;; URL's marker can precede the final header separator.
		      (when (looking-at "\r?\n") (goto-char (match-end 0)))
		      (point)))
	     (size (- (point-max) start)))
	(if (> size hub/music-artwork--bytes-limit)
	    (hub/music-artwork--finish source entry nil "Artwork exceeds the size limit")
	  (hub/music-artwork--finish source entry
				     (buffer-substring-no-properties start (point-max)) nil))))))

(defun hub/music-artwork--download (source entry)
  "Start one bounded, cookie-free public download for SOURCE and ENTRY."
  (let* ((url-request-method "GET") (url-request-data nil)
	 ;; Explicit empty auth headers disable URL's cached-auth lookup and
	 ;; challenge handling. Public artwork must never authenticate.
	 (url-request-extra-headers '(("Accept-Encoding" . "identity")
				      ("Authorization" . "") ("Proxy-Authorization" . "")))
	 (url-request-noninteractive t) (url-privacy-level 'paranoid)
	 (url-automatic-caching nil) (url-http-attempt-keepalives nil)
	 (buffer (url-retrieve (cdr source) #'hub/music-artwork--received (list source entry) t t)))
    (when (hub/music-artwork--owned-p source entry)
      (unless (buffer-live-p buffer) (error "No artwork response buffer"))
      (setf (hub/music-artwork--entry-buffer entry) buffer)
      (with-current-buffer buffer
	;; url-retrieve does not preserve arbitrary dynamic bindings for callbacks.
	(setq-local url-max-redirections 0 url-automatic-caching nil
		    url-http-attempt-keepalives nil))
      (when-let* ((process (get-buffer-process buffer)) (filter (process-filter process)))
	(set-process-filter
	 process
	 (lambda (process chunk)
	   (when (hub/music-artwork--owned-p source entry)
	     (if (> (+ (with-current-buffer buffer (buffer-size)) (string-bytes chunk))
		    (+ hub/music-artwork--bytes-limit 16384))
		 (hub/music-artwork--finish source entry nil "Artwork exceeds the size limit")
	       (funcall filter process chunk))))))
      (setf (hub/music-artwork--entry-timer entry)
	    (run-at-time hub/music-artwork--timeout nil
			 (lambda () (hub/music-artwork--finish source entry nil "Artwork download timed out")))))))

(defun hub/music-artwork-request (source callback)
  "Ensure SOURCE is cached; notify CALLBACK only on pending download completion.
Completed and failed entries are reused without retries. Local reads complete
synchronously; callers paint their returned entry, not a recursive notification."
  (unless (or (and (eq (car source) 'remote) (hub/music-artwork-valid-url-p (cdr source)))
	      (and (eq (car source) 'file) (stringp (cdr source))
		   (not (file-remote-p (cdr source)))
		   (string-match-p "\\.\\(?:png\\|jpe?g\\|webp\\)\\'" (cdr source))))
    (error "Unsupported artwork location"))
  (let ((entry (hub/music-artwork-cached source)))
    (if entry
	(when (eq (hub/music-artwork--entry-state entry) 'pending)
	  (cl-pushnew callback (hub/music-artwork--entry-waiters entry)))
      (when (>= (hash-table-count hub/music-artwork--cache) hub/music-artwork--entries-limit)
	(let (oldest oldest-time)
	  (maphash (lambda (key value)
		     (when (or (not oldest-time) (< (hub/music-artwork--entry-stamp value) oldest-time))
		       (setq oldest key oldest-time (hub/music-artwork--entry-stamp value))))
		   hub/music-artwork--cache)
	  (hub/music-artwork-forget oldest)))
      (setq entry (hub/music-artwork--make))
      (puthash source entry hub/music-artwork--cache)
      (condition-case nil
	  (if (eq (car source) 'remote)
	      (progn (setf (hub/music-artwork--entry-waiters entry) (list callback))
		     (hub/music-artwork--download source entry))
	    (let ((attributes (file-attributes (cdr source))))
	      (unless (and attributes (<= (file-attribute-size attributes) hub/music-artwork--bytes-limit))
		(error "Cover unavailable"))
	      (with-temp-buffer
		(set-buffer-multibyte nil)
		(insert-file-contents-literally (cdr source) nil 0
						(min (file-attribute-size attributes)
						     (1+ hub/music-artwork--bytes-limit)))
		(hub/music-artwork--finish source entry (buffer-string) nil))))
	(error (hub/music-artwork--finish source entry nil "Artwork unavailable"))))
    entry))

(provide 'apps/music-artwork)
;;; music-artwork.el ends here
