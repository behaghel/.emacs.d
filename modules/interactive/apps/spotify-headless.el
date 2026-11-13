;;; spotify-headless.el --- Private headless receiver lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;; An explicitly started, loopback-only go-librespot instance.  Its credentials
;; are encrypted in pass and materialized only in an owned private /tmp directory.
;; Daemon output is discarded: pairing links are read from its local API, not logs.

;;; Code:

(require 'hub-utils)
(require 'apps/spotify-auth)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(require 'auth-source-pass)
(require 'browse-url)

(defgroup hub/spotify-headless nil
  "Private Spotify receiver controlled by EMMS."
  :group 'multimedia)
(defcustom hub/spotify-headless-program "go-librespot"
  "Receiver binary provisioned by devenv."
  :type 'file :group 'hub/spotify-headless)
(defcustom hub/spotify-headless-pass-entry "dev/spotify/emacs-music/headless"
  "Pass entry holding the receiver's encrypted stored credentials."
  :type 'string :group 'hub/spotify-headless)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defvar hub/spotify-headless--process nil "Owned receiver process, never Spotify desktop.")
(defvar hub/spotify-headless--directory nil "Owned temporary plaintext runtime directory.")
(defvar hub/spotify-headless--port nil "Loopback API port reserved for this receiver.")
(defvar hub/spotify-headless--key nil "Memory-only API key for this receiver instance.")
(defvar hub/spotify-headless--verified nil "Non-nil after unauthenticated access is rejected.")
(defvar hub/spotify-headless--ready nil "Non-nil after authentication and encrypted persistence.")
(defvar hub/spotify-headless--timer nil "Pairing/readiness probe timer.")
(defvar hub/spotify-headless--probing nil "Non-nil while a readiness probe is in flight.")
(defvar hub/spotify-headless--saved nil "Hash of the last encrypted credential record.")
(defvar hub/spotify-headless--pair-url nil "Private pairing URL already opened, never logged.")
(defvar hub/spotify-headless--deadline nil "Deadline for explicit authentication.")

(defun hub/spotify-headless--private-build-p ()
  "Check the private-API build before reading or materializing credentials."
  (condition-case nil
      (with-temp-buffer
	(and (zerop (call-process hub/spotify-headless-program nil (list (current-buffer) nil) nil
				  "--emms-api-check"))
	     (equal (string-trim (buffer-string)) "emms-private-api-v1")))
    (error nil)))

(defun hub/spotify-headless--configuration (port)
  "Return the receiver configuration for loopback PORT."
  `((device_name . "Emacs Music") (device_type . "computer")
    (log_level . "error") (zeroconf_enabled . :false) (mpris_enabled . :false)
    (disable_autoplay . t) (crossfade_duration . 0)
    (cache . ((enabled . :false))) (metadata . ((enabled . :false)))
    (credentials . ((type . "device_auth")))
    (server . ((enabled . t) (address . "127.0.0.1") (port . ,port) (allow_origin . "")))))

(defun hub/spotify-headless-ready-p ()
  "Return non-nil only for this process's authenticated, owned receiver."
  (and hub/spotify-headless--ready hub/spotify-headless--verified hub/spotify-headless--key
       (process-live-p hub/spotify-headless--process)))

(defun hub/spotify-headless-request (method path data &optional callback)
  "Request METHOD at local PATH with JSON DATA, never library credentials.
With CALLBACK, asynchronously deliver HTTP status and decoded JSON.  Without
CALLBACK, perform a bounded synchronous request and return (STATUS . JSON)."
  (unless (and (integerp hub/spotify-headless--port)
	       (or hub/spotify-headless--key (and (equal method "GET") (equal path "/")))
	       (member path '("/" "/auth/code" "/player/play" "/player/pause" "/player/resume"
			      "/player/stop" "/player/seek" "/player/repeat_context" "/player/repeat_track"
			      "/player/shuffle_context")))
    (user-error "Invalid local receiver endpoint"))
  (let* ((url (format "http://127.0.0.1:%d%s" hub/spotify-headless--port path))
	 (url-proxy-services nil)
	 (headers (append '(("Content-Type" . "application/json") ("Authorization" . ""))
			  (when hub/spotify-headless--key
			    (list (cons "X-Emacs-Spotify-Key" hub/spotify-headless--key)))))
	 (body (and data (json-serialize data))))
    (if callback
	(progn
	  ;; Reuse the tested cookie-free, non-redirecting transport, not OAuth.
	  (hub/spotify-auth--request url method headers body
				     (lambda (status json _retry) (funcall callback status json)))
	  nil)
      (let ((url-request-method method) (url-request-extra-headers headers)
	    (url-request-data body) (url-max-redirections 0) (url-debug nil)
	    (url-automatic-caching nil)
	    response)
	(unwind-protect
	    (condition-case nil
		(progn
		  (setq response (url-retrieve-synchronously url t t 2))
		  (when (buffer-live-p response)
		    (with-current-buffer response
		      (cons url-http-response-status
			    (condition-case nil
				(progn (goto-char url-http-end-of-headers)
				       (json-parse-buffer :object-type 'alist :null-object nil :false-object nil))
			      (error nil))))))
	      (error nil))
	  (when (buffer-live-p response) (kill-buffer response)))))))

(defun hub/spotify-headless--write-private (file text)
  "Write private TEXT into FILE in the owned runtime directory."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file (insert text)))
  (set-file-modes file #o600))

(defun hub/spotify-headless--record (text)
  "Extract only reusable credentials from private state TEXT, or return nil."
  (condition-case nil
      (let* ((json (json-parse-string text :object-type 'alist :null-object nil))
	     (credentials (alist-get 'credentials json))
	     (username (alist-get 'username credentials)) (data (alist-get 'data credentials)))
	(when (and (stringp username) (stringp data) (not (string-empty-p data)))
	  (json-serialize `((device_id . ,(alist-get 'device_id json))
			    (credentials . ((username . ,username) (data . ,data)))))))
    (error nil)))

(defun hub/spotify-headless--write-pass (record)
  "Encrypt RECORD through stdin, never process arguments or logs."
  (with-temp-buffer
    (insert record)
    (let ((coding-system-for-write 'utf-8-unix)
	  (process-environment
	   (cons (concat "PASSWORD_STORE_DIR=" (expand-file-name auth-source-pass-filename))
		 process-environment)))
      (unless (zerop (call-process-region (point-min) (point-max) "pass" nil '(nil nil) nil
					  "insert" "--force" "--multiline" "--"
					  hub/spotify-headless-pass-entry))
	(error "Cannot encrypt receiver credentials")))))

(defun hub/spotify-headless--save ()
  "Encrypt changed credentials; return non-nil for a valid record."
  (when-let* ((directory hub/spotify-headless--directory)
	      (file (expand-file-name "state.json" directory))
	      (_ (file-exists-p file))
	      (record (with-temp-buffer (insert-file-contents file)
					(hub/spotify-headless--record (buffer-string)))))
    (set-file-modes file #o600)
    (let ((digest (secure-hash 'sha256 record)))
      (unless (equal digest hub/spotify-headless--saved)
	(hub/spotify-headless--write-pass record)
	(setq hub/spotify-headless--saved digest))
      t)))

(defun hub/spotify-headless--cleanup (&optional skip-save)
  "Release owned resources; SKIP-SAVE after an encryption failure."
  (let ((process hub/spotify-headless--process) (directory hub/spotify-headless--directory))
    (setq hub/spotify-headless--process nil hub/spotify-headless--ready nil
	  hub/spotify-headless--probing nil hub/spotify-headless--verified nil)
    (when (timerp hub/spotify-headless--timer) (cancel-timer hub/spotify-headless--timer))
    (setq hub/spotify-headless--timer nil)
    (unwind-protect
	(progn
	  (when (and process (process-live-p process)) (delete-process process))
	  (unless skip-save
	    (condition-case nil (hub/spotify-headless--save)
	      (error (message "Receiver credentials could not be saved; unlock GPG and pair again")))))
      (setq hub/spotify-headless--directory nil hub/spotify-headless--port nil
	    hub/spotify-headless--pair-url nil hub/spotify-headless--saved nil
	    hub/spotify-headless--key nil)
      (when (and directory (file-directory-p directory)) (delete-directory directory t))
      (when (fboundp 'hub/spotify-player-receiver-lost) (hub/spotify-player-receiver-lost)))))

(defun hub/spotify-headless--pairing (json)
  "Open a trusted Spotify HTTPS pairing link from JSON without echoing it."
  (let* ((url (and (listp json) (alist-get 'url json)))
	 (parsed (and (stringp url) (url-generic-parse-url url))))
    (when (and parsed (equal (url-type parsed) "https")
	       (member (url-host parsed) '("spotify.com" "www.spotify.com" "accounts.spotify.com"))
	       (not (equal url hub/spotify-headless--pair-url)))
      (setq hub/spotify-headless--pair-url url)
      (browse-url url)
      (message "Complete headless Spotify pairing in your browser"))))

(defun hub/spotify-headless--probe ()
  "Verify the API guard before probing authentication or opening a browser."
  (when (and (process-live-p hub/spotify-headless--process) (not hub/spotify-headless--probing))
    (cond
     (hub/spotify-headless--verified (hub/spotify-headless--probe-ready))
     ((> (float-time) hub/spotify-headless--deadline)
      (hub/spotify-headless--cleanup)
      (message "Private receiver API timed out; connect again"))
     (t
      (let ((process hub/spotify-headless--process) (hub/spotify-headless--key nil))
	(setq hub/spotify-headless--probing t)
	(hub/spotify-headless-request
	 "GET" "/" nil
	 (lambda (status _json)
	   (when (eq process hub/spotify-headless--process)
	     (setq hub/spotify-headless--probing nil)
	     (cond ((eq status 403) (setq hub/spotify-headless--verified t))
		   (status (hub/spotify-headless--cleanup t)
			   (message "Refusing an unauthenticated Spotify API; use the devenv build")))))))))))

(defun hub/spotify-headless--probe-ready ()
  "Probe one verified receiver without starting playback or overlapping probes."
  (when (and (process-live-p hub/spotify-headless--process) (not hub/spotify-headless--probing))
    (if (> (float-time) hub/spotify-headless--deadline)
	(progn (hub/spotify-headless--cleanup) (message "Receiver pairing timed out; connect again"))
      (let ((process hub/spotify-headless--process))
	(setq hub/spotify-headless--probing t)
	(hub/spotify-headless-request
	 "GET" "/" nil
	 (lambda (status json)
	   (when (eq process hub/spotify-headless--process)
	     (if (and (eq status 200) (listp json) (eq (alist-get 'playback_ready json) t))
		 (condition-case nil
		     (progn
		       (unless (hub/spotify-headless--save) (error "Receiver credentials are missing"))
		       (when (process-live-p process)
			 (setq hub/spotify-headless--ready t hub/spotify-headless--probing nil)
			 (when (timerp hub/spotify-headless--timer) (cancel-timer hub/spotify-headless--timer))
			 (setq hub/spotify-headless--timer nil)
			 (message "Headless Spotify connected; use a to enqueue or RET to play in Liked Songs")))
		   (error (hub/spotify-headless--cleanup t)
			  (message "Cannot save receiver credentials in pass; unlock GPG and reconnect")))
	       (hub/spotify-headless-request
		"GET" "/auth/code" nil
		(lambda (_status auth)
		  (when (eq process hub/spotify-headless--process)
		    (setq hub/spotify-headless--probing nil)
		    (hub/spotify-headless--pairing auth))))))))))))

;;;###autoload
(defun hub/spotify-headless-connect (&optional fresh)
  "Start and pair the receiver; with FRESH ignore cached credentials."
  (interactive "P")
  (when fresh (hub/spotify-headless--cleanup))
  (unless (process-live-p hub/spotify-headless--process)
    (hub/spotify-headless--cleanup)
    (unless (executable-find hub/spotify-headless-program)
      (user-error "go-librespot is missing; launch Emacs from devenv or set hub/spotify-headless-program"))
    (unless (hub/spotify-headless--private-build-p)
      (user-error "Use the private-API go-librespot build from this devenv"))
    (condition-case nil
	(let ((record (unless fresh
			(when-let* ((text (auth-source-pass-get 'secret hub/spotify-headless-pass-entry)))
			  (hub/spotify-headless--record text))))
	      (reservation (make-network-process :name "Spotify port reservation" :server t
						 :host "127.0.0.1" :family 'ipv4 :service 0 :noquery t)))
	  (unwind-protect (setq hub/spotify-headless--port (process-contact reservation :service))
	    (delete-process reservation))
	  (setq hub/spotify-headless--directory (make-temp-file "/tmp/emms-spotify-" t)
		hub/spotify-headless--ready nil hub/spotify-headless--saved nil
		hub/spotify-headless--key (hub/spotify-auth--random)
		hub/spotify-headless--verified nil
		hub/spotify-headless--probing nil hub/spotify-headless--pair-url nil
		hub/spotify-headless--deadline (+ (float-time) 300))
	  (set-file-modes hub/spotify-headless--directory #o700)
	  (hub/spotify-headless--write-private
	   (expand-file-name "config.yml" hub/spotify-headless--directory)
	   (json-serialize (hub/spotify-headless--configuration hub/spotify-headless--port)))
	  (when record
	    (hub/spotify-headless--write-private (expand-file-name "state.json" hub/spotify-headless--directory) record)
	    (setq hub/spotify-headless--saved (secure-hash 'sha256 record)))
	  (let ((process-environment
		 (cons (concat "EMMS_SPOTIFY_API_KEY=" hub/spotify-headless--key) process-environment)))
	    (setq hub/spotify-headless--process
		  (make-process :name "EMMS Spotify receiver" :buffer nil :filter #'ignore
				:stderr nil :connection-type 'pipe :noquery t
				:command (list hub/spotify-headless-program "--config_dir" hub/spotify-headless--directory)
				:sentinel (lambda (child _event)
					    (when (and (eq child hub/spotify-headless--process)
						       (memq (process-status child) '(exit signal failed)))
					      (hub/spotify-headless--cleanup)
					      (message "Headless Spotify exited; reconnect to retry"))))))
	  (setq hub/spotify-headless--timer (run-at-time 0.2 0.5 #'hub/spotify-headless--probe)))
      (quit (hub/spotify-headless--cleanup t) (signal 'quit nil))
      (error (hub/spotify-headless--cleanup t)
	     (user-error "Cannot start headless Spotify; check pass and the receiver binary"))))
  nil)

;;;###autoload
(defun hub/spotify-headless-disconnect ()
  "Stop only the owned headless receiver and remove its private runtime state."
  (interactive)
  (hub/spotify-headless--cleanup)
  nil)

(add-hook 'kill-emacs-hook #'hub/spotify-headless-disconnect)

(provide 'apps/spotify-headless)
;;; spotify-headless.el ends here
