;;; spotify-auth.el --- Private Spotify library authorization -*- lexical-binding: t; -*-

;;; Commentary:
;; PKCE authorization for a public native client.  Client IDs and encrypted
;; refresh credentials live in pass; access tokens exist only in memory.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url)
(require 'url-http)
(require 'gnutls)
(require 'auth-source-pass)
(require 'browse-url)

(defgroup hub/spotify nil
  "Personal Spotify library access."
  :group 'multimedia)

(defcustom hub/spotify-client-id-entry "dev/spotify/emacs-music/client-id"
  "Pass entry containing the public Spotify client ID on its first line."
  :type 'string
  :group 'hub/spotify)

(defcustom hub/spotify-token-entry "dev/spotify/emacs-music/oauth"
  "Pass entry containing encrypted refresh authorization as one-line JSON."
  :type 'string
  :group 'hub/spotify)

(defcustom hub/spotify-callback-port 8765
  "Loopback callback port, matching the redirect URI registered with Spotify."
  :type 'integer
  :group 'hub/spotify)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defvar hub/spotify-auth--context nil
  "Current in-memory authorization context; never serialize it.")

(cl-defstruct (hub/spotify-auth--session (:constructor hub/spotify-auth--session-create))
  "Authorization for one captured client and pass entry."
  client-id client-entry token-entry access-token expires-at refresh-token
  waiters busy listener clients timer state verifier redirect)

(defun hub/spotify-auth--challenge (verifier)
  "Return the unpadded S256 PKCE challenge for VERIFIER."
  (base64url-encode-string (secure-hash 'sha256 verifier nil nil t) t))

(defun hub/spotify-auth--form (pairs)
  "Encode string PAIRS as form data."
  (mapconcat (lambda (pair)
	       (concat (url-hexify-string (car pair)) "="
		       (url-hexify-string (cdr pair)))) pairs "&"))

(defun hub/spotify-auth--request (url method headers data callback)
  "Request Spotify URL using METHOD, HEADERS and DATA.
Call CALLBACK with HTTP status, decoded JSON and optional Retry-After seconds.
Do not log payloads or retain response buffers.  Refuse HTTP redirects."
  (let ((url-request-method method)
	(url-request-extra-headers headers)
	(url-request-data data)
	(url-max-redirections 0)
	(url-debug nil)
	(gnutls-verify-error t)
	(url-automatic-caching nil)
	response timer done)
    (cl-labels ((finish (status json retry)
		  (unless done
		    (setq done t)
		    (when timer (cancel-timer timer))
		    (funcall callback status json retry))))
      (condition-case err
	  (progn
	    (setq response
		  (url-retrieve
		   url
		   (lambda (_status)
		     (let ((buffer (current-buffer)))
		       (unwind-protect
			   (let ((status url-http-response-status) json retry)
			     (condition-case nil
				 (when url-http-end-of-headers
				   (goto-char (point-min))
				   (let ((case-fold-search t))
				     (when (re-search-forward "^Retry-After: *\\([0-9]+\\)" url-http-end-of-headers t)
				       (setq retry (string-to-number (match-string 1)))))
				   (goto-char url-http-end-of-headers)
				   (setq json (json-parse-buffer :object-type 'alist :array-type 'list
								 :null-object nil :false-object nil)))
			       (error nil))
			     (finish status json retry))
			 (when (buffer-live-p buffer) (kill-buffer buffer)))))
		   nil t t))
	    (when (buffer-live-p response)
	      (with-current-buffer response
		(setq-local url-max-redirections 0 url-debug nil
			    gnutls-verify-error t url-automatic-caching nil)))
	    (unless done
	      (setq timer
		    (run-at-time 30 nil
				 (lambda ()
				   (unwind-protect (finish nil nil nil)
				     (when (buffer-live-p response)
				       (when-let* ((process (get-buffer-process response)))
					 (delete-process process))
				       (kill-buffer response))))))))
	(error (if done (signal (car err) (cdr err)) (finish nil nil nil)))))))

(defun hub/spotify-auth--notify (context token error)
  "Complete CONTEXT's waiting requests with TOKEN or safe ERROR text."
  (setf (hub/spotify-auth--session-busy context) nil)
  (let ((waiters (hub/spotify-auth--session-waiters context)))
    (setf (hub/spotify-auth--session-waiters context) nil)
    (dolist (waiter (nreverse waiters)) (funcall waiter token error))))

(defun hub/spotify-auth--new-context ()
  "Read the configured client ID and its encrypted refresh authorization."
  (let ((client (auth-source-pass-get 'secret hub/spotify-client-id-entry)))
    (unless (and (stringp client) (string-match-p "\\`[[:xdigit:]]\\{32\\}\\'" client))
      (user-error "Cannot read Spotify client ID from pass: %s" hub/spotify-client-id-entry))
    (let* ((context (hub/spotify-auth--session-create
		     :client-id client :client-entry hub/spotify-client-id-entry
		     :token-entry hub/spotify-token-entry))
	   (stored (auth-source-pass-get 'secret hub/spotify-token-entry))
	   (record (and stored (condition-case nil
				   (json-parse-string stored :object-type 'alist)
				 (error nil)))))
      (when (and (listp record) (equal (alist-get 'client_id record) client)
		 (stringp (alist-get 'refresh_token record)))
	(setf (hub/spotify-auth--session-refresh-token context)
	      (alist-get 'refresh_token record)))
      context)))

(defun hub/spotify-auth--exchange (context pairs)
  "Exchange form PAIRS for CONTEXT's access token."
  (hub/spotify-auth--request
   "https://accounts.spotify.com/api/token" "POST"
   '(("Content-Type" . "application/x-www-form-urlencoded"))
   (hub/spotify-auth--form pairs)
   (lambda (status json _retry)
     (when (eq context hub/spotify-auth--context)
       (setq json (and (listp json) json))
       (let ((token (alist-get 'access_token json))
	     (expires (alist-get 'expires_in json)))
	 (if (not (and (eq status 200) (stringp token)
		       (not (string-match-p "[[:space:]]" token))
		       (> (length token) 0) (numberp expires) (> expires 0)))
	     (hub/spotify-auth--notify context nil
				       "Authorization failed; M-x hub/spotify-connect to reconnect")
	   (condition-case nil
	       (let ((refresh (or (alist-get 'refresh_token json)
				  (hub/spotify-auth--session-refresh-token context))))
		 (unless (and (stringp refresh) (not (string-empty-p refresh)))
		   (error "Missing refresh authorization"))
		 (unless (equal refresh (hub/spotify-auth--session-refresh-token context))
		   (hub/spotify-auth--store context refresh))
		 (setf (hub/spotify-auth--session-refresh-token context) refresh
		       (hub/spotify-auth--session-access-token context) token
		       (hub/spotify-auth--session-expires-at context) (+ (float-time) expires -30))
		 (hub/spotify-auth--notify context token nil))
	     (error (hub/spotify-auth--notify context nil
					      "Cannot save Spotify authorization in pass; unlock GPG and reconnect")))))))))

(defun hub/spotify-auth-with-token (callback)
  "Call CALLBACK with a valid access token, or nil and safe error text.
Read secrets only on explicit library use, never on module load.
Return nil rather than exposing token-bearing callback results."
  (condition-case nil
      (let ((context (or hub/spotify-auth--context
			 (setq hub/spotify-auth--context (hub/spotify-auth--new-context)))))
	(if (and (hub/spotify-auth--session-access-token context)
		 (> (hub/spotify-auth--session-expires-at context) (float-time)))
	    (funcall callback (hub/spotify-auth--session-access-token context) nil)
	  (push callback (hub/spotify-auth--session-waiters context))
	  (unless (hub/spotify-auth--session-busy context)
	    (setf (hub/spotify-auth--session-busy context) t)
	    (if (hub/spotify-auth--session-refresh-token context)
		(hub/spotify-auth--exchange
		 context `(("grant_type" . "refresh_token")
			   ("refresh_token" . ,(hub/spotify-auth--session-refresh-token context))
			   ("client_id" . ,(hub/spotify-auth--session-client-id context))))
	      (hub/spotify-auth--authorize context)))))
    (error
     (if hub/spotify-auth--context
	 (progn
	   (hub/spotify-auth--stop-listener hub/spotify-auth--context)
	   (hub/spotify-auth--notify hub/spotify-auth--context nil
				     "Cannot start Spotify authorization; check pass and callback port 8765, then reconnect"))
       (funcall callback nil "Cannot read Spotify client ID; check pass and retry"))))
  nil)

(defun hub/spotify-auth--store (context refresh)
  "Encrypt CONTEXT's REFRESH credential in pass, using stdin rather than argv."
  (with-temp-buffer
    (insert (json-serialize `((client_id . ,(hub/spotify-auth--session-client-id context))
			      (refresh_token . ,refresh))))
    (let ((coding-system-for-write 'utf-8-unix)
	  (process-environment
	   (cons (concat "PASSWORD_STORE_DIR=" (expand-file-name auth-source-pass-filename))
		 process-environment)))
      (unless (zerop (call-process-region (point-min) (point-max) "pass" nil '(nil nil) nil
					  "insert" "--force" "--multiline" "--"
					  (hub/spotify-auth--session-token-entry context)))
	(error "Pass could not encrypt Spotify authorization")))))

(defun hub/spotify-auth--random ()
  "Return 32 cryptographically random bytes encoded as unpadded base64url."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (unless (and (zerop (process-file "openssl" nil (list (current-buffer) nil) nil "rand" "32"))
		 (= (buffer-size) 32))
      (error "OpenSSL could not generate secure OAuth randomness"))
    (base64url-encode-string (buffer-string) t)))

(defun hub/spotify-auth--stop-listener (context)
  "Release CONTEXT's loopback listener, clients, timer and one-time secrets."
  (when (timerp (hub/spotify-auth--session-timer context))
    (cancel-timer (hub/spotify-auth--session-timer context)))
  (dolist (process (cons (hub/spotify-auth--session-listener context)
			 (hub/spotify-auth--session-clients context)))
    (when (and (processp process) (process-live-p process)) (delete-process process)))
  (setf (hub/spotify-auth--session-listener context) nil
	(hub/spotify-auth--session-clients context) nil
	(hub/spotify-auth--session-timer context) nil
	(hub/spotify-auth--session-verifier context) nil
	(hub/spotify-auth--session-state context) nil))

(defun hub/spotify-auth--reply (process status text)
  "Reply to callback PROCESS with HTTP STATUS and static TEXT, then close it."
  (when (process-live-p process)
    (process-send-string process
			 (format "HTTP/1.1 %d %s\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
				 status (if (= status 200) "OK" "Bad Request")
				 (string-bytes text) text))
    (process-put process 'hub/spotify-request nil)
    (delete-process process)))

(defun hub/spotify-auth--receive (context process chunk)
  "Handle bounded HTTP CHUNK from callback PROCESS belonging to CONTEXT."
  (let ((request (concat (process-get process 'hub/spotify-request) chunk)))
    (cond
     ((> (length request) 8192)
      (hub/spotify-auth--reply process 400 "Callback too large."))
     ((not (string-match-p "\r\n\r\n" request))
      (process-put process 'hub/spotify-request request))
     (t
      (let* ((query (and (string-match "\\`GET /spotify/callback[?]\\([^ ]*\\) HTTP/1\\.[01]\r\n" request)
			 (match-string 1 request)))
	     (params (and query (condition-case nil (url-parse-query-string query) (error nil))))
	     (state (cadr (assoc "state" params)))
	     (code (cadr (assoc "code" params))))
	(if (not (and (eq context hub/spotify-auth--context)
		      (stringp state) (not (string-empty-p state))
		      (equal state (hub/spotify-auth--session-state context))
		      (= (cl-count "state" params :key #'car :test #'equal) 1)
		      (or (and (stringp code) (not (string-empty-p code))
			       (= (cl-count "code" params :key #'car :test #'equal) 1))
			  (assoc "error" params))))
	    (hub/spotify-auth--reply process 400 "Unrecognized callback; retry the original login.")
	  (let ((verifier (hub/spotify-auth--session-verifier context)))
	    (hub/spotify-auth--reply process 200 "You can return to Emacs.")
	    (hub/spotify-auth--stop-listener context)
	    (if (assoc "error" params)
		(hub/spotify-auth--notify context nil "Authorization denied; M-x hub/spotify-connect to retry")
	      (hub/spotify-auth--exchange
	       context `(("grant_type" . "authorization_code")
			 ("client_id" . ,(hub/spotify-auth--session-client-id context))
			 ("code" . ,code) ("code_verifier" . ,verifier)
			 ("redirect_uri" . ,(hub/spotify-auth--session-redirect context))))))))))))

(defun hub/spotify-auth--authorize (context)
  "Start browser authorization with a loopback-only listener for CONTEXT."
  (setf (hub/spotify-auth--session-state context) (hub/spotify-auth--random)
	(hub/spotify-auth--session-verifier context) (hub/spotify-auth--random)
	(hub/spotify-auth--session-listener context)
	(make-network-process
	 :name "spotify-oauth" :server t :host "127.0.0.1" :family 'ipv4
	 :service hub/spotify-callback-port :coding 'binary :noquery t :buffer nil
	 :log (lambda (_server client _message)
		(set-process-query-on-exit-flag client nil)
		(push client (hub/spotify-auth--session-clients context)))
	 :filter (lambda (process chunk) (hub/spotify-auth--receive context process chunk))))
  (setf (hub/spotify-auth--session-redirect context)
	(format "http://127.0.0.1:%d/spotify/callback"
		(process-contact (hub/spotify-auth--session-listener context) :service))
	(hub/spotify-auth--session-timer context)
	(run-at-time 300 nil (lambda ()
			       (hub/spotify-auth--stop-listener context)
			       (hub/spotify-auth--notify context nil
							 "Authorization timed out; M-x hub/spotify-connect to retry"))))
  (browse-url
   (concat "https://accounts.spotify.com/authorize?"
	   (hub/spotify-auth--form
	    `(("response_type" . "code") ("scope" . "user-library-read")
	      ("client_id" . ,(hub/spotify-auth--session-client-id context))
	      ("redirect_uri" . ,(hub/spotify-auth--session-redirect context))
	      ("state" . ,(hub/spotify-auth--session-state context))
	      ("code_challenge_method" . "S256")
	      ("code_challenge" . ,(hub/spotify-auth--challenge
				    (hub/spotify-auth--session-verifier context))))))))

;;;###autoload
(defun hub/spotify-connect ()
  "Explicitly reauthorize read-only library access, replacing saved authorization."
  (interactive)
  (when hub/spotify-auth--context
    (hub/spotify-auth--stop-listener hub/spotify-auth--context)
    (hub/spotify-auth--notify hub/spotify-auth--context nil "Authorization restarted; refresh Liked Songs"))
  (setq hub/spotify-auth--context nil)
  (let ((context (hub/spotify-auth--new-context)))
    (setf (hub/spotify-auth--session-refresh-token context) nil)
    (setq hub/spotify-auth--context context)
    (hub/spotify-auth-with-token
     (lambda (_token error)
       (message "Spotify: %s" (or error "Connected; open M-x hub/spotify-liked-songs"))))))

(provide 'apps/spotify-auth)
;;; spotify-auth.el ends here
