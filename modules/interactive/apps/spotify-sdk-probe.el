;;; spotify-sdk-probe.el --- Isolated official browser playback check -*- lexical-binding: t; -*-

;;; Commentary:
;; One Chrome SDK device, independent of EMMS. OAuth uses the existing callback
;; registration, but no saved library grant or client secret. The standalone
;; check is memory-only; receiver hosting can opt into a separate encrypted
;; refresh grant. The browser bootstrap capability is scrubbed from its URL.

;;; Code:

(require 'hub-utils)
(require 'apps/spotify-auth)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)

(defconst hub/spotify-sdk-probe--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the public SDK check assets.")
(defconst hub/spotify-sdk-probe--scopes
  "streaming user-read-email user-read-private user-modify-playback-state"
  "Permissions required for the official SDK and explicit track selection.")
(defvar hub/spotify-sdk-probe--session nil "Owned ephemeral SDK session, never serialized.")
(cl-defstruct (hub/spotify-sdk-probe--context (:constructor hub/spotify-sdk-probe--context-create))
  "Owned browser session; only receiver hosting opts into encrypted renewal."
  client key track port listener clients timer state verifier token expires error ending-check handler on-stop
  token-entry refresh-token busy waiters retry-at (generation 0))

(defun hub/spotify-sdk-probe--origin (context)
  "Return CONTEXT's exact loopback origin."
  (format "http://127.0.0.1:%d" (hub/spotify-sdk-probe--context-port context)))

(defun hub/spotify-sdk-probe--valid-token-p (context)
  "Return non-nil when CONTEXT has a usable in-memory token."
  (and (hub/spotify-sdk-probe--context-token context)
       (> (or (hub/spotify-sdk-probe--context-expires context) 0) (float-time))))

(defun hub/spotify-sdk-probe--json (status object)
  "Return a JSON response with STATUS for OBJECT."
  (list status "application/json" (json-serialize object :null-object nil :false-object :false)))

(defun hub/spotify-sdk-probe--asset (name type)
  "Return public asset NAME with MIME TYPE."
  (list 200 type (with-temp-buffer
		   (insert-file-contents (expand-file-name name hub/spotify-sdk-probe--directory))
		   (buffer-string))))

(defun hub/spotify-sdk-probe--accept-token (context status json)
  "Accept a validated token response for CONTEXT, saving only refresh material."
  (let ((token (and (listp json) (alist-get 'access_token json)))
	(expires (and (listp json) (alist-get 'expires_in json))))
    (if (not (and (eq status 200) (stringp token) (not (string-empty-p token))
		  (not (string-match-p "[[:space:]]" token)) (numberp expires) (> expires 30)))
	(setf (hub/spotify-sdk-probe--context-error context)
	      (if (eq status 400) "authorization_required" "authorization_failed"))
      (condition-case nil
	  (progn
	    (when (hub/spotify-sdk-probe--context-token-entry context)
	      (let ((refresh (or (alist-get 'refresh_token json)
				 (hub/spotify-sdk-probe--context-refresh-token context))))
		(unless (and (stringp refresh) (not (string-empty-p refresh))
			     (not (string-match-p "[[:space:]]" refresh)))
		  (error "Missing refresh authorization"))
		(unless (equal refresh (hub/spotify-sdk-probe--context-refresh-token context))
		  (hub/spotify-auth--store
		   (hub/spotify-auth--session-create
		    :client-id (hub/spotify-sdk-probe--context-client context)
		    :token-entry (hub/spotify-sdk-probe--context-token-entry context)) refresh))
		(setf (hub/spotify-sdk-probe--context-refresh-token context) refresh)))
	    (setf (hub/spotify-sdk-probe--context-token context) token
		  (hub/spotify-sdk-probe--context-expires context) (+ (float-time) expires -30)
		  (hub/spotify-sdk-probe--context-error context) nil))
	(error (setf (hub/spotify-sdk-probe--context-error context) "authorization_storage"))))))

(defun hub/spotify-sdk-probe--renewal-due-p (context)
  "Return non-nil when CONTEXT's renewable access token needs replacement."
  (and (hub/spotify-sdk-probe--context-token-entry context)
       (hub/spotify-sdk-probe--context-refresh-token context)
       (<= (or (hub/spotify-sdk-probe--context-expires context) 0) (+ (float-time) 120))))

(defun hub/spotify-sdk-probe--notify-token (context)
  "Complete CONTEXT's bounded pending token requests, including closed clients."
  (let ((waiters (hub/spotify-sdk-probe--context-waiters context)))
    (setf (hub/spotify-sdk-probe--context-waiters context) nil)
    (dolist (waiter waiters)
      ;; A client may have closed its request during the HTTPS exchange.
      (condition-case nil (funcall waiter) (error nil)))))

(defun hub/spotify-sdk-probe--defer-token (context process)
  "Reply to authenticated PROCESS after CONTEXT's in-flight renewal finishes."
  (push (lambda ()
	  (hub/spotify-sdk-probe--reply
	   process (if (and (eq context hub/spotify-sdk-probe--session)
			    (hub/spotify-sdk-probe--valid-token-p context))
		       (hub/spotify-sdk-probe--json
			200 `((access_token . ,(hub/spotify-sdk-probe--context-token context))))
		     '(409 "text/plain" "Playback authorization unavailable; reconnect from Emacs."))))
	(hub/spotify-sdk-probe--context-waiters context))
  (hub/spotify-sdk-probe--refresh-token context)
  (unless (hub/spotify-sdk-probe--context-busy context)
    (hub/spotify-sdk-probe--notify-token context)))

(defun hub/spotify-sdk-probe--refresh-token (context)
  "Renew CONTEXT's saved playback grant without starting OAuth or reading secrets."
  (when (and (eq context hub/spotify-sdk-probe--session)
	     (hub/spotify-sdk-probe--context-token-entry context)
	     (hub/spotify-sdk-probe--context-refresh-token context)
	     (not (hub/spotify-sdk-probe--context-busy context))
	     (<= (or (hub/spotify-sdk-probe--context-retry-at context) 0) (float-time)))
    (let ((generation (hub/spotify-sdk-probe--context-generation context)))
      (setf (hub/spotify-sdk-probe--context-busy context) t)
      (hub/spotify-auth--request
       "https://accounts.spotify.com/api/token" "POST"
       '(("Content-Type" . "application/x-www-form-urlencoded"))
       (hub/spotify-auth--form
	`(("grant_type" . "refresh_token")
	  ("refresh_token" . ,(hub/spotify-sdk-probe--context-refresh-token context))
	  ("client_id" . ,(hub/spotify-sdk-probe--context-client context))))
       (lambda (status json retry)
	 (when (and (eq context hub/spotify-sdk-probe--session)
		    (= generation (hub/spotify-sdk-probe--context-generation context)))
	   ;; Keep single-flight ownership through encrypted refresh-token rotation.
	   (hub/spotify-sdk-probe--accept-token context status json)
	   (cond
	    ((null (hub/spotify-sdk-probe--context-error context))
	     (setf (hub/spotify-sdk-probe--context-retry-at context) nil))
	    ((member status '(400 401 403))
	     (setf (hub/spotify-sdk-probe--context-token context) nil
		   (hub/spotify-sdk-probe--context-expires context) nil
		   (hub/spotify-sdk-probe--context-refresh-token context) nil
		   (hub/spotify-sdk-probe--context-error context) "authorization_required"))
	    (t
	     (setf (hub/spotify-sdk-probe--context-retry-at context)
		   (+ (float-time) (max 5 (if (numberp retry) retry 30))))
	     (when (and (hub/spotify-sdk-probe--valid-token-p context)
			(not (equal (hub/spotify-sdk-probe--context-error context) "authorization_storage")))
	       (setf (hub/spotify-sdk-probe--context-error context) nil))))
	   (setf (hub/spotify-sdk-probe--context-busy context) nil)
	   (hub/spotify-sdk-probe--notify-token context))))))
  ;; Never return a timer or callback closure containing captured credentials.
  nil)

(defun hub/spotify-sdk-probe--restore (context)
  "Restore only CONTEXT's configured encrypted playback authorization."
  (when-let* ((entry (hub/spotify-sdk-probe--context-token-entry context)))
    ;; Constructor bindings are synchronous; callbacks retain the captured context.
    (let* ((hub/spotify-token-entry entry)
	   (stored (hub/spotify-auth--new-context)))
      (unless (equal (hub/spotify-auth--session-client-id stored)
		     (hub/spotify-sdk-probe--context-client context))
	(user-error "Spotify client changed during authorization"))
      (setf (hub/spotify-sdk-probe--context-refresh-token context)
	    (hub/spotify-auth--session-refresh-token stored))
      (hub/spotify-sdk-probe--refresh-token context))))

(defun hub/spotify-sdk-probe--authorize (context)
  "Start a new explicit PKCE attempt for CONTEXT, invalidating older callbacks."
  (cl-incf (hub/spotify-sdk-probe--context-generation context))
  (setf (hub/spotify-sdk-probe--context-state context) (hub/spotify-auth--random)
	(hub/spotify-sdk-probe--context-verifier context) (hub/spotify-auth--random)
	(hub/spotify-sdk-probe--context-token context) nil
	(hub/spotify-sdk-probe--context-expires context) nil
	(hub/spotify-sdk-probe--context-error context) nil
	(hub/spotify-sdk-probe--context-refresh-token context) nil
	(hub/spotify-sdk-probe--context-busy context) nil
	(hub/spotify-sdk-probe--context-retry-at context) nil)
  (hub/spotify-sdk-probe--notify-token context)
  (hub/spotify-sdk-probe--json
   200 `((authorization_url . ,(concat
				"https://accounts.spotify.com/authorize?"
				(hub/spotify-auth--form
				 `(("client_id" . ,(hub/spotify-sdk-probe--context-client context))
				   ("response_type" . "code") ("scope" . ,hub/spotify-sdk-probe--scopes)
				   ("redirect_uri" . ,(concat (hub/spotify-sdk-probe--origin context) "/spotify/callback"))
				   ("state" . ,(hub/spotify-sdk-probe--context-state context))
				   ("code_challenge_method" . "S256")
				   ("code_challenge" . ,(hub/spotify-auth--challenge
							 (hub/spotify-sdk-probe--context-verifier context))))))))))

(defun hub/spotify-sdk-probe--callback (context target)
  "Consume a validated, single-use OAuth callback TARGET for CONTEXT."
  (let* ((query (and (string-match "\\`/spotify/callback[?]\\(.*\\)\\'" target) (match-string 1 target)))
	 (params (and query (condition-case nil (url-parse-query-string query) (error nil))))
	 (state (cadr (assoc "state" params))) (code (cadr (assoc "code" params)))
	 (denied (assoc "error" params)))
    (if (not (and (eq context hub/spotify-sdk-probe--session)
		  (stringp state) (not (string-empty-p state))
		  (equal state (hub/spotify-sdk-probe--context-state context))
		  (= (cl-count "state" params :key #'car :test #'equal) 1)
		  (if denied (not code)
		    (and (stringp code) (not (string-empty-p code))
			 (= (cl-count "code" params :key #'car :test #'equal) 1)))))
	'(400 "text/plain" "Unrecognized authorization callback.")
      (let ((verifier (hub/spotify-sdk-probe--context-verifier context))
	    (generation (hub/spotify-sdk-probe--context-generation context)))
	(setf (hub/spotify-sdk-probe--context-state context) nil
	      (hub/spotify-sdk-probe--context-verifier context) nil)
	(if denied
	    (setf (hub/spotify-sdk-probe--context-error context) "authorization_denied")
	  (hub/spotify-auth--request
	   "https://accounts.spotify.com/api/token" "POST"
	   '(("Content-Type" . "application/x-www-form-urlencoded"))
	   (hub/spotify-auth--form
	    `(("grant_type" . "authorization_code") ("code" . ,code) ("code_verifier" . ,verifier)
	      ("client_id" . ,(hub/spotify-sdk-probe--context-client context))
	      ("redirect_uri" . ,(concat (hub/spotify-sdk-probe--origin context) "/spotify/callback"))))
	   (lambda (status json _retry)
	     (when (and (eq context hub/spotify-sdk-probe--session)
			(= generation (hub/spotify-sdk-probe--context-generation context)))
	       (hub/spotify-sdk-probe--accept-token context status json)))))
	;; No code or third-party script in the response or final browser URL.
	'(303 "text/plain" "Return to the SDK check tab." "/complete")))))

(defun hub/spotify-sdk-probe--dispatch (context method target headers)
  "Route bounded METHOD/TARGET with HEADERS for CONTEXT without logging data."
  (let* ((path (car (split-string target "?")))
	 (origin (cdr (assoc "origin" headers)))
	 (host (cdr (assoc "host" headers)))
	 (private (and (equal (cdr (assoc "x-emacs-spotify-probe" headers))
			      (hub/spotify-sdk-probe--context-key context))
		       (hub/spotify-sdk-probe--context-key context)
		       (or (not origin) (equal origin (hub/spotify-sdk-probe--origin context))))))
    (cond
     ((not (equal host (format "127.0.0.1:%d" (hub/spotify-sdk-probe--context-port context))))
      '(403 "text/plain" "Invalid host."))
     ((and (hub/spotify-sdk-probe--context-handler context) (equal method "GET")
	   (member path '("/" "/receiver.js")))
      (funcall (hub/spotify-sdk-probe--context-handler context) context method target headers))
     ((and (equal method "GET") (equal path "/"))
      (hub/spotify-sdk-probe--asset "spotify-sdk-probe.html" "text/html"))
     ((and (equal method "GET") (equal path "/probe.js"))
      (hub/spotify-sdk-probe--asset "spotify-sdk-probe.js" "text/javascript"))
     ((and (equal method "GET") (equal path "/probe.css"))
      (hub/spotify-sdk-probe--asset "spotify-sdk-probe.css" "text/css"))
     ((and (equal method "GET") (equal path "/complete"))
      '(200 "text/plain" "Authorization received. Return to the SDK check tab; you can close this tab."))
     ((and (equal method "GET") (equal path "/spotify/callback"))
      (hub/spotify-sdk-probe--callback context target))
     ((not private) '(403 "text/plain" "Private SDK session required."))
     ((and (hub/spotify-sdk-probe--context-handler context) (equal method "GET") (equal path "/bridge-config"))
      (funcall (hub/spotify-sdk-probe--context-handler context) context method target headers))
     ((and (equal method "POST") (equal path "/authorize")) (hub/spotify-sdk-probe--authorize context))
     ((and (equal method "GET") (equal path "/state"))
      (hub/spotify-sdk-probe--json
       200 `((authorized . ,(if (hub/spotify-sdk-probe--valid-token-p context) t :false))
	     (track_uri . ,(hub/spotify-sdk-probe--context-track context))
	     (ending_check . ,(if (hub/spotify-sdk-probe--context-ending-check context) t :false))
	     (error . ,(hub/spotify-sdk-probe--context-error context)))))
     ((and (equal method "GET") (equal path "/token"))
      (cond
       ((or (hub/spotify-sdk-probe--context-busy context)
	    (and (hub/spotify-sdk-probe--renewal-due-p context)
		 (<= (or (hub/spotify-sdk-probe--context-retry-at context) 0) (float-time))))
	'await-token)
       ((hub/spotify-sdk-probe--valid-token-p context)
	(hub/spotify-sdk-probe--json 200 `((access_token . ,(hub/spotify-sdk-probe--context-token context)))))
       (t '(409 "text/plain" "Authorize the SDK session first."))))
     (t '(404 "text/plain" "Not found.")))))

(defun hub/spotify-sdk-probe--reply (process response)
  "Send static or authenticated RESPONSE to PROCESS, clearing its request first."
  (process-put process 'hub/spotify-sdk-request nil)
  (when (process-live-p process)
    (let ((body (encode-coding-string (nth 2 response) 'utf-8-unix)))
      (process-send-string
       process (concat
		(format "HTTP/1.1 %d Response\r\nContent-Type: %s; charset=utf-8\r\nContent-Length: %d\r\n"
			(car response) (nth 1 response) (length body))
		"Cache-Control: no-store\r\nReferrer-Policy: no-referrer\r\nX-Content-Type-Options: nosniff\r\n"
		"Content-Security-Policy: default-src 'self'; script-src 'self' https://sdk.scdn.co; style-src 'self'; connect-src 'self' https: wss: ws://127.0.0.1:*; frame-src https://sdk.scdn.co https://open.spotify.com; media-src blob: https:; img-src https: data:; object-src 'none'; base-uri 'none'; frame-ancestors 'none'\r\n"
		"Permissions-Policy: encrypted-media=(self \"https://sdk.scdn.co\" \"https://open.spotify.com\"), autoplay=(self \"https://sdk.scdn.co\" \"https://open.spotify.com\")\r\n"
		(when (nth 3 response) (concat "Location: " (nth 3 response) "\r\n"))
		"Connection: close\r\n\r\n" body)))
    (delete-process process)))

(defun hub/spotify-sdk-probe--receive (context process chunk)
  "Parse a bounded request CHUNK from PROCESS for owned CONTEXT."
  (let ((request (concat (process-get process 'hub/spotify-sdk-request) chunk)))
    (cond
     ((or (not (eq context hub/spotify-sdk-probe--session)) (> (length request) 16384))
      (hub/spotify-sdk-probe--reply process '(400 "text/plain" "Invalid request.")))
     ((not (string-match-p "\r\n\r\n" request)) (process-put process 'hub/spotify-sdk-request request))
     (t
      (process-put process 'hub/spotify-sdk-request nil)
      (condition-case nil
	  (if (not (string-match "\\`\\(GET\\|POST\\) \\([^ \r\n]+\\) HTTP/1\\.[01]\r\n" request))
	      (hub/spotify-sdk-probe--reply process '(400 "text/plain" "Invalid request."))
	    (let ((method (match-string 1 request)) (target (match-string 2 request)) headers duplicate)
	      (dolist (line (cdr (split-string (car (split-string request "\r\n\r\n")) "\r\n")))
		(when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
		  (let ((name (downcase (match-string 1 line))) (value (match-string 2 line)))
		    (when (assoc name headers) (setq duplicate t))
		    (push (cons name value) headers))))
	      (let ((response (if duplicate '(400 "text/plain" "Duplicate headers.")
				(hub/spotify-sdk-probe--dispatch context method target headers))))
		(if (eq response 'await-token)
		    (hub/spotify-sdk-probe--defer-token context process)
		  (hub/spotify-sdk-probe--reply process response)))))
	(error (hub/spotify-sdk-probe--reply process '(500 "text/plain" "SDK session request failed."))))))))

(defun hub/spotify-sdk-probe--open-chrome (url)
  "Open the private bootstrap URL in Chrome without printing it."
  (unless (eq system-type 'darwin) (user-error "This Chrome launcher requires macOS"))
  (let ((process (start-process "Spotify SDK Chrome" nil "/usr/bin/open" "-a" "Google Chrome" url)))
    (set-process-query-on-exit-flag process nil)
    (set-process-filter process #'ignore)))

;;;###autoload
(defun hub/spotify-sdk-probe-stop ()
  "Expire the owned SDK session and discard all in-memory authorization."
  (interactive)
  (when-let* ((context hub/spotify-sdk-probe--session))
    (setq hub/spotify-sdk-probe--session nil)
    (hub/spotify-sdk-probe--notify-token context)
    (when (timerp (hub/spotify-sdk-probe--context-timer context))
      (cancel-timer (hub/spotify-sdk-probe--context-timer context)))
    (dolist (process (cons (hub/spotify-sdk-probe--context-listener context)
			   (hub/spotify-sdk-probe--context-clients context)))
      (when (processp process)
	(process-put process 'hub/spotify-sdk-request nil)
	(when (process-live-p process) (delete-process process))))
    (setf (hub/spotify-sdk-probe--context-key context) nil
	  (hub/spotify-sdk-probe--context-token context) nil
	  (hub/spotify-sdk-probe--context-refresh-token context) nil
	  (hub/spotify-sdk-probe--context-busy context) nil
	  (hub/spotify-sdk-probe--context-state context) nil
	  (hub/spotify-sdk-probe--context-verifier context) nil
	  (hub/spotify-sdk-probe--context-clients context) nil
	  (hub/spotify-sdk-probe--context-listener context) nil)
    (when-let* ((callback (hub/spotify-sdk-probe--context-on-stop context)))
      (funcall callback)))
  nil)

;;;###autoload
(defun hub/spotify-sdk-probe (track &optional ending-check handler on-stop lifetime token-entry)
  "Open an independent SDK check for Spotify TRACK URL or URI in Chrome.
With prefix argument ENDING-CHECK, start normally, then let the listener
seek to the final eight seconds after hearing music.  Show safe playback-state
observations instead of a fifteen-second preview.
Neither mode modifies or advances the EMMS playlist.
Internal receiver hosting uses HANDLER for named routes, ON-STOP for cleanup,
and LIFETIME seconds instead of the default ten minutes (zero keeps a receiver
alive until disconnected). TOKEN-ENTRY opts the receiver into encrypted renewal;
the standalone compatibility check never saves or restores authorization."
  (interactive (list (read-string "Spotify track URL or URI: ") current-prefix-arg))
  (let ((uri (cond ((string-match-p "\\`spotify:track:[A-Za-z0-9]\\{22\\}\\'" track) track)
		   ((string-match "\\`https://open\\.spotify\\.com/\\(?:intl-[a-z][a-z]/\\)?track/\\([A-Za-z0-9]\\{22\\}\\)\\(?:[?].*\\)?\\'" track)
		    (concat "spotify:track:" (match-string 1 track))))))
    (unless (or uri (functionp handler)) (user-error "Use an individual Spotify track URL or URI"))
    (when (and (not handler) hub/spotify-sdk-probe--session
	       (hub/spotify-sdk-probe--context-handler hub/spotify-sdk-probe--session))
      (user-error "Disconnect the browser receiver before starting an isolated proof"))
    (hub/spotify-sdk-probe-stop)
    (let ((client (auth-source-pass-get 'secret hub/spotify-client-id-entry)))
      (unless (and (stringp client) (string-match-p "\\`[[:xdigit:]]\\{32\\}\\'" client))
	(user-error "Cannot read the public Spotify client ID from pass"))
      (let ((context (hub/spotify-sdk-probe--context-create
		      :client client :key (hub/spotify-auth--random) :track uri
		      :ending-check (and ending-check t) :handler handler :on-stop on-stop :token-entry token-entry)))
	(setq hub/spotify-sdk-probe--session context)
	(condition-case nil
	    (let ((listener
		   (make-network-process
		    :name "Spotify SDK check" :server t :host "127.0.0.1" :family 'ipv4
		    :service hub/spotify-callback-port :coding 'binary :noquery t :buffer nil
		    :log (lambda (_server connection _event)
			   (if (>= (length (hub/spotify-sdk-probe--context-clients context)) 32)
			       (delete-process connection)
			     (push connection (hub/spotify-sdk-probe--context-clients context))
			     (set-process-query-on-exit-flag connection nil)
			     (set-process-sentinel connection
						   (lambda (child _event)
						     (unless (process-live-p child)
						       (setf (hub/spotify-sdk-probe--context-clients context)
							     (delq child (hub/spotify-sdk-probe--context-clients context))))))))
		    :filter (lambda (process chunk) (hub/spotify-sdk-probe--receive context process chunk)))))
	      (setf (hub/spotify-sdk-probe--context-listener context) listener
		    (hub/spotify-sdk-probe--context-port context) (process-contact listener :service))
	      (unless (and handler (eq lifetime 0))
		(setf (hub/spotify-sdk-probe--context-timer context)
		      (run-at-time (or lifetime 600) nil
				   (lambda () (when (eq context hub/spotify-sdk-probe--session)
						(hub/spotify-sdk-probe-stop))))))
	      (hub/spotify-sdk-probe--restore context)
	      (hub/spotify-sdk-probe--open-chrome
	       (concat (hub/spotify-sdk-probe--origin context) "/#" (hub/spotify-sdk-probe--context-key context)))
	      (message "SDK check opened in Chrome; authorize, then click %s"
		       (if ending-check "Start ending check" "Play 15 seconds")))
	  (quit (hub/spotify-sdk-probe-stop) (signal 'quit nil))
	  (error (hub/spotify-sdk-probe-stop)
		 (user-error "Cannot start SDK check; check Chrome and callback port 8765"))))))
  nil)

(add-hook 'kill-emacs-hook #'hub/spotify-sdk-probe-stop)

(provide 'apps/spotify-sdk-probe)
;;; spotify-sdk-probe.el ends here
