;;; spotify-test.el --- Spotify library scenarios -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic credentials and responses only; never call real pass or Spotify.
;; Scenarios: spec/personal-apps/apps/stories/spotify-library/scenarios.feature.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'context-panels)
(require 'apps/spotify)

(ert-deftest hub/spotify-pkce-matches-rfc-7636 ()
  "PKCE uses the RFC 7636 S256 challenge, without base64 padding."
  (should (equal (hub/spotify-auth--challenge
		  "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
		 "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")))

(ert-deftest hub/spotify-liked-songs-browses-and-reopens-without-login ()
  "AC-1/2: Saved authorization yields a read-only library in the shared slot."
  (let ((hub/spotify-auth--context nil)
	(hub/spotify-buffer-name " *Spotify library test*")
	(source (generate-new-buffer " *Spotify source test*"))
	(stored (json-serialize '((client_id . "0123456789abcdef0123456789abcdef")
				  (refresh_token . "synthetic-refresh"))))
	(requests nil))
    (unwind-protect
	(save-window-excursion
	  (switch-to-buffer source)
	  (cl-letf (((symbol-function 'auth-source-pass-get)
		     (lambda (_key entry)
		       (if (string-suffix-p "/client-id" entry)
			   "0123456789abcdef0123456789abcdef" stored)))
		    ((symbol-function 'browse-url)
		     (lambda (&rest _) (ert-fail "Unexpected login browser")))
		    ((symbol-function 'hub/spotify-auth--request)
		     (lambda (url method _headers data callback)
		       (push (list url method data) requests)
		       (if (string-match-p "/api/token" url)
			   (funcall callback 200 '((access_token . "synthetic-access")
						   (token_type . "Bearer")
						   (expires_in . 3600)) nil)
			 (funcall callback 200
				  '((total . 1)
				    (items . (((track . ((id . "test-song")
							 (name . "First song")
							 (artists . (((name . "First artist"))))
							 (album . ((name . "First album")))
							 (duration_ms . 180000))))))) nil)))))
	    (hub/spotify-liked-songs)
	    (should (eq (window-parameter (selected-window) 'window-side) 'bottom))
	    (should (derived-mode-p 'hub/spotify-library-mode))
	    (should buffer-read-only)
	    (should (string-match-p "First song" (buffer-string)))
	    (should (string-match-p "3:00" (buffer-string)))
	    (should (string-match-p "1 of 1" header-line-format))
	    (kill-buffer (current-buffer))
	    (switch-to-buffer source)
	    (setq hub/spotify-auth--context nil)
	    (hub/spotify-liked-songs)
	    (should (string-match-p "First artist" (buffer-string)))
	    (should (= (length requests) 4))))
      (dolist (buffer (list source (get-buffer hub/spotify-buffer-name)))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun hub/spotify-test--callback (port query)
  "Send a synthetic OAuth QUERY to the loopback listener at PORT."
  (let* ((reply "")
	 (client (make-network-process
		  :name "spotify-test-browser" :host "127.0.0.1" :service port
		  :coding 'binary :noquery t
		  :filter (lambda (_process data) (setq reply (concat reply data))))))
    (unwind-protect
	(progn
	  (process-send-string client (concat "GET /spotify/callback?" query
					      " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"))
	  (cl-loop repeat 60 while (process-live-p client)
		   do (accept-process-output nil 0.02))
	  reply)
      (when (process-live-p client) (delete-process client)))))

(ert-deftest hub/spotify-browser-authorization-validates-state-and-persists-refresh ()
  "AC-1/5: Local callbacks require state, exchange PKCE, and store no access token."
  (let ((hub/spotify-auth--context nil)
	(hub/spotify-callback-port 0)
	(hub/spotify-token-entry "synthetic/spotify/oauth")
	browser-url result stored posted)
    (cl-letf (((symbol-function 'auth-source-pass-get)
	       (lambda (_key entry)
		 (when (string-suffix-p "/client-id" entry)
		   "0123456789abcdef0123456789abcdef")))
	      ((symbol-function 'browse-url) (lambda (url &rest _) (setq browser-url url)))
	      ((symbol-function 'hub/spotify-auth--store)
	       (lambda (context refresh)
		 (setq stored (list (hub/spotify-auth--session-token-entry context) refresh))))
	      ((symbol-function 'hub/spotify-auth--request)
	       (lambda (_url _method _headers data callback)
		 (setq posted (url-parse-query-string data))
		 (funcall callback 200 '((access_token . "synthetic-access")
					 (refresh_token . "synthetic-refresh")
					 (token_type . "Bearer")
					 (scope . "user-library-read")
					 (expires_in . 3600)) nil))))
      (unwind-protect
	  (progn
	    (hub/spotify-auth-with-token (lambda (token error) (setq result (list token error))))
	    (should (string-prefix-p "https://accounts.spotify.com/authorize?" browser-url))
	    (let* ((params (url-parse-query-string (cadr (split-string browser-url "?"))))
		   (redirect (cadr (assoc "redirect_uri" params)))
		   (port (url-port (url-generic-parse-url redirect)))
		   (state (cadr (assoc "state" params))))
	      (should (equal (cadr (assoc "scope" params)) "user-library-read"))
	      (should (equal (cadr (assoc "code_challenge_method" params)) "S256"))
	      (should (string-prefix-p "http://127.0.0.1:" redirect))
	      (should (string-match-p "400" (hub/spotify-test--callback port "state=wrong&code=bad")))
	      (should-not result)
	      (should (string-match-p "200" (hub/spotify-test--callback port (concat "state=" state "&code=synthetic-code"))))
	      (should (equal result '("synthetic-access" nil)))
	      (should (equal stored '("synthetic/spotify/oauth" "synthetic-refresh")))
	      (should (equal (cadr (assoc "grant_type" posted)) "authorization_code"))
	      (should (>= (length (cadr (assoc "code_verifier" posted))) 43))
	      (should-not (assoc "client_secret" posted))
	      (should-not (hub/spotify-auth--session-listener hub/spotify-auth--context))))
	(when hub/spotify-auth--context (hub/spotify-auth--stop-listener hub/spotify-auth--context))))))

(ert-deftest hub/spotify-refresh-rotation-is-persisted ()
  "AC-2: A rotated refresh token replaces the old encrypted credential."
  (let* ((context (hub/spotify-auth--session-create :client-id "test-client" :refresh-token "old-refresh"))
	 (hub/spotify-auth--context context)
	 stored)
    (cl-letf (((symbol-function 'hub/spotify-auth--store)
	       (lambda (_context refresh) (setq stored refresh)))
	      ((symbol-function 'hub/spotify-auth--request)
	       (lambda (_url _method _headers _data callback)
		 (funcall callback 200 '((access_token . "new-access") (refresh_token . "new-refresh")
					 (expires_in . 3600)) nil))))
      (hub/spotify-auth--exchange context nil)
      (should (equal stored "new-refresh"))
      (should (equal (hub/spotify-auth--session-refresh-token context) "new-refresh")))))

(defun hub/spotify-test--page (total &rest names)
  "Return a synthetic saved-track page with TOTAL and track NAMES."
  `((total . ,total) (items . ,(mapcar (lambda (name) `((track . ,(and name `((name . ,name)))))) names))))

(ert-deftest hub/spotify-pages-preserve-focus-ignore-stale-and-stay-closed ()
  "AC-3: Paginated callbacks preserve editing and never replace newer data."
  (let ((hub/spotify-buffer-name " *Spotify async test*")
	(source (generate-new-buffer " *Spotify editing test*"))
	callbacks urls library)
    (unwind-protect
	(save-window-excursion
	  (switch-to-buffer source)
	  (cl-letf (((symbol-function 'hub/spotify-auth-with-token)
		     (lambda (callback) (funcall callback "synthetic-access" nil)))
		    ((symbol-function 'hub/spotify-auth--request)
		     (lambda (url _method _headers _data callback)
		       (push url urls) (setq callbacks (append callbacks (list callback))))))
	    (hub/spotify-liked-songs)
	    (setq library (current-buffer))
	    (should (string-match-p "Complete authorization" (buffer-string)))
	    (should-error (hub/spotify-load-more) :type 'user-error)
	    (funcall (pop callbacks) 200 (hub/spotify-test--page 3 "First" nil) nil)
	    (should (string-match-p "Unavailable track" (buffer-string)))
	    (should (string-match-p "Unknown artist" (buffer-string)))
	    (hub/spotify-load-more)
	    (should (string-suffix-p "offset=2" (car urls)))
	    (select-window (get-buffer-window source))
	    (insert "Keep editing")
	    (let ((position (point)) (window (selected-window)))
	      (funcall (pop callbacks) 200 (hub/spotify-test--page 3 "Last") nil)
	      (should (= position (point)))
	      (should (eq window (selected-window))))
	    (with-current-buffer library
	      (should (= (length tabulated-list-entries) 3))
	      (should-error (hub/spotify-load-more) :type 'user-error)
	      (hub/spotify-refresh)
	      (hub/spotify-refresh))
	    (let ((old (pop callbacks)) (new (pop callbacks)))
	      (funcall new 200 (hub/spotify-test--page 2 "Newest") nil)
	      (funcall old 200 (hub/spotify-test--page 2 "Stale") nil))
	    (with-current-buffer library
	      (should (string-match-p "Newest" (buffer-string)))
	      (should-not (string-match-p "Stale" (buffer-string)))
	      (hub/spotify-load-more)
	      (hub/spotify-close))
	    (funcall (pop callbacks) 200 (hub/spotify-test--page 2 "Hidden") nil)
	    (should-not (get-buffer-window library))
	    (should (eq (current-buffer) source))))
      (dolist (buffer (list source library))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hub/spotify-rate-limit-and-denied-access-are-actionable ()
  "AC-4: Retry-After is respected and permission errors explain recovery."
  (with-temp-buffer
    (hub/spotify-library-mode)
    (let ((status 429) (requests 0))
      (cl-letf (((symbol-function 'hub/spotify-auth-with-token)
		 (lambda (callback) (funcall callback "synthetic-access" nil)))
		((symbol-function 'hub/spotify-auth--request)
		 (lambda (_url _method _headers _data callback)
		   (cl-incf requests) (funcall callback status nil 15))))
	(hub/spotify-refresh)
	(should (string-match-p "15" header-line-format))
	(should-error (hub/spotify-refresh) :type 'user-error)
	(should (= requests 1))
	(setq hub/spotify--retry-at 0 status 403)
	(hub/spotify-refresh)
	(should (string-match-p "reconnect" header-line-format))))))

(ert-deftest hub/spotify-pass-storage-uses-stdin-not-arguments ()
  "Refresh credentials are encrypted with pass, never exposed as process arguments."
  (let ((context (hub/spotify-auth--session-create :client-id "synthetic-client"
						   :token-entry "synthetic/spotify/oauth"))
	argv payload)
    (cl-letf (((symbol-function 'call-process-region)
	       (lambda (start end &rest args)
		 (setq argv args payload (buffer-substring-no-properties start end)) 0)))
      (hub/spotify-auth--store context "synthetic-refresh"))
    (should (equal (car argv) "pass"))
    (should-not (member "synthetic-refresh" argv))
    (should (equal (alist-get 'refresh_token (json-parse-string payload :object-type 'alist))
		   "synthetic-refresh"))
    (should-not (string-match-p "access_token" payload))))

(ert-deftest hub/spotify-http-callback-disposes-only-its-response ()
  "HTTP parsing does not kill the editing buffer when a callback changes buffers."
  (let ((response (generate-new-buffer " *Spotify HTTP test*"))
	(editing (generate-new-buffer " *Spotify HTTP editing*"))
	(gnutls-verify-error nil)
	callback result)
    (unwind-protect
	(cl-letf (((symbol-function 'url-retrieve)
		   (lambda (_url handler _args _silent inhibit-cookies)
		     (should inhibit-cookies)
		     (should gnutls-verify-error)
		     (setq callback handler)
		     response)))
	  (hub/spotify-auth--request "https://api.spotify.com/v1/me/tracks" "GET" nil nil
				     (lambda (status json retry)
				       (setq result (list status json retry))
				       (set-buffer editing)))
	  (with-current-buffer response
	    (should (local-variable-p 'url-max-redirections))
	    (should (zerop url-max-redirections))
	    (insert "HTTP/1.1 200 OK\r\nRetry-After: 12\r\n\r\n")
	    (setq-local url-http-response-status 200
			url-http-end-of-headers (point))
	    (insert "{\"total\":0,\"items\":[]}")
	    (funcall callback nil))
	  (should (equal result '(200 ((total . 0) (items)) 12)))
	  (should-not (buffer-live-p response))
	  (should (buffer-live-p editing)))
      (dolist (buffer (list response editing))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hub/spotify-browser-to-empty-library-is-end-to-end ()
  "AC-1/4: First login completes into an explicit empty library, not a blank view."
  (let ((hub/spotify-auth--context nil) (hub/spotify-callback-port 0)
	(hub/spotify-buffer-name " *Spotify first login test*")
	(source (generate-new-buffer " *Spotify first login source*"))
	browser-url)
    (unwind-protect
	(save-window-excursion
	  (switch-to-buffer source)
	  (cl-letf (((symbol-function 'auth-source-pass-get)
		     (lambda (_key entry)
		       (when (string-suffix-p "/client-id" entry)
			 "0123456789abcdef0123456789abcdef")))
		    ((symbol-function 'browse-url) (lambda (url &rest _) (setq browser-url url)))
		    ((symbol-function 'hub/spotify-auth--store) (lambda (&rest _) nil))
		    ((symbol-function 'hub/spotify-auth--request)
		     (lambda (url _method _headers _data callback)
		       (funcall callback 200
				(if (string-match-p "/api/token" url)
				    '((access_token . "synthetic-access") (refresh_token . "synthetic-refresh") (expires_in . 3600))
				  (hub/spotify-test--page 0)) nil))))
	    (hub/spotify-liked-songs)
	    (let* ((params (url-parse-query-string (cadr (split-string browser-url "?"))))
		   (port (url-port (url-generic-parse-url (cadr (assoc "redirect_uri" params))))))
	      (hub/spotify-test--callback port (concat "state=" (cadr (assoc "state" params)) "&code=synthetic-code")))
	    (should (string-match-p "No Liked Songs yet" (buffer-string)))
	    (should-not hub/spotify--busy)))
      (when hub/spotify-auth--context (hub/spotify-auth--stop-listener hub/spotify-auth--context))
      (dolist (buffer (list source (get-buffer hub/spotify-buffer-name)))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hub/spotify-http-timeout-cleans-up-once ()
  "A stalled request completes once and does not retain its response buffer."
  (let ((response (generate-new-buffer " *Spotify timeout test*")) timeout (calls 0))
    (unwind-protect
	(cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) response))
		  ((symbol-function 'run-at-time)
		   (lambda (_time _repeat function &rest _) (setq timeout function) nil)))
	  (hub/spotify-auth--request "https://api.spotify.com/v1/me/tracks" "GET" nil nil
				     (lambda (&rest _) (cl-incf calls)))
	  (funcall timeout)
	  (funcall timeout)
	  (should (= calls 1))
	  (should-not (buffer-live-p response)))
      (when (buffer-live-p response) (kill-buffer response)))))

(ert-deftest hub/spotify-loading-modules-does-not-read-secrets-or-start-processes ()
  "Autoloaded modules impose no authentication, capture or startup networking."
  (cl-letf (((symbol-function 'auth-source-pass-get) (lambda (&rest _) (ert-fail "Secret read at load")))
	    ((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Network at load")))
	    ((symbol-function 'process-file) (lambda (&rest _) (ert-fail "Process at load"))))
    (load "apps/spotify-auth" nil t)
    (load "apps/spotify" nil t)))

(ert-deftest hub/spotify-errors-preserve-rows-without-echoing-payloads ()
  "AC-4: Network and malformed responses retain results and hide response payloads."
  (with-temp-buffer
    (hub/spotify-library-mode)
    (hub/spotify--received 0 200 (hub/spotify-test--page 1 "Saved") nil)
    (dolist (response '((nil nil) (500 ((error . "synthetic-private-response")))
			(200 ((total . "invalid") (items)))))
      (hub/spotify--received 0 (car response) (cadr response) nil)
      (should (string-match-p "Saved" (buffer-string)))
      (should-not (string-match-p "synthetic-private-response" header-line-format)))))

(ert-deftest hub/spotify-denial-and-timeout-release-one-time-secrets ()
  "AC-4: Denied and expired logins release the socket, state, verifier and timer."
  (dolist (outcome '(denied timeout))
    (let ((hub/spotify-auth--context nil) (hub/spotify-callback-port 0) browser-url result)
      (cl-letf (((symbol-function 'auth-source-pass-get)
		 (lambda (_key entry)
		   (when (string-suffix-p "/client-id" entry)
		     "0123456789abcdef0123456789abcdef")))
		((symbol-function 'browse-url) (lambda (url &rest _) (setq browser-url url)))
		((symbol-function 'hub/spotify-auth--request) (lambda (&rest _) (ert-fail "Unexpected token request"))))
	(unwind-protect
	    (progn
	      (hub/spotify-auth-with-token (lambda (token error) (setq result (list token error))))
	      (let* ((context hub/spotify-auth--context)
		     (server (hub/spotify-auth--session-listener context)))
		(if (eq outcome 'timeout)
		    (funcall (timer--function (hub/spotify-auth--session-timer context)))
		  (let* ((params (url-parse-query-string (cadr (split-string browser-url "?"))))
			 (port (url-port (url-generic-parse-url (cadr (assoc "redirect_uri" params))))))
		    (hub/spotify-test--callback port (concat "state=" (cadr (assoc "state" params)) "&error=access_denied"))))
		(should-not (car result))
		(should (string-match-p (if (eq outcome 'timeout) "timed out" "denied") (cadr result)))
		(should-not (process-live-p server))
		(should-not (hub/spotify-auth--session-state context))
		(should-not (hub/spotify-auth--session-verifier context))
		(should-not (hub/spotify-auth--session-timer context))))
	  (when hub/spotify-auth--context (hub/spotify-auth--stop-listener hub/spotify-auth--context)))))))

(ert-deftest hub/spotify-pass-failure-never-exposes-or-caches-a-token ()
  "A failed encrypted save cannot report a successful login or echo secret errors."
  (let* ((context (hub/spotify-auth--session-create))
	 (hub/spotify-auth--context context) result)
    (setf (hub/spotify-auth--session-waiters context)
	  (list (lambda (token error) (setq result (list token error)))))
    (cl-letf (((symbol-function 'hub/spotify-auth--store)
	       (lambda (&rest _) (error "synthetic-private-pass-error")))
	      ((symbol-function 'hub/spotify-auth--request)
	       (lambda (_url _method _headers _data callback)
		 (funcall callback 200 '((access_token . "synthetic-access") (refresh_token . "synthetic-refresh")
					 (expires_in . 3600)) nil))))
      (hub/spotify-auth--exchange context nil)
      (should-not (car result))
      (should-not (hub/spotify-auth--session-access-token context))
      (should (string-match-p "pass" (cadr result)))
      (should-not (string-match-p "synthetic-" (cadr result))))))

(ert-deftest hub/spotify-obsolete-authorization-cannot-overwrite-a-new-context ()
  "A response from an abandoned login cannot write another profile's pass entry."
  (let* ((context (hub/spotify-auth--session-create))
	 (hub/spotify-auth--context context) callback)
    (cl-letf (((symbol-function 'hub/spotify-auth--store) (lambda (&rest _) (ert-fail "Obsolete token persisted")))
	      ((symbol-function 'hub/spotify-auth--request)
	       (lambda (_url _method _headers _data handler) (setq callback handler))))
      (hub/spotify-auth--exchange context nil)
      (setq hub/spotify-auth--context (hub/spotify-auth--session-create))
      (funcall callback 200 '((access_token . "synthetic-access") (refresh_token . "synthetic-refresh")
			      (expires_in . 3600)) nil)
      (should-not (hub/spotify-auth--session-access-token hub/spotify-auth--context)))))

(ert-deftest hub/spotify-library-settings-stay-buffer-local ()
  "Opening the library does not change other tabulated-list views."
  (let ((tabulated-list-use-header-line t))
    (with-temp-buffer (hub/spotify-library-mode))
    (should tabulated-list-use-header-line)))

(ert-deftest hub/spotify-token-delivery-does-not-return-sensitive-callback-values ()
  "Evaluating a library command must not echo a token-bearing callback result."
  (let ((hub/spotify-auth--context
	 (hub/spotify-auth--session-create :access-token "synthetic-access"
					   :expires-at (+ (float-time) 3600)))
	received)
    (should-not (hub/spotify-auth-with-token
		 (lambda (token _error)
		   (setq received token)
		   (list 'sensitive-callback-result token))))
    (should (equal received "synthetic-access"))))

(provide 'spotify-test)
;;; spotify-test.el ends here
