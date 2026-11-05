;;; spotify-sdk-probe-test.el --- Browser proof authorization boundary -*- lexical-binding: t; -*-

;;; Commentary:
;; Only synthetic credentials and loopback sockets. Never open a real browser.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'apps/spotify-sdk-probe nil t)

(ert-deftest hub/spotify-sdk-probe-opens-isolated-loopback-session ()
  "Start without touching EMMS, saved authorization, or secret client credentials."
  (let ((hub/spotify-sdk-probe--session nil) (hub/spotify-callback-port 0) opened context)
    (cl-letf (((symbol-function 'auth-source-pass-get)
	       (lambda (field entry)
		 (should (eq field 'secret))
		 (should (equal entry hub/spotify-client-id-entry))
		 (make-string 32 ?a)))
	      ((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-capability"))
	      ((symbol-function 'hub/spotify-sdk-probe--open-chrome) (lambda (url) (setq opened url)))
	      ((symbol-function 'call-process-region) (lambda (&rest _) (ert-fail "Must not persist authorization"))))
      (unwind-protect
	  (progn
	    (should-not (hub/spotify-sdk-probe "https://open.spotify.com/track/4uLU6hMCjMI75M1A2tKUQC?si=example"))
	    (setq context hub/spotify-sdk-probe--session)
	    (should (string-match-p "\\`http://127.0.0.1:[0-9]+/#synthetic-capability\\'" opened))
	    (should (process-live-p (hub/spotify-sdk-probe--context-listener context)))
	    (should (equal (hub/spotify-sdk-probe--context-track context) "spotify:track:4uLU6hMCjMI75M1A2tKUQC")))
	(hub/spotify-sdk-probe-stop))
      (should-not (hub/spotify-sdk-probe--context-token context))
      (should-not (hub/spotify-sdk-probe--context-key context)))))

(defmacro hub/spotify-sdk-probe-test--with-context (&rest body)
  "Run BODY with an owned synthetic context and authenticated headers."
  (declare (indent 0))
  `(let* ((context (hub/spotify-sdk-probe--context-create
		    :client (make-string 32 ?a) :key "synthetic-key" :port 8765
		    :track "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
	  (hub/spotify-sdk-probe--session context)
	  (headers '(("host" . "127.0.0.1:8765") ("x-emacs-spotify-probe" . "synthetic-key"))))
     ,@body))

(ert-deftest hub/spotify-sdk-probe-token-is-not-a-public-loopback-endpoint ()
  "Neither cross-origin requests nor a public page can read browser credentials."
  (hub/spotify-sdk-probe-test--with-context
   (setf (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-ACCESS"
	 (hub/spotify-sdk-probe--context-expires context) (+ (float-time) 300))
   (let ((denied (hub/spotify-sdk-probe--dispatch context "GET" "/token" '(("host" . "127.0.0.1:8765")))))
     (should (= (car denied) 403))
     (should-not (string-match-p "SYNTHETIC-ACCESS" (nth 2 denied))))
   (should (= (car (hub/spotify-sdk-probe--dispatch context "GET" "/token"
						    (cons '("origin" . "https://example.org") headers))) 403))
   (should (= (car (hub/spotify-sdk-probe--dispatch context "GET" "/token" headers)) 200))
   (should-not (string-match-p "SYNTHETIC-ACCESS"
			       (nth 2 (hub/spotify-sdk-probe--dispatch context "GET" "/" headers))))))

(ert-deftest hub/spotify-sdk-probe-pkce-is-single-use-and-memory-only ()
  "Accept only the owned OAuth state, with separate scopes and no secret persistence."
  (hub/spotify-sdk-probe-test--with-context
   (let (request)
     (cl-letf (((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-random"))
	       ((symbol-function 'hub/spotify-auth--request)
		(lambda (_url _method _headers data callback)
		  (setq request data)
		  (funcall callback 200 '((access_token . "SYNTHETIC-ACCESS") (expires_in . 3600)
					  (refresh_token . "SYNTHETIC-REFRESH")) nil)))
	       ((symbol-function 'call-process-region) (lambda (&rest _) (ert-fail "Must not persist credentials"))))
       (let ((authorization (hub/spotify-sdk-probe--dispatch context "POST" "/authorize" headers)))
	 (should (= (car authorization) 200))
	 (should (string-match-p "code_challenge_method=S256" (nth 2 authorization)))
	 (should (string-match-p "streaming" (nth 2 authorization))))
       (should (= (car (hub/spotify-sdk-probe--dispatch context "GET"
							"/spotify/callback?state=wrong&code=synthetic-code" headers)) 400))
       (should-not request)
       (should (= (car (hub/spotify-sdk-probe--dispatch context "GET"
							"/spotify/callback?state=synthetic-random&code=synthetic-code" headers)) 303))
       (should (string-match-p "code_verifier=synthetic-random" request))
       (should (equal (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-ACCESS"))
       (should-not (hub/spotify-sdk-probe--context-state context))
       (should (= (car (hub/spotify-sdk-probe--dispatch context "GET"
							"/spotify/callback?state=synthetic-random&code=synthetic-code" headers)) 400))))))

(ert-deftest hub/spotify-sdk-probe-expired-token-is-not-served ()
  "A stale credential cannot reach the SDK."
  (hub/spotify-sdk-probe-test--with-context
   (setf (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-ACCESS"
	 (hub/spotify-sdk-probe--context-expires context) (- (float-time) 1))
   (should (= (car (hub/spotify-sdk-probe--dispatch context "GET" "/token" headers)) 409))))

(ert-deftest hub/spotify-sdk-probe-native-http-serves-only-capability-authenticated-state ()
  "Exercise the real listener, HTTP parser, headers and JSON response."
  (let ((hub/spotify-sdk-probe--session nil) (hub/spotify-callback-port 0) result)
    (cl-letf (((symbol-function 'auth-source-pass-get) (lambda (&rest _) (make-string 32 ?a)))
	      ((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-capability"))
	      ((symbol-function 'hub/spotify-sdk-probe--open-chrome) #'ignore))
      (unwind-protect
	  (progn
	    (hub/spotify-sdk-probe "spotify:track:4uLU6hMCjMI75M1A2tKUQC" t)
	    (hub/spotify-auth--request
	     (concat (hub/spotify-sdk-probe--origin hub/spotify-sdk-probe--session) "/state") "GET"
	     '(("X-Emacs-Spotify-Probe" . "synthetic-capability") ("Authorization" . "")) nil
	     (lambda (status json _retry) (setq result (list status json))))
	    (let ((deadline (+ (float-time) 3)))
	      (while (and (not result) (< (float-time) deadline)) (accept-process-output nil 0.01)))
	    (should (= (car result) 200))
	    (should-not (alist-get 'authorized (cadr result)))
	    (should (equal (alist-get 'track_uri (cadr result)) "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
	    (should (eq (alist-get 'ending_check (cadr result)) t)))
	(hub/spotify-sdk-probe-stop)))))

(ert-deftest hub/spotify-sdk-probe-stale-exchange-cannot-replace-a-newer-grant ()
  "A delayed authorization response cannot revive a superseded browser attempt."
  (hub/spotify-sdk-probe-test--with-context
   (let (pending)
     (cl-letf (((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-random"))
	       ((symbol-function 'hub/spotify-auth--request)
		(lambda (_url _method _headers _data callback) (setq pending callback))))
       (hub/spotify-sdk-probe--dispatch context "POST" "/authorize" headers)
       (hub/spotify-sdk-probe--dispatch context "GET"
					"/spotify/callback?state=synthetic-random&code=synthetic-code" headers)
       (hub/spotify-sdk-probe--dispatch context "POST" "/authorize" headers)
       (funcall pending 200 '((access_token . "SYNTHETIC-STALE") (expires_in . 3600)) nil)
       (should-not (hub/spotify-sdk-probe--context-token context))))))

(ert-deftest hub/spotify-sdk-probe-browser-behavior-tests ()
  "Run the browser state-machine scenarios without credentials or external packages."
  (should (executable-find "node"))
  (with-temp-buffer
    (let ((result (process-file "node" nil (current-buffer) nil "--test"
				(expand-file-name "test/personal-apps/spotify-sdk-probe.test.cjs" default-directory))))
      (unless (and (integerp result) (zerop result)) (ert-fail (buffer-string))))))

(ert-deftest hub/spotify-sdk-probe-ending-mode-is-an-explicit-json-boolean ()
  "Preserve the default preview and expose ending mode only when requested."
  (hub/spotify-sdk-probe-test--with-context
   (dolist (ending '(nil t))
     (setf (hub/spotify-sdk-probe--context-ending-check context) ending)
     (let* ((response (hub/spotify-sdk-probe--dispatch context "GET" "/state" headers))
	    (json (json-parse-string (nth 2 response) :object-type 'hash-table :false-object :false)))
       (should (eq (gethash "ending_check" json) (if ending t :false)))))))

(ert-deftest hub/spotify-sdk-probe-no-error-is-json-null-not-an-object ()
  "Chrome must not interpret an empty Lisp value as a truthy error object."
  (hub/spotify-sdk-probe-test--with-context
   (let* ((response (hub/spotify-sdk-probe--dispatch context "GET" "/state" headers))
	  (json (json-parse-string (nth 2 response) :object-type 'hash-table :null-object nil)))
     (should-not (gethash "error" json)))))

(defun hub/spotify-sdk-probe-test--serve-ui (screen port)
  "Serve synthetic SCREEN on PORT for isolated Chrome screenshot checks.
No real browser is opened, no pass entries are read, and no Spotify API is used."
  (let ((hub/spotify-callback-port port) (hub/spotify-sdk-probe--session nil)
	(original (symbol-function 'hub/spotify-sdk-probe--dispatch))
	(mock (concat
	       "const screen=" (json-serialize (symbol-name screen)) ";let instance;"
	       "const originalFetch=window.fetch.bind(window);"
	       "function state(p,paused){return {position:p,paused,duration:194607,repeat_mode:0,shuffle:false,"
	       "track_window:{current_track:{uri:'spotify:track:4uLU6hMCjMI75M1A2tKUQC',name:'Selected song',"
	       "artists:[{name:'Artist'}]},previous_tracks:[],next_tracks:[]}}}"
	       "window.fetch=(url,options)=>{if(String(url).startsWith('https://api.spotify.com/')){"
	       "queueMicrotask(()=>instance.emit(0,false));"
	       "return Promise.resolve({ok:true,status:204});}return originalFetch(url,options);};"
	       "window.Spotify={Player:class{constructor(){instance=this;this.events={}}"
	       "addListener(n,f){this.events[n]=f}connect(){this.events.ready({device_id:'synthetic'});"
	       "if(['playing','ending-result','ending-listening'].includes(screen))setTimeout(()=>document.getElementById('play').click(),100);"
	       "if(screen==='ending-result')setTimeout(()=>document.getElementById('play').click(),400);"
	       "if(screen==='error')setTimeout(()=>this.events.account_error({message:'synthetic'}),100);"
	       "return Promise.resolve(true)}activateElement(){return Promise.resolve()}"
	       "emit(p,paused){this.state=state(p,paused);this.events.player_state_changed(this.state)}"
	       "seek(p){queueMicrotask(()=>this.emit(p,false));setTimeout(()=>{this.emit(194000,false);this.emit(0,true)},1000);return Promise.resolve()}"
	       "getCurrentState(){return Promise.resolve(this.state)}"
	       "pause(){return Promise.resolve()}disconnect(){}}};window.onSpotifyWebPlaybackSDKReady();")))
    (cl-letf (((symbol-function 'auth-source-pass-get) (lambda (&rest _) (make-string 32 ?a)))
	      ((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-capability"))
	      ((symbol-function 'hub/spotify-sdk-probe--open-chrome) #'ignore)
	      ((symbol-function 'hub/spotify-sdk-probe--dispatch)
	       (lambda (context method target headers)
		 (if (equal target "/mock-sdk.js") (list 200 "text/javascript" mock)
		   (let ((response (funcall original context method target headers)))
		     (when (equal target "/probe.js")
		       (setf (nth 2 response)
			     (replace-regexp-in-string "https://sdk[.]scdn[.]co/spotify-player[.]js"
						       "/mock-sdk.js" (nth 2 response) t t)))
		     response)))))
      (unwind-protect
	  (progn
	    (hub/spotify-sdk-probe "spotify:track:4uLU6hMCjMI75M1A2tKUQC"
				   (memq screen '(ending-ready ending-listening ending-result)))
	    (unless (eq screen 'idle)
	      (setf (hub/spotify-sdk-probe--context-token hub/spotify-sdk-probe--session) "synthetic-token"
		    (hub/spotify-sdk-probe--context-expires hub/spotify-sdk-probe--session) (+ (float-time) 300)))
	    (sleep-for 60))
	(hub/spotify-sdk-probe-stop)))))

(provide 'spotify-sdk-probe-test)
;;; spotify-sdk-probe-test.el ends here
