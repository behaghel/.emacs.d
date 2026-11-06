;;; spotify-browser-test.el --- Native private browser transport tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic client ID and access token only. No real browser or Spotify service.

;;; Code:
(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "websocket" (or (getenv "STRAIGHT_BUILD_PATH")
							  (expand-file-name "straight/build" default-directory))))
(require 'apps/spotify-browser)

(defvar hub/spotify-browser-test--grant nil "Synthetic saved playback authorization.")

(defmacro hub/spotify-browser-test--session (&rest body)
  "Run BODY with an isolated real loopback listener and synthetic authorization."
  (declare (indent 0))
  `(let ((hub/spotify-sdk-probe--session nil) (hub/spotify-callback-port 0)
	 (hub/spotify-browser--context nil) (hub/spotify-browser--server nil)
	 (hub/spotify-browser--socket nil) (hub/spotify-browser--ready nil)
	 (hub/spotify-browser--timer nil) (hub/spotify-browser-lost-hook nil)
	 (hub/spotify-browser-event-function nil))
     (cl-letf (((symbol-function 'auth-source-pass-get)
		(lambda (_field entry)
		  (cond ((equal entry hub/spotify-client-id-entry) (make-string 32 ?a))
			((equal entry hub/spotify-browser-token-entry) hub/spotify-browser-test--grant)
			(t (ert-fail "Unexpected credential entry")))))
	       ((symbol-function 'hub/spotify-sdk-probe--open-chrome) #'ignore))
       (unwind-protect
	   (progn (hub/spotify-browser-connect) ,@body)
	 (hub/spotify-browser-disconnect)))))

(defun hub/spotify-browser-test--wait (predicate)
  "Process native events for at most two seconds until PREDICATE succeeds."
  (let ((deadline (+ (float-time) 2)))
    (while (and (not (funcall predicate)) (< (float-time) deadline)) (accept-process-output nil 0.01))
    (should (funcall predicate))))

(ert-deftest hub/spotify-browser-connect-reuses-current-preparation ()
  "Repeated Connect during preparation preserves the receiver and never rereads pass."
  (hub/spotify-browser-test--session
   (let ((context hub/spotify-browser--context) (server hub/spotify-browser--server))
     (cl-letf (((symbol-function 'auth-source-pass-get)
		(lambda (&rest _) (ert-fail "Existing receiver reread credentials")))
	       ((symbol-function 'hub/spotify-sdk-probe--open-chrome)
		(lambda (&rest _) (ert-fail "Existing receiver opened another tab"))))
       (hub/spotify-browser-connect)
       (should (eq context hub/spotify-browser--context))
       (should (eq server hub/spotify-browser--server))))))

(ert-deftest hub/spotify-browser-fatal-receiver-is-not-ready ()
  "An activated but failed SDK must be prepared again before another Play request."
  (hub/spotify-browser-test--session
   (let ((hub/spotify-browser--socket 'synthetic-socket)
	 (hub/spotify-browser--ready t) (hub/spotify-browser--failed t))
     (setf (hub/spotify-sdk-probe--context-token hub/spotify-browser--context) "SYNTHETIC-TOKEN"
	   (hub/spotify-sdk-probe--context-expires hub/spotify-browser--context) (+ (float-time) 300))
     (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t)))
       (should-not (hub/spotify-browser-ready-p))))))

(ert-deftest hub/spotify-browser-private-routes-do-not-leak-token-or-control-address ()
  "Assets are public but bridge configuration and tokens require the capability."
  (hub/spotify-browser-test--session
   (let* ((context hub/spotify-browser--context)
	  (headers `(("host" . ,(format "127.0.0.1:%d" (hub/spotify-sdk-probe--context-port context))))))
     (should (= (car (hub/spotify-sdk-probe--dispatch context "GET" "/" headers)) 200))
     (should (= (car (hub/spotify-sdk-probe--dispatch context "GET" "/bridge-config" headers)) 403))
     (push (cons "x-emacs-spotify-probe" (hub/spotify-sdk-probe--context-key context)) headers)
     (should (= (car (hub/spotify-sdk-probe--dispatch context "GET" "/bridge-config" headers)) 200))
     (should-not (hub/spotify-browser-ready-p))
     (should-error (hub/spotify-sdk-probe "spotify:track:4uLU6hMCjMI75M1A2tKUQC") :type 'user-error))))

(ert-deftest hub/spotify-browser-native-websocket-authenticates-and-confirms-stop ()
  "Exercise actual handshake, origin, hello capability, command and ACK framing."
  (hub/spotify-browser-test--session
   (let* ((context hub/spotify-browser--context) client commands valid-ack-sent
	  (key (hub/spotify-sdk-probe--context-key context)))
     (setf (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-TOKEN"
	   (hub/spotify-sdk-probe--context-expires context) (+ (float-time) 300))
     (unwind-protect
	 (progn
	   (setq client
		 (websocket-open
		  (format "ws://127.0.0.1:%d/" (process-contact hub/spotify-browser--server :service)) :nowait t
		  :custom-header-alist `(("Origin" . ,(hub/spotify-sdk-probe--origin context)))
		  :on-open (lambda (socket)
			     (hub/spotify-browser--send-json socket `((type . "hello") (key . ,key)))
			     (hub/spotify-browser--send-json socket '((type . "ready") (ready . t))))
		  :on-message (lambda (socket frame)
				(let ((json (json-parse-string (websocket-frame-text frame) :object-type 'alist)))
				  (when (equal (alist-get 'type json) "command")
				    (push json commands)
				    (hub/spotify-browser--send-json socket `((type . "ack") (run . 999) (seq . ,(alist-get 'seq json))))
				    (run-at-time 0.05 nil
						 (lambda ()
						   (setq valid-ack-sent t)
						   (hub/spotify-browser--send-json socket
										   `((type . "ack") (run . ,(alist-get 'run json)) (seq . ,(alist-get 'seq json)))))))))
		  :on-error (lambda (&rest _) nil)))
	   (hub/spotify-browser-test--wait #'hub/spotify-browser-ready-p)
	   (hub/spotify-browser-stop 7)
	   (should valid-ack-sent)
	   (should (equal (alist-get 'kind (car commands)) "stop"))
	   (should (eql (alist-get 'run (car commands)) 7)))
       (when client (websocket-close client))))))

(ert-deftest hub/spotify-browser-wrong-capability-cannot-activate-receiver ()
  "A loopback client with the right origin still cannot guess receiver ownership."
  (hub/spotify-browser-test--session
   (let (client)
     (unwind-protect
	 (progn
	   (setq client (websocket-open
			 (format "ws://127.0.0.1:%d/" (process-contact hub/spotify-browser--server :service)) :nowait t
			 :custom-header-alist `(("Origin" . ,(hub/spotify-sdk-probe--origin hub/spotify-browser--context)))
			 :on-open (lambda (ws) (hub/spotify-browser--send-json ws '((type . "hello") (key . "wrong"))))
			 :on-error (lambda (&rest _) nil)))
	   (hub/spotify-browser-test--wait (lambda () (eq (websocket-ready-state client) 'closed)))
	   (should-not hub/spotify-browser--socket)
	   (should-not (hub/spotify-browser-ready-p)))
       (when client (websocket-close client))))))

(ert-deftest hub/spotify-browser-remembers-playback-grant-across-reconnections ()
  "Authorize once, save only a separate refresh grant, and restore without OAuth."
  (let ((hub/spotify-browser-test--grant nil) saved)
    (hub/spotify-browser-test--session
     (let ((context hub/spotify-browser--context))
       (cl-letf (((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-state"))
		 ((symbol-function 'hub/spotify-auth--store)
		  (lambda (session refresh)
		    (setq saved (hub/spotify-auth--session-token-entry session)
			  hub/spotify-browser-test--grant
			  (json-serialize `((client_id . ,(make-string 32 ?a)) (refresh_token . ,refresh))))))
		 ((symbol-function 'hub/spotify-auth--request)
		  (lambda (_url _method _headers _data callback)
		    (funcall callback 200 '((access_token . "SYNTHETIC-FIRST") (expires_in . 3600)
					    (refresh_token . "SYNTHETIC-REFRESH")) nil))))
	 (hub/spotify-sdk-probe--authorize context)
	 (hub/spotify-sdk-probe--callback context "/spotify/callback?state=synthetic-state&code=synthetic-code")
	 (should saved)
	 (should (equal saved hub/spotify-browser-token-entry))
	 (should-not (equal saved hub/spotify-token-entry))
	 (should-not (string-match-p "SYNTHETIC-FIRST" hub/spotify-browser-test--grant)))
       (hub/spotify-browser-disconnect)
       (cl-letf (((symbol-function 'hub/spotify-auth--request)
		  (lambda (_url _method _headers data callback)
		    (should (string-match-p "grant_type=refresh_token" data))
		    (should (string-match-p "SYNTHETIC-REFRESH" data))
		    (funcall callback 200 '((access_token . "SYNTHETIC-RESTORED") (expires_in . 3600)) nil)))
		 ((symbol-function 'hub/spotify-auth--store)
		  (lambda (&rest _) (ert-fail "Unchanged refresh grant need not be saved"))))
	 (hub/spotify-browser-connect)
	 (should (equal (hub/spotify-sdk-probe--context-token hub/spotify-browser--context)
			"SYNTHETIC-RESTORED"))
	 (should-not (hub/spotify-sdk-probe--context-state hub/spotify-browser--context))
	 (should-not (hub/spotify-sdk-probe--context-timer hub/spotify-browser--context)))))))

(ert-deftest hub/spotify-browser-renews-before-expiry-without-losing-receiver ()
  "A live receiver renews once, keeps its socket, and needs no new consent."
  (hub/spotify-browser-test--session
   (let* ((context hub/spotify-browser--context) (requests 0) pending
	  (hub/spotify-browser--socket 'synthetic-socket)
	  (hub/spotify-browser--ready t)
	  (hub/spotify-browser--pong (float-time))
	  (hub/spotify-browser--tick (float-time)))
     (setf (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-OLD"
	   (hub/spotify-sdk-probe--context-expires context) (+ (float-time) 60)
	   (hub/spotify-sdk-probe--context-refresh-token context) "SYNTHETIC-REFRESH")
     (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
	       ((symbol-function 'hub/spotify-browser--send-json) #'ignore)
	       ((symbol-function 'hub/spotify-auth--request)
		(lambda (_url _method _headers _data callback) (cl-incf requests) (setq pending callback))))
       (hub/spotify-browser--heartbeat)
       (should (= requests 1))
       (setf (hub/spotify-sdk-probe--context-expires context) (- (float-time) 1))
       (should (eq (hub/spotify-browser-ready-p) t))
       (hub/spotify-browser--heartbeat)
       (should (= requests 1))
       (funcall pending 200 '((access_token . "SYNTHETIC-RENEWED") (expires_in . 3600)) nil)
       (should (hub/spotify-browser-ready-p))
       (should (eq hub/spotify-browser--context context))
       (should (equal (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-RENEWED"))
       (should-not (hub/spotify-sdk-probe--context-state context))))))

(ert-deftest hub/spotify-browser-native-token-requests-wait-for-one-renewal ()
  "Concurrent authenticated HTTP token requests await one atomic refresh rotation."
  (hub/spotify-browser-test--session
   (let* ((context hub/spotify-browser--context)
	  (native-request (symbol-function 'hub/spotify-auth--request))
	  (url (concat (hub/spotify-sdk-probe--origin context) "/token"))
	  (headers `(("X-Emacs-Spotify-Probe" . ,(hub/spotify-sdk-probe--context-key context))
		     ("Authorization" . "")))
	  (requests 0) pending results saved)
     (setf (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-EXPIRED"
	   (hub/spotify-sdk-probe--context-expires context) (- (float-time) 1)
	   (hub/spotify-sdk-probe--context-refresh-token context) "SYNTHETIC-REFRESH")
     (cl-letf (((symbol-function 'hub/spotify-auth--request)
		(lambda (_url _method _headers _data callback) (cl-incf requests) (setq pending callback)))
	       ((symbol-function 'hub/spotify-auth--store)
		(lambda (session refresh)
		  (should (hub/spotify-sdk-probe--context-busy context))
		  (should (equal (hub/spotify-auth--session-token-entry session) hub/spotify-browser-token-entry))
		  (setq saved refresh))))
       (dotimes (_ 2)
	 (funcall native-request url "GET" headers nil
		  (lambda (status json _retry) (push (list status json) results))))
       (hub/spotify-browser-test--wait (lambda () pending))
       (accept-process-output nil 0.05)
       (should-not results)
       (should (= requests 1))
       (funcall pending 200 '((access_token . "SYNTHETIC-NEW") (expires_in . 3600)
			      (refresh_token . "SYNTHETIC-ROTATED")) nil)
       (hub/spotify-browser-test--wait (lambda () (= (length results) 2)))
       (should (equal saved "SYNTHETIC-ROTATED"))
       (dolist (result results)
	 (should (= (car result) 200))
	 (should (equal (alist-get 'access_token (cadr result)) "SYNTHETIC-NEW")))))))

(ert-deftest hub/spotify-browser-late-renewal-cannot-revive-disconnected-session ()
  "A delayed rotated grant never writes credentials or revives a discarded receiver."
  (hub/spotify-browser-test--session
   (let ((context hub/spotify-browser--context) pending)
     (setf (hub/spotify-sdk-probe--context-refresh-token context) "SYNTHETIC-REFRESH")
     (cl-letf (((symbol-function 'hub/spotify-auth--request)
		(lambda (_url _method _headers _data callback) (setq pending callback)))
	       ((symbol-function 'hub/spotify-auth--store)
		(lambda (&rest _) (ert-fail "Stale callback wrote authorization"))))
       (should-not (hub/spotify-sdk-probe--refresh-token context))
       (hub/spotify-browser-disconnect)
       (funcall pending 200 '((access_token . "SYNTHETIC-LATE") (expires_in . 3600)
			      (refresh_token . "SYNTHETIC-LATE-ROTATION")) nil)
       (should-not hub/spotify-browser--context)
       (should-not (hub/spotify-sdk-probe--context-token context))
       (should-not (hub/spotify-sdk-probe--context-refresh-token context))))))

(ert-deftest hub/spotify-browser-renewal-rate-limit-keeps-valid-token-and-backs-off ()
  "Temporary renewal failures preserve a usable token and honor Retry-After."
  (hub/spotify-browser-test--session
   (let ((context hub/spotify-browser--context) (requests 0))
     (setf (hub/spotify-sdk-probe--context-token context) "SYNTHETIC-STILL-VALID"
	   (hub/spotify-sdk-probe--context-expires context) (+ (float-time) 90)
	   (hub/spotify-sdk-probe--context-refresh-token context) "SYNTHETIC-REFRESH")
     (cl-letf (((symbol-function 'hub/spotify-auth--request)
		(lambda (_url _method _headers _data callback)
		  (cl-incf requests)
		  (funcall callback 429 '((error . "SYNTHETIC-PRIVATE-DETAIL")) 60)))
	       ((symbol-function 'auth-source-pass-get)
		(lambda (&rest _) (ert-fail "Renewal must not reread credentials"))))
       (hub/spotify-sdk-probe--refresh-token context)
       (hub/spotify-sdk-probe--refresh-token context)
       (should (= requests 1))
       (should (hub/spotify-sdk-probe--valid-token-p context))
       (should-not (hub/spotify-sdk-probe--context-error context))
       (should (> (hub/spotify-sdk-probe--context-retry-at context) (+ (float-time) 50)))))))

(ert-deftest hub/spotify-browser-force-consent-skips-saved-grant-but-remembers-replacement ()
  "Prefix connect requests new consent without reading the old playback grant."
  (hub/spotify-browser-test--session
   (cl-letf (((symbol-function 'auth-source-pass-get)
	      (lambda (_field entry)
		(should (equal entry hub/spotify-client-id-entry)) (make-string 32 ?a)))
	     ((symbol-function 'hub/spotify-auth--request)
	      (lambda (&rest _) (ert-fail "Fresh consent must not refresh a saved grant"))))
     (hub/spotify-browser-connect t)
     (should-not (hub/spotify-sdk-probe--context-token hub/spotify-browser--context))
     (should (equal (hub/spotify-sdk-probe--context-token-entry hub/spotify-browser--context)
		    hub/spotify-browser-token-entry)))))

(ert-deftest hub/spotify-browser-playback-entry-cannot-overwrite-library-authorization ()
  "A mistaken shared credential entry fails before replacing the owned receiver."
  (hub/spotify-browser-test--session
   (let ((context hub/spotify-browser--context)
	 (hub/spotify-browser-token-entry hub/spotify-token-entry))
     (should-error (hub/spotify-browser-connect) :type 'user-error)
     (should (eq hub/spotify-browser--context context)))))

(defun hub/spotify-browser-test--serve-ui (screen port)
  "Serve synthetic SCREEN on PORT for isolated Chrome checks, without account access."
  (let ((hub/spotify-callback-port port)
	(original (symbol-function 'hub/spotify-sdk-probe--dispatch))
	(mock (concat "const screen=" (json-serialize (symbol-name screen)) ";"
		      "window.Spotify={Player:class{constructor(){this.events={}}addListener(n,f){this.events[n]=f}"
		      "connect(){this.events.ready({device_id:'synthetic'});setTimeout(()=>document.getElementById('activate').click(),100);"
		      "if(screen==='error')setTimeout(()=>this.events.playback_error({message:'PRIVATE'}),300);return Promise.resolve(true)}"
		      "activateElement(){return Promise.resolve()}getCurrentState(){return Promise.resolve(null)}"
		      "pause(){return Promise.resolve()}disconnect(){}}};window.onSpotifyWebPlaybackSDKReady();")))
    (cl-letf (((symbol-function 'auth-source-pass-get) (lambda (&rest _) (make-string 32 ?a)))
	      ((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-capability"))
	      ((symbol-function 'hub/spotify-sdk-probe--open-chrome) #'ignore)
	      ((symbol-function 'hub/spotify-sdk-probe--dispatch)
	       (lambda (context method target headers)
		 (if (equal target "/mock-sdk.js") (list 200 "text/javascript" mock)
		   (let ((response (funcall original context method target headers)))
		     (when (equal target "/receiver.js")
		       (setf (nth 2 response) (replace-regexp-in-string
					       "https://sdk[.]scdn[.]co/spotify-player[.]js" "/mock-sdk.js" (nth 2 response) t t)))
		     response)))))
      (unwind-protect
	  (progn
	    (hub/spotify-browser-connect)
	    (setf (hub/spotify-sdk-probe--context-token hub/spotify-browser--context) "SYNTHETIC-TOKEN"
		  (hub/spotify-sdk-probe--context-expires hub/spotify-browser--context) (+ (float-time) 300))
	    (sleep-for 60))
	(hub/spotify-browser-disconnect)))))

(provide 'spotify-browser-test)
;;; spotify-browser-test.el ends here
