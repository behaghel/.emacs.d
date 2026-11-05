;;; spotify-browser-test.el --- Native private browser transport tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic client ID and access token only. No real browser or Spotify service.

;;; Code:
(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "websocket" (or (getenv "STRAIGHT_BUILD_PATH")
							  (expand-file-name "straight/build" default-directory))))
(require 'apps/spotify-browser)

(defmacro hub/spotify-browser-test--session (&rest body)
  "Run BODY with an isolated real loopback listener and synthetic authorization."
  (declare (indent 0))
  `(let ((hub/spotify-sdk-probe--session nil) (hub/spotify-callback-port 0)
	 (hub/spotify-browser--context nil) (hub/spotify-browser--server nil)
	 (hub/spotify-browser--socket nil) (hub/spotify-browser--ready nil)
	 (hub/spotify-browser--timer nil) (hub/spotify-browser-lost-hook nil)
	 (hub/spotify-browser-event-function nil))
     (cl-letf (((symbol-function 'auth-source-pass-get)
		(lambda (_field entry) (should (equal entry hub/spotify-client-id-entry)) (make-string 32 ?a)))
	       ((symbol-function 'hub/spotify-sdk-probe--open-chrome) #'ignore))
       (unwind-protect
	   (progn (hub/spotify-browser-connect) ,@body)
	 (hub/spotify-browser-disconnect)))))

(defun hub/spotify-browser-test--wait (predicate)
  "Process native events for at most two seconds until PREDICATE succeeds."
  (let ((deadline (+ (float-time) 2)))
    (while (and (not (funcall predicate)) (< (float-time) deadline)) (accept-process-output nil 0.01))
    (should (funcall predicate))))

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
