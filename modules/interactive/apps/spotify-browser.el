;;; spotify-browser.el --- Owned SDK transport and renewable authorization -*- lexical-binding: t; -*-

;;; Commentary:
;; Reuse the proven PKCE host; one authenticated loopback WebSocket carries
;; commands for the current occurrence, acknowledgements and safe state only.

;;; Code:
(require 'hub-utils)
(require 'apps/spotify-sdk-probe)
(require 'websocket)
(require 'cl-lib)
(require 'json)

(defcustom hub/spotify-browser-token-entry "dev/spotify/emacs-music/playback"
  "Pass entry for encrypted playback refresh authorization, not access tokens."
  :type 'string :group 'hub/spotify)

(defvar hub/spotify-browser--server nil "Owned loopback WebSocket listener.")
(defvar hub/spotify-browser--socket nil "Single capability-authenticated browser.")
(defvar hub/spotify-browser--context nil "Owned ephemeral authorization context.")
(defvar hub/spotify-browser--ready nil "Non-nil after explicit audio activation.")
(defvar hub/spotify-browser--serial 0 "Monotonic command identity, never a playlist.")
(defvar hub/spotify-browser--timer nil "Owned readiness and heartbeat timer.")
(defvar hub/spotify-browser--pong 0 "Last authenticated browser response time.")
(defvar hub/spotify-browser--tick 0 "Last local scheduler heartbeat time.")
(defvar hub/spotify-browser--wait-seq nil "Stop acknowledgement being awaited.")
(defvar hub/spotify-browser--wait-run nil "Occurrence whose stop is being awaited.")
(defvar hub/spotify-browser--stop-acked nil "Whether the awaited stop was acknowledged.")
(defvar hub/spotify-browser-event-function nil "Consumer of safe occurrence events.")
(defvar hub/spotify-browser-lost-hook nil "Functions notified when this receiver is lost.")

(defun hub/spotify-browser-ready-p ()
  "Return non-nil only for an activated, authenticated, live SDK receiver."
  (and hub/spotify-browser--ready hub/spotify-browser--socket
       (websocket-openp hub/spotify-browser--socket)
       (eq hub/spotify-browser--context hub/spotify-sdk-probe--session)
       (or (hub/spotify-sdk-probe--valid-token-p hub/spotify-browser--context)
	   (and (hub/spotify-sdk-probe--context-busy hub/spotify-browser--context)
		(hub/spotify-sdk-probe--context-refresh-token hub/spotify-browser--context) t))))

(defun hub/spotify-browser--cleanup ()
  "Invalidate ownership before closing resources and notifying the backend."
  (let ((server hub/spotify-browser--server))
    (setq hub/spotify-browser--ready nil hub/spotify-browser--socket nil
	  hub/spotify-browser--server nil hub/spotify-browser--context nil)
    (when (timerp hub/spotify-browser--timer) (cancel-timer hub/spotify-browser--timer))
    (setq hub/spotify-browser--timer nil)
    (when (processp server) (websocket-server-close server))
    (run-hooks 'hub/spotify-browser-lost-hook)))

;;;###autoload
(defun hub/spotify-browser-disconnect ()
  "Disconnect the owned browser receiver without advancing the EMMS queue."
  (interactive)
  (if (and hub/spotify-browser--context
	   (eq hub/spotify-browser--context hub/spotify-sdk-probe--session))
      (hub/spotify-sdk-probe-stop)
    (hub/spotify-browser--cleanup)))

(defun hub/spotify-browser--route (_context _method target _headers)
  "Serve named receiver TARGET assets or authenticated bridge configuration."
  (pcase target
    ("/" (hub/spotify-sdk-probe--asset "spotify-browser.html" "text/html"))
    ("/receiver.js" (hub/spotify-sdk-probe--asset "spotify-browser.js" "text/javascript"))
    ("/bridge-config"
     (hub/spotify-sdk-probe--json 200 `((url . ,(format "ws://127.0.0.1:%d/"
							(process-contact hub/spotify-browser--server :service))))))
    (_ '(404 "text/plain" "Not found."))))

(defun hub/spotify-browser--send-json (socket json)
  "Send controlled JSON to SOCKET without enabling library packet diagnostics."
  (let ((websocket-debug nil))
    (websocket-send-text socket (json-serialize json :null-object nil :false-object :false))))

(defun hub/spotify-browser--message (socket frame)
  "Authenticate SOCKET, then accept only bounded decoded FRAME events."
  (condition-case nil
      (let* ((text (websocket-frame-text frame))
	     (json (and (<= (length text) 8192)
			(json-parse-string text :object-type 'alist :array-type 'list
					   :null-object nil :false-object nil)))
	     (type (alist-get 'type json)))
	(cond
	 ((not (eq socket hub/spotify-browser--socket))
	  (if (and (not hub/spotify-browser--socket) (equal type "hello")
		   hub/spotify-browser--context
		   (eq hub/spotify-browser--context hub/spotify-sdk-probe--session)
		   (equal (alist-get 'key json)
			  (hub/spotify-sdk-probe--context-key hub/spotify-browser--context)))
	      (setq hub/spotify-browser--socket socket hub/spotify-browser--pong (float-time))
	    (websocket-close socket)))
	 ((equal type "pong") (setq hub/spotify-browser--pong (float-time)))
	 ((equal type "ready")
	  (setq hub/spotify-browser--ready (eq (alist-get 'ready json) t)))
	 ((member type '("ack" "state" "ended" "error"))
	  (when (and (equal type "ack") (eql (alist-get 'seq json) hub/spotify-browser--wait-seq)
		     (eql (alist-get 'run json) hub/spotify-browser--wait-run))
	    (setq hub/spotify-browser--stop-acked t))
	  (when hub/spotify-browser-event-function
	    (funcall hub/spotify-browser-event-function json)))
	 (t (websocket-close socket))))
    (error (websocket-close socket))))

(defun hub/spotify-browser--filter (process chunk)
  "Validate bounded handshake input from PROCESS before library parsing CHUNK."
  (let* ((ws (process-get process :websocket))
	 (text (concat (websocket-inflight-input ws) chunk)))
    (if (> (length text) 16384) (websocket-close ws)
      (if (and (eq (websocket-ready-state ws) 'connecting)
	       (not (string-match-p "\r\n\r\n" text)))
	  (setf (websocket-inflight-input ws) text)
	(if (and (eq (websocket-ready-state ws) 'connecting)
		 (not (and hub/spotify-browser--context
			   (string-prefix-p "GET / HTTP/1.1\r\n" text)
			   (string-match-p
			    (regexp-quote (format "\r\nHost: 127.0.0.1:%d\r\n"
						  (process-contact hub/spotify-browser--server :service))) text)
			   (string-match-p
			    (regexp-quote (concat "\r\nOrigin: "
						  (hub/spotify-sdk-probe--origin hub/spotify-browser--context) "\r\n")) text))))
	    (websocket-close ws)
	  ;; Neither malformed headers nor third-party packet text reach logs.
	  (let ((websocket-debug nil) (inhibit-message t) (message-log-max nil))
	    (condition-case nil (websocket-server-filter process chunk)
	      (error (websocket-close ws)))))))))

(defun hub/spotify-browser--heartbeat ()
  "Ping the owned receiver and expire missing or unauthorized sessions safely."
  (let ((now (float-time)))
    ;; Do not blame Chrome for a pause in Emacs's own event loop.
    (when (> (- now hub/spotify-browser--tick) 3) (setq hub/spotify-browser--pong now))
    (setq hub/spotify-browser--tick now)
    (when hub/spotify-browser--socket
      (when (hub/spotify-sdk-probe--renewal-due-p hub/spotify-browser--context)
	(hub/spotify-sdk-probe--refresh-token hub/spotify-browser--context))
      (if (or (> (- now hub/spotify-browser--pong) 8)
	      (and hub/spotify-browser--ready
		   (not (or (hub/spotify-sdk-probe--valid-token-p hub/spotify-browser--context)
			    (and (hub/spotify-sdk-probe--context-busy hub/spotify-browser--context)
				 (hub/spotify-sdk-probe--context-refresh-token hub/spotify-browser--context))))))
	  (hub/spotify-browser-disconnect)
	(condition-case nil
	    (hub/spotify-browser--send-json hub/spotify-browser--socket '((type . "ping")))
	  (error (hub/spotify-browser-disconnect)))))))

;;;###autoload
(defun hub/spotify-browser-connect (&optional reauthorize)
  "Open Chrome for remembered playback authorization and audio activation.
With prefix REAUTHORIZE, request fresh Spotify consent."
  (interactive "P")
  (when (member hub/spotify-browser-token-entry (list hub/spotify-token-entry hub/spotify-client-id-entry))
    (user-error "Playback authorization needs a separate pass entry"))
  (hub/spotify-browser-disconnect)
  (condition-case err
      (progn
	(hub/spotify-sdk-probe "" nil #'hub/spotify-browser--route #'hub/spotify-browser--cleanup
			       0 (unless reauthorize hub/spotify-browser-token-entry))
	;; Fresh consent still saves its new playback grant after skipping restore.
	(setf (hub/spotify-sdk-probe--context-token-entry hub/spotify-sdk-probe--session)
	      hub/spotify-browser-token-entry)
	(setq hub/spotify-browser--context hub/spotify-sdk-probe--session
	      hub/spotify-browser--server
	      (make-network-process
	       :name "Emacs Spotify browser" :server t :host "127.0.0.1" :family 'ipv4 :service 0
	       :noquery t :filter-multibyte nil :filter #'hub/spotify-browser--filter
	       :log (lambda (server client event)
		      (if (>= (cl-count server websocket-server-websockets :key #'websocket-server-conn) 8)
			  (delete-process client)
			(websocket-server-accept server client event)
			(let ((ws (process-get client :websocket)))
			  (run-at-time 3 nil (lambda ()
					       (unless (eq ws hub/spotify-browser--socket) (websocket-close ws)))))))
	       :plist (list :on-open (lambda (ws)
				       (unless (equal (websocket-origin ws)
						      (hub/spotify-sdk-probe--origin hub/spotify-browser--context))
					 (websocket-close ws)))
			    :on-message #'hub/spotify-browser--message
			    :on-close (lambda (ws) (when (eq ws hub/spotify-browser--socket)
						     (hub/spotify-browser-disconnect)))
			    :on-error (lambda (ws &rest _)
					(if (eq ws hub/spotify-browser--socket)
					    (hub/spotify-browser-disconnect)
					  (websocket-close ws)))))
	      hub/spotify-browser--pong (float-time) hub/spotify-browser--tick (float-time)
	      hub/spotify-browser--timer (run-at-time 1 1 #'hub/spotify-browser--heartbeat))
	(message "Receiver opened; saved authorization is reused when available. Enable audio in Chrome"))
    ((error quit) (hub/spotify-browser-disconnect) (signal (car err) (cdr err)))))

(defun hub/spotify-browser-send (kind run &optional data)
  "Send command KIND for occurrence RUN with controlled DATA; return its identity."
  (unless (and hub/spotify-browser--socket (websocket-openp hub/spotify-browser--socket))
    (user-error "Browser receiver disconnected; M-x hub/spotify-browser-connect"))
  (let ((seq (cl-incf hub/spotify-browser--serial)))
    (hub/spotify-browser--send-json
     hub/spotify-browser--socket (append `((type . "command") (kind . ,kind) (run . ,run) (seq . ,seq)) data))
    seq))

(defun hub/spotify-browser-stop (run)
  "Stop RUN and wait for acknowledgement; refuse an unconfirmed transition."
  (let* ((hub/spotify-browser--stop-acked nil)
	 (hub/spotify-browser--wait-run run)
	 (hub/spotify-browser--wait-seq (hub/spotify-browser-send "stop" run))
	 (deadline (+ (float-time) 5)))
    (while (and (not hub/spotify-browser--stop-acked) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (unless hub/spotify-browser--stop-acked
      (hub/spotify-browser-disconnect)
      (user-error "Spotify stop unconfirmed; transition blocked. Close the receiver tab before retrying"))))

(provide 'apps/spotify-browser)
;;; spotify-browser.el ends here
