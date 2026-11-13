;;; spotify-browser-ui-fixture.el --- Isolated Chrome/EMMS integration fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; Dedicated batch child only. Never read real credentials or contact Spotify.

;;; Code:
(require 'ert)
(require 'cl-lib)
(dolist (package '("emms" "websocket" "evil"))
  (add-to-list 'load-path (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
							(expand-file-name "straight/build" default-directory)))))
(require 'apps/spotify-player)
(defvar hub/spotify-browser-ui--file-player nil)

(defun hub/spotify-browser-ui-fixture (screen port)
  "Serve synthetic SCREEN on PORT, including a full mixed queue when requested."
  (let ((hub/spotify-callback-port port)
	(emms-player-started-hook nil) (emms-player-stopped-hook nil)
	(emms-player-finished-hook nil) (emms-track-initialize-functions nil)
	(emms-player-delay 0) (emms-player-next-function #'emms-next-noerror)
	(emms-repeat-track nil) (emms-single-track nil) (emms-player-playing-p nil)
	(emms-playlist-buffer nil) (emms-playlist-buffers nil) (emms-player-list nil)
	(original (symbol-function 'hub/spotify-sdk-probe--dispatch))
	(local-count 0) (renewals 0) (opens 0) (started nil) (done nil) timer playlist
	(mock (concat
	       "const screen=" (json-serialize (symbol-name screen)) ";let instance;window.fixtureStarts=0;"
	       "const originalFetch=window.fetch.bind(window);window.fetch=(url,options)=>{"
	       "if(String(url).startsWith('https://api.spotify.com/')){instance.start(JSON.parse(options.body).uris[0]);return Promise.resolve({ok:true,status:204})}"
	       "return originalFetch(url,options)};"
	       "window.fixtureRenewed=false;window.fixtureRenew=()=>{instance.options.getOAuthToken(token=>{window.fixtureRenewed=token==='SYNTHETIC-RENEWED'});return true};"
	       "window.Spotify={Player:class{constructor(options){if(screen==='pre-error')throw Error('PRIVATE');instance=this;this.options=options;this.events={};this.state=null}"
	       "addListener(n,f){this.events[n]=f}connect(){this.events.ready({device_id:'synthetic'});"
	       "if(screen==='error')setTimeout(()=>this.events.playback_error({message:'PRIVATE'}),1500);return Promise.resolve(true)}"
	       "activateElement(){return Promise.resolve()}getCurrentState(){return Promise.resolve(this.state)}"
	       "emit(position,paused=false,previous=false){this.state={position,paused,duration:10000,repeat_mode:0,shuffle:false,"
	       "track_window:{current_track:{uri:this.uri},"
	       "previous_tracks:previous?[{uri:this.uri}]:[],next_tracks:[]}};"
	       "this.events.player_state_changed(this.state)}"
	       "start(uri){this.uri=uri;window.fixtureStarts++;this.emit(0);setTimeout(()=>this.emit(1200),100);"
	       "if(!['controls','automatic','cancel','selection'].includes(screen)){setTimeout(()=>this.emit(9700),900);setTimeout(()=>this.emit(0,true,true),1600)}}"
	       "resume(){if(this.state)this.state={...this.state,paused:false};return Promise.resolve()}"
	       "seek(position){this.emit(position,this.state.paused);return Promise.resolve()}"
	       "pause(){if(this.state)this.state={...this.state,paused:true};return Promise.resolve()}"
	       "disconnect(){}}};window.onSpotifyWebPlaybackSDKReady();")))
    (cl-letf (((symbol-function 'auth-source-pass-get) (lambda (&rest _) (make-string 32 ?a)))
	      ((symbol-function 'hub/spotify-auth--random) (lambda () "synthetic-capability"))
	      ((symbol-function 'hub/spotify-sdk-probe--open-chrome) (lambda (_) (cl-incf opens)))
	      ((symbol-function 'hub/spotify-auth--request)
	       (lambda (_url _method _headers _data callback)
		 (cl-incf renewals)
		 (run-at-time 6 nil callback 200
			      '((access_token . "SYNTHETIC-RENEWED") (expires_in . 3600)
				(refresh_token . "SYNTHETIC-ROTATED")) nil)))
	      ((symbol-function 'hub/spotify-auth--store) (lambda (&rest _) nil))
	      ((symbol-function 'hub/spotify-sdk-probe--dispatch)
	       (lambda (context method target headers)
		 (cond
		  ((equal target "/mock-sdk.js") (list 200 "text/javascript" mock))
		  ((equal target "/fixture-state")
		   (hub/spotify-sdk-probe--json 200 `((ready . ,(if (hub/spotify-browser-ready-p) t :false))
						      (waiting . ,(if (hub/spotify-browser-player-waiting-p) t :false))
						      (opens . ,opens)
						      (failed . ,(if hub/spotify-browser--failed t :false))
						      (pending . ,(if (and hub/spotify-browser-player--run
									   (hub/spotify-browser-player--occurrence-pending hub/spotify-browser-player--run)) t :false))
						      (playing . ,(if emms-player-playing-p t :false))
						      (error . ,(if hub/spotify-player-last-error t :false))
						      (local . ,local-count) (renewals . ,renewals) (done . ,(if done t :false)))))
		  ((equal target "/fixture-connect")
		   (hub/spotify-browser-connect) (hub/spotify-sdk-probe--json 200 '((sent . t))))
		  ((equal target "/fixture-stop")
		   (emms-stop) (hub/spotify-sdk-probe--json 200 '((sent . t))))
		  ((equal target "/fixture-select")
		   (emms-playlist-current-select-next) (hub/spotify-sdk-probe--json 200 '((sent . t))))
		  ((equal target "/fixture-disconnect")
		   (run-at-time 0.05 nil #'hub/spotify-browser-disconnect)
		   (hub/spotify-sdk-probe--json 200 '((sent . t))))
		  ((equal target "/fixture-renew")
		   (setf (hub/spotify-sdk-probe--context-expires context) (- (float-time) 1)
			 (hub/spotify-sdk-probe--context-refresh-token context) "SYNTHETIC-REFRESH")
		   (hub/spotify-sdk-probe--json 200 '((sent . t))))
		  ((equal target "/fixture-seek") (emms-seek 3) (hub/spotify-sdk-probe--json 200 '((sent . t))))
		  ((equal target "/fixture-pause") (emms-pause) (hub/spotify-sdk-probe--json 200 '((sent . t))))
		  (t (let ((response (funcall original context method target headers)))
		       (when (equal target "/receiver.js")
			 (setf (nth 2 response) (replace-regexp-in-string
						 "https://sdk[.]scdn[.]co/spotify-player[.]js" "/mock-sdk.js" (nth 2 response) t t)))
		       response))))))
      (unwind-protect
	  (progn
	    (unless (memq screen '(automatic cancel selection))
	      (hub/spotify-browser-connect))
	    (when (memq screen '(mixed controls automatic cancel selection))
	      (setq hub/spotify-browser-ui--file-player
		    (emms-player (lambda (_track)
				   (cl-incf local-count)
				   (emms-player-started 'hub/spotify-browser-ui--file-player)
				   (run-at-time 0.2 nil #'emms-player-stopped))
				 #'ignore (lambda (track) (eq (emms-track-get track 'type) 'file)))
		    emms-player-list '(hub/spotify-browser-ui--file-player)
		    playlist (emms-playlist-new " *isolated browser queue*"))
	      (hub/spotify-player-register)
	      (setq emms-playlist-buffer playlist)
	      (with-current-buffer playlist
		(emms-playlist-insert-track (emms-track 'url "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
		(emms-playlist-insert-track (emms-track 'file "/tmp/synthetic.wav"))
		(emms-playlist-insert-track (emms-track 'url "spotify:track:1111111111111111111111"))
		(goto-char (point-min)) (emms-playlist-select (point)))
	      (if (memq screen '(automatic cancel selection))
		  (progn (setq started t) (emms-start))
		(setq timer (run-at-time
			     0.1 0.1 (lambda ()
				       (cond ((and (not started) (hub/spotify-browser-ready-p)) (setq started t) (emms-start))
					     ((and (eq screen 'mixed) started (= local-count 1) (not emms-player-playing-p)) (setq done t))))))))
	    (setf (hub/spotify-sdk-probe--context-token hub/spotify-browser--context) "SYNTHETIC-TOKEN"
		  (hub/spotify-sdk-probe--context-expires hub/spotify-browser--context) (+ (float-time) 300))
	    (sleep-for 45))
	(when (timerp timer) (cancel-timer timer))
	(hub/spotify-browser-disconnect)
	(when (buffer-live-p playlist) (kill-buffer playlist))))))

(provide 'spotify-browser-ui-fixture)
;;; spotify-browser-ui-fixture.el ends here
