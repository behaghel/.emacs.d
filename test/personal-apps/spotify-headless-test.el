;;; spotify-headless-test.el --- Private receiver lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic pairing and credentials only.  Never start the real receiver here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'apps/spotify-headless nil t)

(ert-deftest hub/spotify-headless-config-is-private-and-single-track ()
  "The receiver exposes only loopback control, with discovery and audio caching off."
  (let* ((config (hub/spotify-headless--configuration 12345))
	 (server (alist-get 'server config)))
    (should (equal (alist-get 'address server) "127.0.0.1"))
    (should (= (alist-get 'port server) 12345))
    (should (eq (alist-get 'zeroconf_enabled config) :false))
    (should (eq (alist-get 'disable_autoplay config) t))
    (should (eq (alist-get 'enabled (alist-get 'cache config)) :false))
    (should (equal (alist-get 'type (alist-get 'credentials config)) "device_auth"))))

(ert-deftest hub/spotify-headless-http-never-sends-library-credentials ()
  "The local transport sends no OAuth bearer token and refuses unrelated endpoints."
  (let ((hub/spotify-headless--port 12345) (hub/spotify-headless--key "synthetic-key") seen)
    (cl-letf (((symbol-function 'hub/spotify-auth--request)
	       (lambda (url _method headers _data callback)
		 (setq seen (list url headers)) (funcall callback 200 nil nil))))
      (hub/spotify-headless-request "POST" "/player/pause" nil #'ignore)
      (should (equal (car seen) "http://127.0.0.1:12345/player/pause"))
      (should (equal (cdr (assoc "Authorization" (cadr seen))) ""))
      (should (equal (cdr (assoc "X-Emacs-Spotify-Key" (cadr seen))) "synthetic-key"))
      (should-error (hub/spotify-headless-request "POST" "/token" nil #'ignore)))))

(ert-deftest hub/spotify-headless-pair-save-and-disconnect-clean-up ()
  "Explicit pairing stores encrypted credentials and removes owned plaintext state."
  (let ((hub/spotify-headless--process nil) (hub/spotify-headless--directory nil)
	(hub/spotify-headless--port nil) (hub/spotify-headless--ready nil)
	(hub/spotify-headless--timer nil) (hub/spotify-headless--saved nil)
	(hub/spotify-headless--pair-url nil) (hub/spotify-headless--deadline nil)
	(hub/spotify-headless-program (expand-file-name invocation-name invocation-directory))
	(real-make-process (symbol-function 'make-process))
	ready stored browser directory child)
    (cl-letf (((symbol-function 'hub/spotify-headless--private-build-p) (lambda () t))
	      ((symbol-function 'auth-source-pass-get) (lambda (&rest _) nil))
	      ((symbol-function 'hub/spotify-headless--write-pass) (lambda (record) (setq stored record)))
	      ((symbol-function 'browse-url) (lambda (url &rest _) (setq browser url)))
	      ((symbol-function 'hub/spotify-headless-request)
	       (lambda (_method path _data &optional callback)
		 (funcall callback (if hub/spotify-headless--key 200 403)
			  (if (equal path "/") `((playback_ready . ,ready))
			    '((url . "https://www.spotify.com/pair?code=SYNTHETIC"))))))
	      ((symbol-function 'make-process)
	       (lambda (&rest args)
		 (if (equal (plist-get args :name) "EMMS Spotify receiver")
		     (apply real-make-process (plist-put args :command '("/bin/sh" "-c" "exec sleep 30")))
		   (apply real-make-process args)))))
      (unwind-protect
	  (progn
	    (hub/spotify-headless-connect)
	    (setq directory hub/spotify-headless--directory child hub/spotify-headless--process)
	    (should (= (file-modes directory) #o700))
	    (should-not (file-in-directory-p directory default-directory))
	    (hub/spotify-headless--probe)
	    (hub/spotify-headless--probe)
	    (should (equal browser "https://www.spotify.com/pair?code=SYNTHETIC"))
	    (with-temp-file (expand-file-name "state.json" directory)
	      (insert "{\"device_id\":\"fake\",\"credentials\":{\"username\":\"fake-user\",\"data\":\"synthetic-auth\"}}"))
	    (setq ready t)
	    (hub/spotify-headless--probe)
	    (should (hub/spotify-headless-ready-p))
	    (should (string-match-p "synthetic-auth" stored))
	    (hub/spotify-headless-disconnect)
	    (should-not (process-live-p child))
	    (should-not (file-exists-p directory)))
	(hub/spotify-headless-disconnect)))))

(ert-deftest hub/spotify-headless-does-not-claim-ready-without-encrypted-credentials ()
  "A ready API without saved credentials is not an accepted receiver session."
  (let ((hub/spotify-headless--process 'synthetic-process)
	(hub/spotify-headless--ready nil) (hub/spotify-headless--probing nil)
	(hub/spotify-headless--verified t)
	(hub/spotify-headless--deadline (+ (float-time) 300))
	(hub/spotify-headless--timer nil) cleaned)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
	      ((symbol-function 'hub/spotify-headless--save) (lambda () nil))
	      ((symbol-function 'hub/spotify-headless--cleanup) (lambda (&rest _) (setq cleaned t)))
	      ((symbol-function 'hub/spotify-headless-request)
	       (lambda (_method _path _data callback) (funcall callback 200 '((playback_ready . t))))))
      (hub/spotify-headless--probe)
      (should-not hub/spotify-headless--ready)
      (should cleaned))))

(ert-deftest hub/spotify-headless-secret-storage-uses-stdin-only ()
  "Stored receiver credentials never appear in process arguments."
  (let (arguments contents)
    (cl-letf (((symbol-function 'call-process-region)
	       (lambda (start end &rest args)
		 (setq arguments args contents (buffer-substring-no-properties start end)) 0)))
      (hub/spotify-headless--write-pass "synthetic-private-credential"))
    (should (equal contents "synthetic-private-credential"))
    (should-not (member "synthetic-private-credential" arguments))))

(ert-deftest hub/spotify-headless-pairing-rejects-untrusted-or-insecure-links ()
  "A local service cannot cause arbitrary browser navigation during pairing."
  (let ((hub/spotify-headless--pair-url nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) (ert-fail "Untrusted pairing URL opened"))))
      (dolist (url '("http://www.spotify.com/pair" "https://spotify.com.example.org/pair" "file:///tmp/private"))
	(hub/spotify-headless--pairing `((url . ,url)))))))

(ert-deftest hub/spotify-headless-refuses-unpatched-build-before-reading-pass ()
  "An unrelated receiver cannot receive cached credentials before capability checks."
  (let ((hub/spotify-headless--process nil) (hub/spotify-headless--directory nil)
	(hub/spotify-headless-program (expand-file-name invocation-name invocation-directory))
	called)
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (insert "unpatched-upstream") 0))
	      ((symbol-function 'auth-source-pass-get) (lambda (&rest _) (setq called t)))
	      ((symbol-function 'make-process) (lambda (&rest _) (error "Unpatched receiver must not start"))))
      (should-error (hub/spotify-headless-connect) :type 'user-error)
      (should-not called)
      (should-not hub/spotify-headless--directory))))

(ert-deftest hub/spotify-headless-refuses-an-unauthenticated-api ()
  "A receiver with an unguarded root is stopped before browser pairing."
  (let ((hub/spotify-headless--process 'synthetic-process)
	(hub/spotify-headless--ready nil) (hub/spotify-headless--probing nil)
	(hub/spotify-headless--verified nil) (hub/spotify-headless--key "synthetic")
	(hub/spotify-headless--deadline (+ (float-time) 300)) cleaned)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
	      ((symbol-function 'hub/spotify-headless--cleanup) (lambda (&rest _) (setq cleaned t)))
	      ((symbol-function 'hub/spotify-headless-request)
	       (lambda (_method _path _data callback) (funcall callback 200 '((playback_ready . nil)))))
	      ((symbol-function 'browse-url) (lambda (&rest _) (ert-fail "Unauthenticated API paired"))))
      (hub/spotify-headless--probe)
      (should cleaned)
      (should-not hub/spotify-headless--ready))))

(ert-deftest hub/spotify-headless-native-http-delivers-unauthenticated-status ()
  "A real local 403 reaches attestation without consulting interactive auth."
  (let ((hub/spotify-headless--key nil) server result prompted)
    (unwind-protect
	(progn
	  (setq server (make-network-process
			:name "Synthetic private API" :server t :family 'ipv4
			:host "127.0.0.1" :service 0 :noquery t
			:filter (lambda (client _request)
				  (process-send-string client "HTTP/1.1 403 Forbidden\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}")
				  (delete-process client))))
	  (let ((hub/spotify-headless--port (process-contact server :service))
		(deadline (+ (float-time) 3)))
	    (cl-letf (((symbol-function 'url-get-authentication)
		       (lambda (&rest _) (setq prompted t) nil)))
	      (hub/spotify-headless-request "GET" "/" nil (lambda (status json) (setq result (cons status json))))
	      (while (and (not result) (< (float-time) deadline)) (accept-process-output nil 0.01))
	      (should (= (car result) 403))
	      (should-not prompted))))
      (when (process-live-p server) (delete-process server)))))

(ert-deftest hub/spotify-headless-quit-during-save-still-removes-private-state ()
  "Cancelling GPG cannot strand the receiver's plaintext runtime directory."
  (let* ((directory (make-temp-file "/tmp/emms-spotify-test-" t))
	 (hub/spotify-headless--directory directory) (hub/spotify-headless--process nil)
	 (hub/spotify-headless--timer nil) (hub/spotify-headless--key "synthetic") cancelled)
    (unwind-protect
	(progn
	  (cl-letf (((symbol-function 'hub/spotify-headless--save) (lambda () (signal 'quit nil))))
	    (condition-case nil (hub/spotify-headless--cleanup) (quit (setq cancelled t))))
	  (should cancelled)
	  (should-not (file-exists-p directory))
	  (should-not hub/spotify-headless--key))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest hub/spotify-headless-quit-during-setup-cleans-created-resources ()
  "Cancelling setup removes its directory even before the child is started."
  (let ((hub/spotify-headless--directory nil) (hub/spotify-headless--process nil)
	(hub/spotify-headless--timer nil)
	(hub/spotify-headless-program (expand-file-name invocation-name invocation-directory))
	directory cancelled)
    (unwind-protect
	(progn
	  (cl-letf (((symbol-function 'hub/spotify-headless--private-build-p) (lambda () t))
		    ((symbol-function 'auth-source-pass-get) (lambda (&rest _) nil))
		    ((symbol-function 'hub/spotify-auth--random)
		     (lambda () (setq directory hub/spotify-headless--directory) (signal 'quit nil)))
		    ((symbol-function 'make-process) (lambda (&rest _) (error "Child must not start"))))
	    (condition-case nil (hub/spotify-headless-connect) (quit (setq cancelled t))))
	  (should cancelled)
	  (should directory)
	  (should-not hub/spotify-headless--directory)
	  (should-not (file-exists-p directory)))
      (when (and directory (file-directory-p directory)) (delete-directory directory t)))))

(provide 'spotify-headless-test)
;;; spotify-headless-test.el ends here
