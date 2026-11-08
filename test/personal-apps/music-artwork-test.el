;;; music-artwork-test.el --- Bounded, inert artwork loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Original two-pixel PNG and synthetic HTTP callbacks; never service credentials.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'apps/music-artwork)
(add-to-list 'load-path (expand-file-name "emms" (or (getenv "STRAIGHT_BUILD_PATH")
						     (expand-file-name "straight/build" default-directory))))
(require 'emms)

(defconst hub/music-artwork-test--png
  (base64-decode-string "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAIAAAB7QOjdAAAAD0lEQVR4nGOY/+CQuIwOAAvSAqG87UTwAAAAAElFTkSuQmCC")
  "Original two-by-one mint/navy PNG, not third-party album artwork.")

(defmacro hub/music-artwork-test--isolated (&rest body)
  "Run BODY with a private memory cache and synthetic network requests."
  (declare (indent 0))
  `(let ((hub/music-artwork--cache (make-hash-table :test #'equal)) requests)
     (unwind-protect
	 (cl-letf (((symbol-function 'url-retrieve)
		    (lambda (url callback args silent inhibit-cookies)
		      (let ((buffer (generate-new-buffer " *artwork HTTP fixture*")))
			(push (list url callback args buffer silent inhibit-cookies) requests)
			buffer)))
		   ((symbol-function 'auth-source-pass-get)
		    (lambda (&rest _) (ert-fail "Artwork read credentials"))))
	   ,@body)
       (maphash (lambda (source _) (hub/music-artwork-cancel source)) hub/music-artwork--cache)
       (dolist (request requests)
	 (when (buffer-live-p (nth 3 request)) (kill-buffer (nth 3 request)))))))

(defun hub/music-artwork-test--respond (request bytes &optional status)
  "Complete synthetic REQUEST with BYTES and HTTP STATUS."
  (with-current-buffer (nth 3 request)
    (set-buffer-multibyte nil)
    (insert "HTTP/1.1 200 OK\r\nContent-Type: image/png\r\n\r\n")
    (setq-local url-http-response-status (or status 200)
		url-http-end-of-headers (copy-marker (point)))
    (insert bytes)
    (apply (nth 1 request) nil (nth 2 request))))

(ert-deftest hub/music-artwork-deduplicates-public-cover-downloads ()
  "Repeated requests use one cookie-free download and cache its decoded image type."
  (hub/music-artwork-test--isolated
   (let ((source '(remote . "https://i.scdn.co/image/abc123")) (completed 0))
     (hub/music-artwork-request source (lambda () (cl-incf completed)))
     (hub/music-artwork-request source (lambda () (cl-incf completed)))
     (should (= (length requests) 1))
     (should (nth 4 (car requests)))
     (should (nth 5 (car requests)))
     (should (= (buffer-local-value 'url-max-redirections (nth 3 (car requests))) 0))
     (hub/music-artwork-test--respond (car requests) hub/music-artwork-test--png)
     (sleep-for 0.01)
     (let ((entry (hub/music-artwork-cached source)))
       (should (eq (hub/music-artwork--entry-state entry) 'ready))
       (should (eq (hub/music-artwork--entry-type entry) 'png))
       (should (equal (hub/music-artwork--entry-data entry) hub/music-artwork-test--png)))
     (should (= completed 2))
     (hub/music-artwork-request source #'ignore)
     (should (= (length requests) 1)))))

(ert-deftest hub/music-artwork-rejects-untrusted-or-credentialled-locations ()
  "Only the public Spotify image endpoint is a permitted remote source."
  (hub/music-artwork-test--isolated
   (dolist (url '("http://i.scdn.co/image/abc" "https://evil.test/image/abc"
		  "https://i.scdn.co.evil.test/image/abc" "https://user:secret@i.scdn.co/image/abc"
		  "https://i.scdn.co/image/abc?token=secret" "file:///tmp/cover.png"))
     (should-not (hub/music-artwork-valid-url-p url))
     (should-error (hub/music-artwork-request (cons 'remote url) #'ignore)))
   (should-not requests)))

(ert-deftest hub/music-artwork-invalid-and-oversized-images-fail-once ()
  "Bad images leave a remembered placeholder instead of a redraw retry loop."
  (dolist (bytes (list "not an image" (make-string (1+ hub/music-artwork--bytes-limit) ?x)))
    (hub/music-artwork-test--isolated
     (let ((source '(remote . "https://i.scdn.co/image/failed")))
       (hub/music-artwork-request source #'ignore)
       (hub/music-artwork-test--respond (car requests) bytes)
       (should (eq (hub/music-artwork--entry-state (hub/music-artwork-cached source)) 'failed))
       (hub/music-artwork-request source #'ignore)
       (should (= (length requests) 1))))))

(ert-deftest hub/music-artwork-cancelled-response-cannot-replace-new-request ()
  "A late callback from a retired request cannot populate a newer cache entry."
  (hub/music-artwork-test--isolated
   (let ((source '(remote . "https://i.scdn.co/image/shared")) old)
     (hub/music-artwork-request source #'ignore)
     (setq old (car requests))
     (hub/music-artwork-cancel source)
     (hub/music-artwork-request source #'ignore)
     ;; Simulate a callback already queued before cancellation killed its buffer.
     (with-temp-buffer (apply (nth 1 old) '(:error (error stale)) (nth 2 old)))
     (should (eq (hub/music-artwork--entry-state (hub/music-artwork-cached source)) 'pending))
     (hub/music-artwork-test--respond (car requests) hub/music-artwork-test--png)
     (should (eq (hub/music-artwork--entry-state (hub/music-artwork-cached source)) 'ready)))))

(ert-deftest hub/music-artwork-local-cover-is-read-once-without-network ()
  "Local cover files share the bounded memory cache and never use the network."
  (hub/music-artwork-test--isolated
   (let ((file (make-temp-file "music-cover-" nil ".png")))
     (unwind-protect
	 (progn
	   (let ((coding-system-for-write 'binary))
	     (write-region hub/music-artwork-test--png nil file nil 'silent))
	   (let ((source (cons 'file file)))
	     (hub/music-artwork-request source #'ignore)
	     (should (eq (hub/music-artwork--entry-state (hub/music-artwork-cached source)) 'ready))
	     (delete-file file)
	     (hub/music-artwork-request source #'ignore)
	     (should (eq (hub/music-artwork--entry-state (hub/music-artwork-cached source)) 'ready))))
       (when (file-exists-p file) (delete-file file))))
   (should-not requests)))

(ert-deftest hub/music-artwork-cache-and-timeout-are-bounded ()
  "Cache eviction and a stalled download release owned resources without retrying."
  (hub/music-artwork-test--isolated
   (let ((hub/music-artwork--entries-limit 2) (hub/music-artwork--timeout 0.02))
     (dotimes (index 3)
       (hub/music-artwork-request (cons 'remote (format "https://i.scdn.co/image/cache%d" index)) #'ignore))
     (should (= (hash-table-count hub/music-artwork--cache) 2))
     (should-not (hub/music-artwork-cached '(remote . "https://i.scdn.co/image/cache0")))
     (should-not (buffer-live-p (nth 3 (car (last requests)))))
     (sleep-for 0.04)
     (maphash (lambda (_source entry)
		(should (eq (hub/music-artwork--entry-state entry) 'failed))
		(should-not (hub/music-artwork--entry-buffer entry)))
	      hub/music-artwork--cache))))

(ert-deftest hub/music-artwork-old-queue-uses-loaded-library-without-mutation ()
  "Previously queued songs can use already-loaded album images without rewriting EMMS."
  (let* ((emms-track-initialize-functions nil) (emms-track-updated-functions nil)
	 (track (emms-track 'url "spotify:track:2222222222222222222222")))
    (emms-track-set track 'info-artwork-url nil)
    (with-temp-buffer
      (cl-progv '(hub/spotify-buffer-name) (list (buffer-name))
	(setq-local hub/spotify--tracks
		    '((0 . ((uri . "spotify:track:2222222222222222222222")
			    (album . ((images . (((url . "https://i.scdn.co/image/alreadyLoaded"))))))))))
	(insert "Library browsing position")
	(let ((position (point)) (revision (buffer-chars-modified-tick)))
	  (should (equal (hub/music-artwork-source track)
			 '(remote . "https://i.scdn.co/image/alreadyLoaded")))
	  (should-not (emms-track-get track 'info-artwork-url))
	  (should (= position (point)))
	  (should (= revision (buffer-chars-modified-tick))))))))

(ert-deftest hub/music-artwork-native-http-is-cookie-free-and-rejects-redirects-and-auth ()
  "Real URL callbacks decode PNG bytes without following redirects or seeking credentials."
  (dolist (scenario '(png redirect auth oversized))
    (let* ((status (pcase scenario ('redirect 302) ('auth 401) (_ 200)))
	   (hub/music-artwork--cache (make-hash-table :test #'equal))
	   (url-proxy-services '(("no_proxy" . "127.0.0.1")))
	   (url-request-extra-headers '(("Authorization" . "Bearer fixture-not-forwarded")
					("Cookie" . "fixture-not-forwarded")))
	   (body (pcase scenario
		   ('png hub/music-artwork-test--png)
		   ('oversized (concat hub/music-artwork-test--png
				       (make-string (+ hub/music-artwork--bytes-limit 65536) ?x)))
		   (_ "")))
	   (response (concat (format "HTTP/1.1 %d Fixture\r\nConnection: close\r\nContent-Length: %d\r\n" status (string-bytes body))
			     (pcase status (302 "Location: /second\r\n")
				    (401 "WWW-Authenticate: Basic realm=\"fixture\"\r\n") (_ ""))
			     "\r\n" body))
	   clients headers (auth-lookups 0)
	   (server (make-network-process
		    :name "Artwork HTTP fixture" :server t :host "127.0.0.1" :service t :family 'ipv4
		    :coding 'binary :noquery t
		    :log (lambda (_server client _message) (push client clients)
			   (set-process-sentinel client #'ignore))
		    :filter (lambda (process chunk)
			      (let ((request (concat (process-get process 'request) chunk)))
				(process-put process 'request request)
				(when (and (string-match-p "\r\n\r\n" request)
					   (not (process-get process 'replied)))
				  (process-put process 'replied t)
				  (push request headers)
				  (condition-case nil
				      (progn (process-send-string process response)
					     (process-send-eof process))
				    (error nil)))))))
	   (source (cons 'remote (format "http://127.0.0.1:%d/cover" (process-contact server :service)))))
      (unwind-protect
	  (cl-letf (((symbol-function 'hub/music-artwork-valid-url-p) (lambda (_) t))
		    ((symbol-function 'url-get-authentication)
		     (lambda (&rest _) (cl-incf auth-lookups) (ert-fail "Artwork attempted authentication"))))
	    (hub/music-artwork-request source #'ignore)
	    (cl-loop repeat 100 while (eq (hub/music-artwork--entry-state (hub/music-artwork-cached source)) 'pending)
		     do (accept-process-output nil 0.02))
	    (let ((entry (hub/music-artwork-cached source)))
	      (should (eq (hub/music-artwork--entry-state entry) (if (eq scenario 'png) 'ready 'failed)))
	      (when (eq scenario 'png) (should (equal (hub/music-artwork--entry-data entry) body))))
	    (should (= (length headers) 1))
	    (should (zerop auth-lookups))
	    (should-not (string-match-p "fixture-not-forwarded" (car headers))))
	(hub/music-artwork-cancel source)
	(dolist (process (cons server clients)) (when (process-live-p process) (delete-process process)))))))

(ert-deftest hub/music-artwork-local-discovery-does-not-inspect-remote-files ()
  "Local cover.png discovery is bounded to the album directory and skips TRAMP entirely."
  (let ((directory (make-temp-file "music-album-" t)) (emms-track-initialize-functions nil))
    (unwind-protect
	(let* ((cover (expand-file-name "cover.png" directory))
	       (track (emms-track 'file (expand-file-name "song.ogg" directory))))
	  (let ((coding-system-for-write 'binary))
	    (write-region hub/music-artwork-test--png nil cover nil 'silent))
	  (should (equal (hub/music-artwork-source track) (cons 'file cover)))
	  (let ((remote (emms-track 'file "/ssh:fixture:/album/song.ogg")))
	    (cl-letf (((symbol-function 'file-regular-p)
		       (lambda (&rest _) (ert-fail "Artwork inspected a remote path"))))
	      (should-not (hub/music-artwork-source remote)))))
      (delete-directory directory t))))

(provide 'music-artwork-test)
;;; music-artwork-test.el ends here
