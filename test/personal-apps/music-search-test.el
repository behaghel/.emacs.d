;;; music-search-test.el --- Passive queue search acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Real EMMS occurrence selection, synthetic playback and native completion data.
;; No accounts, media processes, network or capture.

;;; Code:
(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
	     (expand-file-name "emms" (or (getenv "STRAIGHT_BUILD_PATH")
					  (expand-file-name "straight/build" default-directory))))
(require 'emms)
(require 'emms-playlist-mode)
(autoload 'hub/music-search "apps/music-search" nil t)
(defvar hub/music-search-test--player nil)

(defmacro hub/music-search-test--queue (&rest body)
  "Run BODY with duplicate track objects in a real mixed EMMS playlist."
  (declare (indent 0))
  `(let* ((emms-playlist-buffer nil) (emms-playlist-buffers nil)
	  (emms-track-initialize-functions nil) (emms-cache-get-function nil)
	  (emms-cache-set-function nil) (emms-cache-modified-function nil)
	  (emms-player-playing-p nil) (emms-player-paused-p nil)
	  (emms-player-started-hook nil) (emms-player-stopped-hook nil)
	  (emms-player-finished-hook nil) (emms-player-paused-hook nil)
	  (playlist (emms-playlist-new " *music search queue*"))
	  (document (generate-new-buffer " *music search document*"))
	  (duplicate (emms-track 'url "spotify:track:4uLU6hMCjMI75M1A2tKUQC"))
	  positions started (stopped 0)
	  (hub/music-search-test--player
	   (emms-player
	    (lambda (track)
	      (push (list track (with-current-buffer playlist
				  (marker-position emms-playlist-selected-marker))) started)
	      (emms-player-started 'hub/music-search-test--player))
	    (lambda () (cl-incf stopped) (emms-player-stopped))
	    (lambda (_) t)))
	  (emms-player-list '(hub/music-search-test--player)))
     (setq emms-playlist-buffer playlist)
     (emms-track-set duplicate 'info-title "Low Light")
     (emms-track-set duplicate 'info-artist "Quiet Geometry")
     (emms-track-set duplicate 'info-album "Still / Motion")
     (emms-track-set duplicate 'info-playing-time 274)
     (unwind-protect
	 (save-window-excursion
	   (with-current-buffer playlist
	     (dolist (track (list duplicate (emms-track 'file "/tmp/local.ogg") duplicate))
	       (push (point) positions)
	       (emms-playlist-insert-track track))
	     (setq positions (nreverse positions))
	     (emms-playlist-select (car positions)))
	   (switch-to-buffer document)
	   (insert "An unchanged writing buffer.") (goto-char 4)
	   (cl-letf (((symbol-function 'auth-source-pass-get)
		      (lambda (&rest _) (ert-fail "Search read credentials")))
		     ((symbol-function 'url-retrieve)
		      (lambda (&rest _) (ert-fail "Search fetched metadata")))
		     ((symbol-function 'make-process)
		      (lambda (&rest _) (ert-fail "Search started a process"))))
	     ,@body))
       (when emms-player-playing-p (emms-stop))
       (dolist (buffer (list playlist document))
	 (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hub/music-search-ret-plays-the-existing-duplicate-occurrence ()
  "Queue RET selects occurrence #3, not the first equal URI or a new queue item."
  (hub/music-search-test--queue
   (let ((before (with-current-buffer playlist (buffer-string)))
	 (window (selected-window)))
     (cl-letf (((symbol-function 'completing-read)
		(lambda (prompt collection &rest _)
		  (let ((candidates (all-completions "" collection)))
		    (should (string-match-p "Queue: 3" prompt))
		    (should (string-match-p "queue only" prompt))
		    (should (= (length candidates) 3))
		    (should (string-match-p "Low Light" (nth 2 candidates)))
		    (should (string-match-p "Quiet Geometry" (nth 2 candidates)))
		    (should (string-match-p "Queue #3" (nth 2 candidates)))
		    (should-not started)
		    (should (zerop stopped))
		    (nth 2 candidates)))))
       (call-interactively #'hub/music-search))
     (should (= (length started) 1))
     (should (eq (caar started) duplicate))
     (should (= (cadar started) (nth 2 positions)))
     (should (equal before (with-current-buffer playlist (buffer-string))))
     (should (eq (current-buffer) document))
     (should (eq (selected-window) window))
     (should (= (point) 4)))))

(ert-deftest hub/music-search-cancel-restores-document-without-affecting-playback ()
  "Browsing and cancellation leave playback, selected occurrence and focus unchanged."
  (hub/music-search-test--queue
   (emms-start)
   (setq emms-player-paused-p t)
   (let ((before (with-current-buffer playlist (buffer-string)))
	 (window (selected-window)))
     (cl-letf (((symbol-function 'completing-read)
		(lambda (_prompt collection &rest _)
		  (should (= (length (all-completions "Low Light" collection)) 2))
		  (should (eq (completion-metadata-get (completion-metadata "" collection nil)
						       'display-sort-function) #'identity))
		  (should (= (length started) 1))
		  (should (zerop stopped))
		  (switch-to-buffer playlist)
		  (signal 'quit nil))))
       (should (eq (condition-case nil (hub/music-search) (quit 'cancelled)) 'cancelled)))
     (should (eq (current-buffer) document))
     (should (eq (selected-window) window))
     (should (= (point) 4))
     (should emms-player-paused-p)
     (should (eq emms-player-playing-p 'hub/music-search-test--player))
     (should (zerop stopped))
     (should (= (length started) 1))
     (should (equal before (with-current-buffer playlist (buffer-string))))
     (should (= (with-current-buffer playlist (marker-position emms-playlist-selected-marker))
		(car positions))))))

(ert-deftest hub/music-search-rejects-changed-queues-before-stopping-playback ()
  "Edits, identical replacement and playlist loss cannot play an obsolete occurrence."
  (dolist (change '(append reinsert playlist killed))
    (hub/music-search-test--queue
     (emms-start)
     (cl-letf (((symbol-function 'completing-read)
		(lambda (_prompt collection &rest _)
		  (let ((choice (nth 2 (all-completions "" collection))))
		    (pcase change
		      ('append (with-current-buffer playlist
				 (let ((inhibit-read-only t))
				   (goto-char (point-max))
				   (emms-playlist-insert-track (emms-track 'file "/tmp/added.ogg")))))
		      ('reinsert (with-current-buffer playlist
				   (let ((inhibit-read-only t))
				     (delete-region (nth 2 positions) (point-max))
				     (goto-char (point-max))
				     (emms-playlist-insert-track duplicate))))
		      ('playlist (setq emms-playlist-buffer nil))
		      ('killed (kill-buffer playlist)))
		    choice))))
       (should-error (hub/music-search) :type 'user-error))
     (should (zerop stopped))
     (should (= (length started) 1)))))

(ert-deftest hub/music-search-rechecks-after-the-stop-acknowledgement ()
  "A queue edit while waiting for Stop cannot redirect the accepted Play action."
  (hub/music-search-test--queue
   (emms-start)
   (let ((stop (symbol-function 'emms-stop)))
     (cl-letf (((symbol-function 'completing-read)
		(lambda (_prompt collection &rest _) (nth 2 (all-completions "" collection))))
	       ((symbol-function 'emms-stop)
		(lambda ()
		  (funcall stop)
		  (with-current-buffer playlist
		    (let ((inhibit-read-only t))
		      (goto-char (point-max))
		      (emms-playlist-insert-track (emms-track 'file "/tmp/during-stop.ogg")))))))
       (should-error (hub/music-search) :type 'user-error)))
   (should (= stopped 1))
   (should (= (length started) 1))))

(ert-deftest hub/music-search-empty-queue-is-passive-and-actionable ()
  "No playlist or an empty playlist requests no playback and opens no completion."
  (require 'apps/music-search)
  (hub/music-search-test--queue
   (with-current-buffer playlist
     (let ((inhibit-read-only t)) (erase-buffer)))
   (cl-letf (((symbol-function 'completing-read)
	      (lambda (&rest _) (ert-fail "Empty queue opened completion"))))
     (should-error (hub/music-search) :type 'user-error)
     (setq emms-playlist-buffer nil)
     (should-error (hub/music-search) :type 'user-error))
   (should-not started)
   (should (zerop stopped))))

(ert-deftest hub/music-search-input-map-keeps-spaces-as-text ()
  "The completion-local map accepts spaces without changing global completion keys."
  (require 'apps/music-search)
  (let ((original (lookup-key minibuffer-local-must-match-map (kbd "SPC"))))
    (with-temp-buffer
      (use-local-map minibuffer-local-must-match-map)
      (hub/music-search--minibuffer-setup)
      (should (eq (lookup-key (current-local-map) (kbd "SPC")) #'self-insert-command))
      (should-not (eq (current-local-map) minibuffer-local-must-match-map)))
    (should (equal original (lookup-key minibuffer-local-must-match-map (kbd "SPC"))))))

(provide 'music-search-test)
;;; music-search-test.el ends here
