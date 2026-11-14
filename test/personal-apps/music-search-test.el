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
(defvar hub/spotify-buffer-name)
(defvar hub/music-search--operation)
(dolist (package '("consult" "compat" "embark" "embark-consult"))
  (add-to-list 'load-path
	       (expand-file-name package (or (getenv "STRAIGHT_BUILD_PATH")
					     (expand-file-name "straight/build" default-directory)))))

(require 'music-test-helpers
	 (expand-file-name "music-test-helpers" (file-name-directory (or load-file-name buffer-file-name))))

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
  (should (eq (lookup-key hub/music-search-map (kbd "M-RET")) #'hub/music-search--enqueue))
  (let ((original (lookup-key minibuffer-local-must-match-map (kbd "SPC"))))
    (with-temp-buffer
      (use-local-map minibuffer-local-must-match-map)
      (hub/music-search--minibuffer-setup)
      (should (eq (lookup-key (current-local-map) (kbd "SPC")) #'self-insert-command))
      (should-not (eq (current-local-map) minibuffer-local-must-match-map)))
    (should (equal original (lookup-key minibuffer-local-must-match-map (kbd "SPC"))))))

(ert-deftest hub/music-search-reopening-after-queue-clear-shows-only-liked-results ()
  "Clearing the real EMMS queue removes previous combined provenance on reopen."
  (require 'apps/music-search)
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (let ((searches 0))
      (cl-letf (((symbol-function 'completing-read)
		 (lambda (prompt collection &rest _)
		   (cl-incf searches)
		   (let ((candidates (all-completions "" collection)))
		     (if (= searches 1)
			 (progn
			   (should (string-match-p "Queue: 3" prompt))
			   (should (cl-some (lambda (candidate) (string-match-p "Queue #" candidate)) candidates)))
		       (should (string-match-p "Queue: 0" prompt))
		       (should (= (length candidates) 1))
		       (should (string-match-p "♥\\|Liked" (car candidates)))
		       (should-not (string-match-p "Liked #[0-9]" (car candidates)))
		       (should-not (string-match-p "Queue #" (car candidates))))
		     (signal 'quit nil)))))
	(condition-case nil (hub/music-search) (quit nil))
	(with-current-buffer playlist (emms-playlist-clear))
	(condition-case nil (hub/music-search) (quit nil)))
      (should (= searches 2)))
    (should-not added)
    (should-not started)
    (should (zerop stopped)))))

(ert-deftest hub/music-search-loaded-library-works-without-a-queue ()
  "Loaded-only coverage is honest; RET delegates the retained record to Play."
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (setq emms-playlist-buffer nil)
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (prompt collection &rest _)
		 (should (string-match-p "1/1482 loaded" prompt))
		 (let ((candidates (all-completions "Retained" collection)))
		   (should (= (length candidates) 1))
		   (should (string-match-p "♥\\|Liked" (car candidates)))
		   (should-not (string-match-p "Liked #[0-9]" (car candidates)))
		   (car candidates)))))
      (hub/music-search))
    (should (equal added (list (list raw t))))
    (should (zerop stopped))
    (should-not started)
    (should (eq (current-buffer) document)))))

(ert-deftest hub/music-search-enqueue-is-explicit-and-does-not-interrupt ()
  "M-RET delegates append-only, preserving the playing queue and document."
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (setcdr (assq 'uri raw) "spotify:track:0000000000000000000001")
    (emms-start)
    (let ((before (with-current-buffer playlist (buffer-string))))
      (cl-letf (((symbol-function 'completing-read)
		 (lambda (_prompt collection &rest _)
		   (setq hub/music-search--operation 'enqueue)
		   (car (all-completions "Retained" collection)))))
	(hub/music-search))
      (should (equal added (list (list raw nil))))
      (should (equal before (with-current-buffer playlist (buffer-string))))
      (should (zerop stopped))
      (should (= (length started) 1))))))

(ert-deftest hub/music-search-stale-library-records-cannot-mutate-the-queue ()
  "Refresh, removal, in-place mutation and closed buffers reject old actions."
  (dolist (change '(refresh removal mutate killed))
    (hub/music-search-test--queue
     (hub/music-search-test--library
      (setcdr (assq 'uri raw) "spotify:track:0000000000000000000001")
      (emms-start)
      (cl-letf (((symbol-function 'completing-read)
		 (lambda (_prompt collection &rest _)
		   (let ((choice (car (all-completions "Retained" collection))))
		     (with-current-buffer library
		       (pcase change
			 ('refresh (cl-incf hub/spotify--generation))
			 ('removal (setq hub/spotify--tracks nil))
			 ('mutate (setcdr (assq 'uri raw) "spotify:track:0000000000000000000000"))
			 ('killed (kill-buffer library))))
		     choice))))
	(should-error (hub/music-search) :type 'user-error))
      (should-not added)
      (should (zerop stopped))
      (should (= (length started) 1))))))

(ert-deftest hub/music-search-unavailable-library-actions-are-passive ()
  "An unavailable record remains searchable but neither explicit action mutates."
  (dolist (operation '(play enqueue))
    (hub/music-search-test--queue
     (hub/music-search-test--library
      (setcdr (assq 'uri raw) "spotify:track:0000000000000000000001")
      (push '(is_playable . :false) raw)
      (with-current-buffer library (setq hub/spotify--tracks (list (cons 0 raw))))
      (cl-letf (((symbol-function 'completing-read)
		 (lambda (_prompt collection &rest _)
		   (setq hub/music-search--operation operation)
		   (let ((candidate (car (all-completions "Retained" collection))))
		     (should (string-match-p "unavailable" candidate))
		     candidate))))
	(should-error (hub/music-search) :type 'user-error))
      (should-not added)
      (should-not started)
      (should (zerop stopped))))))

(ert-deftest hub/music-search-combined-results-retain-exact-occurrences-and-recordings ()
  "Two queued copies plus one matching Liked record remain two exact results."
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (let ((other (copy-tree raw))
	  (before (with-current-buffer playlist (buffer-string))))
      (setcdr (assq 'uri other) "spotify:track:0000000000000000000001")
      (setcdr (assq 'name other) "Low Light")
      (with-current-buffer library (setq hub/spotify--tracks (list (cons 0 raw) (cons 1 other))))
      (cl-letf (((symbol-function 'completing-read)
		 (lambda (prompt collection &rest _)
		   (let ((candidates (all-completions "" collection)))
		     (should (string-match-p "2/1482 loaded" prompt))
		     (should (= (length candidates) 4))
		     (should (= (length (all-completions "Low Light" collection)) 3))
		     (should-not (all-completions "Retained" collection))
		     (should (= (cl-count-if (lambda (candidate) (and (string-match-p "Queue #" candidate) (string-match-p "♥\\|Liked" candidate))) candidates) 2))
		     (should (cl-find-if (lambda (candidate)
					   (eq (hub/music-row-origin
						(hub/music-search--target (cdr (get-text-property 0 'multi-category candidate))))
					       'liked)) candidates))
		     (cl-find-if (lambda (candidate) (string-match-p "Queue #3" candidate)) candidates)))))
	(hub/music-search))
      (should (= (length started) 1))
      (should (= (cadar started) (nth 2 positions)))
      (should (eq (caar started) duplicate))
      (should-not added)
      (should (equal before (with-current-buffer playlist (buffer-string))))))))

(ert-deftest hub/music-search-combined-enqueue-delegates-one-copy-without-interruption ()
  "A combined result's append action uses the retained Liked record, not queue Play."
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (emms-start)
    (setq emms-player-paused-p t)
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (_prompt collection &rest _)
		 (setq hub/music-search--operation 'enqueue)
		 (cl-find-if (lambda (candidate) (string-match-p "Queue #3" candidate))
			     (all-completions "" collection)))))
      (hub/music-search))
    (should (equal added (list (list raw nil))))
    (should (= (length started) 1))
    (should (zerop stopped))
    (should emms-player-paused-p)
    (should (= (with-current-buffer playlist (marker-position emms-playlist-selected-marker)) (car positions))))))

(ert-deftest hub/music-search-combined-enqueue-rejects-obsolete-liked-records ()
  "Combined append validates its retained library identity before any mutation."
  (dolist (change '(refresh removal mutate killed unavailable))
    (hub/music-search-test--queue
     (hub/music-search-test--library
      (when (eq change 'unavailable) (push '(is_playable . :false) raw)
	    (with-current-buffer library (setq hub/spotify--tracks (list (cons 0 raw)))))
      (emms-start)
      (cl-letf (((symbol-function 'completing-read)
		 (lambda (_prompt collection &rest _)
		   (let ((candidate (cl-find-if (lambda (value) (string-match-p "Queue #3" value))
						(all-completions "" collection))))
		     (setq hub/music-search--operation 'enqueue)
		     (with-current-buffer library
		       (pcase change
			 ('refresh (cl-incf hub/spotify--generation))
			 ('removal (setq hub/spotify--tracks nil))
			 ('mutate (setcdr (assq 'uri raw) "spotify:track:0000000000000000000000"))
			 ('killed (kill-buffer library))))
		     candidate))))
	(should (string-match-p (if (eq change 'unavailable) "no playable Spotify track" "Loaded Liked Songs changed")
				(error-message-string (should-error (hub/music-search) :type 'user-error)))))
      (should-not added)
      (should (zerop stopped))
      (should (= (length started) 1))))))

(ert-deftest hub/music-search-combined-validates-only-the-source-of-the-chosen-action ()
  "Refreshing Likes does not redirect Queue Play; editing the queue does not redirect append."
  (dolist (operation '(play enqueue))
    (hub/music-search-test--queue
     (hub/music-search-test--library
      (emms-start)
      (cl-letf (((symbol-function 'completing-read)
		 (lambda (_prompt collection &rest _)
		   (let ((candidate (cl-find-if (lambda (value) (string-match-p "Queue #3" value))
						(all-completions "" collection))))
		     (setq hub/music-search--operation operation)
		     (if (eq operation 'play)
			 (with-current-buffer library (cl-incf hub/spotify--generation))
		       (with-current-buffer playlist
			 (let ((inhibit-read-only t))
			   (goto-char (point-max))
			   (emms-playlist-insert-track (emms-track 'file "/tmp/another.ogg")))))
		     candidate))))
	(hub/music-search))
      (if (eq operation 'play)
	  (progn (should (= (length started) 2))
		 (should (= (cadar started) (nth 2 positions)))
		 (should-not added))
	(should (equal added (list (list raw nil))))
	(should (zerop stopped))
	(should (= (length started) 1)))))))

(ert-deftest hub/music-search-embark-dispatch-preserves-retained-target-identities ()
  "Real Embark dispatch keeps properties for exact Play and retained Append."
  (require 'apps/music-search)
  (require 'embark)
  (dolist (operation '(queue-play library-play combined-append))
    (hub/music-search-test--queue
     (hub/music-search-test--library
      (let* ((queue (hub/music-search--queue playlist))
	     (liked (hub/music-search--liked library))
	     (row (pcase operation
		    ('library-play (car liked))
		    ('combined-append (nth 2 (car (hub/music-search--combine queue liked))))
		    (_ (nth 2 queue))))
	     (target (propertize (hub/music-row-label row) 'hub/music-row row))
	     (overriding-terminal-local-map (embark--action-keymap 'hub/music-track nil))
	     (action (key-binding (kbd (if (eq operation 'combined-append) "a" "p")) t)))
	(should (eq action (if (eq operation 'combined-append)
			       'hub/music-search-embark-append 'hub/music-search-embark-play)))
	(embark--act action (list :type 'hub/music-track :target target))
	(pcase operation
	  ('queue-play (should (= (length started) 1))
		       (should (= (cadar started) (nth 2 positions))) (should-not added))
	  ('library-play (should (equal added (list (list raw t)))) (should-not started))
	  ('combined-append (should (equal added (list (list raw nil)))) (should-not started))))))))

(ert-deftest hub/music-search-embark-rejects-stripped-and-stale-identities ()
  "Embark never reconstructs a lost identity or bypasses source validation."
  (require 'apps/music-search)
  (require 'embark)
  (dolist (change '(stripped queue library))
    (hub/music-search-test--queue
     (hub/music-search-test--library
      (let* ((rows (car (hub/music-search--combine (hub/music-search--queue playlist)
						   (hub/music-search--liked library))))
	     (target (propertize (hub/music-row-label (nth 2 rows)) 'hub/music-row (nth 2 rows)))
	     (overriding-terminal-local-map (embark--action-keymap 'hub/music-track nil))
	     (action (key-binding (kbd (if (eq change 'library) "a" "p")) t)))
	(emms-start)
	(pcase change
	  ('stripped (setq target (substring-no-properties target)))
	  ('queue (with-current-buffer playlist
		    (let ((inhibit-read-only t)) (goto-char (point-max))
			 (emms-playlist-insert-track (emms-track 'file "/tmp/changed.ogg")))))
	  ('library (with-current-buffer library (cl-incf hub/spotify--generation))))
	(should (eq action (if (eq change 'library)
			       'hub/music-search-embark-append 'hub/music-search-embark-play)))
	(should-error (embark--act action (list :type 'hub/music-track :target target)) :type 'user-error)
	(should-not added)
	(should (= (length started) 1))
	(should (zerop stopped)))))))

(provide 'music-search-test)
;;; music-search-test.el ends here
