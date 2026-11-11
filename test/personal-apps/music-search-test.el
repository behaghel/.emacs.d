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
(dolist (package '("consult" "compat"))
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
		   (should (string-match-p "Liked #1" (car candidates)))
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

(provide 'music-search-test)
;;; music-search-test.el ends here
