;;; music-queue-actions-test.el --- Exact queue insertion actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Actual EMMS order and selection, synthetic players, and no network or capture.

;;; Code:
(require 'ert)
(defvar hub/spotify-buffer-name)
(require 'music-test-helpers
	 (expand-file-name "music-test-helpers" (file-name-directory (or load-file-name buffer-file-name))))
(require 'apps/music-row)

(defun hub/music-queue-actions-test--tracks (playlist)
  "Return PLAYLIST's native track order without moving its point."
  (mapcar #'hub/music-row-track (hub/music-row-queue playlist)))

(ert-deftest hub/music-queue-insert-end-and-next-preserve-playing-occurrence ()
  "Append and next insertion create copies without interrupting the current song."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (let ((original (hub/music-queue-actions-test--tracks playlist)))
     (emms-start)
     (setq emms-player-paused-p t)
     (hub/music-queue-insert duplicate 'next)
     (should (equal (hub/music-queue-actions-test--tracks playlist)
		    (list duplicate duplicate (nth 1 original) duplicate)))
     (hub/music-queue-insert (nth 1 original) 'end)
     (should (equal (hub/music-queue-actions-test--tracks playlist)
		    (list duplicate duplicate (nth 1 original) duplicate (nth 1 original))))
     (should emms-player-paused-p)
     (should (= (length started) 1))
     (should (zerop stopped))
     (with-current-buffer playlist
       (should (= (marker-position emms-playlist-selected-marker) (car positions)))))))

(ert-deftest hub/music-queue-insert-empty-and-last-selected-do-not-autoplay ()
  "Empty queues select their first addition; next at the end appends without play."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (with-current-buffer playlist
     (emms-playlist-select (nth 2 positions)))
   (hub/music-queue-insert duplicate 'next)
   (should (= (length (hub/music-row-queue playlist)) 4))
   (with-current-buffer playlist
     (should (= (marker-position emms-playlist-selected-marker) (nth 2 positions)))
     (emms-playlist-clear))
   (hub/music-queue-insert duplicate 'next)
   (with-current-buffer playlist
     (should (eq (emms-playlist-selected-track) duplicate)))
   (should-not started)
   (should (zerop stopped))))

(ert-deftest hub/music-queue-next-search-copies-a-queued-file-and-rejects-stale-targets ()
  "Queued-only next insertion uses native occurrence identity, never a Liked lookup."
  (require 'apps/music-queue-actions)
  (require 'apps/music-search)
  (hub/music-search-test--queue
   (let* ((rows (hub/music-row-queue playlist)) (file-row (nth 1 rows)))
     (emms-start)
     (hub/music-search--act file-row 'next)
     (should (eq (nth 1 (hub/music-queue-actions-test--tracks playlist)) (hub/music-row-track file-row)))
     (should (eq (nth 2 (hub/music-queue-actions-test--tracks playlist)) (hub/music-row-track file-row)))
     (should-error (hub/music-search--act file-row 'next) :type 'user-error)
     (should (= (length started) 1))
     (should (zerop stopped)))))

(ert-deftest hub/music-queue-file-prompt-appends-and-cancellation-is-passive ()
  "Direct local file insertion appends; cancelling the native prompt changes nothing."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (let ((file (make-temp-file "music append " nil ".wav")))
     (unwind-protect
	 (progn
	   (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file)))
	     (call-interactively #'hub/music-add-file))
	   (should (equal (emms-track-name (car (last (hub/music-queue-actions-test--tracks playlist)))) file))
	   (let ((before (with-current-buffer playlist (buffer-string))))
	     (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) (signal 'quit nil))))
	       (condition-case nil (call-interactively #'hub/music-add-file) (quit nil)))
	     (should (equal before (with-current-buffer playlist (buffer-string))))))
       (delete-file file)))
   (should-not started)
   (should (zerop stopped))))

(ert-deftest hub/music-queue-file-prompt-filters-using-player-extensions ()
  "The native prompt keeps directories and supported files without media probes."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (let* ((directory (make-temp-file "music-picker-" t))
	  (wav (expand-file-name "song.WAV" directory))
	  (flac (expand-file-name "song.flac" directory))
	  (text (expand-file-name "notes.txt" directory))
	  (backup (expand-file-name "song.wav.bak" directory))
	  (subdirectory (expand-file-name "albums.txt" directory)))
     (unwind-protect
	 (progn
	   (make-directory subdirectory)
	   (dolist (file (list wav flac text backup)) (write-region "" nil file nil 'silent))
	   (cl-letf (((symbol-function 'read-file-name)
		      (lambda (&rest arguments)
			(let ((predicate (nth 5 arguments)))
			  (should (functionp predicate))
			  (cl-letf (((symbol-function 'emms-track)
				     (lambda (&rest _) (ert-fail "Completion created a track")))
				    ((symbol-function 'call-process)
				     (lambda (&rest _) (ert-fail "Completion probed media"))))
			    (should (funcall predicate subdirectory))
			    (should (funcall predicate wav))
			    (should (funcall predicate flac))
			    (should-not (funcall predicate text))
			    (should-not (funcall predicate backup))
			    (should-not (funcall predicate "/ssh:example:/song.wav"))
			    (let* ((player (emms-player #'ignore #'ignore #'ignore))
				   (emms-player-list (list player)))
			      (emms-player-set player 'regex "\\.txt\\'")
			      (should (funcall predicate text))
			      (should-not (funcall predicate wav))))
			  wav))))
	     (call-interactively #'hub/music-add-file))
	   (should (equal (emms-track-name (car (last (hub/music-queue-actions-test--tracks playlist)))) wav))
	   (should-not started)
	   (should (zerop stopped)))
       (delete-directory directory t)))))

(ert-deftest hub/music-queue-point-actions-and-library-next-use-correct-sources ()
  "Queue point copies that occurrence; library next retains its generation guard."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (with-current-buffer playlist
     (goto-char (nth 2 positions))
     (hub/music-add-next))
   (should (= (length (hub/music-row-queue playlist)) 4))
   (hub/music-search-test--library
    (with-current-buffer library
      (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda (&rest _) 0))
		((symbol-function 'hub/spotify-player-add)
		 (lambda (metadata &optional play placement)
		   (should (equal metadata raw))
		   (should-not play)
		   (should (eq placement 'next)))))
	(hub/music-add-next))))
   (should-not started)
   (should (zerop stopped))))

(ert-deftest hub/music-queue-remove-other-exact-occurrences-preserves-playback ()
  "Deleting a duplicate or an earlier row leaves the selected occurrence playing."
  (require 'apps/music-queue-actions)
  (dolist (index '(0 2))
    (hub/music-search-test--queue
     (let* ((rows (hub/music-row-queue playlist))
	    (selected (nth (if (= index 0) 1 0) rows)))
       (with-current-buffer playlist (emms-playlist-select (hub/music-row-position selected)))
       (emms-start)
       (hub/music-row-remove (nth index rows))
       (should (= (length (hub/music-row-queue playlist)) 2))
       (with-current-buffer playlist
	 (should (eq (emms-playlist-selected-track) (hub/music-row-track selected))))
       (should (= (length started) 1))
       (should (zerop stopped))
       (should emms-player-playing-p)))))

(ert-deftest hub/music-queue-remove-playing-occurrence-advances-once ()
  "Removing the playing occurrence stops once and starts its native successor."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (let* ((rows (hub/music-row-queue playlist)) (successor (hub/music-row-track (nth 1 rows))))
     (emms-start)
     (hub/music-row-remove (car rows))
     (should (equal (hub/music-queue-actions-test--tracks playlist) (list successor duplicate)))
     (should (= stopped 1))
     (should (= (length started) 2))
     (should (eq (caar started) successor))
     (should-not emms-player-paused-p))))

(ert-deftest hub/music-queue-remove-paused-or-stopped-selection-does-not-start ()
  "Paused removal stops its backend, but selecting the successor never autoplays."
  (require 'apps/music-queue-actions)
  (dolist (paused '(nil t))
    (hub/music-search-test--queue
     (let* ((rows (hub/music-row-queue playlist)) (successor (hub/music-row-track (nth 1 rows))))
       (when paused (emms-start) (setq emms-player-paused-p t))
       (hub/music-row-remove (car rows))
       (with-current-buffer playlist (should (eq (emms-playlist-selected-track) successor)))
       (should (= stopped (if paused 1 0)))
       (should (= (length started) (if paused 1 0)))
       (should-not emms-player-playing-p)
       (should-not emms-player-paused-p)))))

(ert-deftest hub/music-queue-remove-last-or-only-occurrence-stops-without-wrapping ()
  "Removing the last occurrence never wraps or repeats, even with repeat enabled."
  (require 'apps/music-queue-actions)
  (dolist (only '(nil t))
    (hub/music-search-test--queue
     (with-current-buffer playlist
       (when only (emms-playlist-clear) (emms-playlist-insert-track duplicate))
       (emms-playlist-select (hub/music-row-position (car (last (hub/music-row-queue playlist))))))
     (emms-start)
     (let ((emms-repeat-playlist t) (emms-repeat-track t))
       (hub/music-row-remove (car (last (hub/music-row-queue playlist)))))
     (should (= stopped 1))
     (should (= (length started) 1))
     (should-not emms-player-playing-p)
     (when only
       (should-not (hub/music-row-queue playlist))
       (with-current-buffer playlist (should-not (emms-playlist-selected-track)))))))

(ert-deftest hub/music-queue-remove-rejects-stale-and-library-only-targets ()
  "Removal requires an exact fresh queue occurrence, never a URI or library guess."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (let ((stale (car (hub/music-row-queue playlist))))
     (hub/music-queue-insert duplicate 'end)
     (should-error (hub/music-row-remove stale) :type 'user-error)
     (hub/music-search-test--library
      (should-error (hub/music-row-remove (hub/music-row-from-liked raw 0 library)) :type 'user-error))
     (should (= (length (hub/music-row-queue playlist)) 4))
     (should-not started)
     (should (zerop stopped)))))

(ert-deftest hub/music-queue-remove-blocks-on-stop-failure-or-source-race ()
  "Unconfirmed stops or changed queue/selection cannot cause deletion or restart."
  (require 'apps/music-queue-actions)
  (dolist (failure '(stop queue selection))
    (hub/music-search-test--queue
     (let* ((rows (hub/music-row-queue playlist)) (target (car rows)))
       (emms-start)
       (cl-letf (((symbol-function 'emms-stop)
		  (lambda ()
		    (if (eq failure 'stop) (user-error "Stop unconfirmed")
		      (setq emms-player-playing-p nil)
		      (if (eq failure 'queue)
			  (hub/music-queue-insert duplicate 'end)
			(with-current-buffer playlist
			  (emms-playlist-select (hub/music-row-position (nth 1 rows)))))))))
	 (should-error (hub/music-row-remove target) :type 'user-error))
       (should (= (length (hub/music-row-queue playlist)) (if (eq failure 'queue) 4 3)))
       (should (= (length started) 1))))))

(ert-deftest hub/music-remove-player-and-queue-focus-are-independent ()
  "Player removes selected playback, while Queue removes its browsing occurrence."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (emms-start)
   (setq emms-player-paused-p t)
   (with-current-buffer playlist
     (goto-char (nth 2 positions))
     (call-interactively #'hub/music-remove))
   (should (= (length started) 1))
   (should (zerop stopped))
   (with-temp-buffer
     (setq major-mode 'hub/emms-player-mode)
     (call-interactively #'hub/music-remove))
   (should (= stopped 1))
   (should (= (length started) 1))
   (should (= (length (hub/music-row-queue playlist)) 1))
   (should-not emms-player-playing-p)))

(ert-deftest hub/music-remove-embark-retains-identity-and-query-hyphens ()
  "Embark removal uses retained properties; plain hyphens stay query input."
  (require 'apps/music-search)
  (should-not (interactive-form #'hub/music-search-embark-remove))
  (should (eq (lookup-key hub/music-search-embark-map (kbd "-")) #'hub/music-search-embark-remove))
  (with-temp-buffer
    (use-local-map (copy-keymap minibuffer-local-map))
    (hub/music-search--minibuffer-setup)
    (should (eq (key-binding (kbd "-")) #'self-insert-command)))
  (hub/music-search-test--queue
   (let ((target (propertize "Same title, exact third occurrence" 'hub/music-row
			     (nth 2 (hub/music-row-queue playlist)))))
     (hub/music-search-embark-remove target)
     (should (= (length (hub/music-row-queue playlist)) 2))
     (should-error (hub/music-search-embark-remove target) :type 'user-error)
     (should-error (hub/music-search-embark-remove (substring-no-properties target)) :type 'user-error)
     (should (= (length (hub/music-row-queue playlist)) 2)))))

(ert-deftest hub/music-queue-remove-remains-native-undoable ()
  "Removing an occurrence preserves native text-property undo without playback."
  (require 'apps/music-queue-actions)
  (hub/music-search-test--queue
   (with-current-buffer playlist
     (buffer-enable-undo)
     (setq buffer-undo-list nil)
     (let ((before (buffer-string)) (last-command nil))
       (hub/music-row-remove (nth 2 (hub/music-row-queue playlist)))
       (undo-boundary)
       (emms-playlist-mode-undo)
       (should (equal-including-properties before (buffer-string)))
       (should-not started)
       (should (zerop stopped))))))

(provide 'music-queue-actions-test)
;;; music-queue-actions-test.el ends here
