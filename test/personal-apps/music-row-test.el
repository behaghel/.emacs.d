;;; music-row-test.el --- Shared inert music rows -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic EMMS occurrences and retained Spotify metadata; no service calls.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'music-test-helpers
	 (expand-file-name "music-test-helpers" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest hub/music-row-normalizes-library-and-queue-without-constructing-tracks ()
  "Both origins share metadata without touching EMMS or fetching information."
  (require 'apps/music-row)
  (hub/music-search-test--queue
   (let* ((raw '((uri . "spotify:track:4uLU6hMCjMI75M1A2tKUQC")
		 (name . "Low Light") (artists . (((name . "Quiet Geometry"))))
		 (album . ((name . "Still / Motion"))) (duration_ms . 274000)))
	  (before (with-current-buffer playlist (buffer-string))))
     (cl-letf (((symbol-function 'emms-track) (lambda (&rest _) (ert-fail "Constructed a track"))))
       (let ((queue (hub/music-row-from-queue duplicate playlist (car positions) 0))
	     (liked (hub/music-row-from-liked raw 12)))
	 (should (equal (seq-subseq (hub/music-row-cells queue) 1 5)
			(seq-subseq (hub/music-row-cells liked) 1 5)))
	 (should (equal (hub/music-row-title liked) "Low Light"))
	 (should (equal (hub/music-row-time liked) "4:34"))
	 (should (string-match-p "Liked #13" (hub/music-row-label liked)))
	 (should (eq (hub/music-row-track liked) raw))
	 (should (equal before (with-current-buffer playlist (buffer-string)))))))))

(ert-deftest hub/music-row-status-follows-occurrence-not-cursor-or-equal-track-object ()
  "A duplicate has one active queue marker; a matching library row shares state."
  (require 'apps/music-row)
  (hub/music-search-test--queue
   (let ((first (hub/music-row-from-queue duplicate playlist (car positions) 0))
	 (third (hub/music-row-from-queue duplicate playlist (nth 2 positions) 2))
	 (liked (hub/music-row-from-liked '((uri . "spotify:track:4uLU6hMCjMI75M1A2tKUQC")) 0)))
     (with-current-buffer playlist (emms-playlist-select (nth 2 positions)))
     (emms-start)
     (with-current-buffer playlist (goto-char (car positions)))
     (should-not (hub/music-row-status first))
     (should (eq (hub/music-row-status third) 'playing))
     (should (eq (hub/music-row-status liked) 'playing))
     (setq emms-player-paused-p t)
     (cl-letf (((symbol-function 'hub/music-playback-state) (lambda () 'seeking)))
       (should (eq (hub/music-row-status third) 'paused)))
     (cl-letf (((symbol-function 'hub/music-playback-state) (lambda () 'connecting)))
       (should (eq (hub/music-row-status third) 'connecting)))
     (with-current-buffer playlist
       (let ((inhibit-read-only t)) (goto-char (point-max)) (emms-playlist-insert-track duplicate)))
     (should-not (hub/music-row-status third))
     (should (eq (hub/music-row-status liked) 'paused))
     (emms-stop)
     (should-not (hub/music-row-status third))
     (should-not (hub/music-row-status liked)))))

(ert-deftest hub/music-row-library-redraws-status-without-moving-or-fetching ()
  "The retained native table reflects Pause/Stop without reopening or reordering."
  (require 'apps/spotify)
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (with-current-buffer library
      (hub/spotify-library-mode)
      (setq hub/spotify--tracks
	    (list (cons 0 raw) (cons 1 '((name . "Another song")
					 (uri . "spotify:track:0000000000000000000000"))))
	    hub/spotify--total 1482)
      ;; Migrate an already open four-column view without resetting its dataset.
      (setq-local tabulated-list-format [("Title" 32 t) ("Artist" 24 t) ("Album" 24 t) ("Time" 7 nil)])
      (hub/spotify--render)
      (should (= (length tabulated-list-format) 6))
      (should (eq (cdar hub/spotify--tracks) raw))
      (goto-char (text-property-any (point-min) (point-max) 'tabulated-list-id 1)))
    (let ((window (selected-window)))
      (emms-start)
      (with-current-buffer library
	(should (text-property-any (point-min) (point-max) 'hub/music-state 'playing))
	(should (equal (tabulated-list-get-id) 1)))
      (setq emms-player-paused-p t)
      (run-hooks 'emms-player-paused-hook)
      (with-current-buffer library
	(should (text-property-any (point-min) (point-max) 'hub/music-state 'paused))
	(should (equal (tabulated-list-get-id) 1)))
      (emms-stop)
      (with-current-buffer library
	(should-not (text-property-not-all (point-min) (point-max) 'hub/music-state nil))
	(should (string-match-p "Retained song" (buffer-string))))
      (should (eq (selected-window) window))
      (should-not (get-buffer-window library))))))

(provide 'music-row-test)
;;; music-row-test.el ends here
