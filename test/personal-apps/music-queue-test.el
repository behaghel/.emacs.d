;;; music-queue-test.el --- Native queue listing parity -*- lexical-binding: t; -*-

;;; Commentary:
;; Real EMMS editing with presentation-only overlays; no playback backends.

;;; Code:
(require 'ert)
(defvar hub/spotify-buffer-name)
(require 'music-test-helpers
	 (expand-file-name "music-test-helpers" (file-name-directory (or load-file-name buffer-file-name))))
(require 'apps/music-row)

(ert-deftest hub/music-queue-styles-native-occurrences-without-changing-the-model ()
  "Painting preserves text, character revisions, point, markers and undo history."
  (require 'apps/music-queue)
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (with-current-buffer playlist
      (goto-char (nth 2 positions))
      (let ((text (buffer-string)) (revision (buffer-chars-modified-tick))
	    (undo buffer-undo-list) (where (point))
	    (selected (marker-position emms-playlist-selected-marker))
	    (target (hub/music-row-from-queue duplicate playlist (nth 2 positions) 2)))
	(hub/music-queue-listing-mode 1)
	(should (eq major-mode 'emms-playlist-mode))
	(should (equal-including-properties text (buffer-string)))
	(should (= revision (buffer-chars-modified-tick)))
	(should (eq undo buffer-undo-list))
	(should (= where (point)))
	(hub/music-row-assert-current target)
	(should (= selected (marker-position emms-playlist-selected-marker)))
	(should (= (length hub/music-queue--overlays) 3))
	(let ((first (car hub/music-queue--overlays)) (third (nth 2 hub/music-queue--overlays)))
	  (should (eq (emms-playlist-track-at (overlay-start first)) duplicate))
	  (should (eq (emms-playlist-track-at (overlay-start third)) duplicate))
	  (should (string-match-p "Queue #1" (overlay-get first 'display)))
	  (should (string-match-p "Queue #3" (overlay-get third 'display)))
	  (should (string-match-p "♥\\|Liked" (overlay-get first 'display)))
	  (should (get-text-property 4 'tabulated-list-entry (overlay-get first 'display))))
	(hub/music-queue-listing-mode -1)
	(should-not hub/music-queue--overlays)
	(should (equal-including-properties text (buffer-string)))
	(should (= revision (buffer-chars-modified-tick)))))
    (should-not added)
    (should-not started)
    (should (zerop stopped)))))

(ert-deftest hub/music-queue-native-delete-reorder-and-play-retain-exact-occurrences ()
  "EMMS commands, not the display projection, own queue edits and playback."
  (require 'apps/music-queue)
  (hub/music-search-test--queue
   (with-current-buffer playlist
     (hub/music-queue-listing-mode 1)
     (goto-char (nth 1 positions))
     (emms-playlist-mode-kill-track)
     (hub/music-queue--refresh t)
     (should (= (length hub/music-queue--overlays) 2))
     (should (eq (emms-playlist-track-at (overlay-start (car hub/music-queue--overlays))) duplicate))
     (should (eq (emms-playlist-track-at (overlay-start (cadr hub/music-queue--overlays))) duplicate))
     (goto-char (point-max))
     (emms-playlist-insert-track (emms-track 'file "/tmp/new-local.ogg"))
     (goto-char (line-beginning-position 0))
     (emms-playlist-mode-shift-track-up)
     (hub/music-queue--refresh t)
     (should (eq (emms-track-type (emms-playlist-track-at (overlay-start (nth 1 hub/music-queue--overlays)))) 'file))
     (goto-char (overlay-start (nth 2 hub/music-queue--overlays)))
     (emms-playlist-select (point))
     (emms-start)
     (hub/music-queue--refresh t)
     (should (= (cadar started) (overlay-start (nth 2 hub/music-queue--overlays))))
     (should (eq (caar started) duplicate))
     (emms-playlist-clear)
     (hub/music-queue--refresh t)
     (should-not hub/music-queue--overlays))))

(ert-deftest hub/music-queue-resize-and-pause-are-presentation-only ()
  "Layout and transport changes preserve the queue and its browsing position."
  (require 'apps/music-queue)
  (hub/music-search-test--queue
   (with-current-buffer playlist
     (hub/music-queue-listing-mode 1)
     (emms-start)
     (goto-char (nth 2 positions))
     (let ((revision (buffer-chars-modified-tick)) (where (point)))
       (setq emms-player-paused-p t)
       (dolist (width '(80 150))
	 (cl-letf (((symbol-function 'hub/music-queue--width) (lambda () width)))
	   (hub/music-queue--refresh t))
	 (should (= where (point)))
	 (should (= revision (buffer-chars-modified-tick)))
	 (let ((display (overlay-get (car hub/music-queue--overlays) 'display)))
	   (should (text-property-any 0 (length display) 'hub/music-state 'paused display))))))))

(ert-deftest hub/music-queue-native-undo-and-yank-do-not-copy-display-rows ()
  "Undo and yank restore native track properties, never a formatted surrogate."
  (require 'apps/music-queue)
  (hub/music-search-test--queue
   (with-current-buffer playlist
     (let ((interprogram-cut-function nil) (interprogram-paste-function nil)
	   (kill-ring nil) (kill-ring-yank-pointer nil))
       (hub/music-queue-listing-mode 1)
       (buffer-enable-undo)
       (undo-boundary)
       (goto-char (nth 2 positions))
       (emms-playlist-mode-kill-track)
       (undo-boundary)
       (hub/music-queue--refresh t)
       (should (= (length hub/music-queue--overlays) 2))
       (should (eq (get-text-property 0 'emms-track (car kill-ring)) duplicate))
       (should-not (get-text-property 0 'tabulated-list-entry (car kill-ring)))
       (emms-playlist-mode-undo)
       (hub/music-queue--refresh t)
       (should (= (length hub/music-queue--overlays) 3))
       (goto-char (point-max))
       (emms-playlist-mode-yank)
       (hub/music-queue--refresh t)
       (should (= (length hub/music-queue--overlays) 4))
       (should (eq (emms-playlist-track-at (overlay-start (nth 3 hub/music-queue--overlays))) duplicate))))))

(ert-deftest hub/music-queue-display-flattens-nested-native-ellipsis-and-spacing ()
  "Queue replacement strings cannot nest native display replacements."
  (require 'apps/music-queue)
  (hub/music-search-test--queue
   (emms-track-set duplicate 'info-title (concat (make-string 100 ?x) "hidden tail"))
   (with-current-buffer playlist
     (hub/music-queue-listing-mode 1)
     (let ((display (overlay-get (car hub/music-queue--overlays) 'display)))
       (should-not (text-property-not-all 0 (length display) 'display nil display))
       (should-not (string-match-p "hidden tail" display))
       (should (string-match-p "Quiet Geometry" display))))))

(provide 'music-queue-test)
;;; music-queue-test.el ends here
