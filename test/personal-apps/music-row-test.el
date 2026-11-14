;;; music-row-test.el --- Shared inert music rows -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic EMMS occurrences and retained Spotify metadata; no service calls.

;;; Code:
(require 'ert)
(require 'cl-lib)
(defvar hub/spotify-buffer-name)
(require 'music-test-helpers
	 (expand-file-name "music-test-helpers" (file-name-directory (or load-file-name buffer-file-name))))
(require 'apps/music-row)

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
	 (should (string-match-p "♥\\|Liked" (hub/music-row-label liked)))
	 (should-not (string-match-p "Liked #[0-9]" (hub/music-row-label liked)))
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
      (let ((records (buffer-local-value 'hub/spotify--tracks library))
	    (starts started) (stops stopped))
	(cl-letf (((symbol-function 'auth-source-pass-get) (lambda (&rest _) (ert-fail "Read credentials")))
		  ((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Fetched metadata")))
		  ((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Started a process"))))
	  (hub/music-listings-refresh))
	(should (eq records (buffer-local-value 'hub/spotify--tracks library)))
	(with-current-buffer library (should (equal (tabulated-list-get-id) 1)))
	(should (equal starts started))
	(should (= stops stopped)))
      (should (eq (selected-window) window))
      (should-not (get-buffer-window library))))))

(ert-deftest hub/music-row-shares-text-hierarchy-between-table-and-search ()
  "Both projections use the same semantic faces without mutating source text."
  (require 'apps/music-row)
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (let* ((row (hub/music-row-from-liked raw 0 library))
	   (label (hub/music-row-label row))
	   (cells (hub/music-row-cells row))
	   (faces '(hub/music-row-title hub/music-row-artist hub/music-row-album hub/music-row-time)))
      (dotimes (index 4)
	(let* ((cell (aref cells (1+ index)))
	       (face (nth index faces))
	       (position (string-match-p (regexp-quote (substring-no-properties cell)) label)))
	  (should position)
	  (should (eq (get-text-property 0 'face cell) face))
	  (should (eq (get-text-property position 'face label) face))))
      (should (eq (plist-get (cdr (aref cells 5)) 'face) 'hub/music-row-source))
      (should (eq (get-text-property (string-match-p "Spotify" label) 'face label) 'hub/music-row-source))
      (should (eq (get-text-property (string-match-p " — " label) 'face label) 'hub/music-row-detail))
      (dolist (face '(hub/music-row-title hub/music-row-artist hub/music-row-album hub/music-row-time hub/music-row-detail hub/music-row-source))
	(should (facep face))
	(should (equal (face-attribute face :height) (face-attribute 'hub/music-row-title :height)))
	(should (equal (face-attribute face :family nil t) (face-attribute 'hub/music-row-title :family nil t))))
      (should-not (text-properties-at 0 (hub/music-row-title row)))
      (should-not (text-properties-at 0 (alist-get 'name raw)))
      (should-not (hub/music-row-status row))
      (should-not added)
      (should-not started)
      (should (zerop stopped))))))

(ert-deftest hub/music-row-consult-display-preserves-hierarchy-and-cancel-is-passive ()
  "Real Consult completion carries semantic fields without changing identity or playback."
  (require 'apps/music-search)
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (setq emms-playlist-buffer nil)
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (_prompt table &rest _)
		 (let ((candidate (car (all-completions "Retained" table))))
		   (should candidate)
		   (dolist (field '(("Retained song" . hub/music-row-title)
				    ("Library artist" . hub/music-row-artist)
				    ("Retained album" . hub/music-row-album)
				    ("3:01" . hub/music-row-time)
				    ("Spotify" . hub/music-row-source)))
		     (should (eq (get-text-property (string-match-p (regexp-quote (car field)) candidate)
						    'face candidate)
				 (cdr field))))
		   (should (eq (hub/music-row-track
				(hub/music-search--target (cdr (get-text-property 0 'multi-category candidate)))) raw)))
		 (signal 'quit nil))))
      (should (eq (condition-case nil (hub/music-search) (quit 'cancelled)) 'cancelled)))
    (should-not added)
    (should-not started)
    (should (zerop stopped)))))

(defun hub/music-row-test--visible (text)
  "Resolve native ellipsis and character-column spaces in TEXT for assertions."
  (let ((position 0) (column 0) pieces)
    (while (< position (length text))
      (let* ((end (next-single-property-change position 'display text (length text)))
	     (display (get-text-property position 'display text))
	     (piece (cond ((stringp display) (apply #'propertize display (text-properties-at position text)))
			  ((and (consp display) (eq (car display) 'space))
			   (apply #'propertize (make-string (max 0 (- (plist-get (cdr display) :align-to) column)) ?\s)
				  (text-properties-at position text)))
			  (t (substring text position end)))))
	(remove-list-of-text-properties 0 (length piece) '(display) piece)
	(cl-incf column (string-width piece))
	(push piece pieces)
	(setq position end)))
    (apply #'concat (nreverse pieces))))

(defun hub/music-row-test--column-start (text column)
  "Find COLUMN's first native table property in TEXT."
  (cl-loop for position from 0 below (length text)
	   when (equal (get-text-property position 'tabulated-list-column-name text) column)
	   return position))

(ert-deftest hub/music-row-columns-match-native-table-and-keep-long-fields-contained ()
  "Search uses the table's real columns, including truncation and time alignment."
  (require 'apps/music-row)
  (hub/music-search-test--queue
   (dolist (width '(53 80 150))
     (let* ((row (hub/music-row-from-queue duplicate playlist (car positions) 0))
	    (short (hub/music-row-test--visible (hub/music-row-display row width)))
	    (layout (hub/music-row-layout width)) long table)
       (setf (hub/music-row-title row) (concat (make-string 100 ?x) " 日本語 invisible needle")
	     (hub/music-row-album row) (make-string 100 ?a))
       (setq long (hub/music-row-test--visible (hub/music-row-display row width)))
       (with-temp-buffer
	 (let ((tabulated-list-format layout) (tabulated-list-padding 0))
	   (tabulated-list-print-entry 0 (hub/music-row-cells row))
	   (setq table (hub/music-row-test--visible (buffer-substring (point-min) (1- (point-max)))))))
       (should-not (string-match-p "invisible needle" long))
       (should-not (string-match-p "\n" long))
       (should (<= (string-width long) width))
       (dolist (field '(artist album time source))
	 (let ((at (text-property-any 0 (length long) 'hub/music-row-field field long))
	       (short-at (text-property-any 0 (length short) 'hub/music-row-field field short))
	       (table-at (text-property-any 0 (length table) 'hub/music-row-field field table)))
	   (should at) (should short-at) (should table-at)
	   (should (= (string-width (substring long 0 at)) (string-width (substring short 0 short-at))))
	   (should (= (string-width (substring long 0 at)) (string-width (substring table 0 table-at))))))
       (should (plist-get (nthcdr 3 (aref layout 4)) :right-align))))))

(ert-deftest hub/music-row-truncated-vertico-display-keeps-full-search-and-action-target ()
  "A match beyond visible title truncation still appends its full retained record."
  (add-to-list 'load-path
	       (expand-file-name "vertico" (or (getenv "STRAIGHT_BUILD_PATH")
					       (expand-file-name "straight/build" default-directory))))
  (require 'vertico)
  (require 'apps/music-search)
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (setq emms-playlist-buffer nil)
    (setcdr (assq 'name raw) (concat (make-string 100 ?x) " invisible needle"))
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (_prompt collection &rest _)
		 (let* ((candidate (let ((completion-styles '(substring)))
				     (car (completion-all-completions "invisible needle" collection nil 16))))
			(hub/music-search--active t) (vertico--index 0)
			(display (hub/music-row-test--visible (vertico--format-candidate candidate "" "" 0 0))))
		   (should candidate)
		   (should-not (string-match-p "invisible needle" display))
		   (should (hub/music-row-test--column-start display "Artist"))
		   (should (eq (hub/music-row-track
				(hub/music-search--target (cdr (get-text-property 0 'multi-category candidate)))) raw))
		   (setq hub/music-search--operation 'enqueue)
		   candidate))))
      (hub/music-search))
    (should (equal added (list (list raw nil))))
    (should-not started)
    (should (zerop stopped)))))

(ert-deftest hub/music-row-style-refresh-updates-already-defined-local-faces ()
  "Reloading the defaults does not leave the old shared grey album/time face in use."
  (require 'apps/music-row)
  (let ((old (get 'hub/music-row-artist 'face-defface-spec)))
    (unwind-protect
	(progn
	  (face-spec-set 'hub/music-row-artist '((t (:inherit default))) 'face-defface-spec)
	  (hub/music-row--apply-faces)
	  (should-not (equal (get 'hub/music-row-artist 'face-defface-spec) '((t (:inherit default)))))
	  (should (facep 'hub/music-row-album))
	  (should (facep 'hub/music-row-time)))
      (face-spec-set 'hub/music-row-artist old 'face-defface-spec))))

(ert-deftest hub/music-row-palette-uses-active-modus-colours-and-theme-overrides ()
  "Music roles use palette names, respect overrides and follow theme changes."
  (let ((custom-enabled-themes '(synthetic-modus))
	(colours '((blue . "#112233") (cyan . "#223344") (magenta . "#334455")
		   (yellow . "#445566") (fg-dim . "#556677")))
	(roles '((hub/music-row-title . blue) (hub/music-row-artist . cyan)
		 (hub/music-row-album . magenta) (hub/music-row-time . yellow)
		 (hub/music-row-source . fg-dim) (hub/music-row-detail . fg-dim)))
	(before (face-attribute 'default :foreground)))
    (unwind-protect
	(cl-letf (((symbol-function 'modus-themes-get-current-theme) (lambda () 'synthetic-modus))
		  ((symbol-function 'modus-themes-get-color-value)
		   (lambda (colour &optional overrides theme)
		     (should overrides)
		     (should-not theme)
		     (alist-get colour colours))))
	  (hub/music-row--apply-faces)
	  (dolist (role roles)
	    (should (equal (plist-get (cadar (get (car role) 'face-defface-spec)) :foreground)
			   (alist-get (cdr role) colours))))
	  (setq colours '((blue . "#667788") (cyan . "#778899") (magenta . "#8899aa")
			  (yellow . "#99aabb") (fg-dim . "#aabbcc")))
	  (run-hook-with-args 'enable-theme-functions 'synthetic-modus)
	  (dolist (role roles)
	    (should (equal (plist-get (cadar (get (car role) 'face-defface-spec)) :foreground)
			   (alist-get (cdr role) colours))))
	  (should (equal before (face-attribute 'default :foreground))))
      (hub/music-row--apply-faces))))

(ert-deftest hub/music-row-provenance-shows-queue-rank-and-heart-without-liked-rank ()
  "Only queue occurrences have ranks; a heart denotes retained Liked membership."
  (hub/music-search-test--queue
   (hub/music-search-test--library
    (let* ((queued (hub/music-row-from-queue duplicate playlist (nth 2 positions) 17))
	   (liked (hub/music-row-from-liked raw 42 library)))
      (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
	(should (equal (substring-no-properties (hub/music-row--provenance queued)) "Queue #18 · Spotify"))
	(setf (hub/music-row-liked-row queued) liked)
	(should (equal (substring-no-properties (hub/music-row--provenance queued)) "Queue #18 ♥ · Spotify"))
	(should (equal (substring-no-properties (hub/music-row--provenance liked)) "♥ · Spotify"))
	(let* ((label (hub/music-row--provenance queued))
	       (position (string-match-p "♥" label)))
	  (should (equal (get-text-property position 'help-echo label) "Saved in loaded Liked Songs")))
	(should (eq (hub/music-row-liked-row queued) liked)))
      (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
	(should (string-match-p "Liked" (hub/music-row--provenance liked)))
	(should-not (string-match-p "#[0-9]" (hub/music-row--provenance liked))))))))

(provide 'music-row-test)
;;; music-row-test.el ends here
