;;; video-tjm-test.el --- Tests for video TJM mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises editing primitives for the text-driven video major mode.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "test" default-directory))
(require 'test-helpers)

;; Provide minimal stubs when the optional dependencies are absent during tests.
(unless (fboundp 'defhydra)
  (defmacro defhydra (&rest _r)
    nil))
(unless (featurep 'hydra)
  (provide 'hydra))
(unless (featurep 'general)
  (provide 'general))

(add-to-list 'load-path (expand-file-name "modules/interactive" default-directory))
(require 'video/tjm)

(defconst video-tjm-test--sample-data
  '((version . 1)
    (sources . (((id . "clip") (file . "clip.mp4"))))
    (segments .
	      (((id . "seg-1")
		(source . "clip")
		(start . 1.0)
		(end . 4.0)
		(text . "alpha beta gamma delta")
		(broll . nil)
		(words .
		       (((start . 1.0) (end . 1.5) (token . "alpha"))
			((start . 1.5) (end . 2.0) (token . "beta"))
			((start . 2.0) (end . 3.0) (token . "gamma"))
			((start . 3.0) (end . 4.0) (token . "delta"))))))))
  "In-memory manifest used across tests.")

(defmacro video-tjm-test-with-buffer (&rest body)
  "Execute BODY inside a fresh TJM buffer backed by the sample manifest."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((buffer-file-name nil))
       (insert "{}")
       (goto-char (point-min))
       (video-tjm-mode)
       (setq-local video-tjm--data (copy-tree video-tjm-test--sample-data t))
       (video-tjm--render)
       ,@body)))

(defun video-tjm-test--segment-tokens (segment)
  "Return the token list for SEGMENT."
  (mapcar (lambda (word)
	    (alist-get 'token word))
	  (alist-get 'words segment)))

(ert-deftest video-tjm-delete-word-updates-segment ()
  "Deleting a word rewrites text, words array, and timings."
  (video-tjm-test-with-buffer
   (should (video-tjm--goto-segment "seg-1"))
   (should (video-tjm--goto-word-by-index "seg-1" 1))
   (forward-char 1)
   (call-interactively #'video-tjm-delete-word)
   (let* ((segment (car (video-tjm--segments)))
	  (words (alist-get 'words segment)))
     (should (= 3 (length words)))
     (should (equal (video-tjm-test--segment-tokens segment)
		    '("alpha" "gamma" "delta")))
     (should (= (alist-get 'start segment) 1.0))
     (should (= (alist-get 'end segment) 4.0))
     (should (equal (alist-get 'text segment) "alpha gamma delta"))
     (should (looking-at "gamma")))))

(ert-deftest video-tjm-split-segment-creates-new-entry ()
  "Splitting a segment duplicates metadata and assigns a fresh id."
  (video-tjm-test-with-buffer
   (setq video-tjm--visual-separators (list "seg-1"))
   (should (video-tjm--goto-segment "seg-1"))
   (should (video-tjm--goto-word-by-index "seg-1" 2))
   (forward-char 1)
   (call-interactively #'video-tjm-split-segment)
   (let* ((segments (video-tjm--segments))
	  (first (nth 0 segments))
	  (second (nth 1 segments)))
     (should (= 2 (length segments)))
     (should (equal (alist-get 'id first) "seg-1"))
     (should (equal (video-tjm-test--segment-tokens first) '("alpha" "beta")))
     (should (= (alist-get 'start first) 1.0))
     (should (= (alist-get 'end first) 2.0))
     (should (string-prefix-p "seg-1-split-" (alist-get 'id second)))
     (should (equal (video-tjm-test--segment-tokens second) '("gamma" "delta")))
     (should (= (alist-get 'start second) 2.0))
     (should (= (alist-get 'end second) 4.0))
     (should (equal video-tjm--visual-separators (list (alist-get 'id second)))))
   (should (looking-at "gamma"))))

(ert-deftest video-tjm-insert-marker-before-current ()
  "Inserting a marker yields a dedicated marker segment ahead of point."
  (video-tjm-test-with-buffer
   (should (video-tjm--goto-segment "seg-1"))
   (let ((responses '("Launch Plan")))
     (cl-letf (((symbol-function 'read-string)
		(lambda (_prompt &optional _default)
		  (or (pop responses) "")))
	       ((symbol-function 'video-tjm--generate-marker-id)
		(lambda () "marker-001")))
       (call-interactively #'video-tjm-insert-marker)))
   (let ((segments (video-tjm--segments)))
     (should (= 2 (length segments)))
     (let ((marker (car segments)))
       (should (video-tjm--marker-p marker))
       (should (equal (alist-get 'id marker) "marker-001"))
       (should (equal (alist-get 'title marker) "Launch Plan"))
       (should (= (alist-get 'start marker) 1.0))
       (should (equal (alist-get 'source marker) "clip"))))))

(ert-deftest video-tjm-validation-ignores-markers ()
  "Markers without timings do not raise validation issues."
  (video-tjm-test-with-buffer
   (let* ((segments (video-tjm--segments))
	  (marker '((id . "marker-1") (kind . "marker") (title . "Intro"))))
     (setf (alist-get 'segments video-tjm--data) (append (list marker) segments))
     (video-tjm--render)
     (should (null (video-tjm-validate t))))))

(ert-deftest video-tjm-broll-prevents-still-with-broll-audio ()
  "Still-image overlays cannot request b-roll audio."
  (video-tjm-test-with-buffer
   (let ((segment (car (video-tjm--segments)))
	 (responses '("broll/banner.png" "overlay" "broll" "" "" "y")))
     (cl-letf (((symbol-function 'read-string)
		(lambda (_prompt &optional _default)
		  (or (pop responses) "")))
	       ((symbol-function 'image-file-name-p) (lambda (_file) t)))
       (should-error (video-tjm-edit-broll segment) :type 'user-error)))))

(ert-deftest video-tjm-marker-supports-broll ()
  "Markers accept b-roll metadata and render a summary line."
  (video-tjm-test-with-buffer
   (let* ((segments (video-tjm--segments))
	  (marker (list (cons 'id "marker-001")
			(cons 'kind "marker")
			(cons 'title "Launch Intro")
			(cons 'broll nil))))
     (setf (alist-get 'segments video-tjm--data) (cons marker segments))
     (video-tjm--render)
     (should (video-tjm--goto-segment "marker-001"))
     (let ((responses '("intro.mp4" "overlay" "mute" "" "" "n" "n")))
       (cl-letf (((symbol-function 'read-string)
		  (lambda (_prompt &optional default)
		    (or (prog1 (car responses)
			  (setq responses (cdr responses)))
			default))))
	 (call-interactively #'video-tjm-edit-broll)))
     (video-tjm--render)
     (should (video-tjm--goto-segment "marker-001"))
     (let* ((segment (video-tjm--segment-at-point))
	    (broll (alist-get 'broll segment)))
       (should broll)
       (should (string= (alist-get 'file broll) "intro.mp4")))
     (should (string-match-p "\\[b-roll\\] file=intro\\.mp4"
			     (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest video-tjm-edit-placeholders-merges-template ()
  "Placeholder editor preloads template defaults and stores overrides."
  (video-tjm-test-with-buffer
   (let* ((template-file (make-temp-file "tjm-template" nil ".json"
					 "{\"template\":\"card.mp4\",\"placeholders\":{\"cta\":\"Subscribe\",\"title\":\"Weekly Update\"}}"))
	  (cleanup (lambda () (when (file-exists-p template-file) (delete-file template-file)))))
     (unwind-protect
	 (progn
	   (let* ((segment (car (video-tjm--segments))))
	     (setf (alist-get 'broll segment)
		   (list (cons 'file template-file))))
	   (let ((segment (car (video-tjm--segments))))
	     (should (alist-get 'broll segment)))
	   (video-tjm--render)
	   (should (video-tjm--goto-segment "seg-1"))
	   (let ((responses '("Join us" "" "speaker" "Ari" "")))
	     (cl-letf (((symbol-function 'read-string)
			(lambda (_prompt &optional default)
			  (or (prog1 (car responses)
				(setq responses (cdr responses)))
			      default))))
	       (call-interactively #'video-tjm-edit-broll-placeholders)))
	   (let* ((segment (car (video-tjm--segments)))
		  (broll (alist-get 'broll segment))
		  (placeholders (video-tjm--aget 'placeholders broll)))
	     (should placeholders)
	     (should (equal (alist-get "cta" placeholders nil nil #'equal) "Join us"))
	     (should (equal (alist-get "speaker" placeholders nil nil #'equal) "Ari"))
	     (should-not (alist-get "title" placeholders nil nil #'equal))))
       (funcall cleanup)))))

(provide 'video-tjm-test)

;;; video-tjm-test.el ends here
