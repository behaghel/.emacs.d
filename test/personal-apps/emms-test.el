;;; emms-test.el --- Audio opening and seeking tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise personal EMMS integration without external players or packages.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defconst hub/emms-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root for audio integration tests.")

;; Load the audio helpers without installing or configuring optional packages.
(let ((load-path (cons (expand-file-name "lisp" hub/emms-test--root) load-path)))
  (load (expand-file-name "modules/interactive/apps/emms-player.el"
			  hub/emms-test--root) nil t))

(ert-deftest hub/emms-org-audio-links-play-files ()
  "Org opens supported audio links through EMMS, preserving spaces in paths."
  (dolist (extension '("mp3" "wav" "wave" "ogg" "oga" "opus" "flac"
		       "m4a" "aac" "aiff" "aif" "wma" "MP3" "WAV" "OGG"))
    (let* ((file (make-temp-file "emms audio " nil (concat "." extension)))
	   played registered)
      (unwind-protect
	  (cl-letf (((symbol-function 'hub/emms-register-local-players)
		     (lambda () (setq registered t)))
		    ((symbol-function 'emms-play-file)
		     (lambda (path) (should registered) (setq played path))))
	    (with-temp-buffer
	      (org-mode)
	      (insert (format "[[file:%s][Listen]]" file))
	      (goto-char (point-min))
	      (org-open-at-point)
	      (should (equal played file))))
	(delete-file file)))))

(ert-deftest hub/emms-org-non-audio-rules-unchanged ()
  "Audio routing does not claim text files or names with trailing suffixes."
  (dolist (file '("notes.txt" "recording.mp3.txt" "video.mp4"))
    (should-not (string-match-p hub/emms-audio-file-regexp file)))
  (should (= 1 (cl-count (cons hub/emms-audio-file-regexp #'hub/emms-open-file)
			 org-file-apps :test #'equal))))

(ert-deftest hub/emms-seek-commands-use-fixed-offsets ()
  "Interactive commands seek back ten seconds and forward thirty seconds."
  (let (offsets)
    (cl-letf (((symbol-function 'emms-seek)
	       (lambda (offset) (push offset offsets))))
      (call-interactively #'hub/emms-seek-backward-10)
      (call-interactively #'hub/emms-seek-forward-30)
      (should (equal (nreverse offsets) '(-10 30))))))

(ert-deftest hub/emms-fetches-from-established-mirror ()
  "EMMS bootstrap uses the HTTPS mirror rather than depending on Savannah."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/interactive/apps/elfeed.el" hub/emms-test--root))
    (goto-char (point-min))
    (let (form)
      (while (not (and (listp form) (eq (car form) 'use-package)
		       (eq (cadr form) 'emms)))
	(setq form (read (current-buffer))))
      (let ((recipe (plist-get (cddr form) :straight)))
	(should (eq (plist-get recipe :host) 'github))
	(should (equal (plist-get recipe :repo) "emacsmirror/emms"))
	(should (eq (plist-get recipe :protocol) 'https))))))

(provide 'emms-test)
;;; emms-test.el ends here
