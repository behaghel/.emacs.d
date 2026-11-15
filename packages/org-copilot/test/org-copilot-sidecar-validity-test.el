;;; org-copilot-sidecar-validity-test.el --- Transcript ownership tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Real files, visiting buffers and native supersession checks.  Timestamp
;; differences are controlled without suppressing conflict detection.

;;; Code:

(require 'ert)
(require 'org-copilot-sidecar)

(defun org-copilot-sidecar-validity-test--contents (file)
  "Return the exact disk contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defmacro org-copilot-sidecar-validity-test--with-transcript (&rest body)
  "Run BODY with a visited transcript whose timestamp predates the next write."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "copilot-sidecar-validity" t))
	  (source-file (expand-file-name "draft.org" directory))
	  (sidecar-file (org-copilot-sidecar-path source-file))
	  transcript)
     (unwind-protect
	 (progn
	   (org-copilot-sidecar-append-message source-file 'user "Original message")
	   (setq transcript (find-file-noselect sidecar-file))
	   (set-file-times sidecar-file (seconds-to-time 1000000000))
	   (with-current-buffer transcript (set-visited-file-modtime))
	   ,@body)
       (dolist (buffer (buffer-list))
	 (when (and (buffer-file-name buffer)
		    (string-prefix-p directory (buffer-file-name buffer)))
	   (with-current-buffer buffer (set-buffer-modified-p nil))
	   (kill-buffer buffer)))
       (delete-directory directory t))))

(defun org-copilot-sidecar-validity-test--mutate (operation source-file)
  "Perform transcript OPERATION for SOURCE-FILE."
  (pcase operation
    ('append (org-copilot-sidecar-append-message source-file 'assistant "New message"))
    ('archive (org-copilot-sidecar-archive-session source-file "default"))
    ('erase (org-copilot-sidecar-delete-session source-file "default"))))

(defun org-copilot-sidecar-validity-test--assert-synchronized (buffer file)
  "Assert BUFFER and FILE agree in contents and visited-file metadata."
  (with-current-buffer buffer
    (should (verify-visited-file-modtime buffer))
    (should-not (buffer-modified-p))
    (save-restriction
      (widen)
      (should (equal (buffer-string)
		     (org-copilot-sidecar-validity-test--contents file))))))

(ert-deftest org-copilot-sidecar-visited-repeated-append-stays-current ()
  "Repeated persistence updates the same visiting buffer without stale writes."
  (org-copilot-sidecar-validity-test--with-transcript
   (with-current-buffer transcript
     (goto-char (point-min))
     (narrow-to-region (point-min) (line-end-position)))
   (dolist (text '("Second message" "Third message"))
     (org-copilot-sidecar-append-message source-file 'assistant text)
     (should (eq (find-buffer-visiting sidecar-file) transcript))
     (org-copilot-sidecar-validity-test--assert-synchronized transcript sidecar-file))
   (with-current-buffer transcript (should (buffer-narrowed-p)))
   (should (equal (mapcar (lambda (m) (plist-get m :content))
			  (org-copilot-sidecar-load-messages source-file))
		  '("Original message" "Second message" "Third message")))))

(ert-deftest org-copilot-sidecar-visited-archive-append-erase-round-trip ()
  "Archive/new-session/erase retain synchronized views and archive contents."
  (org-copilot-sidecar-validity-test--with-transcript
   (org-copilot-sidecar-archive-session source-file "default")
   (org-copilot-sidecar-validity-test--assert-synchronized transcript sidecar-file)
   (let* ((archive (concat sidecar-file "_archive"))
	  (archived (org-copilot-sidecar-validity-test--contents archive)))
     (should (string-match-p "Original message" archived))
     (set-file-times sidecar-file (seconds-to-time 1000000000))
     (with-current-buffer transcript (set-visited-file-modtime))
     (org-copilot-sidecar-append-message source-file 'user "New session")
     (org-copilot-sidecar-validity-test--assert-synchronized transcript sidecar-file)
     (should (equal (plist-get (car (org-copilot-sidecar-load-messages source-file)) :content)
		    "New session"))
     (org-copilot-sidecar-delete-session source-file "default")
     (org-copilot-sidecar-validity-test--assert-synchronized transcript sidecar-file)
     (should-not (org-copilot-sidecar-load-messages source-file))
     (should (equal archived (org-copilot-sidecar-validity-test--contents archive))))))

(ert-deftest org-copilot-sidecar-unsaved-transcript-refuses-mutation ()
  "Append, archive and erase cannot overwrite or silently save human edits."
  (dolist (operation '(append archive erase))
    (org-copilot-sidecar-validity-test--with-transcript
     (with-current-buffer transcript
       (goto-char (point-max))
       (insert "\nUnsaved human note.\n"))
     (let ((human-text (with-current-buffer transcript (buffer-string)))
	   (disk (org-copilot-sidecar-validity-test--contents sidecar-file)))
       (should-error (org-copilot-sidecar-validity-test--mutate operation source-file)
		     :type 'user-error)
       (should (equal human-text (with-current-buffer transcript (buffer-string))))
       (should (buffer-modified-p transcript))
       (should (equal disk (org-copilot-sidecar-validity-test--contents sidecar-file)))
       (should-not (file-exists-p (concat sidecar-file "_archive")))))))

(ert-deftest org-copilot-sidecar-external-change-refuses-mutation ()
  "A stale visiting buffer is neither overwritten nor automatically reverted."
  (dolist (operation '(append archive erase))
    (org-copilot-sidecar-validity-test--with-transcript
     (let ((old-view (with-current-buffer transcript (buffer-string))))
       (with-temp-file sidecar-file (insert "External human revision.\n"))
       (set-file-times sidecar-file (time-add (current-time) 5))
       (should-not (verify-visited-file-modtime transcript))
       (should-error (org-copilot-sidecar-validity-test--mutate operation source-file)
		     :type 'user-error)
       (should (equal old-view (with-current-buffer transcript (buffer-string))))
       (should-not (buffer-modified-p transcript))
       (should (equal "External human revision.\n"
		      (org-copilot-sidecar-validity-test--contents sidecar-file)))
       (should-not (file-exists-p (concat sidecar-file "_archive")))))))

(provide 'org-copilot-sidecar-validity-test)
;;; org-copilot-sidecar-validity-test.el ends here
