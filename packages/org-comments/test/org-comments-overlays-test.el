;;; org-comments-overlays-test.el --- Overlay tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for minor-mode owned comment overlays.

;;; Code:

(require 'ert)
(require 'org-comments)

(defun org-comments-overlays-test--write-sidecar (source-file body)
  "Write BODY to SOURCE-FILE's comments sidecar."
  (let ((sidecar-file (org-comments-sidecar-path source-file)))
    (make-directory (file-name-directory sidecar-file) t)
    (with-temp-file sidecar-file
      (insert body))
    sidecar-file))

(ert-deftest org-comments-overlays-mode-adds-and-removes-region-overlays ()
  "`org-comments-mode' owns persistent inline comment overlays."
  (let* ((directory (make-temp-file "org-comments-overlays" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "Alpha beta gamma\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-comments-append-to-sidecar
	     (org-comments-create-record source-file 1 6 "Body" "c1" "Alice" "now"))
	    (org-comments-mode 1)
	    (should (seq-some (lambda (overlay)
				(eq (overlay-get overlay 'face)
				    'org-comments-region-face))
			      (overlays-at 1)))
	    (org-comments-mode -1)
	    (should-not (seq-some (lambda (overlay)
				    (eq (overlay-get overlay 'face)
					'org-comments-region-face))
				  (overlays-at 1)))))
      (delete-directory directory t))))

(ert-deftest org-comments-overlays-mode-can-disable-overlays ()
  "`org-comments-mode' can run as commands-only when overlays are disabled."
  (let* ((directory (make-temp-file "org-comments-overlays-disabled" t))
	 (source-file (expand-file-name "source.org" directory))
	 (org-comments-enable-overlays nil))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "Alpha beta gamma\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-comments-append-to-sidecar
	     (org-comments-create-record source-file 1 6 "Body" "c1" "Alice" "now"))
	    (org-comments-mode 1)
	    (should org-comments-mode)
	    (should-not (seq-some (lambda (overlay)
				    (eq (overlay-get overlay 'face)
					'org-comments-region-face))
				  (overlays-at 1)))
	    (org-comments-mode -1)))
      (delete-directory directory t))))

(ert-deftest org-comments-overlays-mode-adds-page-marker ()
  "`org-comments-mode' shows a top marker for page-level comments."
  (let* ((directory (make-temp-file "org-comments-page-overlays" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+TITLE: Source\n\nBody\n"))
	  (org-comments-overlays-test--write-sidecar
	   source-file
	   "#+SOURCE: source.org\n\n* OPEN Page note\n:PROPERTIES:\n:ORG_COMMENTS_ID: p1\n:ORG_COMMENTS_SOURCE_FILE: source.org\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nPage body\n")
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-comments-mode 1)
	    (should (overlayp org-comments-page-comment-overlay))
	    (should (string-match-p "PAGE comment"
				    (overlay-get org-comments-page-comment-overlay
						 'after-string)))
	    (org-comments-mode -1)
	    (should-not (overlayp org-comments-page-comment-overlay))))
      (delete-directory directory t))))

(provide 'org-comments-overlays-test)
;;; org-comments-overlays-test.el ends here
