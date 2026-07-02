;;; org-comments-migrate-test.el --- Migration tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for explicit sidecar property migration helpers.

;;; Code:

(require 'ert)
(require 'org-comments-migrate)

(ert-deftest org-comments-migrate-detects-legacy-sidecar-properties ()
  "Legacy sidecar detection finds old comment property names."
  (with-temp-buffer
    (insert "* OPEN Note\n:PROPERTIES:\n:HUB_COMMENT_ID: c1\n:END:\n")
    (should (org-comments-legacy-sidecar-p))))

(ert-deftest org-comments-migrate-sidecar-rewrites-properties ()
  "Sidecar migration rewrites legacy properties without changing content."
  (let* ((directory (make-temp-file "org-comments-migrate" t))
	 (sidecar (expand-file-name "source.comments.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n\n")
	    (insert "* OPEN Note\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":HUB_COMMENT_ID: c1\n")
	    (insert ":HUB_COMMENT_TARGET_TEXT: Alpha\n")
	    (insert ":HUB_COMMENT_REMOTE_ID: r1\n")
	    (insert ":END:\n\nBody stays.\n"))
	  (should (= 3 (org-comments-migrate-sidecar sidecar)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":HUB_COMMENT_ID:" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_ID: c1" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: Alpha" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: r1" nil t))
	    (should (search-forward "Body stays." nil t))))
      (delete-directory directory t))))

(ert-deftest org-comments-migrate-sidecar-is-idempotent ()
  "Migrating an already migrated sidecar is a no-op."
  (let* ((directory (make-temp-file "org-comments-migrate" t))
	 (sidecar (expand-file-name "source.comments.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "* OPEN Note\n:PROPERTIES:\n:ORG_COMMENTS_ID: c1\n:END:\n"))
	  (should (= 0 (org-comments-migrate-sidecar sidecar))))
      (delete-directory directory t))))

(ert-deftest org-comments-migrate-directory-sidecars-reports-counts ()
  "Directory migration returns a file/count alist."
  (let* ((directory (make-temp-file "org-comments-migrate" t))
	 (sidecar-a (expand-file-name "a.comments.org" directory))
	 (sidecar-b (expand-file-name "nested/b.comments.org" directory)))
    (unwind-protect
	(progn
	  (make-directory (file-name-directory sidecar-b) t)
	  (with-temp-file sidecar-a
	    (insert "* OPEN A\n:PROPERTIES:\n:HUB_COMMENT_ID: a\n:END:\n"))
	  (with-temp-file sidecar-b
	    (insert "* OPEN B\n:PROPERTIES:\n:HUB_COMMENT_ID: b\n:HUB_COMMENT_REMOTE_ID: rb\n:END:\n"))
	  (should (equal (sort (mapcar #'cdr
				       (org-comments-migrate-directory-sidecars directory))
			       #'<)
			 '(1 2))))
      (delete-directory directory t))))

(provide 'org-comments-migrate-test)
;;; org-comments-migrate-test.el ends here
