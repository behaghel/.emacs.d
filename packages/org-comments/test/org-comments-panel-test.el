;;; org-comments-panel-test.el --- Panel tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the standalone package comments panel lifecycle.

;;; Code:

(require 'ert)
(require 'org-comments)

(defmacro org-comments-panel-test--with-source (contents &rest body)
  "Visit a temporary Org source buffer with CONTENTS, then run BODY."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "org-comments-panel" t))
	  (source-file (expand-file-name "source.org" directory)))
     (unwind-protect
	 (with-current-buffer (find-file-noselect source-file)
	   (erase-buffer)
	   (insert ,contents)
	   (save-buffer)
	   (org-mode)
	   (prog1 (progn ,@body)
	     (when (get-buffer org-comments-panel-buffer-name)
	       (kill-buffer org-comments-panel-buffer-name))))
       (delete-directory directory t))))

(ert-deftest org-comments-panel-open-creates-panel-mode-buffer ()
  "Opening the package panel creates a panel buffer tied to the source buffer."
  (org-comments-panel-test--with-source "Alpha selected text omega"
					(let ((source-buffer (current-buffer)))
					  (org-comments-panel-open)
					  (with-current-buffer org-comments-panel-buffer-name
					    (should (derived-mode-p 'org-comments-panel-mode))
					    (should (eq org-comments-panel-source-buffer source-buffer))))))

(ert-deftest org-comments-panel-open-renders-comments-from-source ()
  "The package panel renders a standalone list of source comments."
  (org-comments-panel-test--with-source "Alpha selected text omega"
					(let ((record (org-comments-create-record buffer-file-name 7 20 "Review this." "c1" "Alice" "now")))
					  (org-comments-append-to-sidecar record)
					  (org-comments-panel-open)
					  (with-current-buffer org-comments-panel-buffer-name
					    (should (derived-mode-p 'org-comments-panel-mode))
					    (should (string-match-p "1 comment" (buffer-string)))
					    (should (string-match-p "Review this" (buffer-string)))
					    (should (string-match-p "selected text" (buffer-string)))))))

(ert-deftest org-comments-panel-filters-resolved-comments ()
  "The package panel applies source-buffer-scoped filter state while rendering."
  (org-comments-panel-test--with-source "Alpha selected text omega"
					(org-comments-append-to-sidecar
					 (org-comments-create-record buffer-file-name 7 20 "Open note" "open" "Alice" "now"))
					(let ((resolved (org-comments-create-record
							 buffer-file-name 7 20 "Resolved note" "resolved" "Alice" "now")))
					  (setq resolved (plist-put resolved :status "RESOLVED"))
					  (org-comments-append-to-sidecar resolved))
					(org-comments-panel-open)
					(org-comments-filter-set-state '(:show-resolved nil) (current-buffer))
					(with-current-buffer org-comments-panel-buffer-name
					  (org-comments-panel-refresh)
					  (should (string-match-p "Open note" (buffer-string)))
					  (should-not (string-match-p "Resolved note" (buffer-string))))))

(ert-deftest org-comments-panel-close-kills-panel-buffer ()
  "Closing the package panel removes its buffer."
  (org-comments-panel-test--with-source "Alpha"
					(org-comments-panel-open)
					(with-current-buffer org-comments-panel-buffer-name
					  (org-comments-panel-close))
					(should-not (get-buffer org-comments-panel-buffer-name))))

(provide 'org-comments-panel-test)
;;; org-comments-panel-test.el ends here
