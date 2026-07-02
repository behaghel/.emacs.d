;;; org-comments-page-panel-test.el --- Page panel tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for package-owned page-comments panel behavior.

;;; Code:

(require 'ert)
(require 'org-comments)

(defmacro org-comments-page-panel-test--with-page-comment (&rest body)
  "Create a temporary source with one page comment, then run BODY."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "org-comments-page-panel" t))
	  (source-file (expand-file-name "source.org" directory))
	  (org-comments-ui-page-open-function nil))
     (unwind-protect
	 (with-current-buffer (find-file-noselect source-file)
	   (erase-buffer)
	   (insert "#+TITLE: Source\n\nBody\n")
	   (save-buffer)
	   (org-mode)
	   (org-comments-page--append-to-sidecar
	    (org-comments-page--create-record buffer-file-name "Page note" "p1" "Alice" "now"))
	   ,@body)
       (when (get-buffer org-comments-page-panel-buffer-name)
	 (kill-buffer org-comments-page-panel-buffer-name))
       (when (get-buffer org-comments-panel-buffer-name)
	 (kill-buffer org-comments-panel-buffer-name))
       (when-let* ((source-buffer (find-buffer-visiting source-file)))
	 (kill-buffer source-buffer))
       (delete-directory directory t))))

(ert-deftest org-comments-page-panel-open-renders-only-page-comments ()
  "Opening the page panel renders page comments without inline comments."
  (org-comments-page-panel-test--with-page-comment
   (org-comments-append-to-sidecar
    (org-comments-create-record buffer-file-name (point-min) (point-min) "Inline note" "c1" "Alice" "now"))
   (org-comments-page-open)
   (with-current-buffer org-comments-page-panel-buffer-name
     (should (derived-mode-p 'org-comments-panel-mode))
     (should (string-match-p "1 comment" (buffer-string)))
     (should (string-match-p "Page note" (buffer-string)))
     (should-not (string-match-p "Inline note" (buffer-string))))))

(ert-deftest org-comments-page-panel-marker-ret-opens-page-panel ()
  "The page marker keymap opens the package-owned page panel view."
  (org-comments-page-panel-test--with-page-comment
   (org-comments-mode 1)
   (let* ((keymap (overlay-get org-comments-page-comment-overlay 'keymap))
	  (command (lookup-key keymap (kbd "RET"))))
     (should (eq command #'org-context-panel-open-marker-view))
     (goto-char (overlay-start org-comments-page-comment-overlay))
     (call-interactively command))
   (should (get-buffer org-comments-page-panel-buffer-name))))

(ert-deftest org-comments-page-panel-actions-work-on-page-comments ()
  "Generic panel actions work in the page-only panel."
  (org-comments-page-panel-test--with-page-comment
   (org-comments-page-open)
   (with-current-buffer org-comments-page-panel-buffer-name
     (goto-char (point-min))
     (search-forward "Page note")
     (org-comments-panel-mark-resolved)
     (should (string-match-p "\\[RESOLVED\\]" (buffer-string)))
     (goto-char (point-min))
     (search-forward "Page note")
     (org-comments-panel-delete)
     (should-not (string-match-p "Page note" (buffer-string))))))

(provide 'org-comments-page-panel-test)
;;; org-comments-page-panel-test.el ends here
