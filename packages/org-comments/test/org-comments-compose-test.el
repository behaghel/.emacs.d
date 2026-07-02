;;; org-comments-compose-test.el --- Compose tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for simple minibuffer reply composition.

;;; Code:

(require 'ert)
(require 'org-comments)

(defmacro org-comments-compose-test--with-panel-comment (&rest body)
  "Create a temporary source with one comment, then run BODY in its panel."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "org-comments-compose" t))
	  (source-file (expand-file-name "source.org" directory)))
     (unwind-protect
	 (progn
	   (with-temp-file source-file
	     (insert "Alpha selected text omega"))
	   (with-current-buffer (find-file-noselect source-file)
	     (org-mode)
	     (org-comments-append-to-sidecar
	      (org-comments-create-record buffer-file-name 7 20 "Review this." "c1" "Alice" "now"))
	     (org-comments-panel-open))
	   (with-current-buffer org-comments-panel-buffer-name
	     (goto-char (point-min))
	     (search-forward "Review this")
	     ,@body))
       (when (get-buffer org-comments-panel-buffer-name)
	 (kill-buffer org-comments-panel-buffer-name))
       (when-let* ((source-buffer (find-buffer-visiting source-file)))
	 (kill-buffer source-buffer))
       (delete-directory directory t))))

(ert-deftest org-comments-compose-reply-appends-child-heading ()
  "Reply composition appends a child reply under the selected comment."
  (org-comments-compose-test--with-panel-comment
   (let ((sidecar-file (plist-get (org-comments-panel-current-comment) :sidecar-file)))
     (org-comments-compose-reply (org-comments-panel-current-comment) "Looks good.")
     (with-temp-buffer
       (insert-file-contents sidecar-file)
       (should (search-forward "** OPEN Reply" nil t))
       (should (search-forward ":ORG_COMMENTS_SYNC_KIND: reply" nil t))
       (should (search-forward "Looks good." nil t))))))

(ert-deftest org-comments-compose-reply-prompts-from-panel-row ()
  "The panel reply command prompts in the minibuffer and refreshes the panel."
  (org-comments-compose-test--with-panel-comment
   (cl-letf (((symbol-function 'read-string)
	      (lambda (&rest _args) "Reply from prompt.")))
     (org-comments-panel-reply))
   (should (string-match-p "1 reply" (buffer-string)))
   (should (string-match-p "Reply from prompt" (buffer-string)))))

(ert-deftest org-comments-compose-reply-command-has-minibuffer-fallback ()
  "The source-buffer reply command works without a rich UI adapter."
  (let ((org-comments-ui-compose-reply-function nil))
    (org-comments-compose-test--with-panel-comment
     (let* ((comment (org-comments-panel-current-comment))
	    (source-buffer org-comments-panel-source-buffer)
	    (sidecar-file (plist-get comment :sidecar-file)))
       (with-current-buffer source-buffer
	 (goto-char 8)
	 (cl-letf (((symbol-function 'read-string)
		    (lambda (&rest _args) "Source reply.")))
	   (org-comments-reply)))
       (with-temp-buffer
	 (insert-file-contents sidecar-file)
	 (should (search-forward "Source reply." nil t)))))))

(provide 'org-comments-compose-test)
;;; org-comments-compose-test.el ends here
