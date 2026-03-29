;;; denote-note-test.el --- Denote note editing defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression test for Denote note creation behavior.

;;; Code:

(require 'ert)

(let ((user-emacs-directory (file-name-as-directory default-directory)))
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/interactive" user-emacs-directory))
  (require 'core-packages)
  (require 'editing/evil)
  (require 'org/core)
  (require 'notes/brain))

(ert-deftest hub/denote-note-keeps-org-editing-experience ()
  "A new Denote note opens as Org and preserves key editing behaviors."
  (let* ((tmp-dir (make-temp-file "hub-denote-test-" t))
	 (denote-directory tmp-dir)
	 (path (denote "ERT denote note" '("emacs") nil denote-directory nil nil nil nil)))
    (unwind-protect
	(with-current-buffer (find-file-noselect path)
	  (should (derived-mode-p 'org-mode))
	  (should (featurep 'org-tempo))
	  (should (equal (alist-get "c" org-structure-template-alist nil nil #'equal)
			 "comment"))
	  (evil-insert-state)
	  (should (eq (key-binding (kbd "<escape>")) 'evil-normal-state))
	  (erase-buffer)
	  (insert "<c")
	  (org-cycle)
	  (goto-char (point-min))
	  (should (search-forward "#+begin_comment" nil t)))
      (ignore-errors
	(when-let* ((buf (find-buffer-visiting path)))
	  (kill-buffer buf))
	(when (file-exists-p path)
	  (delete-file path))
	(when (file-directory-p tmp-dir)
	  (delete-directory tmp-dir t))))))

(provide 'denote-note-test)
;;; denote-note-test.el ends here
