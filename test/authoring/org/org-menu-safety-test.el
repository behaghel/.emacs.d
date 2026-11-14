;;; org-menu-safety-test.el --- Org mode-line menu safety -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify personal Org settings omit agenda submenu rebuilding during comment
;; counter evaluation without disabling agenda commands or suggestion counts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-comments)

(defvar hub/org-directory)

(ert-deftest hub/org-comment-counter-does-not-refresh-agenda-menu ()
  "Reading a real sidecar for the lighter must not rebuild the agenda menu."
  (let* ((hub/org-directory nil)
	 (org-agenda-file-menu-enabled t)
	 (org-agenda-files nil)
	 (org-comments-mode-line-open-suggestion-format " ✍%d")
	 (directory (make-temp-file "org-menu-safety-" t))
	 (file (expand-file-name "source.org" directory))
	 (menu-updates 0)
	 source)
    (unwind-protect
	(progn
	  (load (expand-file-name "modules/interactive/org/settings.el"
				  default-directory) nil t)
	  (with-temp-file file (insert "Review this paragraph.\n"))
	  (setq source (find-file-noselect file))
	  (with-current-buffer source
	    (org-comments-append-to-sidecar
	     (org-comments-create-record file 1 7 "Synthetic comment."
					 "menu-check" "Demo" "2026-11-14"))
	    (cl-letf (((symbol-function 'easy-menu-change)
		       (lambda (&rest _) (setq menu-updates (1+ menu-updates)))))
	      (should (equal (org-comments-mode-line-string) " OrgC")))
	    (should (= menu-updates 0))
	    (should-not org-agenda-file-menu-enabled)
	    (should (commandp 'org-agenda))
	    (cl-letf (((symbol-function 'org-comments-collect)
		       (lambda (&rest _)
			 '((:id "linked" :status "OPEN"
				:suggestion-thread-id "thread-one")))))
	      (should (equal (org-comments-mode-line-string) " OrgC ✍1")))))
      (dolist (buffer (buffer-list))
	(when-let* ((path (buffer-file-name buffer)))
	  (when (string-prefix-p directory path)
	    (with-current-buffer buffer (set-buffer-modified-p nil))
	    (kill-buffer buffer))))
      (delete-directory directory t))))

(provide 'org-menu-safety-test)
;;; org-menu-safety-test.el ends here
