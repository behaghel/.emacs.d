;;; hb-static-site-standalone-test.el --- Standalone static-site tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify neutral loading, project-local creation/validation, and actionable
;; missing-dependency errors without personal configuration or repository helpers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'package)
(require 'hb-static-site)

(defconst hb-static-site-standalone-test--examples
  (expand-file-name "examples" (file-name-directory (locate-library "hb-static-site")))
  "Directory containing the disposable Hugo project example.")

(defmacro hb-static-site-standalone-test--with-project (&rest body)
  "Run BODY in an isolated project with the documented directory-local settings."
  (declare (indent 0))
  `(let* ((root (make-temp-file "hb-static-site-project" t))
	  (default-directory (file-name-as-directory root))
	  (content (expand-file-name "content-org" root))
	  (welcome (expand-file-name "pages/welcome/index.org" content))
	  (org-hugo-base-dir nil)
	  (denote-directory nil)
	  (hb-static-site-content-org-directory nil)
	  (hb-static-site-mode-hook nil)
	  (enable-local-variables t)
	  (enable-local-eval t))
     (unwind-protect
	 (save-window-excursion
	   (make-directory (file-name-directory welcome) t)
	   (copy-file (expand-file-name "hugo.toml" hb-static-site-standalone-test--examples)
		      (expand-file-name "hugo.toml" root))
	   (copy-file (expand-file-name "site-settings.sexp" hb-static-site-standalone-test--examples)
		      (expand-file-name ".dir-locals.el" root))
	   (copy-file (expand-file-name "standalone.org" hb-static-site-standalone-test--examples)
		      welcome)
	   (find-file welcome)
	   ,@body)
       (dolist (buffer (buffer-list))
	 (when (and (buffer-local-value 'buffer-file-name buffer)
		    (file-in-directory-p (buffer-local-value 'buffer-file-name buffer) root))
	   (with-current-buffer buffer (set-buffer-modified-p nil))
	   (kill-buffer buffer)))
       (delete-directory root t))))

(ert-deftest hb-static-site-standalone-has-package-metadata ()
  "The package declares its version and hard dependencies without optional tools."
  (with-temp-buffer
    (insert-file-contents (locate-library "hb-static-site"))
    (let ((descriptor (package-buffer-info)))
      (should (package-desc-version descriptor))
      (should (equal (mapcar #'car (package-desc-reqs descriptor)) '(emacs org))))))

(ert-deftest hb-static-site-standalone-does-not-install-personal-integrations ()
  "Loading and enabling the library do not install Evil keys or prose policy."
  (let ((after-load-alist nil)
	(hb-static-site-enable-auto-export nil)
	(hb-static-site-mode-hook nil)
	calls)
    (cl-letf (((symbol-function 'evil-define-key)
	       (lambda (&rest _) (push 'evil calls)))
	      ((symbol-function 'which-key-add-key-based-replacements)
	       (lambda (&rest _) (push 'which-key calls)))
	      ((symbol-function 'writing/enable-basics)
	       (lambda () (push 'personal-prose calls))))
      (load (locate-library "hb-static-site") nil t)
      (should-not (assq 'evil after-load-alist))
      (should-not (assq 'which-key after-load-alist))
      (with-temp-buffer
	(org-mode)
	(hb-static-site-mode 1))
      (should-not calls))))

(ert-deftest hb-static-site-standalone-creates-and-validates-project-local-pages ()
  "Native keys create and validate a section and leaf page with local settings."
  (hb-static-site-standalone-test--with-project
   (should hb-static-site-mode)
   (should-not hb-static-site-enable-auto-export)
   (should (eq (key-binding (kbd "C-c w v")) #'hb-static-site-validate-buffer))
   (execute-kbd-macro (kbd "C-c w v"))
   (cl-letf (((symbol-function 'read-string)
	      (lambda (prompt &rest _)
		(if (string-match-p "slug" prompt) "notes" "Notes"))))
     (should (eq (key-binding (kbd "C-c w s")) #'hb-static-site-create-section))
     (execute-kbd-macro (kbd "C-c w s")))
   (should (equal (buffer-file-name) (expand-file-name "notes/_index.org" content)))
   (should hb-static-site-mode)
   (execute-kbd-macro (kbd "C-x C-s"))
   (cl-letf (((symbol-function 'completing-read)
	      (lambda (_prompt collection &rest _)
		(should (member "notes" collection))
		"notes"))
	     ((symbol-function 'read-string)
	      (lambda (prompt &rest _)
		(if (string-match-p "slug" prompt) "first-page" "First page"))))
     (should (eq (key-binding (kbd "C-c w p")) #'hb-static-site-create-page))
     (execute-kbd-macro (kbd "C-c w p")))
   (should (equal (buffer-file-name)
		  (expand-file-name "notes/first-page/index.org" content)))
   (should hb-static-site-mode)
   (should (string-match-p "^#\\+hugo_bundle: first-page" (buffer-string)))
   (should (equal (file-truename (hb-static-site-hugo-base-dir))
		  (file-name-as-directory (file-truename root))))
   (execute-kbd-macro (kbd "C-x C-s"))
   (execute-kbd-macro (kbd "C-c w v"))
   (should (hb-static-site-validate-buffer))))

(ert-deftest hb-static-site-standalone-auto-export-receives-project-root ()
  "Automatic export receives an absolute site root, not the source directory."
  (hb-static-site-standalone-test--with-project
   (let ((original-require (symbol-function 'require))
	 export-base)
     (cl-letf (((symbol-function 'require)
		(lambda (feature &optional filename noerror)
		  (if (eq feature 'ox-hugo) t
		    (funcall original-require feature filename noerror))))
	       ((symbol-function 'org-hugo-auto-export-mode)
		(lambda (arg)
		  (should (= arg 1))
		  (setq export-base org-hugo-base-dir))))
       (let ((hb-static-site-enable-auto-export t))
	 (hb-static-site-mode 1)))
     (should (file-name-absolute-p export-base))
     (should (equal (directory-file-name (file-truename export-base))
		    (file-truename root))))))

(ert-deftest hb-static-site-standalone-missing-denote-does-not-create-a-post ()
  "Post creation explains a missing Denote dependency before writing a post."
  (hb-static-site-standalone-test--with-project
   (let ((original-require (symbol-function 'require))
	 (before (buffer-string)))
     (cl-letf (((symbol-function 'require)
		(lambda (feature &optional filename noerror)
		  (unless (eq feature 'denote)
		    (funcall original-require feature filename noerror)))))
       (should (string-match-p
		"Denote is not available; install/load denote"
		(error-message-string
		 (should-error (call-interactively #'hb-static-site-create-post)
			       :type 'user-error)))))
     (should (equal (buffer-string) before))
     (should-not (file-exists-p (expand-file-name "posts" content))))))

(ert-deftest hb-static-site-standalone-missing-ox-hugo-does-not-export ()
  "Export explains a missing ox-hugo dependency without mutating source."
  (hb-static-site-standalone-test--with-project
   (let ((original-require (symbol-function 'require))
	 (before (buffer-string)))
     (cl-letf (((symbol-function 'require)
		(lambda (feature &optional filename noerror)
		  (unless (eq feature 'ox-hugo)
		    (funcall original-require feature filename noerror)))))
       (should (string-match-p
		"ox-hugo is not available; install/load ox-hugo"
		(error-message-string
		 (should-error (call-interactively #'hb-static-site-export-buffer)
			       :type 'user-error)))))
     (should (equal (buffer-string) before))
     (should-not (directory-files-recursively root "\\.md\\'")))))

(provide 'hb-static-site-standalone-test)
;;; hb-static-site-standalone-test.el ends here
