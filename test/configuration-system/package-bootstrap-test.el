;;; package-bootstrap-test.el --- Package bootstrap regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise startup failure propagation and the declared freeze command in
;; subprocesses without installing packages or changing the real lockfile.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst hub/bootstrap-test--root
  (expand-file-name
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository root containing the startup and developer command definitions.")

(ert-deftest hub/bootstrap-failure-propagates-without-fallback ()
  "A required bootstrap failure stops startup without a second bootstrap."
  (dolist (ci '(nil "true"))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "GITHUB_ACTIONS" ci)
      (with-temp-buffer
	(let* ((form
		`(progn
		   (require 'cl-lib)
		   (let ((original-require (symbol-function 'require))
			 (original-load (symbol-function 'load)))
		     (cl-labels
			 ((duplicate (&rest _args)
			    (princ "DUPLICATE-BOOTSTRAP-ATTEMPT\n")
			    (error "Duplicate bootstrap attempted")))
		       (cl-letf
			   (((symbol-function 'require)
			     (lambda (feature &rest args)
			       (if (eq feature 'core-packages)
				   (error "Fixture bootstrap failure")
				 (apply original-require feature args))))
			    ((symbol-function 'load)
			     (lambda (file &rest args)
			       (if (string-suffix-p "straight.el/bootstrap.el" file)
				   (duplicate)
				 (apply original-load file args))))
			    ((symbol-function 'url-retrieve-synchronously)
			     #'duplicate))
			 (load ,(expand-file-name "init.el" hub/bootstrap-test--root)))))))
	       (status
		(call-process (expand-file-name invocation-name invocation-directory)
			      nil t nil "--batch" "-Q" "--eval"
			      (prin1-to-string form)))
	       (output (buffer-string)))
	  (should (integerp status))
	  (should-not (zerop status))
	  (should (string-match-p "Fixture bootstrap failure" output))
	  (should-not (string-match-p "^DUPLICATE-BOOTSTRAP-ATTEMPT$" output)))))))

(ert-deftest hub/freeze-command-uses-canonical-bootstrap-from-checkout ()
  "The declared freeze command reaches the real bootstrap with isolated HOME."
  (let* ((root (make-temp-file "hub-freeze-command-" t))
	 (default-directory (file-name-as-directory root))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (setenv "HOME" root)
	  (make-directory (expand-file-name "core" root) t)
	  (dolist (file '("core/core-packages.el" "core/core-predicates.el"))
	    (copy-file (expand-file-name file hub/bootstrap-test--root)
		       (expand-file-name file root)))
	  (let ((bootstrap (expand-file-name
			    "straight/repos/straight.el/bootstrap.el" root)))
	    (make-directory (file-name-directory bootstrap) t)
	    (with-temp-file bootstrap
	      (insert
	       "(princ \"CANONICAL-BOOTSTRAP\\n\")\n"
	       "(defun straight-use-package (&rest _args) nil)\n"
	       "(dolist (feature '(use-package use-package-ensure\n"
	       "                   use-package-delight use-package-diminish))\n"
	       "  (provide feature))\n"
	       "(defun straight-freeze-versions ()\n"
	       "  (princ (concat \"FROZEN:\" user-emacs-directory \"\\n\")))\n")))
	  (let ((command
		 (with-temp-buffer
		   (insert-file-contents
		    (expand-file-name "devenv.nix" hub/bootstrap-test--root))
		   (goto-char (point-min))
		   (should (re-search-forward "freeze\\.exec = ''" nil t))
		   (let ((start (point)))
		     (should (re-search-forward "^[ \t]*''" nil t))
		     (buffer-substring-no-properties start (match-beginning 0))))))
	    (with-temp-buffer
	      (let ((status (call-process "bash" nil t nil "-c" command)))
		(should (equal status 0))
		(should (string-match-p "CANONICAL-BOOTSTRAP" (buffer-string)))
		(should (string-match-p
			 (regexp-quote (concat "FROZEN:" default-directory))
			 (buffer-string)))))))
      (delete-directory root t))))

(ert-deftest hub/ai-dependencies-have-version-pins ()
  "Configured gptel libraries have reproducible commit pins without activation."
  (let ((versions
	 (with-temp-buffer
	   (insert-file-contents
	    (expand-file-name "straight/versions/default.el" hub/bootstrap-test--root))
	   (read (current-buffer)))))
    (dolist (package '("gptel" "gptel-agent"))
      (let ((pin (cdr (assoc package versions))))
	(should (stringp pin))
	(should (string-match-p "\\`[[:xdigit:]]\\{40\\}\\'" pin))))))

(provide 'package-bootstrap-test)
;;; package-bootstrap-test.el ends here
