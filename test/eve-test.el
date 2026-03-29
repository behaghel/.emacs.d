;;; eve-test.el --- eve package test entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(let* ((repo-root (file-name-directory
		   (directory-file-name
		    (file-name-directory (or load-file-name buffer-file-name)))))
       (user-emacs-directory repo-root)
       (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
					 repo-root))
       (eve-recipe '(eve :type git :host github :repo "behaghel/eve.el"
			 :local-repo "eve.el"
			 :build (:not compile)))
       (eve-test-file (expand-file-name "straight/repos/eve.el/test/eve-test.el"
					repo-root)))
  (unless (file-exists-p bootstrap-file)
    (error "straight bootstrap not found at %s" bootstrap-file))
  (load bootstrap-file nil 'nomessage)
  (straight-use-package eve-recipe)
  (load eve-test-file nil 'nomessage))

;;; eve-test.el ends here
