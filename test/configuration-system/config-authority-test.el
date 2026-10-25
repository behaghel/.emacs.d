;;; config-authority-test.el --- Configuration source authority tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration Elisp is maintained directly.  Documentation and developer
;; workflows must not generate alternative copies of the executable config.

;;; Code:

(require 'ert)
(require 'org)
(require 'ob-core)

(defconst hub/config-authority-test--root
  (expand-file-name
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository root containing authoritative configuration and its workflows.")

(ert-deftest hub/config-documentation-does-not-tangle-elisp ()
  "Tracked Org documentation does not generate configuration Elisp."
  (let ((default-directory hub/config-authority-test--root)
	(org-mode-hook nil)
	targets)
    (dolist (file (process-lines "git" "ls-files" "docs/*.org"))
      (when (file-exists-p file)
	(with-temp-buffer
	  (insert-file-contents file)
	  (setq buffer-file-name (expand-file-name file))
	  (org-mode)
	  (org-babel-map-src-blocks nil
				    (let* ((info (org-babel-get-src-block-info 'light))
					   (language (nth 0 info))
					   (target (cdr (assq :tangle (nth 2 info)))))
				      (when (and (member language '("emacs-lisp" "elisp"))
						 target
						 (not (equal target "no")))
					(push (list file target) targets)))))))
    (should-not targets)))

(ert-deftest hub/ci-does-not-tangle-configuration ()
  "CI validates executable configuration without generating it from Org."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name ".github/workflows/emacs.yml" hub/config-authority-test--root))
    (should-not (re-search-forward "org-babel-\\(?:tangle\\|load\\)-file" nil t))
    (goto-char (point-min))
    (should-not (search-forward "docs/config.org" nil t))))

(ert-deftest hub/development-commands-do-not-tangle-configuration ()
  "The development environment does not offer a configuration tangle command."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "devenv.nix" hub/config-authority-test--root))
    (should-not (re-search-forward "org-babel-\\(?:tangle\\|load\\)-file" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "\\btangle\\.exec" nil t))
    (goto-char (point-min))
    (should-not (search-forward "docs/config.org" nil t))))

(provide 'config-authority-test)
;;; config-authority-test.el ends here
