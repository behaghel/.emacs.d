;;; standalone-package-test.el --- Standalone launcher regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run the interactive launcher's real command-line sequence in batch Emacs.
;; Visiting a command-line file does not initially display it in a window;
;; panel placement must still match an explicitly displayed source buffer.

;;; Code:

(require 'ert)

(defconst hub/standalone-package-test--root
  (file-name-as-directory
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository root containing the standalone launchers.")

(ert-deftest hub/standalone-marginalia-startup-opens-right-panel ()
  "Startup opens the marginalia panel on the right, not the popup fallback below."
  (let* ((directory (make-temp-file "standalone-launcher-test" t))
	 (wrapper (expand-file-name "emacs" directory))
	 (real-emacs (expand-file-name invocation-name invocation-directory))
	 (context-library (locate-library "context-panels"))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (should context-library)
	  (with-temp-file wrapper
	    (insert "#!/usr/bin/env bash\n"
		    "exec \"$STANDALONE_TEST_EMACS\" --batch \"$@\" --eval "
		    "'(let* ((source (get-file-buffer \"draft.org\")) "
		    "(panel (buffer-local-value (quote context-panels-side-panel-buffer) source)) "
		    "(window (get-buffer-window panel))) "
		    "(unless (and (get-buffer-window source) "
		    "(eq (window-parameter window (quote window-side)) (quote right))) "
		    "(error \"Startup did not place the panel to the right of its source\")))'\n"))
	  (set-file-modes wrapper #o755)
	  (setenv "STANDALONE_TEST_EMACS" real-emacs)
	  (setenv "CONTEXT_PANELS_PATH" (file-name-directory context-library))
	  (setenv "PATH" (concat directory path-separator (getenv "PATH")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/cp-org-marginalia-standalone"
					     hub/standalone-package-test--root)
			   nil (list t t) nil "--interactive")))
	      (ert-info ((buffer-string))
			(should (zerop status))))))
      (delete-directory directory t))))

(ert-deftest hub/standalone-static-site-startup-applies-project-settings ()
  "The real launcher activates Site mode before using project-local sources."
  (let* ((directory (make-temp-file "standalone-site-launcher-test" t))
	 (wrapper (expand-file-name "emacs" directory))
	 (real-emacs (expand-file-name invocation-name invocation-directory))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (with-temp-file wrapper
	    (insert "#!/usr/bin/env bash\n"
		    "exec \"$STANDALONE_TEST_EMACS\" --batch \"$@\" --eval "
		    "'(unless (and hb-static-site-mode "
		    "(not hb-static-site-enable-auto-export) "
		    "(get-buffer-window (current-buffer)) "
		    "(eq (key-binding (kbd \"C-c w p\")) "
		    "(quote hb-static-site-create-page)) "
		    "(hb-static-site-validate-buffer)) "
		    "(error \"Startup did not activate the disposable site\"))'\n"))
	  (set-file-modes wrapper #o755)
	  (setenv "STANDALONE_TEST_EMACS" real-emacs)
	  (setenv "CONTEXT_PANELS_PATH" (expand-file-name "absent-context-panels" directory))
	  (setenv "PATH" (concat directory path-separator (getenv "PATH")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/hb-static-site-standalone"
					     hub/standalone-package-test--root)
			   nil (list t t) nil "--interactive")))
	      (ert-info ((buffer-string)) (should (zerop status))))))
      (delete-directory directory t))))

(provide 'standalone-package-test)
;;; standalone-package-test.el ends here
