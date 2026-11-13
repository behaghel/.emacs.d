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

(ert-deftest hub/standalone-copies-context-panels-source-dependency-closure ()
  "The copier includes new source modules but excludes compiled/private files."
  (let* ((directory (make-temp-file "standalone-context-closure-" t))
	 (dependency (expand-file-name "dependency" directory))
	 (wrapper (expand-file-name "emacs" directory))
	 (real-emacs (expand-file-name invocation-name invocation-directory))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (make-directory dependency)
	  (dolist (entry '(("context-panels.el" . "(require 'context-panels-fixture-extra)\n(provide 'context-panels)\n")
			   ("context-panels-org.el" . "(provide 'context-panels-org)\n")
			   ("context-panels-fixture-extra.el" . "(provide 'context-panels-fixture-extra)\n")
			   ("must-not-copy.elc" . "not compiled source")
			   ("must-not-copy.txt" . "fixture-only private state")))
	    (with-temp-file (expand-file-name (car entry) dependency)
	      (insert (cdr entry))))
	  (with-temp-file wrapper
	    (insert "#!/usr/bin/env bash\nset -euo pipefail\n"
		    "for arg in \"$@\"; do\n"
		    "  if [[ -d \"$arg\" && -r \"$arg/context-panels.el\" ]]; then\n"
		    "    exec \"$STANDALONE_TEST_EMACS\" --batch -Q -L \"$arg\" --eval "
		    "'(progn (require (quote context-panels)) "
		    "(unless (featurep (quote context-panels-fixture-extra)) "
		    "(error \"Transitive source module is absent\")) "
		    "(dolist (file (quote (\"must-not-copy.elc\" \"must-not-copy.txt\"))) "
		    "(when (file-exists-p (expand-file-name file "
		    "(file-name-directory (locate-library \"context-panels\")))) "
		    "(error \"Non-source dependency state was copied\"))))'\n"
		    "  fi\ndone\nexit 1\n"))
	  (set-file-modes wrapper #o755)
	  (setenv "STANDALONE_TEST_EMACS" real-emacs)
	  (setenv "CONTEXT_PANELS_PATH" dependency)
	  (setenv "PATH" (concat directory path-separator (getenv "PATH")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/cp-org-marginalia-standalone"
					     hub/standalone-package-test--root)
			   nil (list t t) nil "--interactive")))
	      (ert-info ((buffer-string)) (should (zerop status))))))
      (delete-directory directory t))))

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

(ert-deftest hub/standalone-ox-sync-startup-opens-offline-bottom-panel ()
  "The real launcher opens a bottom status panel without an implicit fetch."
  (let* ((directory (make-temp-file "standalone-sync-launcher-test" t))
	 (wrapper (expand-file-name "emacs" directory))
	 (real-emacs (expand-file-name invocation-name invocation-directory))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (with-temp-file wrapper
	    (insert "#!/usr/bin/env bash\n"
		    "exec \"$STANDALONE_TEST_EMACS\" --batch \"$@\" --eval "
		    "'(let* ((source (get-file-buffer \"draft.org\")) "
		    "(panel (buffer-local-value (quote context-panels-bottom-panel-buffer) source))) "
		    "(unless (and (get-buffer-window source) "
		    "(eq (window-parameter (get-buffer-window panel) (quote window-side)) (quote bottom)) "
		    "(zerop (plist-get (buffer-local-value (quote ox-sync-demo--state) source) :fetches))) "
		    "(error \"Startup did not open the offline sync panel\")))'\n"))
	  (set-file-modes wrapper #o755)
	  (setenv "STANDALONE_TEST_EMACS" real-emacs)
	  (setenv "PATH" (concat directory path-separator (getenv "PATH")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/ox-sync-standalone"
					     hub/standalone-package-test--root)
			   nil (list t t) nil "--interactive")))
	      (ert-info ((buffer-string)) (should (zerop status))))))
      (delete-directory directory t))))

(ert-deftest hub/standalone-confluence-startup-converts-with-remote-guards ()
  "The real launcher activates local conversion with process/auth/browser guards."
  (let* ((directory (make-temp-file "standalone-confluence-launcher-test" t))
	 (wrapper (expand-file-name "emacs" directory))
	 (real-emacs (expand-file-name invocation-name invocation-directory))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (with-temp-file wrapper
	    (insert "#!/usr/bin/env bash\n"
		    "exec \"$STANDALONE_TEST_EMACS\" --batch \"$@\" --eval "
		    "'(progn (unless (and org-confluence-mode org-comments-mode "
		    "(not org-export-use-babel) "
		    "(get-buffer-window org-confluence-demo--source-buffer) "
		    "(advice-member-p (function org-confluence-demo--disabled) "
		    "(quote org-confluence-api--read-cfl-config)) "
		    "(advice-member-p (function org-confluence-demo--disabled) (quote browse-url))) "
		    "(error \"Offline Confluence startup guards are missing\")) "
		    "(unless (condition-case nil (progn (process-file \"sh\" nil nil nil \"-c\" \"exit 0\") nil) "
		    "(user-error t)) (error \"External processes were not blocked\")) "
		    "(let ((source org-confluence-demo--source-buffer) (before (buffer-string))) "
		    "(org-confluence-demo-preview) (org-confluence-demo-roundtrip) "
		    "(unless (and buffer-read-only "
		    "(equal before (with-current-buffer source (buffer-string)))) "
		    "(error \"Local preview modified its source\"))))'\n"))
	  (set-file-modes wrapper #o755)
	  (setenv "STANDALONE_TEST_EMACS" real-emacs)
	  (setenv "PATH" (concat directory path-separator (getenv "PATH")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/org-confluence-standalone"
					     hub/standalone-package-test--root)
			   nil (list t t) nil "--interactive")))
	      (ert-info ((buffer-string)) (should (zerop status))))))
      (delete-directory directory t))))

(ert-deftest hub/standalone-copilot-startup-opens-offline-chat-and-sends-native-input ()
  "The real launcher opens source-owned chat and handles RET without a provider."
  (let* ((directory (make-temp-file "standalone-copilot-launcher-test" t))
	 (wrapper (expand-file-name "emacs" directory))
	 (real-emacs (expand-file-name invocation-name invocation-directory))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (with-temp-file wrapper
	    (insert "#!/usr/bin/env bash\n"
		    "exec \"$STANDALONE_TEST_EMACS\" --batch \"$@\" --eval "
		    "'(let* ((source (get-file-buffer \"draft.org\")) "
		    "(chat (buffer-local-value (quote context-panels-bottom-panel-buffer) source)) "
		    "(before (with-current-buffer source (buffer-string)))) "
		    "(unless (and (get-buffer-window source) "
		    "(not (featurep (quote gptel))) "
		    "(eq (window-parameter (get-buffer-window chat) (quote window-side)) (quote bottom))) "
		    "(error \"Standalone Copilot did not open a source-owned offline chat\")) "
		    "(dolist (function (quote (make-network-process process-file auth-source-search))) "
		    "(unless (advice-member-p (quote org-copilot-standalone-io-guard) function) "
		    "(error \"Standalone I/O guard is missing\"))) "
		    "(unless (condition-case nil "
		    "(progn (ignore-errors (org-copilot-standalone--deny-io (quote fixture))) nil) "
		    "(org-copilot-standalone-unexpected-io t)) "
		    "(error \"Ordinary error handling swallowed the I/O guard\")) "
		    "(with-selected-window (get-buffer-window chat) "
		    "(org-copilot-chat-goto-prompt) (insert \"answer\") "
		    "(execute-kbd-macro (kbd \"RET\"))) "
		    "(unless (= 1 (length org-copilot-demo--pending)) "
		    "(error \"Native RET did not queue the scripted reply\")) "
		    "(with-temp-buffer (org-copilot-demo-respond)) "
		    "(unless (and (file-exists-p (org-copilot-sidecar-path (buffer-file-name source))) "
		    "(equal before (with-current-buffer source (buffer-string)))) "
		    "(error \"Scripted answer changed source or lost its transcript\")))'\n"))
	  (set-file-modes wrapper #o755)
	  (setenv "STANDALONE_TEST_EMACS" real-emacs)
	  (setenv "PATH" (concat directory path-separator (getenv "PATH")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/org-copilot-standalone"
					     hub/standalone-package-test--root)
			   nil (list t t) nil "--interactive")))
	      (ert-info ((buffer-string)) (should (zerop status))))))
      (delete-directory directory t))))

(ert-deftest hub/standalone-google-docs-startup-runs-real-local-pipeline ()
  "The source-only launcher exercises the real semantic converter without Google."
  (let* ((directory (make-temp-file "standalone-google-docs-launcher-" t))
	 (wrapper (expand-file-name "emacs" directory))
	 (real-emacs (expand-file-name invocation-name invocation-directory))
	 (build (or (getenv "STRAIGHT_BUILD_PATH")
		    (expand-file-name "straight/build" hub/standalone-package-test--root)))
	 (upstream (or (getenv "GDOCS_PATH") (expand-file-name "gdocs" build)))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (unless (file-readable-p (expand-file-name "gdocs-convert.el" upstream))
	    (if (getenv "GDOCS_PATH")
		(ert-fail "Invalid explicit GDOCS_PATH")
	      (ert-skip "Provide cached semantic gdocs source via GDOCS_PATH")))
	  (with-temp-file wrapper
	    (insert "#!/usr/bin/env bash\n"
		    "exec \"$STANDALONE_TEST_EMACS\" --batch \"$@\" --eval "
		    "'(let* ((source (get-file-buffer \"draft.org\")) "
		    "(before (with-current-buffer source (buffer-string))) "
		    "(snapshot (with-current-buffer source (org-google-docs--debug-local-snapshot)))) "
		    "(unless (and org-google-docs-mode "
		    "(not (getenv \"ORG_GOOGLE_DOCS_LIVE_TESTS\")) "
		    "(not gdocs-auto-push-on-save) (not gdocs-auto-pull-on-open) "
		    "(not (locate-library \"hub-utils\")) "
		    "(consp (plist-get snapshot :local-ir)) "
		    "(not (plist-get (plist-get snapshot :local-ir) :error)) "
		    "(consp (plist-get snapshot :local-requests)) "
		    "(not (plist-get (plist-get snapshot :local-requests) :error))) "
		    "(error \"Offline Google Docs did not produce real IR and requests\")) "
		    "(unless (condition-case nil (progn "
		    "(ignore-errors (call-process \"sh\" nil nil nil \"-c\" \"exit 0\")) nil) "
		    "(org-google-docs-standalone-unexpected-io t)) "
		    "(error \"Only the local diff tool may execute\")) "
		    "(let ((trace (org-google-docs-demo-preview))) "
		    "(unless (with-current-buffer trace (and buffer-read-only "
		    "(string-match-p \"remote-fetch-requested: nil\" (buffer-string)))) "
		    "(error \"Local pipeline trace is missing\"))) "
		    "(let* ((expected (with-current-buffer source "
		    "(gdocs-convert-ir-to-org (gdocs-convert-org-buffer-to-ir)))) "
		    "(roundtrip (org-google-docs-demo-roundtrip))) "
		    "(unless (with-current-buffer roundtrip (and buffer-read-only "
		    "visual-line-mode word-wrap (not truncate-lines) "
		    "(equal expected (buffer-string)) "
		    "(string-search \"\\n\\n#+BEGIN_SRC\" (buffer-string)) "
		    "(string-match-p \"Bépo\" (buffer-string)) "
		    "(string-match-p \"First list item\" (buffer-string)))) "
		    "(error \"Local semantic round trip failed\"))) "
		    "(unless (equal before (with-current-buffer source (buffer-string))) "
		    "(error \"Local preview rewrote source\")) "
		    "(with-current-buffer source "
		    "(goto-char (point-max)) (insert \"\\n[fn::Unsupported inline note]\\n\") "
		    "(unless (condition-case nil (progn (org-google-docs-demo-preview) nil) "
		    "(user-error t)) (error \"Unsupported footnote did not fail closed\"))))'\n"))
	  (set-file-modes wrapper #o755)
	  (setenv "STANDALONE_TEST_EMACS" real-emacs)
	  (setenv "GDOCS_PATH" upstream)
	  (setenv "ORG_GOOGLE_DOCS_LIVE_TESTS" "1")
	  (setenv "PATH" (concat directory path-separator (getenv "PATH")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/org-google-docs-standalone"
					     hub/standalone-package-test--root)
			   nil (list t t) nil "--interactive")))
	      (ert-info ((buffer-string)) (should (zerop status))))))
      (delete-directory directory t))))

(provide 'standalone-package-test)
;;; standalone-package-test.el ends here
