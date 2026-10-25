;;; ai-activation-lifecycle-test.el --- AI activation lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Fresh Emacs processes load the real AI configuration, use-package/Straight
;; integration, Copilot mode, and context panels.  Only package installation and
;; optional AI providers are faked.  Credential, process, and network access is
;; forbidden while exercising the configuration.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst hub/ai-lifecycle-test--file (expand-file-name (or load-file-name buffer-file-name))
  "Absolute path to this test file, also loaded by isolated child processes.")

(defconst hub/ai-lifecycle-test--root
  (expand-file-name (locate-dominating-file hub/ai-lifecycle-test--file "domains.yaml"))
  "Repository containing the real AI configuration and local packages.")

(defvar hub/ai-lifecycle-test--adapter-calls 0
  "Number of explicit Copilot adapter configurations in the child process.")

(defvar hub/ai-lifecycle-test--external-calls 0
  "Number of forbidden external operations attempted in the child process.")

(defvar hub/ai-lifecycle-test--recipes nil
  "Package recipes recorded instead of installing packages in the child.")

(defun hub/ai-lifecycle-test--forbid-external (&rest _args)
  "Record and reject an unexpected credential, process, or network operation."
  (cl-incf hub/ai-lifecycle-test--external-calls)
  (error "AI lifecycle fixture attempted external access"))

(defun hub/ai-lifecycle-test--configure-adapter (&rest _args)
  "Record optional provider setup without requesting credentials or a model."
  (cl-incf hub/ai-lifecycle-test--adapter-calls)
  (setq org-copilot-review-function #'hub/ai-lifecycle-test--forbid-external
	org-copilot-chat-function #'hub/ai-lifecycle-test--forbid-external))

(defun hub/ai-lifecycle-test--write (root name contents)
  "Write fixture library NAME with CONTENTS beneath ROOT."
  (with-temp-file (expand-file-name name root)
    (insert ";;; Fixture -*- lexical-binding: t; -*-\n" contents)))

(defun hub/ai-lifecycle-test--check-registrations ()
  "Check that reloading leaves one activation hook and one review adapter advice."
  (should (= 1 (cl-count #'hub/org-copilot-configure-on-activation
			 org-copilot-mode-hook)))
  (let ((count 0))
    (advice-mapc
     (lambda (function _properties)
       (when (eq function #'hub/org-copilot-enable-gptel)
	 (cl-incf count)))
     'org-copilot-review-dwim)
    (should (= count 1)))
  (should-not (memq #'org-copilot-mode org-mode-hook))
  (should-not (memq 'org-copilot--window-selection-changed
		    window-selection-change-functions)))

(defun hub/ai-lifecycle-test--load-config ()
  "Load the actual personal AI module without replacing its package declarations."
  (load (expand-file-name "modules/interactive/tools/ai.el"
			  hub/ai-lifecycle-test--root)
	nil 'nomessage))

(defun hub/ai-lifecycle-test--exercise (order fault)
  "Exercise the lifecycle with load ORDER and optional negative-control FAULT.
ORDER is `org-first' or `gptel-first'.  FAULT can inject `auto-activation' or
`duplicate-setup' to demonstrate that lifecycle assertions detect regressions."
  (require 'use-package)
  (require 'straight)
  (require 'auth-source-pass)
  (require 'url)
  ;; The fixture needs Org core, not unrelated mail/IRC module initialization.
  (setq org-modules nil
	straight-use-package-by-default t)
  (straight-use-package-mode 1)
  (cl-letf (((symbol-function 'straight-use-package)
	     (lambda (recipe &rest _args)
	       (push recipe hub/ai-lifecycle-test--recipes)
	       t))
	    ((symbol-function 'auth-source-pass-get) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'auth-source-search) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'url-retrieve) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'url-retrieve-synchronously) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'make-network-process) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'open-network-stream) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'call-process) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'process-file) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'start-process) #'hub/ai-lifecycle-test--forbid-external)
	    ((symbol-function 'make-process) #'hub/ai-lifecycle-test--forbid-external))
    (require (if (eq order 'org-first) 'org 'gptel))
    (hub/ai-lifecycle-test--load-config)
    (hub/ai-lifecycle-test--load-config)
    (require 'org-copilot)
    (hub/ai-lifecycle-test--check-registrations)
    (should
     (cl-some (lambda (recipe)
		(eq (if (consp recipe) (car recipe) recipe) 'gptel))
	      hub/ai-lifecycle-test--recipes))
    (pcase fault
      ('auto-activation (add-hook 'org-mode-hook #'org-copilot-mode))
      ('duplicate-setup
       (add-hook 'org-copilot-mode-hook
		 (lambda ()
		   (when org-copilot-mode
		     (hub/org-copilot-enable-gptel))))))
    (princ "AI-LIFECYCLE:PASSIVE\n")
    (with-temp-buffer
      (insert "* Draft\nAlpha beta.\n")
      (org-mode)
      (should-not org-copilot-mode)
      (should-not (context-panels-registered-provider 'copilot))
      (should-not (featurep 'org-copilot-gptel))
      (should (zerop hub/ai-lifecycle-test--adapter-calls))
      (require 'gptel)
      (should-not org-copilot-mode)
      (should-not (featurep 'org-copilot-gptel))
      (should (zerop hub/ai-lifecycle-test--adapter-calls))
      (should (zerop hub/ai-lifecycle-test--external-calls))
      (princ "AI-LIFECYCLE:ENABLE\n")
      (org-copilot-mode 1)
      (should org-copilot-mode)
      (should (context-panels-registered-provider 'copilot))
      (should (= hub/ai-lifecycle-test--adapter-calls 1))
      (should (eq org-copilot-review-function #'hub/ai-lifecycle-test--forbid-external))
      (org-copilot-add-comment
       '(:id "fixture-comment" :source-start 1 :source-end 8 :status active))
      (org-copilot-refresh-overlays)
      (should org-copilot--overlays)
      (let ((overlays (copy-sequence org-copilot--overlays)))
	(hub/ai-lifecycle-test--load-config)
	(hub/ai-lifecycle-test--load-config)
	(hub/ai-lifecycle-test--check-registrations)
	(should org-copilot-mode)
	(should (context-panels-registered-provider 'copilot))
	(should (= hub/ai-lifecycle-test--adapter-calls 1))
	(should (cl-every #'overlay-buffer overlays))
	(org-copilot-mode -1)
	(should-not (context-panels-registered-provider 'copilot))
	(should-not org-copilot--overlays)
	(should (cl-every (lambda (overlay) (null (overlay-buffer overlay))) overlays)))
      (should (= hub/ai-lifecycle-test--adapter-calls 1))
      (org-copilot-mode 1)
      (should (context-panels-registered-provider 'copilot))
      (should org-copilot--overlays)
      (should (= hub/ai-lifecycle-test--adapter-calls 2))
      (org-copilot-mode -1)
      (should-not (context-panels-registered-provider 'copilot))
      (should-not org-copilot--overlays))
    (with-temp-buffer
      (org-mode)
      (should-not org-copilot-mode)
      (should-not (context-panels-registered-provider 'copilot)))
    (should (= hub/ai-lifecycle-test--adapter-calls 2))
    (should (zerop hub/ai-lifecycle-test--external-calls))
    (princ "AI-LIFECYCLE:PASS\n")))

(defun hub/ai-lifecycle-test--run (order &optional fault)
  "Run ORDER with optional FAULT in a fresh Emacs, returning status and output."
  (let* ((root (make-temp-file "ai-lifecycle-" t))
	 ;; A caller's commit-hook Git context does not belong to this fixture.
	 (process-environment
	  (cl-remove-if (lambda (entry) (string-prefix-p "GIT_" entry))
			process-environment))
	 (build (expand-file-name (or (getenv "STRAIGHT_BUILD_PATH") "straight/build")
				  hub/ai-lifecycle-test--root))
	 (context (let ((override (getenv "CONTEXT_PANELS_PATH")))
		    (if (and override (not (equal override "")))
			(expand-file-name override)
		      (expand-file-name "context-panels" build))))
	 (default-directory (file-name-as-directory root)))
    (unwind-protect
	(progn
	  (setenv "HOME" root)
	  (hub/ai-lifecycle-test--write
	   root "gptel.el"
	   (concat "(defvar gptel-api-key nil)\n(defvar gptel-backend nil)\n"
		   "(defvar gptel-model nil)\n"
		   "(dolist (command '(gptel gptel-send gptel-request))\n"
		   "  (defalias command #'hub/ai-lifecycle-test--forbid-external))\n"
		   "(provide 'gptel)\n"))
	  (hub/ai-lifecycle-test--write
	   root "gptel-agent.el"
	   "(defun gptel-agent-update () nil)\n(provide 'gptel-agent)\n")
	  (hub/ai-lifecycle-test--write
	   root "gptel-openai-oauth.el"
	   (concat "(defun gptel-make-openai-oauth (&rest _args) 'fixture-backend)\n"
		   "(provide 'gptel-openai-oauth)\n"))
	  (hub/ai-lifecycle-test--write
	   root "org-copilot-gptel.el"
	   (concat "(defalias 'org-copilot-gptel-use-openai-oauth\n"
		   "  #'hub/ai-lifecycle-test--configure-adapter)\n"
		   "(defalias 'org-copilot-gptel-enable\n"
		   "  #'hub/ai-lifecycle-test--configure-adapter)\n"
		   "(provide 'org-copilot-gptel)\n"))
	  (let ((paths (list root
			     (expand-file-name "use-package" build)
			     (expand-file-name "straight" build)
			     context
			     (expand-file-name "lisp" hub/ai-lifecycle-test--root))))
	    (dolist (package (directory-files
			      (expand-file-name "packages" hub/ai-lifecycle-test--root)
			      t "\\`[^.]"))
	      (when (file-directory-p package)
		(setq paths (append paths (list package)))))
	    ;; First -L wins: fake optional providers shadow their real libraries.
	    (with-temp-buffer
	      (let ((status
		     (apply
		      #'call-process
		      (expand-file-name invocation-name invocation-directory)
		      nil t nil
		      (append
		       '("--batch" "-Q")
		       (cl-mapcan (lambda (path) (list "-L" path)) paths)
		       (list "--eval" (format "(setq user-emacs-directory %S)"
					      (file-name-as-directory root))
			     "-l" hub/ai-lifecycle-test--file
			     "--eval" (prin1-to-string
				       `(condition-case err
					    (hub/ai-lifecycle-test--exercise ',order ',fault)
					  (error
					   (princ (format "AI-LIFECYCLE:FAIL %S\n" err))
					   (kill-emacs 1)))))))))
		(list status (buffer-string))))))
      (delete-directory root t))))

(ert-deftest hub/ai-lifecycle-org-before-gptel-stays-opt-in-across-reloads ()
  "Org-first loading and repeated reloads preserve explicit activation and cleanup."
  (pcase-let ((`(,status ,output) (hub/ai-lifecycle-test--run 'org-first)))
    (ert-info ((format "Isolated lifecycle output:\n%s" output))
	      (should (equal status 0))
	      (should (string-match-p "AI-LIFECYCLE:PASS\n" output)))))

(ert-deftest hub/ai-lifecycle-gptel-before-org-stays-opt-in-across-reloads ()
  "Gptel-first loading and repeated reloads preserve explicit activation and cleanup."
  (pcase-let ((`(,status ,output) (hub/ai-lifecycle-test--run 'gptel-first)))
    (ert-info ((format "Isolated lifecycle output:\n%s" output))
	      (should (equal status 0))
	      (should (string-match-p "AI-LIFECYCLE:PASS\n" output)))))

(ert-deftest hub/ai-lifecycle-checks-detect-stale-autoactivation ()
  "The passive-buffer assertions reject a deliberately injected activation hook."
  (pcase-let ((`(,status ,output)
	       (hub/ai-lifecycle-test--run 'org-first 'auto-activation)))
    (ert-info ((format "Negative-control output:\n%s" output))
	      (should-not (equal status 0))
	      (should (string-match-p "AI-LIFECYCLE:PASSIVE\n" output))
	      (should (string-match-p "ert-test-failed" output))
	      (should (string-match-p (regexp-quote "(should-not org-copilot-mode)") output)))))

(ert-deftest hub/ai-lifecycle-checks-detect-duplicate-adapter-setup ()
  "The activation assertions reject a deliberately injected duplicate callback."
  (pcase-let ((`(,status ,output)
	       (hub/ai-lifecycle-test--run 'org-first 'duplicate-setup)))
    (ert-info ((format "Negative-control output:\n%s" output))
	      (should-not (equal status 0))
	      (should (string-match-p "AI-LIFECYCLE:ENABLE\n" output))
	      (should (string-match-p "ert-test-failed" output))
	      (should (string-match-p (regexp-quote ":form (= 2 1)") output)))))

(provide 'ai-activation-lifecycle-test)
;;; ai-activation-lifecycle-test.el ends here
