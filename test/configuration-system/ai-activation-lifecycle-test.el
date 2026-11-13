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

(defvar hub/ai-lifecycle-test--adapter-route nil
  "Authentication route selected by the fake Copilot adapter.")

(defvar hub/ai-lifecycle-test--external-calls 0
  "Number of forbidden external operations attempted in the child process.")

(defvar hub/ai-lifecycle-test--models nil
  "Model catalogue declared by the fixture subscription backend.")

(defvar hub/ai-lifecycle-test--recipes nil
  "Package recipes recorded instead of installing packages in the child.")

(defun hub/ai-lifecycle-test--forbid-external (&rest _args)
  "Record and reject an unexpected credential, process, or network operation."
  (cl-incf hub/ai-lifecycle-test--external-calls)
  (error "AI lifecycle fixture attempted external access"))

(defun hub/ai-lifecycle-test--configure-adapter (route &rest _args)
  "Record optional provider setup for ROUTE without credentials or a model."
  (cl-incf hub/ai-lifecycle-test--adapter-calls)
  (setq hub/ai-lifecycle-test--adapter-route route)
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

(defun hub/ai-lifecycle-test--recover-unavailable-adapter ()
  "Check failed activation, offline UI, and late availability in this Org buffer.
Leave the mode disabled so the caller's next explicit enable retries setup."
  (let ((original-require (symbol-function 'require))
	messages)
    (princ "AI-LIFECYCLE:UNAVAILABLE\n")
    (cl-letf (((symbol-function 'require)
	       (lambda (feature &rest args)
		 (if (eq feature 'org-copilot-gptel)
		     (unless (nth 1 args)
		       (signal 'file-missing '("Fixture adapter unavailable")))
		   (apply original-require feature args))))
	      ((symbol-function 'message)
	       (lambda (format-string &rest args)
		 (when format-string
		   (let ((text (apply #'format-message format-string args)))
		     (push text messages)
		     text)))))
      ;; Missing optional code must not break reloads or passive Org editing.
      (hub/ai-lifecycle-test--load-config)
      (hub/ai-lifecycle-test--load-config)
      (should-not org-copilot-mode)
      (should (zerop hub/ai-lifecycle-test--adapter-calls))
      (org-copilot-mode 1))
    (should (cl-some (lambda (text) (string-match-p "unavailable" text)) messages))
    (should (cl-some (lambda (text) (string-match-p "re-enable org-copilot-mode" text))
		     messages)))
  (should org-copilot-mode)
  (should (context-panels-registered-provider 'copilot))
  (should (zerop hub/ai-lifecycle-test--adapter-calls))
  (should-not hub/ai-lifecycle-test--adapter-route)
  (should-not org-copilot-review-function)
  (should-not org-copilot-chat-function)
  ;; Offline source comments remain usable even without an AI adapter.
  (org-copilot-add-comment
   '(:id "fixture-comment" :source-start 1 :source-end 8 :status active))
  (org-copilot-refresh-overlays)
  (should org-copilot--overlays)
  ;; Availability alone must neither configure the adapter nor activate buffers.
  (require 'org-copilot-gptel)
  (should (zerop hub/ai-lifecycle-test--adapter-calls))
  (should-not org-copilot-review-function)
  (with-temp-buffer
    (org-mode)
    (should-not org-copilot-mode))
  (should (zerop hub/ai-lifecycle-test--external-calls))
  (org-copilot-mode -1)
  (should-not org-copilot--overlays)
  (should-not (context-panels-registered-provider 'copilot))
  (princ "AI-LIFECYCLE:RETRY\n"))

(defun hub/ai-lifecycle-test--exercise-babel (fault)
  "Exercise the real Babel path with optional subscription setup FAULT."
  (require 'use-package)
  (require 'straight)
  (require 'auth-source-pass)
  (require 'url)
  (setq org-modules nil straight-use-package-by-default t)
  (straight-use-package-mode 1)
  (cl-letf (((symbol-function 'straight-use-package) (lambda (&rest _) t))
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
    (hub/ai-lifecycle-test--load-config)
    (require 'ob-copilot)
    (require 'gptel-openai-oauth)
    (hub/ai-lifecycle-test--load-config)
    (should (eq hub/gptel-openai-oauth-model 'gpt-6-luna))
    (let ((count 0))
      (advice-mapc
       (lambda (function _properties)
	 (when (eq function #'hub/org-copilot-babel-with-subscription)
	   (cl-incf count)))
       'org-copilot-babel-gptel-generate)
      (should (= count 1)))
    (setq hub/gptel-use-openai-oauth t
	  hub/gptel-openai-oauth-model (if (eq fault 'unlisted-model)
					   'fixture-unlisted-model 'fixture-supported-model)
	  gptel--known-backends (when (eq fault 'registry-api)
				  '(("OpenAI-sub" . fixture-api)))
	  gptel-backend (when (eq fault 'stale-api) 'fixture-api)
	  gptel-model nil
	  org-copilot-gptel-backend (when (eq fault 'stale-api) 'fixture-api)
	  org-copilot-gptel-model nil)
    (when (eq fault 'outdated-adapter)
      (fmakunbound 'gptel--openai-oauth-default-backend))
    (let ((original-constructor (symbol-function 'gptel-make-openai-oauth))
	  (calls 0) (constructions 0) observed-backend observed-model)
      (cl-letf (((symbol-function 'gptel-make-openai-oauth)
		 (lambda (&rest args)
		   (cl-incf constructions)
		   (pcase fault
		     ('unavailable (error "Fixture OAuth unavailable"))
		     ('invalid-backend nil)
		     (_ (apply original-constructor args)))))
		((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (cl-incf calls)
		   (should-not gptel-api-key)
		   ;; Emulate gptel's provider/model fallback without any I/O.
		   (setq observed-backend (or gptel-backend 'fixture-api)
			 observed-model (if (memq gptel-model hub/ai-lifecycle-test--models)
					    gptel-model 'fixture-fallback-model))
		   (let ((callback (plist-get args :callback)))
		     (funcall callback "{\"output\":\"Generated via subscription.\"}" nil)
		     (when (plist-get args :stream) (funcall callback t nil))))))
	(with-temp-buffer
	  (org-mode)
	  (insert "#+begin_src copilot\nWrite a summary.\n#+end_src\n\n#+RESULTS:\nOld reviewed result.\n")
	  (goto-char (point-min))
	  (let ((before (buffer-string)) (org-confirm-babel-evaluate nil))
	    (if (memq fault '(unavailable invalid-backend unlisted-model outdated-adapter registry-api))
		(progn
		  (should-error (org-babel-execute-src-block) :type 'user-error)
		  (should (zerop calls))
		  (should (equal (buffer-string) before)))
	      (org-babel-execute-src-block)
	      (should (= calls 1))
	      (should (eq observed-backend 'fixture-backend))
	      (should (eq observed-model 'fixture-supported-model))
	      (should (string-match-p "Generated via subscription" (buffer-string)))
	      ;; Repeating generation retains the already configured backend.
	      (org-babel-execute-src-block)
	      (should (= calls 2))
	      (should (= constructions 1)))
	    (should-not org-copilot-mode)))))
    (should (zerop hub/ai-lifecycle-test--external-calls))
    (princ "AI-BABEL:PASS\n")))

(defun hub/ai-lifecycle-test--exercise (order fault &optional recovery-route)
  "Exercise the lifecycle with load ORDER and optional negative-control FAULT.
ORDER is `org-first' or `gptel-first'.  FAULT can inject `auto-activation' or
`duplicate-setup' to demonstrate that lifecycle assertions detect regressions.
RECOVERY-ROUTE, when non-nil, is `oauth' or `api-key' and adds an unavailable
adapter followed by late availability and explicit activation retry."
  (require 'use-package)
  (require 'straight)
  (require 'auth-source-pass)
  (require 'url)
  ;; The fixture needs Org core, not unrelated mail/IRC module initialization.
  (setq org-modules nil
	straight-use-package-by-default t)
  (straight-use-package-mode 1)
  (when recovery-route
    (setq hub/gptel-use-openai-oauth (eq recovery-route 'oauth)))
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
      (when recovery-route
	(hub/ai-lifecycle-test--recover-unavailable-adapter))
      (princ "AI-LIFECYCLE:ENABLE\n")
      (org-copilot-mode 1)
      (should org-copilot-mode)
      (should (context-panels-registered-provider 'copilot))
      (should (= hub/ai-lifecycle-test--adapter-calls 1))
      (when recovery-route
	(should (eq hub/ai-lifecycle-test--adapter-route recovery-route)))
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

(defun hub/ai-lifecycle-test--run (order &optional fault recovery-route)
  "Run ORDER with FAULT and RECOVERY-ROUTE in a fresh Emacs.
Return the exit status and output; see `hub/ai-lifecycle-test--exercise'."
  (let* ((root (make-temp-file "ai-lifecycle-" t))
	 ;; A caller's commit-hook Git context does not belong to this fixture.
	 (process-environment
	  (cl-remove-if (lambda (entry) (string-prefix-p "GIT_" entry))
			(copy-sequence process-environment)))
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
		   "(defvar gptel-model nil)\n(defvar gptel--known-backends nil)\n"
		   "(dolist (command '(gptel gptel-send gptel-request))\n"
		   "  (defalias command #'hub/ai-lifecycle-test--forbid-external))\n"
		   "(provide 'gptel)\n"))
	  (hub/ai-lifecycle-test--write
	   root "gptel-agent.el"
	   "(defun gptel-agent-update () nil)\n(provide 'gptel-agent)\n")
	  (hub/ai-lifecycle-test--write
	   root "gptel-openai-oauth.el"
	   (concat "(defun gptel-make-openai-oauth (_name &rest args)\n"
		   " (setq hub/ai-lifecycle-test--models (or (plist-get args :models) '(fixture-supported-model fixture-luna-model)))\n"
		   " (setf (alist-get _name gptel--known-backends nil nil #'equal) 'fixture-backend)\n"
		   " 'fixture-backend)\n"
		   "(defun gptel-backend-models (_backend) hub/ai-lifecycle-test--models)\n"
		   "(defun gptel-openai-oauth-p (backend) (eq backend 'fixture-backend))\n"
		   "(defun gptel--openai-oauth-default-backend () 'fixture-backend)\n"
		   "(provide 'gptel-openai-oauth)\n"))
	  (hub/ai-lifecycle-test--write
	   root "org-copilot-gptel.el"
	   (concat "(defun org-copilot-gptel-use-openai-oauth (&optional _model)\n"
		   "  (hub/ai-lifecycle-test--configure-adapter 'oauth))\n"
		   "(defun org-copilot-gptel-enable ()\n"
		   "  (hub/ai-lifecycle-test--configure-adapter 'api-key))\n"
		   "(provide 'org-copilot-gptel)\n"))
	  (when (eq order 'babel)
	    (delete-file (expand-file-name "org-copilot-gptel.el" root)))
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
					    (if (eq ',order 'babel)
						(hub/ai-lifecycle-test--exercise-babel ',fault)
					      (hub/ai-lifecycle-test--exercise ',order ',fault ',recovery-route))
					  (error
					   (princ (format "AI-LIFECYCLE:FAIL %S\n" err))
					   (kill-emacs 1)))))))))
		(list status (buffer-string))))))
      (delete-directory root t))))

(ert-deftest hub/ai-lifecycle-fixture-preserves-caller-environment ()
  "Filtering commit-hook variables must not share the caller's HOME cons cell."
  (let* ((process-environment
	  (cl-loop for entry in process-environment
		   unless (string-prefix-p "GIT_" entry) collect entry))
	 (before (copy-sequence process-environment))
	 (home (expand-file-name "~/"))
	 (abbreviation (abbreviate-file-name hub/ai-lifecycle-test--root)))
    (pcase-let ((`(,status ,output) (hub/ai-lifecycle-test--run 'org-first)))
      (ert-info ((format "Isolated lifecycle output:\n%s" output))
		(should (equal status 0))
		;; Report only a boolean on failure, never dump environment values.
		(let ((unchanged (equal process-environment before)))
		  (should unchanged))
		(should (equal (expand-file-name "~/") home))
		(should (equal (abbreviate-file-name hub/ai-lifecycle-test--root)
			       abbreviation))))))

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

(ert-deftest hub/ai-lifecycle-oauth-missing-adapter-recovers-on-explicit-retry ()
  "OAuth unavailability is diagnosed; late availability needs an explicit retry."
  (pcase-let ((`(,status ,output) (hub/ai-lifecycle-test--run 'org-first nil 'oauth)))
    (ert-info ((format "OAuth recovery output:\n%s" output))
	      (should (equal status 0))
	      (should (string-match-p "AI-LIFECYCLE:RETRY\n" output))
	      (should (string-match-p "AI-LIFECYCLE:PASS\n" output)))))

(ert-deftest hub/ai-lifecycle-api-key-missing-adapter-recovers-on-explicit-retry ()
  "API-key unavailability is diagnosed and retry preserves the selected route."
  (pcase-let ((`(,status ,output) (hub/ai-lifecycle-test--run 'gptel-first nil 'api-key)))
    (ert-info ((format "API-key recovery output:\n%s" output))
	      (should (equal status 0))
	      (should (string-match-p "AI-LIFECYCLE:RETRY\n" output))
	      (should (string-match-p "AI-LIFECYCLE:PASS\n" output)))))

(ert-deftest hub/ai-babel-subscription-route-survives-cold-start-and-stale-api ()
  "Babel configures subscription OAuth without mode activation or model fallback."
  (dolist (fault '(nil stale-api))
    (pcase-let ((`(,status ,output) (hub/ai-lifecycle-test--run 'babel fault)))
      (ert-info ((format "Babel route fixture:\n%s" output))
		(should (equal status 0))
		(should (string-match-p "AI-BABEL:PASS" output))))))

(ert-deftest hub/ai-babel-subscription-failure-never-requests-or-changes-results ()
  "Missing or invalid OAuth setup cannot fall back to API billing."
  (dolist (fault '(unavailable invalid-backend unlisted-model outdated-adapter registry-api))
    (pcase-let ((`(,status ,output) (hub/ai-lifecycle-test--run 'babel fault)))
      (ert-info ((format "Babel setup failure fixture:\n%s" output))
		(should (equal status 0))
		(should (string-match-p "AI-BABEL:PASS" output))))))

(ert-deftest hub/ai-real-oauth-login-selector-and-backend-reuse ()
  "Real dependency selection returns an object and retains authenticated state."
  (let* ((build (expand-file-name (or (getenv "STRAIGHT_BUILD_PATH") "straight/build")
				  hub/ai-lifecycle-test--root))
	 (source (or (getenv "GPTEL_PATH") (expand-file-name "gptel" build)))
	 (home (make-temp-file "ai-real-oauth-" t))
	 (default-directory (file-name-as-directory home))
	 (process-environment (copy-sequence process-environment))
	 (paths (cons source (cl-remove-if-not #'file-directory-p
					       (directory-files build t "\\`[^.]")))))
    (unwind-protect
	(progn
	  (setenv "HOME" home)
	  (with-temp-buffer
	    (let* ((program
		    `(progn
		       (require 'cl-lib)
		       (require 'gptel-openai-oauth)
		       (let* ((gptel-backend nil)
			      (gptel--known-backends nil)
			      (backend (gptel-make-openai-oauth "OpenAI-sub")))
			 ;; Evaluate only interactive argument selection, never login.
			 (unless (eq (car (eval (cadr (interactive-form
						       #'gptel-openai-oauth-login)) t)) backend)
			   (error "OAuth login selector returned an entry instead of a backend"))
			 (with-temp-buffer
			   (insert-file-contents
			    ,(expand-file-name "modules/interactive/tools/ai.el"
					       hub/ai-lifecycle-test--root))
			   (goto-char (point-min))
			   (condition-case nil
			       (while t
				 (let ((form (read (current-buffer))))
				   (when (or (and (eq (car-safe form) 'defun)
						  (memq (cadr form)
							'(hub/gptel-subscription-backend
							  hub/gptel-secure-oauth-cache
							  hub/gptel-write-private-oauth-token)))
					     (and (eq (car-safe form) 'with-eval-after-load)
						  (equal (cadr form) '(quote gptel-oauth))))
				     (eval form t))))
			     (end-of-file nil)))
			 (setf (gptel-openai-oauth-token backend) 'fixture-state)
			 (unless (and (eq (hub/gptel-subscription-backend) backend)
				      (eq (gptel-openai-oauth-token backend) 'fixture-state))
			   (error "Subscription setup replaced existing authenticated state"))
			 ;; Exercise only synthetic cache data under the temp HOME.
			 (unless (file-in-directory-p gptel--openai-oauth-token-file
						      user-emacs-directory)
			   (error "Fixture cache escaped its temporary directory"))
			 (let ((fixture (list :access_token "fixture-only"
					      :refresh_token "fixture-only"
					      :expires_at (+ (float-time) 3600))))
			   (gptel-oauth--write-token gptel--openai-oauth-token-file fixture)
			   (unless (= (file-modes gptel--openai-oauth-token-file) #o600)
			     (error "New OAuth cache was not private"))
			   (let ((modes (default-file-modes)))
			     (condition-case nil
				 (hub/gptel-write-private-oauth-token
				  (lambda (&rest _) (error "Fixture write failed"))
				  gptel--openai-oauth-token-file fixture)
			       (error nil))
			     (unless (= (default-file-modes) modes)
			       (error "Failed OAuth write changed process file creation modes")))
			   (set-file-modes gptel--openai-oauth-token-file #o644)
			   (gptel-oauth--write-token gptel--openai-oauth-token-file fixture)
			   (unless (= (file-modes gptel--openai-oauth-token-file) #o600)
			     (error "OAuth cache write left credentials readable by others"))
			   (set-file-modes gptel--openai-oauth-token-file #o644)
			   (hub/gptel-subscription-backend)
			   (unless (= (file-modes gptel--openai-oauth-token-file) #o600)
			     (error "Existing OAuth cache was not secured"))
			   (setf (gptel-openai-oauth-token backend) nil)
			   (cl-letf (((symbol-function 'gptel-openai-oauth-login)
				      (lambda (&rest _) (error "Unexpected reauthentication")))
				     ((symbol-function 'gptel--openai-oauth-refresh)
				      (lambda (&rest _) (error "Unexpected network refresh"))))
			     (gptel--openai-oauth-ensure backend)
			     (unless (equal (gptel-openai-oauth-token backend) fixture)
			       (error "Cached subscription login was not restored"))))
			 ;; Build the request locally: no headers, auth, or HTTP.
			 (let* ((gptel-backend backend)
				(gptel-model 'gpt-6-luna)
				(gptel-stream t)
				(payload (gptel--request-data backend nil)))
			   (unless (and (equal (plist-get payload :model) "gpt-6-luna")
					(eq (plist-get payload :stream) t)
					(eq (plist-get payload :store) :json-false))
			     (error "Subscription request changed model or streaming semantics"))))
		       (message "REAL-OAUTH:PASS")))
		   (status
		    (apply #'call-process
			   (expand-file-name invocation-name invocation-directory)
			   nil t nil
			   (append '("--batch" "-Q")
				   (cl-mapcan (lambda (path) (list "-L" path)) paths)
				   (list "--eval" (format "(setq user-emacs-directory %S)" home)
					 "--eval" (prin1-to-string program))))))
	      (ert-info ((buffer-string))
			(should (equal status 0))
			(should (string-match-p "REAL-OAUTH:PASS" (buffer-string)))))))
      (delete-directory home t))))

(provide 'ai-activation-lifecycle-test)
;;; ai-activation-lifecycle-test.el ends here
