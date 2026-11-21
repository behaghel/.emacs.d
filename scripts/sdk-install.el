;;; sdk-install.el --- Scoped cached SDK installation -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;;; Commentary:
;; Read-only planning by default.  Explicit apply runs Straight in an isolated
;; batch child, guards its build scope and verifies a fresh compiled library.
;; Never load personal init, clone/fetch, thaw a profile or reload live Emacs.
;; The supported cache wire format is deliberately narrow and fails closed.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'package)
(require 'subr-x)

(defconst hub/sdk-install--script (or load-file-name buffer-file-name)
  "Installer source used by isolated workers.")

;; These are bound before loading the existing Straight implementation.
(defvar straight-base-dir)
(defvar straight-check-for-modifications)
(defvar straight-initial-recipe-repositories)
(defvar straight-use-symlinks)
(defvar straight-disable-native-compile)
(defvar native-comp-jit-compilation)
(defvar print-symbols-bare)
(defvar straight--build-cache-version)
(defvar straight--build-cache)
(defvar straight--autoloads-cache)
(defvar straight--recipe-lookup-cache)
(declare-function straight--make-build-cache-available "straight" (&rest arguments))
(declare-function straight-register-package "straight" (recipe))
(declare-function straight-rebuild-package "straight" (package))
(declare-function straight--transaction-finalize "straight" ())

(defun hub/sdk-install--git (directory &rest arguments)
  "Run local Git ARGUMENTS in DIRECTORY without inherited index selection."
  (let ((default-directory directory)
        (process-environment (copy-sequence process-environment)))
    (dolist (name '("GIT_INDEX_FILE" "GIT_DIR" "GIT_WORK_TREE" "GIT_COMMON_DIR"))
      (setenv name nil))
    (setenv "GIT_OPTIONAL_LOCKS" "0")
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (user-error "Local Git check failed in %s: %s" directory (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun hub/sdk-install--clean (directory)
  "Require DIRECTORY to be a clean Git checkout, not merely inside one."
  (unless (and (file-directory-p directory)
               (file-equal-p directory
                             (hub/sdk-install--git directory "rev-parse" "--show-toplevel")))
    (user-error "Expected an existing Git checkout: %s" directory))
  (unless (string-empty-p (hub/sdk-install--git directory "status" "--porcelain"))
    (user-error "Checkout has existing changes; preserve them before installation: %s" directory)))

(defun hub/sdk-install--text (file)
  "Read FILE without evaluating its contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun hub/sdk-install--read-forms (file)
  "Read all data forms in FILE, never evaluate them."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (let (forms)
          (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
            (push (read (current-buffer)) forms))
          (nreverse forms)))
    (error (user-error "Unreadable SDK pin or cache data: %s" file))))

(defun hub/sdk-install--pins (config)
  "Read CONFIG's committed pins, rejecting hidden working-copy differences."
  (let* ((committed (hub/sdk-install--git config "show" "HEAD:straight/versions/default.el"))
         (file (expand-file-name "straight/versions/default.el" config)))
    (unless (equal committed (string-trim (hub/sdk-install--text file)))
      (user-error "Working pin file differs from committed installation intent"))
    (with-temp-buffer
      (insert committed)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun hub/sdk-install--child (directory relative)
  "Resolve existing RELATIVE path below DIRECTORY, rejecting escapes."
  (unless (and (stringp relative) (not (file-name-absolute-p relative))
               (file-in-directory-p (expand-file-name relative directory) directory))
    (user-error "Cached repository path escapes its managed directory"))
  (expand-file-name relative directory))

(defun hub/sdk-install--plan (config package)
  "Inspect clean CONFIG and cached PACKAGE without bootstrapping Straight."
  (setq config (file-name-as-directory (expand-file-name config)))
  (unless (and (stringp package) (string-match-p "\\`[[:alnum:]][[:alnum:]._-]*\\'" package))
    (user-error "Select a single cached package name"))
  (when (member package '("straight" "straight.el"))
    (user-error "The package manager cannot install itself through this command"))
  (hub/sdk-install--clean config)
  (let* ((pins (hub/sdk-install--pins config))
         (revision (cdr (assoc package pins)))
         (cache-file (expand-file-name "straight/build-cache.el" config))
         (text (hub/sdk-install--text cache-file))
         (forms (hub/sdk-install--read-forms cache-file))
         (cache (nth 2 forms)) (autoloads (nth 3 forms))
         (seen (make-hash-table :test 'equal)) dependencies)
    (unless (and (stringp revision) (string-match-p "\\`[0-9a-f]\\{40\\}\\'" revision))
      (user-error "Package has no committed full revision pin: %s" package))
    (unless (and (= (length forms) 7) (eq (nth 0 forms) :tanat)
                 (equal (nth 1 forms) emacs-version)
                 (hash-table-p cache) (eq (hash-table-test cache) 'equal)
                 (hash-table-p autoloads) (eq (hash-table-test autoloads) 'equal)
                 (hash-table-p (nth 4 forms)) (listp (nth 5 forms))
                 (cl-every #'stringp (nth 5 forms)) (eq (nth 6 forms) t))
      (user-error "Incompatible Straight cache schema, Emacs version or build layout"))
    (cl-labels
        ((visit (name)
           (unless (or (gethash name seen)
                       (and (not (gethash name cache))
                            (package-built-in-p (intern name))))
             (puthash name t seen)
             (let* ((entry (gethash name cache)) (recipe (nth 2 entry))
                    (local (plist-get recipe :local-repo))
                    (builtin (eq (plist-get recipe :type) 'built-in)))
               (unless (and (= (length entry) 3) (equal (plist-get recipe :package) name)
                            (listp (nth 1 entry)))
                 (user-error "Missing compatible cached recipe/dependencies: %s" name))
               (unless builtin
                 (let ((repo (hub/sdk-install--child (expand-file-name "straight/repos/" config) local)))
                   (hub/sdk-install--clean repo)
                   (unless (file-directory-p (expand-file-name (concat "straight/build/" name) config))
                     (user-error "Existing dependency build is unavailable: %s" name))
                   (when (and (not (equal name package)) (not (gethash name autoloads)))
                     (user-error "Existing dependency autoload cache is unavailable: %s" name))
                   (when (and (not (equal name package)) (assoc name pins)
                              (not (equal (hub/sdk-install--git repo "rev-parse" "HEAD") (cdr (assoc name pins)))))
                     (user-error "Cached dependency checkout differs from its pin: %s" name))))
               (dolist (dependency (nth 1 entry)) (visit dependency))
               (push name dependencies)))))
      (visit package))
    (let* ((recipe (nth 2 (gethash package cache)))
           (repo (hub/sdk-install--child (expand-file-name "straight/repos/" config)
                                         (plist-get recipe :local-repo))))
      (maphash
       (lambda (name entry)
         (when (and (not (equal name package))
                    (equal (plist-get (nth 2 entry) :local-repo) (plist-get recipe :local-repo)))
           (user-error "Selected repository is shared by another package: %s" name))) cache)
      (unless (equal revision (hub/sdk-install--git repo "rev-parse" "--verify" (concat revision "^{commit}")))
        (user-error "Pinned commit must already be available locally"))
      (list :config config :package package :revision revision :repo repo
            :build (expand-file-name (concat "straight/build/" package) config)
            :forms forms :cache-file cache-file :cache-text text
            :dependencies (nreverse dependencies)))))

(defun hub/sdk-install--fingerprints (table except)
  "Snapshot TABLE entries other than EXCEPT, independent of later mutation."
  (let ((result (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value)
               (unless (equal key except) (puthash key (prin1-to-string value) result))) table)
    result))

(defun hub/sdk-install--assert-cache (before after package)
  "Require BEFORE fingerprints to match AFTER outside selected PACKAGE."
  (let ((current (hub/sdk-install--fingerprints after package)))
    (unless (and (= (hash-table-count before) (hash-table-count current))
                 (cl-loop for key being the hash-keys of before
                          always (equal (gethash key before) (gethash key current))))
      (user-error "Refusing to publish unrelated package cache changes"))))

(defun hub/sdk-install--register (cache name)
  "Register NAME's already normalized recipe from CACHE without activation."
  (let ((recipe (copy-sequence (nth 2 (gethash name cache)))) properties)
    (while recipe
      (let ((key (pop recipe)) (value (pop recipe)))
        (unless (eq key :package) (setq properties (append properties (list key value))))))
    (straight-register-package (cons (intern name) properties))))

(defun hub/sdk-install--verify-library (plan)
  "Load PLAN's selected compiled library in a fresh Emacs and return its path."
  (let* ((package (plist-get plan :package))
         (library (expand-file-name (concat package ".elc") (plist-get plan :build)))
         (directories (mapcar (lambda (name)
                                (expand-file-name (concat "straight/build/" name) (plist-get plan :config)))
                              (plist-get plan :dependencies)))
         (expression `(progn
                        (setq load-path (append ',directories load-path))
                        (cl-letf (((symbol-function 'make-network-process)
                                   (lambda (&rest _) (error "Network disabled during SDK verification"))))
                          (require ',(intern package))
                          (unless (equal (locate-library ,package) ,library)
                            (error "Installed library resolved outside the selected compiled build"))))))
    (unless (file-regular-p library) (user-error "Selected compiled library is missing: %s" library))
    (with-temp-buffer
      (unless (zerop (call-process (expand-file-name invocation-name invocation-directory)
                                   nil t nil "--batch" "-Q" "--eval" "(progn (require 'cl-lib) (setq native-comp-jit-compilation nil))"
                                   "--eval" (prin1-to-string expression)))
        (princ (buffer-string) #'external-debugging-output)
        (user-error "Fresh installed-library verification failed")))
    library))

(defun hub/sdk-install--worker (config package expected result)
  "Apply the reviewed CONFIG/PACKAGE/EXPECTED plan and write private RESULT.
This function runs only in a fresh batch child, never the caller's live Emacs."
  (let* ((plan (hub/sdk-install--plan config package))
         (lock (expand-file-name "straight/sdk-install.lock" config))
         (forms (plist-get plan :forms))
         (before-build (hub/sdk-install--fingerprints (nth 2 forms) package))
         (before-autoloads (hub/sdk-install--fingerprints (nth 3 forms) package))
         (before-lookup (hub/sdk-install--fingerprints (nth 4 forms) nil))
         (user-emacs-directory (plist-get plan :config))
         (straight-base-dir (plist-get plan :config))
         (straight-check-for-modifications nil)
         (straight-initial-recipe-repositories nil)
         (straight-use-symlinks t)
         (straight-disable-native-compile t)
         (native-comp-jit-compilation nil)
         library)
    (unless (equal expected (plist-get plan :revision))
      (user-error "Expected revision differs from the committed pin; review a new dry-run"))
    (when (file-exists-p lock) (user-error "Another SDK installation lock exists; do not remove it implicitly"))
    (make-directory lock)
    (unwind-protect
        (progn
          ;; Load existing manager definitions, not bootstrap.el or personal init.
          (load (expand-file-name "straight/repos/straight.el/straight.el" config) nil 'nomessage)
          (unless (eq straight--build-cache-version :tanat)
            (user-error "Installed Straight implementation uses an unsupported cache schema"))
          (let ((build (symbol-function 'straight--build-package)))
            (cl-letf (((symbol-function 'straight--build-package)
                       (lambda (recipe &optional cause)
                         (unless (equal (plist-get recipe :package) package)
                           (user-error "Refusing unrelated package rebuild: %s" (plist-get recipe :package)))
                         (funcall build recipe cause)))
                      ((symbol-function 'straight--save-build-cache) #'ignore)
                      ((symbol-function 'straight-vc-clone)
                       (lambda (&rest _) (user-error "Cloning is disabled during scoped installation")))
                      ((symbol-function 'straight-vc-fetch)
                       (lambda (&rest _) (user-error "Fetching is disabled during scoped installation")))
                      ((symbol-function 'make-network-process)
                       (lambda (&rest _) (user-error "Network is disabled during scoped installation")))
                      ((symbol-function 'native-compile-async)
                       (lambda (&rest _) (user-error "Asynchronous native compilation is disabled during scoped installation"))))
              (straight--make-build-cache-available :nosave t)
              (hub/sdk-install--assert-cache
               (hub/sdk-install--fingerprints (nth 2 forms) nil) straight--build-cache nil)
              (dolist (name (plist-get plan :dependencies))
                (hub/sdk-install--register straight--build-cache name))
              (hub/sdk-install--git (plist-get plan :repo) "checkout" "--detach" expected)
              (straight-rebuild-package package)
              (straight--transaction-finalize))
            (setq library (hub/sdk-install--verify-library plan))
            (hub/sdk-install--assert-cache before-build straight--build-cache package)
            (hub/sdk-install--assert-cache before-autoloads straight--autoloads-cache package)
            (hub/sdk-install--assert-cache before-lookup straight--recipe-lookup-cache nil)
            (unless (equal (plist-get plan :cache-text)
                           (hub/sdk-install--text (plist-get plan :cache-file)))
              (user-error "Build cache changed concurrently; refusing to overwrite it"))
            ;; Publish only the two selected-package cache slots.  Straight's
            ;; general writer derives eager checks from this isolated process's
            ;; profile, changing unrelated metadata even without other rebuilds.
            (let* ((file (plist-get plan :cache-file))
                   (temporary (make-temp-file (concat file ".sdk-install-")))
                   (print-level nil) (print-length nil) (print-symbols-bare t))
              (unwind-protect
                  (progn
                    (setf (nth 2 forms) straight--build-cache
                          (nth 3 forms) straight--autoloads-cache)
                    (with-temp-file temporary
                      (dolist (form forms) (prin1 form (current-buffer)) (insert "\n")))
                    (set-file-modes temporary (file-modes file))
                    (unless (equal (plist-get plan :cache-text) (hub/sdk-install--text file))
                      (user-error "Build cache changed concurrently; refusing to overwrite it"))
                    (rename-file temporary file t))
                (when (file-exists-p temporary) (delete-file temporary)))))
          (with-temp-file result
            (prin1 (list :mode "installed" :package package :revision expected
                         :library library :unrelated-cache-changes 0 :restart-required t)
                   (current-buffer))))
      (delete-directory lock))))

(defun hub/sdk-install (config package &optional apply expected)
  "Plan one pinned PACKAGE in CONFIG; APPLY requires matching EXPECTED revision.
Dry-run never bootstraps Straight.  Apply builds in a fresh worker and verifies
in another fresh Emacs.  A post-checkout failure leaves selected source/build
state for explicit inspection; it never rolls back or reports installation."
  (let ((plan (hub/sdk-install--plan config package)))
    (if (not apply)
        (list :mode "dry-run" :package package :revision (plist-get plan :revision)
              :current-revision (hub/sdk-install--git (plist-get plan :repo) "rev-parse" "HEAD")
              :dependencies (plist-get plan :dependencies))
      (unless (equal expected (plist-get plan :revision))
        (user-error "Apply requires --expect matching the reviewed committed pin"))
      (unless noninteractive (user-error "Run SDK installation in batch Emacs"))
      (let* ((directory (make-temp-file "sdk-install-worker-" t))
             (result (expand-file-name "result.el" directory))
             (expression `(condition-case err
                              (hub/sdk-install--worker ,config ,package ,expected ,result)
                            (error (princ (concat (error-message-string err) "\n")
                                          #'external-debugging-output)
                                   (kill-emacs 1)))))
        (unwind-protect
            (with-temp-buffer
              (unless (zerop (call-process (expand-file-name invocation-name invocation-directory)
                                           nil t nil "--batch" "-Q" "-l" hub/sdk-install--script
                                           "--eval" (prin1-to-string expression)))
                (princ (buffer-string) #'external-debugging-output)
                (user-error "Scoped SDK install failed; selected source/build may have changed; inspect before retrying"))
              (car (hub/sdk-install--read-forms result)))
          (delete-directory directory t))))))

(defun hub/sdk-install-main ()
  "Parse command-line arguments for the dry-run-first SDK installer."
  (let ((arguments (if (equal (car command-line-args-left) "--")
                       (cdr command-line-args-left) command-line-args-left))
        config package apply expected help)
    (setq command-line-args-left nil)
    (condition-case err
        (progn
          (while arguments
            (pcase (pop arguments)
              ("--config" (setq config (pop arguments)))
              ("--package" (setq package (pop arguments)))
              ("--apply" (setq apply t))
              ("--expect" (setq expected (pop arguments)))
              ("--help" (setq help t))
              (_ (user-error "Unknown SDK installer argument"))))
          (if help
              (princ "Usage: sdk-install --config DIR --package NAME [--apply --expect REVISION]\nDefault: read-only plan. Existing local commits/builds only; restart remains manual.\n")
            (unless (and config package) (user-error "Specify --config DIR and --package NAME"))
            (princ (json-encode (hub/sdk-install config package apply expected)))
            (princ "\n")))
      (error (princ (format "sdk-install: %s\n" (error-message-string err)) #'external-debugging-output)
             (kill-emacs 1)))))

(provide 'sdk-install)
;;; sdk-install.el ends here
