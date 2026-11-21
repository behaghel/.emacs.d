;;; package-install-test.el --- Scoped installation fixtures -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;;; Commentary:
;; Only disposable Git repositories and public Straight source are used.  The
;; installed configuration, dependency repositories and running Emacs are never
;; installation targets.  Fixture commits use an isolated Git configuration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require 'json)

(defconst hub/package-install-test--root (expand-file-name default-directory)
  "Checkout containing the installer under test.")
(load (expand-file-name "scripts/package-install.el" hub/package-install-test--root) t t)

(defun hub/package-install-test--git (directory &rest arguments)
  "Run local Git ARGUMENTS in fixture DIRECTORY and return stdout."
  (let ((default-directory directory))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (ert-fail (buffer-string)))
      (string-trim (buffer-string)))))

(defun hub/package-install-test--commit (directory)
  "Commit fixture DIRECTORY with its isolated synthetic identity."
  (hub/package-install-test--git directory "add" "-A")
  (hub/package-install-test--git directory "-c" "user.name=Package fixture"
                                 "-c" "user.email=package@example.invalid" "commit" "-qm" "Fixture")
  (hub/package-install-test--git directory "rev-parse" "HEAD"))

(defun hub/package-install-test--write-cache (config &optional version format)
  "Write a minimal real Straight cache for CONFIG, VERSION and FORMAT."
  (let ((build (make-hash-table :test 'equal))
        (autoloads (make-hash-table :test 'equal)))
    (dolist (name '("gdocs" "helperdep"))
      (puthash name (list "2000-01-01 00:00:00"
                          (when (equal name "gdocs") '("helperdep"))
                          (list :package name :type 'git :local-repo name
                                :repo (concat "example.invalid/" name) :files '("*.el"))) build))
    (dolist (name '("gdocs" "helperdep"))
      (puthash name (cons (list (intern name)) nil) autoloads))
    (with-temp-file (expand-file-name "straight/build-cache.el" config)
      (dolist (form (list (or format :tanat) (or version emacs-version) build
                          autoloads (make-hash-table :test 'eq)
                          '("gdocs" "helperdep") t))
        (prin1 form (current-buffer)) (insert "\n")))))

(defun hub/package-install-test--artifact-state (directory)
  "Snapshot DIRECTORY entries, targets and bytes without read access timestamps."
  (mapcar
   (lambda (file)
     (let ((link-attributes (file-attributes file))
           (target-attributes (file-attributes (file-truename file))))
       ;; Linux may update atime simply while resolving a symlink or reading
       ;; cached source. Preserve every other attribute, including mtime/ctime.
       (setf (nth 4 link-attributes) nil (nth 4 target-attributes) nil)
       (list (file-name-nondirectory file) (file-symlink-p file)
             link-attributes target-attributes
             (when (file-regular-p file)
               (secure-hash 'sha256 (hub/package-install--text file))))))
   (directory-files directory t directory-files-no-dot-files-regexp)))

(defun hub/package-install-test--setup (config)
  "Populate disposable CONFIG with two pinned Git packages and cached builds."
  (hub/package-install-test--git config "init" "-q")
  (with-temp-file (expand-file-name ".gitignore" config)
    (insert "straight/*\n!straight/versions/\n"))
  (dolist (name '("gdocs" "helperdep"))
    (let ((repo (expand-file-name (concat "straight/repos/" name) config))
          (build (expand-file-name (concat "straight/build/" name) config)))
      (make-directory repo t) (make-directory build t)
      (hub/package-install-test--git repo "init" "-q")
      (with-temp-file (expand-file-name (concat name ".el") repo)
        (insert (format ";;; %s.el --- Fixture -*- lexical-binding: t; -*-\n;; Version: 1.0\n" name))
        (when (equal name "gdocs")
          (insert ";; Package-Requires: ((helperdep \"1.0\"))\n(require 'helperdep)\n"))
        (insert (format "(defconst %s-fixture-version 1)\n(provide '%s)\n" name name)))
      (hub/package-install-test--commit repo)
      (make-symbolic-link (expand-file-name (concat name ".el") repo)
                          (expand-file-name (concat name ".el") build))))
  (let* ((library-repo (expand-file-name "straight/repos/gdocs" config))
         (old (hub/package-install-test--git library-repo "rev-parse" "HEAD"))
         (dep (expand-file-name "straight/repos/helperdep" config))
         (pins (expand-file-name "straight/versions/default.el" config)))
    (with-temp-file (expand-file-name "gdocs.el" library-repo)
      (insert ";;; gdocs.el --- Fixture -*- lexical-binding: t; -*-\n;; Version: 1.0\n;; Package-Requires: ((helperdep \"1.0\"))\n(require 'helperdep)\n(defconst gdocs-fixture-version 2)\n(provide 'gdocs)\n"))
    (let ((revision (hub/package-install-test--commit library-repo)))
      (hub/package-install-test--git library-repo "checkout" "--detach" old)
      (make-directory (file-name-directory pins) t)
      (with-temp-file pins
        (prin1 (list (cons "gdocs" revision)
                     (cons "helperdep" (hub/package-install-test--git dep "rev-parse" "HEAD")))
               (current-buffer)))
      (hub/package-install-test--commit config)
      (hub/package-install-test--write-cache config)
      (list :old old :revision revision :repo library-repo))))

(defmacro hub/package-install-test--fixture (&rest body)
  "Run BODY against isolated CONFIG and STATE, without inherited Git settings."
  (declare (indent 0))
  `(let* ((config (make-temp-file "package-install-fixture-" t))
          (process-environment (copy-sequence process-environment)))
     (setenv "GIT_CONFIG_GLOBAL" "/dev/null")
     (setenv "GIT_CONFIG_NOSYSTEM" "1")
     (setenv "GIT_INDEX_FILE" nil)
     (unwind-protect
         (let ((state (hub/package-install-test--setup config))) ,@body)
       (delete-directory config t))))

(ert-deftest hub/package-install-dry-run-does-not-change-checkout-or-cache ()
  "A valid plan names the committed pin without bootstrapping or changing files."
  (hub/package-install-test--fixture
   (let* ((cache (expand-file-name "straight/build-cache.el" config))
          (before (with-temp-buffer (insert-file-contents cache) (buffer-string)))
          (summary (hub/package-install config "gdocs")))
     (should (equal (plist-get summary :mode) "dry-run"))
     (should (equal (plist-get summary :revision) (plist-get state :revision)))
     (should (equal (hub/package-install-test--git (plist-get state :repo) "rev-parse" "HEAD")
                    (plist-get state :old)))
     (should (equal before (with-temp-buffer (insert-file-contents cache) (buffer-string)))))))

(ert-deftest hub/package-install-rejects-dirty-selected-repository ()
  "Uncommitted selected-package changes block even a dry-run plan."
  (hub/package-install-test--fixture
   (with-temp-file (expand-file-name "untracked" (plist-get state :repo)) (insert "keep"))
   (should-error (hub/package-install config "gdocs") :type 'user-error)))

(ert-deftest hub/package-install-rejects-incompatible-cache-before-checkout ()
  "Schema or Emacs mismatch cannot silently trigger a broad rebuild."
  (dolist (kind '(emacs schema))
    (hub/package-install-test--fixture
     (hub/package-install-test--write-cache config
                                            (when (eq kind 'emacs) "0.0")
                                            (when (eq kind 'schema) :unsupported))
     (should-error (hub/package-install config "gdocs") :type 'user-error)
     (should (equal (hub/package-install-test--git (plist-get state :repo) "rev-parse" "HEAD")
                    (plist-get state :old))))))

(ert-deftest hub/package-install-apply-requires-the-reviewed-revision ()
  "Apply requires an explicit matching expectation, before any checkout."
  (hub/package-install-test--fixture
   (dolist (expect (list nil (make-string 40 ?0)))
     (should-error (hub/package-install config "gdocs" t expect) :type 'user-error))
   (should (equal (hub/package-install-test--git (plist-get state :repo) "rev-parse" "HEAD")
                  (plist-get state :old)))))

(ert-deftest hub/package-install-apply-builds-and-verifies-only-selected-package ()
  "Real Straight compiles the selected pin and leaves the dependency untouched."
  (let* ((build (or (getenv "STRAIGHT_BUILD_PATH") "straight/build"))
         (straight (expand-file-name "../repos/straight.el/straight.el" build)))
    (unless (file-readable-p straight) (ert-skip "Public Straight source is unavailable"))
    (hub/package-install-test--fixture
     (let ((destination (expand-file-name "straight/repos/straight.el/straight.el" config))
           (dependency (expand-file-name "straight/build/helperdep/helperdep.el" config)))
       (make-directory (file-name-directory destination) t)
       (copy-file straight destination)
       (let* ((before (hub/package-install-test--artifact-state (file-name-directory dependency)))
              (original (hub/package-install--read-forms (expand-file-name "straight/build-cache.el" config)))
              (summary (with-temp-buffer
                         (should (= 0 (call-process
                                       (expand-file-name "scripts/package-install" hub/package-install-test--root)
                                       nil t nil "--config" config "--package" "gdocs"
                                       "--apply" "--expect" (plist-get state :revision))))
                         (goto-char (point-min))
                         (let ((json-object-type 'plist) (json-key-type 'keyword)) (json-read)))))
         (should (equal (plist-get summary :mode) "installed"))
         (should (equal (plist-get summary :revision) (plist-get state :revision)))
         (should (= (plist-get summary :unrelated-cache-changes) 0))
         (should (string-suffix-p "straight/build/gdocs/gdocs.elc" (plist-get summary :library)))
         (should (equal before (hub/package-install-test--artifact-state (file-name-directory dependency))))
         (let ((updated (hub/package-install--read-forms (expand-file-name "straight/build-cache.el" config))))
           (dolist (slot '(2 3))
             (should (equal (gethash "helperdep" (nth slot original))
                            (gethash "helperdep" (nth slot updated)))))
           (should (equal (nth 5 original) (nth 5 updated))))
         (should (equal (hub/package-install-test--git (plist-get state :repo) "rev-parse" "HEAD")
                        (plist-get state :revision))))))))

(ert-deftest hub/package-install-recognizes-uncached-emacs-builtins ()
  "An ordinary Emacs dependency needs neither a cached recipe nor a checkout."
  (hub/package-install-test--fixture
   (let* ((file (expand-file-name "straight/build-cache.el" config))
          (forms (hub/package-install--read-forms file)))
     (setf (nth 1 (gethash "gdocs" (nth 2 forms))) '("emacs" "helperdep"))
     (with-temp-file file
       (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
   (should (equal (plist-get (hub/package-install config "gdocs") :mode) "dry-run"))))

(ert-deftest hub/package-install-cli-default-is-a-read-only-plan ()
  "The public command defaults to dry-run without needing manager bootstrap."
  (hub/package-install-test--fixture
   (with-temp-buffer
     (should (= 0 (call-process (expand-file-name "scripts/package-install" hub/package-install-test--root)
                                nil t nil "--config" config "--package" "gdocs")))
     (goto-char (point-min))
     (let ((json-object-type 'alist))
       (should (equal (alist-get 'mode (json-read)) "dry-run"))))
   (should (equal (hub/package-install-test--git (plist-get state :repo) "rev-parse" "HEAD")
                  (plist-get state :old)))))

(ert-deftest hub/package-install-missing-dependency-cache-stops-before-checkout ()
  "A missing dependency recipe cannot become an implicit installation."
  (hub/package-install-test--fixture
   (let* ((file (expand-file-name "straight/build-cache.el" config))
          (forms (hub/package-install--read-forms file)))
     (remhash "helperdep" (nth 2 forms))
     (with-temp-file file
       (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
   (let ((error (should-error (hub/package-install config "gdocs") :type 'user-error)))
     (should (string-match-p "Missing compatible cached recipe" (error-message-string error))))
   (should (equal (hub/package-install-test--git (plist-get state :repo) "rev-parse" "HEAD")
                  (plist-get state :old)))))

(ert-deftest hub/package-install-cache-audit-checks-added-removed-and-modified-keys ()
  "Unrelated cache changes fail while selected-package changes remain allowed."
  (dolist (kind '(added removed modified selected))
    (let ((table (make-hash-table :test 'equal)))
      (puthash "gdocs" '("old") table)
      (puthash "helperdep" '("keep") table)
      (let ((before (hub/package-install--fingerprints table "gdocs")))
        (pcase kind
          ('added (puthash "other" '("new") table))
          ('removed (remhash "helperdep" table))
          ('modified (setcar (gethash "helperdep" table) "changed"))
          ('selected (puthash "gdocs" '("new") table)))
        (if (eq kind 'selected)
            (hub/package-install--assert-cache before table "gdocs")
          (should-error (hub/package-install--assert-cache before table "gdocs") :type 'user-error))))))

(ert-deftest hub/package-install-worker-refuses-unrelated-build-and-cache-publication ()
  "The real manager cannot rebuild another package or publish a changed cache."
  (dolist (attack '(build cache))
    (let* ((build (or (getenv "STRAIGHT_BUILD_PATH") "straight/build"))
           (straight (expand-file-name "../repos/straight.el/straight.el" build)))
      (unless (file-readable-p straight) (ert-skip "Public Straight source is unavailable"))
      (hub/package-install-test--fixture
       (let* ((destination (expand-file-name "straight/repos/straight.el/straight.el" config))
              (cache (expand-file-name "straight/build-cache.el" config))
              (before (hub/package-install--text cache))
              (diagnostic ""))
         (make-directory (file-name-directory destination) t)
         (copy-file straight destination)
         (with-temp-buffer
           (insert-file-contents destination)
           (goto-char (point-max))
           (insert
            (if (eq attack 'build)
                "\n(advice-add 'straight-rebuild-package :after (lambda (&rest _) (straight--build-package (gethash \"helperdep\" straight--recipe-cache))))\n"
              "\n(advice-add 'straight-rebuild-package :after (lambda (&rest _) (setcar (gethash \"helperdep\" straight--build-cache) \"unexpected\")))\n"))
           (write-region (point-min) (point-max) destination nil 'silent))
         (cl-letf (((symbol-function 'external-debugging-output)
                    (lambda (character) (setq diagnostic (concat diagnostic (string character))))))
           (should-error (hub/package-install config "gdocs" t (plist-get state :revision))
                         :type 'user-error))
         (should (string-match-p (if (eq attack 'build) "Refusing unrelated package rebuild"
                                   "Refusing to publish unrelated package cache changes") diagnostic))
         (should (equal before (hub/package-install--text cache)))
         (should-not (file-exists-p (expand-file-name "straight/package-install.lock" config))))))))

(ert-deftest hub/package-install-rejects-a-repository-shared-by-other-packages ()
  "A single-package apply cannot implicitly move another package's sources."
  (hub/package-install-test--fixture
   (let* ((file (expand-file-name "straight/build-cache.el" config))
          (forms (hub/package-install--read-forms file))
          (cache (nth 2 forms))
          (other (copy-tree (gethash "gdocs" cache))))
     (setf (plist-get (nth 2 other) :package) "companion")
     (puthash "companion" other cache)
     (with-temp-file file
       (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
   (let ((error (should-error (hub/package-install config "gdocs") :type 'user-error)))
     (should (string-match-p "shared" (error-message-string error))))))

(ert-deftest hub/package-install-requires-a-committed-pin-file ()
  "An ignored local lockfile cannot masquerade as committed installation intent."
  (hub/package-install-test--fixture
   (hub/package-install-test--git config "rm" "--cached" "straight/versions/default.el")
   (with-temp-file (expand-file-name ".gitignore" config) (insert "straight/\n"))
   (hub/package-install-test--commit config)
   (should-error (hub/package-install config "gdocs") :type 'user-error)))

(ert-deftest hub/package-install-artifact-state-ignores-reads-but-detects-content-writes ()
  "Read access time is not a rebuild; dependency content changes remain visible."
  (hub/package-install-test--fixture
   (let* ((directory (expand-file-name "straight/build/helperdep" config))
          (file (expand-file-name "helperdep.el" directory))
          (before (hub/package-install-test--artifact-state directory))
          (attributes (symbol-function 'file-attributes)))
     (cl-letf (((symbol-function 'file-attributes)
                (lambda (path &optional id-format)
                  (let ((value (funcall attributes path id-format)))
                    (when value (setf (nth 4 value) '(0 1 0 0)))
                    value))))
       (should (equal before (hub/package-install-test--artifact-state directory))))
     (with-temp-file file (insert ";; changed dependency source\n"))
     (should-not (equal before (hub/package-install-test--artifact-state directory))))))

(ert-deftest hub/package-install-cli-help-uses-package-terminology ()
  "The renamed public command describes a package installer in its help."
  (with-temp-buffer
    (should (= 0 (call-process (expand-file-name "scripts/package-install" hub/package-install-test--root)
                               nil t nil "--help")))
    (should (string-match-p "Usage: package-install" (buffer-string)))))

(provide 'package-install-test)
;;; package-install-test.el ends here
