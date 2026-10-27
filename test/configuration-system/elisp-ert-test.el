;;; elisp-ert-test.el --- Regression tests for the ERT runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise discovery, selection and exit status through the real shell runner
;; in temporary repositories.  Fixtures use real batch Emacs, not a mock ERT.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst hub/ert-runner-test--script
  (expand-file-name "../../scripts/elisp-ert"
		    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the repository's ERT runner.")

(defun hub/ert-runner-test--write (root path contents)
  "Write CONTENTS to PATH beneath fixture ROOT."
  (let ((file (expand-file-name path root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert contents))))

(defmacro hub/ert-runner-test--with-repository (&rest body)
  "Run BODY in a temporary repository with configuration and package tests."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "hub-ert-runner-" t))
	  (default-directory (file-name-as-directory root))
	  (process-environment (copy-sequence process-environment)))
     (unwind-protect
	 (progn
	   (setenv "HOME" root)
	   (hub/ert-runner-test--write root "test/test-helpers.el" "")
	   (hub/ert-runner-test--write
	    root "test/configuration-system/example-test.el"
	    "(ert-deftest hub/fixture-config () (should t))\n")
	   (hub/ert-runner-test--write
	    root "packages/example/example.el"
	    "(defvar hub/fixture-package-value t)\n(provide 'example)\n")
	   (hub/ert-runner-test--write
	    root "packages/example/test/example-test.el"
	    (concat "(require 'example)\n"
		    "(ert-deftest hub/fixture-package ()\n"
		    "  (should hub/fixture-package-value))\n"))
	   ,@body)
       (delete-directory root t))))

(defun hub/ert-runner-test--run (&rest arguments)
  "Run the shell runner with ARGUMENTS; return its exit status and output."
  (with-temp-buffer
    (let ((status (apply #'call-process "bash" nil t nil
			 hub/ert-runner-test--script arguments)))
      (list status (buffer-string)))))

(ert-deftest hub/ert-runner-lists-configuration-and-package-tests ()
  "The inventory contains both test roots in deterministic order."
  (hub/ert-runner-test--with-repository
   (should
    (equal (hub/ert-runner-test--run "--list")
	   '(0 "packages/example/test/example-test.el\ntest/configuration-system/example-test.el\n")))))

(ert-deftest hub/ert-runner-selects-package-tests-from-source-changes ()
  "A package change selects its tests, without duplicates or unrelated tests."
  (hub/ert-runner-test--with-repository
   (should
    (equal (hub/ert-runner-test--run
	    "--list" "packages/example/example.el"
	    "packages/example/test/example-test.el")
	   '(0 "packages/example/test/example-test.el\n")))))

(ert-deftest hub/ert-runner-preserves-focused-configuration-selection ()
  "Explicit configuration test paths remain independently runnable."
  (hub/ert-runner-test--with-repository
   (should
    (equal (hub/ert-runner-test--run
	    "--list" "test/configuration-system/example-test.el")
	   '(0 "test/configuration-system/example-test.el\n")))))

(ert-deftest hub/ert-runner-executes-package-tests-with-local-libraries ()
  "The default invocation executes package tests with their library available."
  (hub/ert-runner-test--with-repository
   (pcase-let ((`(,status ,output) (hub/ert-runner-test--run)))
     (should (equal status 0))
     (should (string-match-p "passed.*hub/fixture-config" output))
     (should (string-match-p "passed.*hub/fixture-package" output)))))

(ert-deftest hub/ert-runner-fails-on-package-test-failure ()
  "A failing package-local test makes the full runner fail."
  (hub/ert-runner-test--with-repository
   (hub/ert-runner-test--write
    root "packages/example/test/failing-test.el"
    "(ert-deftest hub/fixture-failure () (should nil))\n")
   (pcase-let ((`(,status ,output) (hub/ert-runner-test--run)))
     (should (integerp status))
     (should-not (zerop status))
     (should (string-match-p "FAILED.*hub/fixture-failure" output)))))

(ert-deftest hub/ert-runner-reports-skips-without-claiming-they-passed ()
  "Skipped package tests remain visible as skips in ERT output."
  (hub/ert-runner-test--with-repository
   (hub/ert-runner-test--write
    root "packages/example/test/skipped-test.el"
    "(ert-deftest hub/fixture-skip () (ert-skip \"Live test is opt-in\"))\n")
   (pcase-let ((`(,status ,output) (hub/ert-runner-test--run)))
     (should (equal status 0))
     (should (string-match-p "SKIPPED.*hub/fixture-skip" output))
     (should-not (string-match-p "passed.*hub/fixture-skip" output)))))

(ert-deftest hub/ert-runner-isolates-package-and-configuration-test-names ()
  "Independent suites can use the same ERT name without hiding either test."
  (hub/ert-runner-test--with-repository
   (dolist (path '("test/shared-test.el"
		   "packages/example/test/shared-test.el"))
     (hub/ert-runner-test--write
      root path "(ert-deftest hub/fixture-shared () (should t))\n"))
   (pcase-let ((`(,status ,output) (hub/ert-runner-test--run)))
     (should (equal status 0))
     (should (= 2 (cl-count-if
		   (lambda (line)
		     (string-match-p "passed.*hub/fixture-shared" line))
		   (split-string output "\n")))))))

(ert-deftest hub/ert-runner-reports-load-failure-and-runs-remaining-suites ()
  "A load error fails the run without concealing other package results."
  (hub/ert-runner-test--with-repository
   (hub/ert-runner-test--write
    root "packages/example/test/broken-test.el"
    "(error \"Fixture load failure\")\n")
   (hub/ert-runner-test--write
    root "packages/later/test/later-test.el"
    "(ert-deftest hub/fixture-later () (should t))\n")
   (pcase-let ((`(,status ,output) (hub/ert-runner-test--run)))
     (should (integerp status))
     (should-not (zerop status))
     (should (string-match-p "Fixture load failure" output))
     (should (string-match-p "passed.*hub/fixture-later" output)))))

(ert-deftest hub/ert-runner-protects-hook-index-from-nested-git ()
  "Git in a test fixture cannot overwrite the index inherited from a hook."
  (hub/ert-runner-test--with-repository
   ;; Keep fixture setup independent of any enclosing commit's Git routing.
   (dolist (variable '("GIT_INDEX_FILE" "GIT_DIR" "GIT_WORK_TREE" "GIT_COMMON_DIR"
		       "GIT_OBJECT_DIRECTORY" "GIT_ALTERNATE_OBJECT_DIRECTORIES"))
     (setenv variable nil))
   (should (zerop (process-file "git" nil nil nil "init" "--quiet")))
   (hub/ert-runner-test--write root "staged.txt" "Keep this staged.\n")
   (should (zerop (process-file "git" nil nil nil "add" "--" "staged.txt")))
   (should (zerop (process-file "git" nil nil nil "init" "--quiet" "foreign")))
   (hub/ert-runner-test--write root "foreign/checkout.txt" "Foreign repository.\n")
   (hub/ert-runner-test--write
    root "test/configuration-system/example-test.el"
    (concat "(ert-deftest hub/fixture-foreign-git ()\n"
	    "  (let ((default-directory (expand-file-name \"foreign/\")))\n"
	    "    (should (zerop (process-file \"git\" nil nil nil\n"
	    "                                \"add\" \"--\" \"checkout.txt\")))))\n"))
   (let* ((index (expand-file-name ".git/index" root))
	  (before (with-temp-buffer
		    (insert-file-contents-literally index)
		    (secure-hash 'sha256 (current-buffer)))))
     (setenv "GIT_INDEX_FILE" index)
     (pcase-let ((`(,status ,output) (hub/ert-runner-test--run)))
       (ert-info (output) (should (equal status 0))))
     (should (equal before (with-temp-buffer
			     (insert-file-contents-literally index)
			     (secure-hash 'sha256 (current-buffer)))))
     (should (file-exists-p (expand-file-name "foreign/.git/index" root))))))

(provide 'elisp-ert-test)
;;; elisp-ert-test.el ends here
