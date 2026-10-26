;;; org-suggestions-standalone-test.el --- Standalone suggestions tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the public persisted suggestion workflow and the runnable example
;; without comments, Copilot, configuration modules, or shared test helpers.

;;; Code:

(require 'ert)
(require 'package)
(require 'ob-emacs-lisp)
(require 'org-suggestions)

(ert-deftest org-suggestions-standalone-declares-only-emacs-and-org ()
  "The standalone package needs no sibling packages or external providers."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "org-suggestions.el"
		       (file-name-directory (locate-library "org-suggestions"))))
    (should (equal (mapcar #'car (package-desc-reqs (package-buffer-info)))
		   '(emacs org)))))

(ert-deftest org-suggestions-standalone-persist-preview-accept-and-undo ()
  "Every supported hunk can be persisted, previewed, accepted, and undone."
  (dolist (case
	   '(("* Intro\nOld body.\n" "* Intro\nNew body.\n"
	      (:id "h1" :kind section-replace :section-title "Intro"
		   :replacement "New body.\n")
	      "-Old body.\n+New body.\n")
	     ("Alpha.\nOmega.\n" "Revised alpha.\nOmega.\n"
	      (:id "h1" :kind replace :original "Alpha.\n"
		   :replacement "Revised alpha.\n")
	      "-Alpha.\n+Revised alpha.\n")
	     ("Alpha beta gamma.\n" "Alpha BETA gamma.\n"
	      (:id "h1" :kind replace :original "beta" :replacement "BETA")
	      "-beta\n+BETA\n")
	     ("Alpha beta gamma.\n" "Alpha  gamma.\n"
	      (:id "h1" :kind replace :original "beta" :replacement "")
	      "-beta\n")
	     ("Alpha.\nOmega.\n" "Alpha.\nMiddle.\nOmega.\n"
	      (:id "h1" :kind insert :anchor-text "Omega." :placement before
		   :replacement "Middle.\n")
	      "+Middle.\n")))
    (let* ((directory (make-temp-file "org-suggestions-standalone" t))
	   (source-file (expand-file-name "draft.org" directory))
	   (org-suggestions--rollback-data nil)
	   source)
      (unwind-protect
	  (progn
	    (with-temp-file source-file (insert (nth 0 case)))
	    (org-suggestions-write-sidecar
	     source-file
	     (list (list :id "manual-thread" :provider "manual"
			 :candidates
			 (list (list :id "manual-1" :status 'active
				     :hunks (list (copy-tree (nth 2 case))))))))
	    (setq source (find-file-noselect source-file))
	    (let* ((threads (org-suggestions-load-sidecar source-file))
		   (match (org-suggestions-find-candidate threads "manual-1")))
	      (should (equal (org-suggestions-candidate-diff
			      source (car match) (cdr match))
			     (nth 3 case)))
	      (with-current-buffer source
		(should (equal (buffer-string) (nth 0 case)))
		(should-not (buffer-modified-p)))
	      (should (eq (plist-get (cdr match) :status) 'active)))
	    (org-suggestions-accept-candidate-id source "manual-1")
	    (with-current-buffer source
	      (should (equal (buffer-string) (nth 1 case))))
	    (let* ((threads (org-suggestions-load-sidecar source-file))
		   (match (org-suggestions-find-candidate threads "manual-1")))
	      (should (eq (plist-get (cdr match) :status) 'accepted))
	      (org-suggestions-undo-accepted-candidate source (cdr match))
	      (org-suggestions-write-sidecar source-file threads))
	    (with-current-buffer source
	      (should (equal (buffer-string) (nth 0 case))))
	    (let* ((threads (org-suggestions-load-sidecar source-file))
		   (match (org-suggestions-find-candidate threads "manual-1")))
	      (should (eq (plist-get (cdr match) :status) 'active))))
	(when (buffer-live-p source)
	  (with-current-buffer source (set-buffer-modified-p nil))
	  (kill-buffer source))
	(delete-directory directory t)))))

(ert-deftest org-suggestions-standalone-preview-refuses-unresolved-hunks ()
  "Preview reports stale text anchors rather than producing an empty preview."
  (dolist (hunk '((:kind replace :original "missing" :replacement "New")
		  (:kind insert :anchor-text "missing" :replacement "New")))
    (with-temp-buffer
      (org-mode)
      (insert "Original source.\n")
      (let ((candidate (list :id "manual-1" :status 'active :hunks (list hunk))))
	(should-error (org-suggestions-candidate-diff (current-buffer) nil candidate)
		      :type 'user-error)
	(should (equal (buffer-string) "Original source.\n"))
	(should (eq (plist-get candidate :status) 'active))))))

(ert-deftest org-suggestions-standalone-example-runs-public-workflow ()
  "The documented example round-trips source text and persisted lifecycle."
  (let* ((example (expand-file-name
		   "examples/standalone.org"
		   (file-name-directory (locate-library "org-suggestions"))))
	 (directory (make-temp-file "org-suggestions-example" t))
	 (default-directory (file-name-as-directory directory))
	 (source-file (expand-file-name "draft.org" directory))
	 (org-suggestions--rollback-data nil)
	 (org-confirm-babel-evaluate nil)
	 results)
    (unwind-protect
	(progn
	  (with-temp-buffer
	    (insert-file-contents example)
	    (org-mode)
	    (org-babel-map-src-blocks nil
				      (push (org-babel-execute-src-block) results)))
	  (setq results (nreverse results))
	  (should (= (length results) 4))
	  (should (equal (nth 1 results) "-beta\n+BETA\n"))
	  (should (equal (nth 2 results) "Alpha BETA gamma.\n"))
	  (should (equal (nth 3 results) "Alpha beta gamma.\n"))
	  (with-temp-buffer
	    (insert-file-contents source-file)
	    (should (equal (buffer-string) "Alpha beta gamma.\n")))
	  (let* ((threads (org-suggestions-load-sidecar source-file))
		 (match (org-suggestions-find-candidate threads "manual-1")))
	    (should (eq (plist-get (cdr match) :status) 'active))))
      (when-let* ((source (find-buffer-visiting source-file)))
	(kill-buffer source))
      (delete-directory directory t))))

(provide 'org-suggestions-standalone-test)
;;; org-suggestions-standalone-test.el ends here
