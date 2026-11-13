;;; blog-integration-test.el --- Personal static-site activation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep reusable package tests independent while verifying that tracked personal
;; configuration retains its existing Evil, which-key, and prose integrations.

;;; Code:

(require 'ert)

(defconst hub/blog-integration-test--root
  (file-name-as-directory
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository root containing personal blog configuration.")

(ert-deftest hub/blog-retains-personal-site-bindings-and-prose-policy ()
  "Personal activation installs the historical shortcuts and prose hook."
  (with-temp-buffer
    (let ((status
	   (process-file
	    (expand-file-name invocation-name invocation-directory)
	    nil (list t t) nil "--batch" "-Q"
	    "-L" (expand-file-name "packages/hb-static-site" hub/blog-integration-test--root)
	    "-L" (expand-file-name "modules/interactive" hub/blog-integration-test--root)
	    "--eval"
	    (prin1-to-string
	     '(progn
		(require 'hb-static-site)
		(defvar evil-normal-state-map (make-sparse-keymap))
		(defvar hub/test-site-label nil)
		(defvar hub/test-site-prose-calls 0)
		(fset 'use-package (cons 'macro (lambda (&rest _args) nil)))
		(fset 'evil-define-key
		      (lambda (_state map &rest bindings)
			(while bindings
			  (define-key map (pop bindings) (pop bindings)))))
		(fset 'which-key-add-key-based-replacements
		      (lambda (&rest args) (setq hub/test-site-label args)))
		(fset 'writing/enable-basics
		      (lambda () (setq hub/test-site-prose-calls
				       (1+ hub/test-site-prose-calls))))
		(provide 'evil)
		(provide 'which-key)
		(require 'tools/blog)
		(dolist (binding '((",w n" . hb-static-site-create-post)
				   (",w s" . hb-static-site-create-section)
				   (",w p" . hb-static-site-create-page)
				   (",w P" . hb-static-site-create-basic-page)
				   (",w o" . hb-static-site-find-page)
				   (",w e" . hb-static-site-export-buffer)
				   (",w E" . hb-static-site-export-all)
				   (",w v" . hb-static-site-validate-buffer)))
		  (unless (eq (lookup-key hb-static-site-mode-map (kbd (car binding)))
			      (cdr binding))
		    (error "Missing personal site binding: %s" (car binding))))
		(unless (eq (lookup-key evil-normal-state-map (kbd ",np"))
			    #'hb-static-site-create-post)
		  (error "Missing historical global post shortcut"))
		(unless (equal hub/test-site-label '("C-c w" "static site"))
		  (error "Missing personal which-key label"))
		(with-temp-buffer
		  (org-mode)
		  (let ((hb-static-site-enable-auto-export nil))
		    (hb-static-site-mode 1)
		    (hb-static-site-mode -1)))
		(unless (= hub/test-site-prose-calls 1)
		  (error "Personal prose setup must run only when enabling Site mode")))))))
      (ert-info ((buffer-string)) (should (zerop status))))))

(provide 'blog-integration-test)
;;; blog-integration-test.el ends here
