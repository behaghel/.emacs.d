;;; org-confluence-standalone-test.el --- Offline package-boundary checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real local conversion and mode activation without repository helpers,
;; credentials, network requests, or Confluence CLI execution.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-confluence-test-support
	 (expand-file-name "org-confluence-test-support.el"
			   (file-name-directory (or load-file-name buffer-file-name))))
(require 'package)
(require 'org-confluence)

(defvar org-confluence-demo--source-buffer)

(ert-deftest org-confluence-standalone-declares-its-sync-dependency ()
  "Package metadata declares the shared kernel already needed by export code."
  (with-temp-buffer
    (insert-file-contents (locate-library "org-confluence"))
    (should (assq 'ox-sync (package-desc-reqs (package-buffer-info))))))

(ert-deftest org-confluence-standalone-customizations-belong-to-org ()
  "Reusable customization does not require a personal Hub customization group."
  (should (member '(org-confluence-api custom-group) (get 'org 'custom-group))))

(ert-deftest org-confluence-standalone-import-explains-missing-libxml ()
  "Missing native HTML parsing produces an actionable error before conversion."
  (cl-letf (((symbol-function 'libxml-available-p) (lambda () nil)))
    (should (string-match-p
	     "libxml"
	     (error-message-string
	      (should-error (org-confluence-import-storage-to-org "<p>Hello</p>")
			    :type 'user-error))))))

(ert-deftest org-confluence-standalone-missing-cli-does-not-run-a-process ()
  "Missing cfl is diagnosed before any shell command can execute."
  (let ((org-confluence-api-cfl-command "standalone-missing-cfl"))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
	      ((symbol-function 'org-confluence-process-run-process)
	       (lambda (&rest _) (ert-fail "Must not execute a missing CLI"))))
      (should (string-match-p
	       "Cannot find.*standalone-missing-cfl"
	       (error-message-string
		(should-error
		 (org-confluence-process-run-output
		  (org-confluence-api--page-view-storage-command "offline-demo"))
		 :type 'user-error)))))))

(ert-deftest org-confluence-standalone-cli-errors-omit-path-values ()
  "Both CLI runners report actionable errors without dumping environment paths."
  (let ((process-environment (copy-sequence process-environment))
	(org-confluence-api-cfl-command "standalone-missing-cfl"))
    (setenv "PATH" "/private/path-value-must-not-appear")
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (dolist (runner '(org-confluence-process-run org-confluence-process-run-output))
	(let ((text (error-message-string
		     (should-error (funcall runner "unused") :type 'user-error))))
	  (should (string-match-p "standalone-missing-cfl" text))
	  (should (string-match-p "org-confluence-api-cfl-command" text))
	  (should-not (string-match-p (regexp-quote (getenv "PATH")) text)))))))

(ert-deftest org-confluence-standalone-cli-demo-preserves-editing-context ()
  "The expected diagnostic is reported without unwinding or moving the editor."
  (load (expand-file-name "examples/org-confluence-demo.el"
			  (file-name-directory (locate-library "org-confluence"))) nil t)
  (let ((source (generate-new-buffer " *confluence-cli-origin*"))
	(other (generate-new-buffer " *confluence-cli-other*"))
	reported)
    (unwind-protect
	(save-window-excursion
	  (switch-to-buffer source)
	  (insert "Keep the cursor here.\n")
	  (goto-char 7)
	  (let ((window (selected-window))
		(position (point))
		(layout (mapcar (lambda (item)
				  (list item (window-buffer item)))
				(window-list))))
	    (cl-letf (((symbol-function 'org-confluence-process-run-output)
		       (lambda (&rest _)
			 ;; Exercise restoration even if a diagnostic path
			 ;; changes the selected buffer before reporting failure.
			 (switch-to-buffer other)
			 (insert "Diagnostic buffer")
			 (user-error "Cannot find the demo CLI")))
		      ((symbol-function 'message)
		       (lambda (format-string &rest arguments)
			 (setq reported (apply #'format format-string arguments)))))
	      (call-interactively #'org-confluence-demo-check-cli))
	    (should (equal reported "Cannot find the demo CLI"))
	    (should (eq (current-buffer) source))
	    (should (eq (selected-window) window))
	    (should (= (point) position))
	    (should (equal layout
			   (mapcar (lambda (item)
				     (list item (window-buffer item)))
				   (window-list))))))
      (kill-buffer source)
      (kill-buffer other))))

(ert-deftest org-confluence-standalone-native-export-and-local-roundtrip ()
  "Mode activation and the native export menu preserve a disposable source."
  (let* ((directory (make-temp-file "confluence-standalone-" t))
	 (source-file (expand-file-name "source.org" directory))
	 (examples (expand-file-name "examples" (file-name-directory (locate-library "org-confluence"))))
	 (org-export-use-babel nil)
	 (org-confluence-api--rest-transport
	  (lambda (&rest _) (ert-fail "Local conversion must not contact Confluence")))
	 source)
    (load (expand-file-name "org-confluence-demo.el" examples) nil t)
    (unwind-protect
	(save-window-excursion
	  (copy-file (expand-file-name "standalone.org" examples) source-file)
	  (find-file source-file)
	  (setq source (current-buffer))
	  (let ((before (buffer-string))
		(org-confluence-demo--source-buffer source))
	    (cl-letf (((symbol-function 'org-confluence-api--read-cfl-config)
		       (lambda (&rest _) (ert-fail "Must not read credentials")))
		      ((symbol-function 'org-confluence-process-run-process)
		       (lambda (&rest _) (ert-fail "Must not execute cfl"))))
	      (org-confluence-mode 1)
	      (should org-confluence-mode)
	      (should org-comments-mode)
	      (execute-kbd-macro (kbd "C-c C-e C X"))
	      (let* ((storage (get-buffer "*Org Confluence Export*"))
		     (xhtml (with-current-buffer storage (buffer-string)))
		     (roundtrip (org-confluence-demo-roundtrip)))
		(should (string-match-p "<strong>Bépo</strong>" xhtml))
		(should (string-match-p "ac:name=\"info\"" xhtml))
		(should (string-match-p "datetime=\"2026-07-02\"" xhtml))
		(with-current-buffer roundtrip
		  (should (derived-mode-p 'org-mode))
		  (should buffer-read-only)
		  (should (string-match-p "\\*Bépo\\*" (buffer-string)))
		  (should (string-match-p "No execution: Bépo & <XML>" (buffer-string)))
		  (let ((reexport (org-confluence-export)))
		    ;; HTML spacing is normalized; semantic blocks and verbatim
		    ;; code must survive the complete conversion cycle.
		    (dolist (fragment '("<h1>Sample content</h1>"
					"<strong>Bépo</strong>"
					"datetime=\"2026-07-02\""
					"ac:name=\"info\""
					"<ac:parameter ac:name=\"title\">Local preview</ac:parameter>"
					"<ul><li>First item</li><li>Second item</li></ul>"
					"<![CDATA[print(\"No execution: Bépo & <XML>\")]]>"))
		      (should (string-match-p (regexp-quote fragment) reexport)))
		    (should (equal (buffer-string)
				   (org-confluence-import-storage-to-org reexport directory)))))
		(should (equal before (with-current-buffer source (buffer-string)))))
	      (with-current-buffer source (org-confluence-mode -1)))))
      (dolist (buffer (list source (get-buffer "*Org Confluence Export*")
			    (get-buffer "*Org Confluence Round Trip*")))
	(when (buffer-live-p buffer)
	  (with-current-buffer buffer (set-buffer-modified-p nil))
	  (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest org-confluence-tests-reject-unmocked-io-even-if-errors-are-swallowed ()
  "Fixture guards stop default credentials, networking, and external processes."
  (should (featurep 'org-confluence-test-support))
  (dolist (operation (list (lambda () (org-confluence-api--read-cfl-config))
			   (lambda () (make-network-process :name "unexpected-test-network"
							    :host "example.invalid" :service 443))
			   (lambda () (process-file "unexpected-test-executable"))))
    (should (condition-case nil
		(progn (ignore-errors (funcall operation)) nil)
	      (org-confluence-test-unexpected-io t)))))

(provide 'org-confluence-standalone-test)
;;; org-confluence-standalone-test.el ends here
