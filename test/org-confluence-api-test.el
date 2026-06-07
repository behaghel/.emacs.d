;;; org-confluence-api-test.el --- Tests for Org Confluence cfl API wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavior tests for Confluence publish command construction.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

;; Ensure repo modules are reachable for isolated batch test runners.
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root))
  (add-to-list 'load-path (expand-file-name "modules/org/export-confluence" root))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "core" root)))

(load "api" nil 'nomessage)
(load "commands" nil 'nomessage)

(defun hub/confluence-api-test--with-org-buffer (contents thunk)
  "Run THUNK in a temporary Org buffer containing CONTENTS."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (funcall thunk)))

(ert-deftest hub/confluence-api--page-update-command ()
  "Build a cfl page update command string with storage output enabled."
  (should (equal (hub/confluence-api--page-update-command "123")
		 "cfl page edit 123 --storage")))

(ert-deftest hub/confluence-api--page-create-command ()
  "Build a cfl page create command string with space, title, and storage flag."
  (should (equal (hub/confluence-api--page-create-command "ENG" "Roadmap")
		 "cfl page create --space ENG --title Roadmap --storage")))

(ert-deftest hub/confluence-api--page-id-from-buffer ()
  "Read CONFLUENCE_PAGE_ID from the current Org buffer."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n* Title"
   (lambda ()
     (should (equal (hub/confluence-api--page-id-from-buffer) "123")))))

(ert-deftest hub/confluence-api--page-id-from-subtree-property ()
  "Read CONFLUENCE_PAGE_ID from the current Org subtree property."
  (hub/confluence-api-test--with-org-buffer
   "* Page\n:PROPERTIES:\n:CONFLUENCE_PAGE_ID: 456\n:END:\nBody"
   (lambda ()
     (org-back-to-heading)
     (should (equal (hub/confluence-api--page-id-from-buffer t) "456")))))

(ert-deftest hub/confluence-api--space-from-buffer ()
  "Read CONFLUENCE_SPACE from the current Org buffer."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_SPACE: ENG\n* Title"
   (lambda ()
     (should (equal (hub/confluence-api--space-from-buffer) "ENG")))))

(ert-deftest hub/confluence-api--page-update-command-with-file ()
  "Build a cfl page update command that reads XHTML from a file."
  (should (equal (hub/confluence-api--page-update-command "123" "/tmp/page.xhtml")
		 "cfl page edit 123 --file /tmp/page.xhtml --storage")))

(ert-deftest hub/confluence-api--page-create-command-with-parent ()
  "Build a cfl page create command with a parent page ID."
  (should (equal (hub/confluence-api--page-create-command "ENG" "Roadmap" nil "456")
		 "cfl page create --space ENG --title Roadmap --parent 456 --storage")))

(ert-deftest hub/confluence-api--page-create-missing-space ()
  "Signal an error when creating a page without a space key."
  (should-error (hub/confluence-api--page-create-command nil "Roadmap") :type 'user-error))

(ert-deftest hub/confluence-api--page-update-missing-id ()
  "Signal an error when updating a page without a page ID."
  (should-error (hub/confluence-api--page-update-command nil) :type 'user-error))

(ert-deftest hub/confluence-api--attachment-upload-command ()
  "Build a cfl attachment upload command string."
  (should (equal (hub/confluence-api--attachment-upload-command "123" "/tmp/foo bar.png")
		 "cfl attachment upload --page 123 --file /tmp/foo\\ bar.png")))

(ert-deftest hub/confluence-publish-from-export-dispatch-passes-options ()
  "Publish from Org export dispatch with the dispatcher subtree flag."
  (let ((received nil))
    (cl-letf (((symbol-function 'hub/confluence-publish)
	       (lambda (&rest args)
		 (setq received args))))
      (hub/confluence-publish-from-export-dispatch nil t nil t)
      (should (equal received '(nil t nil t nil))))))

(ert-deftest hub/confluence-publish-uses-subtree-page-id-and-export ()
  "Publish a subtree selected through Org export dispatch."
  (hub/confluence-api-test--with-org-buffer
   "* Page\n:PROPERTIES:\n:CONFLUENCE_PAGE_ID: 456\n:END:\nBody"
   (lambda ()
     (let ((commands nil)
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (progn
	     (org-back-to-heading)
	     (cl-letf (((symbol-function 'org-confluence-export)
			(lambda (_async subtreep visible-only body-only ext-plist)
			  (should subtreep)
			  (should-not visible-only)
			  (should body-only)
			  (should (plist-member ext-plist :confluence-image-filenames))
			  "<p>subtree</p>"))
		       ((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		       ((symbol-function 'hub/confluence-commands--write-temp-xhtml) (lambda (_xhtml) xhtml-file))
		       ((symbol-function 'hub/confluence-commands--run) (lambda (command) (push command commands) 0)))
	       (hub/confluence-publish nil t nil t nil)
	       (should (equal commands (list (format "cfl page edit 456 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(ert-deftest hub/confluence-publish-uploads-images-before-page-edit ()
  "Upload all referenced images before editing the Confluence page."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n[[./foo.png]]"
   (lambda ()
     (let ((commands nil)
	   (source-file (make-temp-file "org-confluence-source-" nil ".png"))
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (progn
	     (with-temp-file source-file (insert "png"))
	     (cl-letf (((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
		       ((symbol-function 'org-confluence-image-assets)
			(lambda (&rest _)
			  (list (list :path source-file
				      :source-path source-file
				      :source-link "./foo.png"
				      :filename "foo-hash.png"))))
		       ((symbol-function 'hub/confluence-commands--write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'hub/confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'hub/confluence-commands--run)
			(lambda (command) (push command commands) 0)))
	       (hub/confluence-publish)
	       (should (equal (nreverse commands)
			      (list "upload:123:foo-hash.png"
				    (format "cfl page edit 123 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p source-file)
	   (delete-file source-file))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(ert-deftest hub/confluence-publish-images-require-page-id ()
  "Reject image documents in create flow for this iteration."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_SPACE: ENG\n[[./foo.png]]"
   (lambda ()
     (cl-letf (((symbol-function 'org-confluence-image-assets)
		(lambda () (list (list :path "/tmp/foo.png" :source-path "/tmp/foo.png" :source-link "./foo.png" :filename "foo-hash.png")))))
       (should-error (hub/confluence-publish-dwim "Page") :type 'user-error)))))

(ert-deftest hub/confluence-commands--run-reports-command-output ()
  "Report command output explicitly when a cfl command fails."
  (cl-letf (((symbol-function 'hub/confluence-api--cfl-available-p) (lambda () t))
	    ((symbol-function 'process-file)
	     (lambda (_program _infile buffer _display &rest _args)
	       (with-current-buffer buffer
		 (insert "explicit failure"))
	       1)))
    (should-error (hub/confluence-commands--run "cfl fail") :type 'user-error)))

(ert-deftest hub/confluence-publish-continues-when-hashed-attachment-exists ()
  "Continue publishing when uploading an already-present hashed attachment."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n[[./foo.png]]"
   (lambda ()
     (let ((commands nil)
	   (source-file (make-temp-file "org-confluence-source-" nil ".png"))
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (progn
	     (with-temp-file source-file (insert "png"))
	     (cl-letf (((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
		       ((symbol-function 'org-confluence-image-assets)
			(lambda (&rest _)
			  (list (list :path source-file
				      :source-path source-file
				      :source-link "./foo.png"
				      :filename "foo-hash.png"))))
		       ((symbol-function 'hub/confluence-commands--write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'hub/confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'hub/confluence-commands--run)
			(lambda (command)
			  (push command commands)
			  (when (string-prefix-p "upload:" command)
			    (user-error "Cannot add a new attachment with same file name as an existing attachment: foo-hash.png"))
			  0)))
	       (hub/confluence-publish)
	       (should (equal (nreverse commands)
			      (list "upload:123:foo-hash.png"
				    (format "cfl page edit 123 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p source-file)
	   (delete-file source-file))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(ert-deftest hub/confluence-publish-cleans-temp-xhtml-on-upload-failure ()
  "Delete temporary XHTML when an image upload fails."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n[[./foo.png]]"
   (lambda ()
     (let ((commands nil)
	   (source-file (make-temp-file "org-confluence-source-" nil ".png"))
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (progn
	     (with-temp-file source-file (insert "png"))
	     (cl-letf (((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
		       ((symbol-function 'org-confluence-image-assets)
			(lambda (&rest _)
			  (list (list :path source-file
				      :source-path source-file
				      :source-link "./foo.png"
				      :filename "foo-hash.png"))))
		       ((symbol-function 'hub/confluence-commands--write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'hub/confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'hub/confluence-commands--run)
			(lambda (command)
			  (push command commands)
			  (user-error "upload failed"))))
	       (should-error (hub/confluence-publish) :type 'user-error)
	       (should-not (file-exists-p xhtml-file))
	       (should (equal commands (list "upload:123:foo-hash.png")))))
	 (when (file-exists-p source-file)
	   (delete-file source-file)))))))

(provide 'org-confluence-api-test)
;;; org-confluence-api-test.el ends here
