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

(ert-deftest hub/confluence-api--page-view-storage-command ()
  "Build a cfl page view command that returns raw storage XHTML only."
  (should (equal (hub/confluence-api--page-view-storage-command "123")
		 "cfl page view 123 --raw --content-only")))

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

(ert-deftest hub/confluence-import-storage-to-org-basic ()
  "Convert basic Confluence storage XHTML to Org text."
  (should (equal (hub/confluence-import-storage-to-org
		  "<h1>Title</h1><p>Hello <strong>world</strong> and <a href=\"https://example.com\">link</a>.</p><ul><li>One</li><li>Two</li></ul>")
		 "* Title\nHello *world* and [[https://example.com][link]].\n- One\n- Two")))

(ert-deftest hub/confluence-import-storage-to-org-nested-lists ()
  "Convert nested Confluence storage lists to nested Org lists."
  (should (equal (hub/confluence-import-storage-to-org
		  "<ul><li>Parent<ul><li>Child</li></ul></li><li>Second</li></ul>")
		 "- Parent\n  - Child\n- Second")))

(ert-deftest hub/confluence-import-storage-to-org-nested-ordered-lists ()
  "Convert nested ordered storage lists to nested Org lists."
  (should (equal (hub/confluence-import-storage-to-org
		  "<ol><li><p>Parent</p><ol><li><p>Child</p></li></ol></li><li><p>Second</p></li></ol>")
		 "1. Parent\n  1. Child\n2. Second")))

(ert-deftest hub/confluence-import-storage-to-org-table ()
  "Convert Confluence storage tables to Org tables."
  (should (equal (hub/confluence-import-storage-to-org
		  "<table><tbody><tr><th><p>Name</p></th><th><p>Score</p></th></tr><tr><td><p>Ada</p></td><td><p>10</p></td></tr><tr><td><p>Bo</p></td><td><p>8</p></td></tr></tbody></table>")
		 "| Name | Score |\n|------+-------|\n| Ada | 10 |\n| Bo | 8 |")))

(ert-deftest hub/confluence-import-storage-to-org-table-inline-markup ()
  "Preserve inline markup inside imported Org table cells."
  (should (equal (hub/confluence-import-storage-to-org
		  "<table><tbody><tr><th>Item</th></tr><tr><td><strong>Bold</strong> and <a href=\"https://example.com\">link</a></td></tr></tbody></table>")
		 "| Item |\n|------|\n| *Bold* and [[https://example.com][link]] |")))

(ert-deftest hub/confluence-import-storage-to-org-table-trims-bold-cell-text ()
  "Trim storage whitespace inside bold table cell text."
  (should (equal (hub/confluence-import-storage-to-org
		  "<table><tbody><tr><th><p><strong> T ime</strong></p></th></tr></tbody></table>")
		 "| *T ime* |\n|---------|")))

(ert-deftest hub/confluence-import-storage-to-org-structured-macro-body ()
  "Import structured macro rich text body without macro parameters."
  (should (equal (hub/confluence-import-storage-to-org
		  "<ac:structured-macro ac:name=\"panel\"><ac:parameter ac:name=\"bgColor\">#fff</ac:parameter><ac:rich-text-body><p>Body</p><ul><li><p>Point</p></li></ul></ac:rich-text-body></ac:structured-macro><table><tbody><tr><th>H</th></tr></tbody></table>")
		 "Body\n- Point\n| H |\n|---|")))

(ert-deftest hub/confluence-import-storage-to-org-status-macro ()
  "Import a Confluence status macro as a status Org link."
  (should (equal (hub/confluence-import-storage-to-org
		  "<p><ac:structured-macro ac:name=\"status\"><ac:parameter ac:name=\"title\">Medium</ac:parameter><ac:parameter ac:name=\"colour\">Purple</ac:parameter></ac:structured-macro></p>")
		 "[[confluence-status:Purple][Medium]]")))

(ert-deftest hub/confluence-import-storage-to-org-emoji-fallback ()
  "Import Confluence emoticons as their Unicode fallback."
  (should (equal (hub/confluence-import-storage-to-org
		  "<p><ac:emoticon ac:name=\"blue-star\" ac:emoji-shortname=\":calendar:\" ac:emoji-id=\"1f4c6\" ac:emoji-fallback=\"📆\" /> Plan</p>")
		 "📆 Plan")))

(ert-deftest hub/confluence-import-storage-to-org-emoji-shortname-fallback ()
  "Fallback to emoji shortname when Unicode fallback is absent."
  (should (equal (hub/confluence-import-storage-to-org
		  "<p><ac:emoticon ac:name=\"unknown\" ac:emoji-shortname=\":unknown:\" /> Info</p>")
		 ":unknown: Info")))

(ert-deftest hub/confluence-import-storage-to-org-atlassian-info-emoji ()
  "Map Atlassian info emoticon metadata to a Unicode emoji."
  (should (equal (hub/confluence-import-storage-to-org
		  "<h2><ac:emoticon ac:name=\"information\" ac:emoji-shortname=\":info:\" ac:emoji-id=\"atlassian-info\" ac:emoji-fallback=\":info:\" /> Context</h2>")
		 "** ℹ️ Context")))

(ert-deftest hub/confluence-pull-opens-import-buffer ()
  "Fetch raw storage XHTML and open a converted Org buffer."
  (let ((opened nil))
    (cl-letf (((symbol-function 'hub/confluence-commands--run-output)
	       (lambda (command)
		 (should (equal command "cfl page view 123 --raw --content-only"))
		 "<h1>Title</h1><p>Body</p>"))
	      ((symbol-function 'pop-to-buffer)
	       (lambda (buffer &rest _)
		 (setq opened buffer))))
      (hub/confluence-pull "123")
      (unwind-protect
	  (with-current-buffer opened
	    (should (derived-mode-p 'org-mode))
	    (should (equal (buffer-string)
			   "#+CONFLUENCE_PAGE_ID: 123\n\n* Title\nBody\n")))
	(when (buffer-live-p opened)
	  (kill-buffer opened))))))

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
