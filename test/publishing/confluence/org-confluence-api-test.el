;;; org-confluence-api-test.el --- Tests for Org Confluence cfl API wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavior tests for Confluence publish command construction.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'org)

;; Ensure repo modules are reachable for isolated batch test runners.
(let ((root (file-name-as-directory
	     (locate-dominating-file (or load-file-name buffer-file-name)
				     "domains.yaml"))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root))
  (add-to-list 'load-path (expand-file-name "packages/org-comments" root))
  (add-to-list 'load-path (expand-file-name "packages/org-confluence" root))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "core" root)))

(require 'org-comments)
(require 'org-confluence-api)
(require 'org-confluence-commands)

(defun org-confluence-api-test--with-org-buffer (contents thunk)
  "Run THUNK in a temporary Org buffer containing CONTENTS."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (let ((org-confluence-publish--skip-inline-comment-preflight t))
      (funcall thunk))))

(defun org-confluence-api-test--page-response (id title version storage &optional metadata)
  "Return a fake page response for ID, TITLE, VERSION, STORAGE, and METADATA."
  (let ((page `((id . ,id)
		(title . ,title)
		(version . ((number . ,version)))
		(body . ((storage . ((value . ,storage))))))))
    (when-let* ((space (plist-get metadata :space)))
      (push `(space . ((key . ,space))) page))
    (when-let* ((base (plist-get metadata :base-url))
		(webui (plist-get metadata :webui)))
      (push `(_links . ((base . ,base) (webui . ,webui))) page))
    `(:status 200 :body ,(json-encode (nreverse page)))))

(ert-deftest org-confluence-api--page-update-command ()
  "Build a cfl page update command string with storage output enabled."
  (should (equal (org-confluence-api--page-update-command "123")
		 "cfl page edit 123 --storage")))

(ert-deftest org-confluence-api--page-create-command ()
  "Build a cfl page create command string with space, title, and storage flag."
  (should (equal (org-confluence-api--page-create-command "ENG" "Roadmap")
		 "cfl page create --space ENG --title Roadmap --storage")))

(ert-deftest org-confluence-inline-comments-preserve-marker-exact-selection ()
  "Reinsert a Confluence inline comment marker around matching exported text."
  (let* ((old "<p>Alpha <ac:inline-comment-marker ac:ref=\"m1\">selected text</ac:inline-comment-marker> omega.</p>")
	 (new "<p>Alpha selected text omega.</p>")
	 (comments '((status . "current")
		     (resolutionStatus . "open")
		     (properties . ((inlineMarkerRef . "m1")
				    (inlineOriginalSelection . "selected text"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new old (list comments))
		   old))))

(ert-deftest org-confluence-inline-comments-preserve-marker-uses-context ()
  "Use old marker context to choose between repeated selected text."
  (let* ((old "<p>First selected text.</p><p>Alpha <ac:inline-comment-marker ac:ref=\"m1\">selected text</ac:inline-comment-marker> omega.</p>")
	 (new "<p>First selected text.</p><p>Alpha selected text omega.</p>")
	 (comments '((resolutionStatus . "open")
		     (properties . ((inlineMarkerRef . "m1")
				    (inlineOriginalSelection . "selected text"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new old (list comments))
		   old))))

(ert-deftest org-confluence-inline-comments-preserve-marker-across-formatting ()
  "Match plain selected text across inline storage formatting tags."
  (let* ((new "<p>aaa <strong>bold</strong> bbb</p>")
	 (expected "<p><ac:inline-comment-marker ac:ref=\"m1\">aaa <strong>bold</strong> bbb</ac:inline-comment-marker></p>")
	 (comments '((resolutionStatus . "open")
		     (properties . ((inlineMarkerRef . "m1")
				    (inlineOriginalSelection . "aaa bold bbb"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new new (list comments))
		   expected))))

(ert-deftest org-confluence-inline-comments-preserve-marker-inside-formatting ()
  "Match selected text inside an inline formatting tag."
  (let* ((new "<p><strong>bold text</strong></p>")
	 (expected "<p><strong><ac:inline-comment-marker ac:ref=\"m1\">bold</ac:inline-comment-marker> text</strong></p>")
	 (comments '((resolutionStatus . "open")
		     (properties . ((inlineMarkerRef . "m1")
				    (inlineOriginalSelection . "bold"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new new (list comments))
		   expected))))

(ert-deftest org-confluence-inline-comments-preserve-marker-across-entities ()
  "Match selected text across decoded storage entities and inline tags."
  (let* ((new "<p>aaa <strong>Tom &amp; Jerry</strong> bbb</p>")
	 (expected "<p><ac:inline-comment-marker ac:ref=\"m1\">aaa <strong>Tom &amp; Jerry</strong> bbb</ac:inline-comment-marker></p>")
	 (comments '((resolutionStatus . "open")
		     (properties . ((inlineMarkerRef . "m1")
				    (inlineOriginalSelection . "aaa Tom & Jerry bbb"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new new (list comments))
		   expected))))

(ert-deftest org-confluence-inline-comments-preserve-marker-matches-nbsp-as-space ()
  "Match a normal-space selected text against a storage non-breaking space."
  (let* ((new "<p>Ready&nbsp;in days</p>")
	 (expected "<p><ac:inline-comment-marker ac:ref=\"m1\">Ready&nbsp;in</ac:inline-comment-marker> days</p>")
	 (comments '((resolutionStatus . "open")
		     (properties . ((inlineMarkerRef . "m1")
				    (inlineOriginalSelection . "Ready in"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new new (list comments))
		   expected))))

(ert-deftest org-confluence-inline-comments-candidates-skip-macro-parameters ()
  "Do not insert markers into Confluence macro parameters."
  (let ((storage "<ac:structured-macro ac:name=\"info\"><ac:parameter ac:name=\"title\">Danger</ac:parameter><ac:rich-text-body><p>Safe Danger</p></ac:rich-text-body></ac:structured-macro>"))
    (should (equal (org-confluence-inline-comments-inline-comment-candidates storage "Danger")
		   '((:start 114 :end 120))))))

(ert-deftest org-confluence-inline-comments-candidates-allow-rich-text-body ()
  "Allow markers inside Confluence rich-text macro bodies."
  (let* ((storage "<ac:structured-macro ac:name=\"info\"><ac:rich-text-body><p>Panel text</p></ac:rich-text-body></ac:structured-macro>")
	 (comments '((resolutionStatus . "open")
		     (properties . ((inlineMarkerRef . "m1")
				    (inlineOriginalSelection . "Panel text")))))
	 (result (org-confluence-inline-comments-insert-inline-comment-markers
		  storage storage (list comments))))
    (should (string-match-p "<ac:inline-comment-marker ac:ref=\"m1\">Panel text</ac:inline-comment-marker>" result))))

(ert-deftest org-confluence-inline-comments-candidates-skip-plain-text-body ()
  "Do not insert markers into Confluence plain-text macro bodies."
  (let ((storage "<ac:structured-macro ac:name=\"code\"><ac:plain-text-body><![CDATA[Panel text]]></ac:plain-text-body></ac:structured-macro>"))
    (should-not (org-confluence-inline-comments-inline-comment-candidates storage "Panel text"))))

(ert-deftest org-confluence-inline-comments-candidates-allow-link-body ()
  "Allow markers inside Confluence rich link bodies."
  (let ((storage "<p><ac:link><ri:page ri:content-title=\"Target\"/><ac:link-body>Linked text</ac:link-body></ac:link></p>"))
    (should (org-confluence-inline-comments-inline-comment-candidates storage "Linked text"))))

(ert-deftest org-confluence-inline-comments-candidates-skip-plain-text-link-body ()
  "Do not insert markers into Confluence plain-text link bodies."
  (let ((storage "<p><ac:link><ri:page ri:content-title=\"Target\"/><ac:plain-text-link-body><![CDATA[Linked text]]></ac:plain-text-link-body></ac:link></p>"))
    (should-not (org-confluence-inline-comments-inline-comment-candidates storage "Linked text"))))

(ert-deftest org-confluence-inline-comments-candidates-skip-cross-paragraph-selection ()
  "Do not synthesize one marker across paragraph boundaries."
  (let ((storage "<p>Alpha</p><p>Beta</p>"))
    (should-not (org-confluence-inline-comments-inline-comment-candidates storage "AlphaBeta"))))

(ert-deftest org-confluence-inline-comments-candidates-skip-cross-table-cell-selection ()
  "Do not synthesize one marker across table cell boundaries."
  (let ((storage "<table><tbody><tr><td><p>Alpha</p></td><td><p>Beta</p></td></tr></tbody></table>"))
    (should-not (org-confluence-inline-comments-inline-comment-candidates storage "AlphaBeta"))))

(ert-deftest org-confluence-inline-comments-complex-marker-ref-reasons-detects-nesting-and-splits ()
  "Detect marker refs whose current Confluence topology is complex."
  (let* ((storage "<p><ac:inline-comment-marker ac:ref=\"outer\">Alpha <ac:inline-comment-marker ac:ref=\"inner\">Beta</ac:inline-comment-marker> Gamma</ac:inline-comment-marker><ac:inline-comment-marker ac:ref=\"outer\">.</ac:inline-comment-marker></p>")
	 (reasons (org-confluence-inline-comments-complex-inline-marker-ref-reasons storage)))
    (should (equal (cdr (assoc-string "outer" reasons t))
		   "split inline marker topology"))
    (should (equal (cdr (assoc-string "inner" reasons t))
		   "nested inline marker topology"))))

(ert-deftest org-confluence-inline-comments-preserve-marker-nested-topology ()
  "Preserve nested Confluence inline marker topology."
  (let* ((old "<p><ac:inline-comment-marker ac:ref=\"outer\">Alpha <ac:inline-comment-marker ac:ref=\"inner\">Beta</ac:inline-comment-marker> Gamma</ac:inline-comment-marker></p>")
	 (new "<p>Alpha Beta Gamma</p>")
	 (outer '((resolutionStatus . "open")
		  (properties . ((inlineMarkerRef . "outer")
				 (inlineOriginalSelection . "Alpha Beta Gamma")))))
	 (inner '((resolutionStatus . "open")
		  (properties . ((inlineMarkerRef . "inner")
				 (inlineOriginalSelection . "Beta"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new old (list outer inner))
		   old))))

(ert-deftest org-confluence-inline-comments-preserve-marker-split-topology ()
  "Preserve split same-ref marker topology around nested comments."
  (let* ((old "<p><ac:inline-comment-marker ac:ref=\"outer\">This </ac:inline-comment-marker><ac:inline-comment-marker ac:ref=\"inner\"><ac:inline-comment-marker ac:ref=\"outer\">matters</ac:inline-comment-marker></ac:inline-comment-marker><ac:inline-comment-marker ac:ref=\"outer\">.</ac:inline-comment-marker></p>")
	 (new "<p>This matters.</p>")
	 (outer '((resolutionStatus . "open")
		  (properties . ((inlineMarkerRef . "outer")
				 (inlineOriginalSelection . "This matters.")))))
	 (inner '((resolutionStatus . "open")
		  (properties . ((inlineMarkerRef . "inner")
				 (inlineOriginalSelection . "matters"))))))
    (should (equal (org-confluence-inline-comments-insert-inline-comment-markers
		    new old (list outer inner))
		   old))))

(ert-deftest org-confluence-publish-preflight-preserves-complex-active-marker-topology ()
  "Mark pages with complex active marker topology for preservation."
  (with-temp-buffer
    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha Beta Gamma.\n")
    (org-mode)
    (let ((buffer-file-name "/tmp/confluence-complex.org")
	  (org-confluence-publish--pages-needing-inline-marker-preservation nil))
      (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		 (lambda (&rest _args)
		   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"outer\",\"inlineOriginalSelection\":\"Alpha Beta Gamma\"}}]}")))
		((symbol-function 'org-confluence-api--get-page)
		 (lambda (&rest _args)
		   '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p><ac:inline-comment-marker ac:ref=\\\"outer\\\">Alpha <ac:inline-comment-marker ac:ref=\\\"inner\\\">Beta</ac:inline-comment-marker> Gamma</ac:inline-comment-marker></p>\"}}}")))
		((symbol-function 'org-confluence-comments-import-remote-comments)
		 (lambda (&rest _args) (list :imported 0)))
		((symbol-function 'org-confluence-publish--preflight-report-buffer)
		 (lambda (&rest _args) nil)))
	(let ((report (org-confluence-publish--inline-comment-preflight '("123"))))
	  (should-not (plist-get report :blockers))
	  (should (member "123" org-confluence-publish--pages-needing-inline-marker-preservation)))))))

(ert-deftest org-confluence-inline-repair-uses-sidecar-occurrence-index ()
  "Choose the storage occurrence matching sidecar source anchor occurrence."
  (let* ((dir (make-temp-file "hub-confluence-repair-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (storage "<ol><li>First ordered item</li><li>Second ordered item</li><li>Third ordered item</li></ol>")
	 (comment '((id . "i1")
		    (resolutionStatus . "open")
		    (properties . ((inlineMarkerRef . "m1")
				   (inlineOriginalSelection . "ordered"))))))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\n1. First ordered item\n2. Second ordered item\n3. Third ordered item\n"))
	  (let ((target-start nil))
	    (with-temp-buffer
	      (insert-file-contents source)
	      (goto-char (point-min))
	      (search-forward "Third ")
	      (setq target-start (point)))
	    (with-temp-file sidecar
	      (insert "#+source: article.org\n* OPEN Remote\n:PROPERTIES:\n:ORG_COMMENTS_REMOTE_ID: i1\n:ORG_COMMENTS_TARGET_TEXT: ordered\n")
	      (insert (format ":ORG_COMMENTS_TARGET: %s %s\n" target-start (+ target-start 7)))
	      (insert ":END:\n")))
	  (let ((candidate (org-confluence-inline-repair-candidate-for-comment storage comment source)))
	    (should (equal (plist-get candidate :start) 69))
	    (should (equal (substring storage (plist-get candidate :start) (plist-get candidate :end))
			   "ordered"))))
      (delete-directory dir t))))

(ert-deftest org-confluence-inline-repair-comment-anchors-applies-safe-repairs ()
  "Repair command writes marker-restored storage and updates the page on apply."
  (let ((commands nil)
	(written nil))
    (cl-letf (((symbol-function 'org-confluence-comments-page-id-or-read)
	       (lambda (&optional _page-id) "123"))
	      ((symbol-function 'org-confluence-api--get-page)
	       (lambda (&rest _args)
		 '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p>Alpha selected text omega.</p>\"}}}")))
	      ((symbol-function 'org-confluence-api--list-page-comments)
	       (lambda (&rest _args)
		 '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"m1\",\"inlineOriginalSelection\":\"selected text\"}}]}")))
	      ((symbol-function 'org-confluence-inline-repair-run)
	       (lambda (command) (push command commands) 0)))
      (let ((report (org-confluence-inline-repair-comment-anchors "123" t)))
	(setq written (plist-get report :repair-file))
	(should (= 1 (length (plist-get report :repaired))))
	(should (equal (length commands) 1))
	(with-temp-buffer
	  (insert-file-contents written)
	  (should (search-forward "<ac:inline-comment-marker ac:ref=\"m1\">selected text</ac:inline-comment-marker>" nil t)))))))

(ert-deftest org-confluence-api--page-id-from-buffer ()
  "Read CONFLUENCE_PAGE_ID from the current Org buffer."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n* Title"
   (lambda ()
     (should (equal (org-confluence-api--page-id-from-buffer) "123")))))

(ert-deftest org-confluence-api--page-id-from-subtree-property ()
  "Read CONFLUENCE_PAGE_ID from the current Org subtree property."
  (org-confluence-api-test--with-org-buffer
   "* Page\n:PROPERTIES:\n:CONFLUENCE_PAGE_ID: 456\n:END:\nBody"
   (lambda ()
     (org-back-to-heading)
     (should (equal (org-confluence-api--page-id-from-buffer t) "456")))))

(ert-deftest org-confluence-api--space-from-buffer ()
  "Read CONFLUENCE_SPACE from the current Org buffer."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_SPACE: ENG\n* Title"
   (lambda ()
     (let ((org-confluence-api-default-space "~personal"))
       (should (equal (org-confluence-api--space-from-buffer) "ENG"))))))

(ert-deftest org-confluence-api--space-from-default ()
  "Read default Confluence space when Org buffer has no space keyword."
  (org-confluence-api-test--with-org-buffer
   "* Title"
   (lambda ()
     (let ((org-confluence-api-default-space "~personal"))
       (should (equal (org-confluence-api--space-from-buffer) "~personal"))))))

(ert-deftest org-confluence-api--page-url ()
  "Build browser URLs from Confluence page metadata."
  (let ((org-confluence-api-base-url "https://example.atlassian.net/"))
    (should (equal (org-confluence-api--page-url "123" "ENG")
		   "https://example.atlassian.net/wiki/spaces/ENG/pages/123"))))

(ert-deftest org-confluence-api--page-url-strips-wiki-suffix ()
  "Accept a Confluence base URL that already includes /wiki."
  (let ((org-confluence-api-base-url "https://example.atlassian.net/wiki"))
    (should (equal (org-confluence-api--page-url "123")
		   "https://example.atlassian.net/wiki/pages/123"))))

(ert-deftest org-confluence-api--comment-url-focuses-comment ()
  "Build browser URLs that focus a Confluence comment on a page."
  (let ((org-confluence-api-base-url "https://example.atlassian.net/"))
    (should (equal (org-confluence-api--comment-url "123" "c456" "ENG")
		   "https://example.atlassian.net/wiki/spaces/ENG/pages/123?focusedCommentId=c456"))))

(ert-deftest org-confluence-api--read-cfl-config ()
  "Read Confluence Cloud REST credentials from a cfl config file."
  (let ((config-file (make-temp-file "hub-cfl-config-" nil ".yml")))
    (unwind-protect
	(progn
	  (with-temp-file config-file
	    (insert "base_url: https://example.atlassian.net/wiki\n")
	    (insert "cloud_id: cloud-123\n")
	    (insert "email: author@example.com\n")
	    (insert "api_token: secret-token\n"))
	  (should (equal (org-confluence-api--read-cfl-config config-file)
			 '(:base-url "https://example.atlassian.net"
				     :cloud-id "cloud-123"
				     :email "author@example.com"
				     :api-token "secret-token"))))
      (delete-file config-file))))

(ert-deftest org-confluence-api--read-cfl-config-cloud-id-optional ()
  "Read cfl configs that omit cloud ID when base URL auth is available."
  (let ((config-file (make-temp-file "hub-cfl-config-" nil ".yml")))
    (unwind-protect
	(progn
	  (with-temp-file config-file
	    (insert "base_url: https://example.atlassian.net\n")
	    (insert "email: author@example.com\n")
	    (insert "api_token: secret-token\n"))
	  (should (equal (org-confluence-api--read-cfl-config config-file)
			 '(:base-url "https://example.atlassian.net"
				     :cloud-id nil
				     :email "author@example.com"
				     :api-token "secret-token"))))
      (delete-file config-file))))

(ert-deftest org-confluence-api--read-cfl-config-missing-required-field-redacts-token ()
  "Report missing required cfl config fields without leaking secrets."
  (let ((config-file (make-temp-file "hub-cfl-config-" nil ".yml")))
    (unwind-protect
	(progn
	  (with-temp-file config-file
	    (insert "base_url: https://example.atlassian.net\n")
	    (insert "cloud_id: cloud-123\n")
	    (insert "api_token: secret-token\n"))
	  (should-error (org-confluence-api--read-cfl-config config-file)
			:type 'user-error)
	  (condition-case error
	      (org-confluence-api--read-cfl-config config-file)
	    (user-error
	     (should (string-match-p "email" (error-message-string error)))
	     (should-not (string-match-p "secret-token" (error-message-string error))))))
      (delete-file config-file))))

(ert-deftest org-confluence-api--comment-list-url ()
  "Build Confluence Cloud REST URLs for page comments."
  (let ((config '(:base-url "https://example.atlassian.net/wiki")))
    (should (equal (org-confluence-api--comment-list-url
		    "123" "inline-comments" "storage" config)
		   "https://example.atlassian.net/wiki/api/v2/pages/123/inline-comments?body-format=storage"))))

(ert-deftest org-confluence-api--child-pages-url ()
  "Build Confluence Cloud REST URLs for page and folder children."
  (let ((config '(:base-url "https://example.atlassian.net/wiki")))
    (should (equal (org-confluence-api--child-pages-url "123" nil config)
		   "https://example.atlassian.net/wiki/api/v2/pages/123/direct-children"))
    (should (equal (org-confluence-api--child-pages-url "123" 50 config)
		   "https://example.atlassian.net/wiki/api/v2/pages/123/direct-children?limit=50"))
    (should (equal (org-confluence-api--folder-children-url "456" 50 config)
		   "https://example.atlassian.net/wiki/api/v2/folders/456/direct-children?limit=50"))))

(ert-deftest org-confluence-api--rest-request-uses-authenticated-transport ()
  "Send REST requests through the injected transport without network access."
  (let* ((received nil)
	 (org-confluence-api--rest-transport
	  (lambda (method url headers body)
	    (setq received (list method url headers body))
	    '(:status 200 :body "{}"))))
    (should (equal (org-confluence-api--rest-request
		    "GET"
		    "https://example.atlassian.net/wiki/api/v2/pages/123/inline-comments"
		    nil
		    '(:email "author@example.com" :api-token "secret-token"))
		   '(:status 200 :body "{}")))
    (cl-destructuring-bind (method url headers body) received
      (should (equal method "GET"))
      (should (equal url "https://example.atlassian.net/wiki/api/v2/pages/123/inline-comments"))
      (should-not body)
      (should (equal (alist-get "Accept" headers nil nil #'equal) "application/json"))
      (should (string-prefix-p "Basic " (alist-get "Authorization" headers nil nil #'equal)))
      (should-not (string-match-p "secret-token"
				  (alist-get "Authorization" headers nil nil #'equal))))))

(ert-deftest org-confluence-api--users-bulk-uses-rest-request ()
  "Look up users through Confluence users-bulk without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"results\":[]}"))))
      (should (equal (org-confluence-api--users-bulk
		      '("acct-1" "acct-2") '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"results\":[]}")))
      (should (equal received
		     '("POST"
		       "https://example.atlassian.net/wiki/api/v2/users-bulk"
		       "{\"accountIds\":[\"acct-1\",\"acct-2\"]}"
		       (:base-url "https://example.atlassian.net")))))))

(ert-deftest org-confluence-api--list-page-comments-uses-read-only-rest-request ()
  "List page comments through the REST wrapper without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"results\":[]}"))))
      (should (equal (org-confluence-api--list-page-comments
		      "123" "footer-comments" "storage" '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"results\":[]}")))
      (should (equal received
		     '("GET"
		       "https://example.atlassian.net/wiki/api/v2/pages/123/footer-comments?body-format=storage"
		       nil
		       (:base-url "https://example.atlassian.net")))))))

(ert-deftest org-confluence-api--list-child-pages-uses-read-only-rest-request ()
  "List child pages through the REST wrapper without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"results\":[]}"))))
      (should (equal (org-confluence-api--list-child-pages
		      "123" 50 '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"results\":[]}")))
      (should (equal received
		     '("GET"
		       "https://example.atlassian.net/wiki/api/v2/pages/123/direct-children?limit=50"
		       nil
		       (:base-url "https://example.atlassian.net")))))))

(ert-deftest org-confluence-api--list-folder-children-uses-read-only-rest-request ()
  "List folder children through the REST wrapper without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"results\":[]}"))))
      (should (equal (org-confluence-api--list-folder-children
		      "456" 50 '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"results\":[]}")))
      (should (equal received
		     '("GET"
		       "https://example.atlassian.net/wiki/api/v2/folders/456/direct-children?limit=50"
		       nil
		       (:base-url "https://example.atlassian.net")))))))

(ert-deftest org-confluence-api--url-transport-encodes-request-body-as-utf-8 ()
  "Encode multibyte JSON request bodies before handing them to url.el."
  (let ((captured nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
	       (lambda (_url &rest _args)
		 (setq captured url-request-data)
		 (generate-new-buffer " *hub-url-utf8-test*"))))
      (let ((buffer (org-confluence-api--url-transport
		     "POST" "https://example.test" nil
		     "{\"value\":\"réponse fière\"}")))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))
    (should (not (multibyte-string-p captured)))
    (should (equal (decode-coding-string captured 'utf-8)
		   "{\"value\":\"réponse fière\"}"))))

(ert-deftest org-confluence-api--rest-request-normalizes-url-buffer-response ()
  "Normalize url.el response buffers into plist responses."
  (let* ((url-buffer (generate-new-buffer " *hub-url-response-test*"))
	 (org-confluence-api--rest-transport
	  (lambda (&rest _args)
	    url-buffer)))
    (with-current-buffer url-buffer
      (setq-local url-http-response-status 200)
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"results\":[]}"))
    (should (equal (org-confluence-api--rest-request
		    "GET" "https://example.test" nil
		    '(:email "author@example.com" :api-token "secret-token"))
		   '(:status 200 :body "{\"results\":[]}")))
    (should-not (buffer-live-p url-buffer))))

(ert-deftest org-confluence-people-mark-current-user-caches-me ()
  "Mark authenticated Confluence user as me through the people cache."
  (let* ((global-dir (make-temp-file "hub-confluence-me-global-" t))
	 (org-confluence-people-store-directory global-dir)
	 (people-file (org-confluence-people-store-global-file)))
    (unwind-protect
	(progn
	  (cl-letf (((symbol-function 'org-confluence-api--current-user)
		     (lambda ()
		       '(:status 200 :body "{\"accountId\":\"acct-me\",\"displayName\":\"Hubert\",\"email\":\"hubert@example.com\"}"))))
	    (should (equal (org-confluence-people-mark-current-user) people-file)))
	  (with-temp-buffer
	    (insert-file-contents people-file)
	    (should (search-forward "* Hubert" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_CONFLUENCE_ACCOUNT_ID: acct-me" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_CONFLUENCE_DISPLAY_NAME: Hubert" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_CONFLUENCE_EMAIL: hubert@example.com" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_CONFLUENCE_ME: t" nil t))))
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-mark-current-user-preserves-manual-name ()
  "Marking current user preserves manual people display names."
  (let* ((global-dir (make-temp-file "hub-confluence-me-global-" t))
	 (org-confluence-people-store-directory global-dir)
	 (people-file (org-confluence-people-store-global-file)))
    (unwind-protect
	(progn
	  (org-confluence-people-store-cache-identity "acct-me" "Manual Hubert")
	  (cl-letf (((symbol-function 'org-confluence-api--current-user)
		     (lambda ()
		       '(:status 200 :body "{\"accountId\":\"acct-me\",\"displayName\":\"Remote Hubert\"}"))))
	    (org-confluence-people-mark-current-user))
	  (with-temp-buffer
	    (insert-file-contents people-file)
	    (should (search-forward ":ORG_CONFLUENCE_DISPLAY_NAME: Manual Hubert" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_ME: t" nil t))))
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-resolve-updates-unresolved-entries ()
  "Resolve cached Confluence account IDs through users-bulk."
  (let* ((dir (make-temp-file "org-confluence-people-store-resolve-" t))
	 (source (expand-file-name "article.org" dir))
	 (global-dir (make-temp-file "org-confluence-people-store-global-" t))
	 (org-confluence-people-store-directory global-dir)
	 (people-file (org-confluence-people-store-global-file)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (org-confluence-people-store-cache-identity "acct-1" nil)
	  (cl-letf (((symbol-function 'org-confluence-api--users-bulk)
		     (lambda (account-ids &optional _config)
		       (should (equal account-ids '("acct-1")))
		       '(:status 200 :body "{\"results\":[{\"accountId\":\"acct-1\",\"displayName\":\"Alice Example\",\"email\":\"alice@example.com\",\"accountStatus\":\"active\",\"timeZone\":\"Europe/Tallinn\"}]}") )))
	    (should (= 1 (org-confluence-people-resolve))))
	  (with-temp-buffer
	    (insert-file-contents people-file)
	    (should (search-forward ":ORG_CONFLUENCE_DISPLAY_NAME: Alice Example" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_EMAIL: alice@example.com" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_ACCOUNT_STATUS: active" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_TIME_ZONE: Europe/Tallinn" nil t))))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-resolve-reports-no-unresolved-people ()
  "Resolving people without unresolved entries only reports a message."
  (let* ((global-dir (make-temp-file "org-confluence-people-store-global-" t))
	 (org-confluence-people-store-directory global-dir)
	 (reported nil))
    (unwind-protect
	(progn
	  (with-temp-file (org-confluence-people-store-global-file)
	    (insert "#+title: Confluence People\n\n"))
	  (cl-letf (((symbol-function 'message)
		     (lambda (format-string &rest args)
		       (setq reported (apply #'format format-string args))))
		    ((symbol-function 'org-confluence-api--users-bulk)
		     (lambda (&rest _args)
		       (error "Should not call users-bulk without unresolved people"))))
	    (should (= 0 (org-confluence-people-resolve)))
	    (should (equal reported "No unresolved Confluence people"))))
      (delete-directory global-dir t))))

(ert-deftest org-confluence-comments-import-report-message-includes-imported-ids ()
  "Include imported remote IDs in non-zero import reports."
  (let (reported)
    (cl-letf (((symbol-function 'message)
	       (lambda (format-string &rest args)
		 (setq reported (apply #'format format-string args)))))
      (org-confluence-comments-import-report-message
       '(:imported 2 :imported-ids ("i2" "i1"))
       "Imported 0 Confluence inline comments"))
    (should (equal reported "Imported new: 2 (i1, i2)"))))

(ert-deftest org-confluence-comments-import-report-message-splits-roots-and-replies ()
  "Report imported root and reply IDs separately when available."
  (let (reported)
    (cl-letf (((symbol-function 'message)
	       (lambda (format-string &rest args)
		 (setq reported (apply #'format format-string args)))))
      (org-confluence-comments-import-report-message
       '(:imported 3
		   :imported-ids ("r2" "r1" "i1")
		   :imported-root-ids ("i1")
		   :imported-reply-ids ("r2" "r1"))
       "Imported 0 Confluence inline comments"))
    (should (equal reported "Imported roots: 1 (i1); replies: 2 (r1, r2)"))))

(ert-deftest org-confluence-comments-import-report-message-omits-zero-counts ()
  "Use the fallback message when import reports contain no non-zero counts."
  (let (reported)
    (cl-letf (((symbol-function 'message)
	       (lambda (format-string &rest args)
		 (setq reported (apply #'format format-string args)))))
      (org-confluence-comments-import-report-message
       '(:imported 0 :imported-ids nil)
       "Imported 0 Confluence inline comments"))
    (should (equal reported "Imported 0 Confluence inline comments"))))

(ert-deftest org-confluence-comments-open-current-opens-sidecar-comment-url ()
  "Open the remote comment URL for the current sidecar heading."
  (let* ((dir (make-temp-file "hub-confluence-open-comment-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (org-confluence-api-base-url "https://example.atlassian.net")
	 (org-confluence-api-default-space nil)
	 opened)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n#+CONFLUENCE_SPACE: ENG\n\nBody\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Remote\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:ORG_COMMENTS_REMOTE_ID: c456\n:ORG_COMMENTS_SYNC_KIND: inline\n:END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Remote")
	    (cl-letf (((symbol-function 'browse-url)
		       (lambda (url &rest _args) (setq opened url))))
	      (should (equal (org-confluence-comments-open-current)
			     "https://example.atlassian.net/wiki/spaces/ENG/pages/123?focusedCommentId=c456"))
	      (should (equal opened "https://example.atlassian.net/wiki/spaces/ENG/pages/123?focusedCommentId=c456")))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-open-current-delegates-to-comments-backend ()
  "Open current Confluence comment through the org-comments backend seam."
  (let* ((dir (make-temp-file "hub-confluence-open-backend-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n#+CONFLUENCE_SPACE: ENG\n\nBody\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Remote\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:ORG_COMMENTS_REMOTE_ID: c456\n:ORG_COMMENTS_SYNC_KIND: inline\n:END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Remote")
	    (cl-letf (((symbol-function 'org-comments-backend-open-remote)
		       (lambda (backend comment)
			 (setq called (list :backend backend :comment comment))
			 "remote-url")))
	      (should (equal (org-confluence-comments-open-current) "remote-url"))))
	  (should (equal (plist-get called :backend) 'confluence))
	  (should (equal (plist-get (plist-get called :comment) :page-id) "123"))
	  (should (equal (plist-get (plist-get called :comment) :space) "ENG"))
	  (should (equal (plist-get (plist-get called :comment) :remote-id) "c456")))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-open-current-opens-active-source-comment-url ()
  "Open the remote comment URL for an active source-buffer sidecar comment."
  (let* ((dir (make-temp-file "hub-confluence-open-source-comment-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (org-confluence-api-base-url "https://example.atlassian.net")
	 (org-confluence-api-default-space nil)
	 opened)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Remote\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:ORG_COMMENTS_REMOTE_ID: c456\n:ORG_COMMENTS_SYNC_KIND: inline\n:ORG_COMMENTS_TARGET: 34 47\n:ORG_COMMENTS_TARGET_TEXT: selected text\n:END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (goto-char (point-min))
	    (search-forward "selected")
	    (cl-letf (((symbol-function 'browse-url)
		       (lambda (url &rest _args) (setq opened url))))
	      (should (equal (org-confluence-comments-open-current)
			     "https://example.atlassian.net/wiki/pages/123?focusedCommentId=c456"))
	      (should (equal opened "https://example.atlassian.net/wiki/pages/123?focusedCommentId=c456")))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-sync--local-org-hash-ignores-confluence-keywords ()
  "Local sync hash ignores all top-level CONFLUENCE metadata keywords."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Article\n\nBody.\n")
    (let ((hash (org-confluence-sync--local-org-hash)))
      (goto-char (point-min))
      (insert "#+CONFLUENCE_PAGE_ID: 123\n#+CONFLUENCE_SPACE: ENG\n#+CONFLUENCE_CUSTOM: value\n")
      (should (equal (org-confluence-sync--local-org-hash) hash)))))

(ert-deftest org-confluence-sync--local-org-hash-normalizes-structural-blank-lines ()
  "Local sync hash ignores import-only structural blank-line differences."
  (let (compact-hash formatted-hash)
    (with-temp-buffer
      (org-mode)
      (insert "* Title\nBody\n#+begin_quote\nQuote\n#+end_quote\n| A |\n|---|\n")
      (setq compact-hash (org-confluence-sync--local-org-hash)))
    (with-temp-buffer
      (org-mode)
      (insert "* Title\n\nBody\n\n#+begin_quote\nQuote\n#+end_quote\n\n| A |\n|---|\n")
      (setq formatted-hash (org-confluence-sync--local-org-hash)))
    (should (equal formatted-hash compact-hash))))

(ert-deftest org-confluence-comments--sync-is-comments-only ()
  "Comments-only sync imports comments and pushes pending comments without page sync."
  (let (imported pushed)
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'org-confluence-sync-page-current)
		 (lambda (&rest _args)
		   (ert-fail "comment sync must not sync page content")))
		((symbol-function 'org-confluence-comments-import)
		 (lambda (&optional page-id body-format)
		   (setq imported (list page-id body-format))
		   2))
		((symbol-function 'org-confluence-sync--push-pending-comments)
		 (lambda (source-buffer &optional page-id)
		   (setq pushed (list source-buffer page-id))
		   '(:pushed 3 :errors ("local-1: failed")))))
	(should (equal (org-confluence-comments--sync "123" "storage")
		       '(:comments-imported 2
					    :comments-pushed 3
					    :comment-push-errors ("local-1: failed"))))
	(should (equal imported '("123" "storage")))
	(should (equal pushed (list (current-buffer) "123")))))))

(ert-deftest org-confluence-sync-current-imports-and-pushes-after-page-sync ()
  "Full sync delegates comments to the comments-only helper after page sync."
  (let (comment-sync)
    (org-confluence-api-test--with-org-buffer
     "#+CONFLUENCE_PAGE_ID: 123\n\nBody."
     (lambda ()
       (cl-letf (((symbol-function 'org-confluence-sync-page-current)
		  (lambda (page-id) (list :noop 1 :page-id page-id)))
		 ((symbol-function 'org-confluence-comments--sync)
		  (lambda (page-id body-format)
		    (setq comment-sync (list page-id body-format))
		    '(:comments-imported 2
					 :comments-pushed 1
					 :comment-push-errors nil)))
		 ((symbol-function 'org-confluence-comments-import)
		  (lambda (&rest _args)
		    (ert-fail "full sync should use comments helper, not public import wrapper")))
		 ((symbol-function 'org-confluence-sync--push-pending-comments)
		  (lambda (&rest _args)
		    (ert-fail "full sync should use comments helper, not push directly"))))
	 (let ((result (org-confluence-sync-current nil "storage")))
	   (should (equal (plist-get result :comments-imported) 2))
	   (should (equal (plist-get result :comments-pushed) 1))
	   (should (equal comment-sync '("123" "storage")))))))))

(ert-deftest org-confluence-sync-current-skips-comments-on-page-conflict ()
  "Full sync does not import or push comments after a page conflict."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n\nBody."
   (lambda ()
     (cl-letf (((symbol-function 'org-confluence-sync-page-current)
		(lambda (page-id) (list :conflict 1 :page-id page-id)))
	       ((symbol-function 'org-confluence-comments-import)
		(lambda (&rest _args) (ert-fail "comment import should be skipped")))
	       ((symbol-function 'org-confluence-sync--push-pending-comments)
		(lambda (&rest _args) (ert-fail "comment push should be skipped"))))
       (let ((result (org-confluence-sync-current)))
	 (should (equal (plist-get result :comments-skipped) 1)))))))

(ert-deftest org-confluence-pull-to-file-creates-new-file ()
  "Pull a Confluence page into a new Org file with sync metadata."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-create-" t))
	 (source (expand-file-name "nested/article.org" dir)))
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-api--get-page)
		   (lambda (page-id body-format)
		     (should (equal page-id "123"))
		     (should (equal body-format "storage"))
		     (org-confluence-api-test--page-response
		      "123" "Remote Title" 12 "<p>Remote <strong>body</strong>.</p>"))))
	  (let ((result (org-confluence-pull-to-file "123" source)))
	    (should (equal (plist-get result :status) 'created))
	    (should (equal (plist-get result :page-id) "123"))
	    (should (equal (plist-get result :file) source))
	    (should (equal (plist-get result :title) "Remote Title"))
	    (should (equal (plist-get result :version) "12"))
	    (should (equal (plist-get result :remote-version) "12")))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should (search-forward "#+TITLE: Remote Title" nil t))
	    (should (search-forward "#+CONFLUENCE_PAGE_ID: 123" nil t))
	    (should (search-forward "#+CONFLUENCE_PAGE_VERSION: 12" nil t))
	    (should (search-forward "#+CONFLUENCE_PAGE_STORAGE_HASH: sha256:" nil t))
	    (should (search-forward "#+CONFLUENCE_LOCAL_ORG_HASH: sha256:" nil t))
	    (should (search-forward "Remote *body*." nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-can-include-comments ()
  "Pull a page and its remote comments into an adjacent sidecar."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-api--get-page)
		   (lambda (page-id body-format)
		     (should (equal page-id "123"))
		     (should (equal body-format "storage"))
		     (org-confluence-api-test--page-response
		      "123" "Remote Title" 12 "<p>Remote body.</p>")))
		  ((symbol-function 'org-confluence-api--list-page-comments)
		   (lambda (_page-id endpoint _body-format)
		     (pcase endpoint
		       ("footer-comments"
			'(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"author\":{\"displayName\":\"Ada\"},\"createdAt\":\"2026-07-01T10:00:00.000Z\",\"body\":{\"storage\":{\"value\":\"<p>Footer body</p>\",\"representation\":\"storage\"}}}]}") )
		       ("inline-comments"
			'(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"author\":{\"displayName\":\"Bo\"},\"createdAt\":\"2026-07-02T11:00:00.000Z\",\"properties\":{\"inlineOriginalSelection\":\"Remote body\",\"inlineMarkerRef\":\"m1\"},\"body\":{\"storage\":{\"value\":\"<p>Inline body</p>\",\"representation\":\"storage\"}}}]}") ))))
		  ((symbol-function 'org-confluence-api--list-comment-children)
		   (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	  (let* ((org-confluence-comments-import-resolve-people nil)
		 (result (org-confluence-pull-to-file "123" source :include-comments t)))
	    (should (equal (plist-get result :status) 'created))
	    (should (equal (plist-get result :comments-file) sidecar))
	    (should (equal (plist-get result :comments-count) 2))
	    (should (equal (plist-get result :footer-comments-count) 1))
	    (should (equal (plist-get result :inline-comments-count) 1)))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should (search-forward "Remote body." nil t))
	    (should-not (search-forward "Footer body" nil t))
	    (should-not (search-forward "Inline body" nil t)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "#+source: article.org" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: f1" nil t))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: footer" nil t))
	    (should (search-forward "Footer body" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: i1" nil t))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: inline" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: Remote body" nil t))
	    (should (search-forward "Inline body" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-include-comments-signals-comment-failure ()
  "Signal when requested comment import fails after the page pull."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-comments-fail-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (cl-letf (((symbol-function 'org-confluence-api--get-page)
		     (lambda (_page-id _body-format)
		       (org-confluence-api-test--page-response
			"123" "Remote Title" 12 "<p>Remote body.</p>")))
		    ((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       (user-error "comment API unavailable"))))
	    (should-error (org-confluence-pull-to-file "123" source :include-comments t)
			  :type 'user-error))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should (search-forward "Remote body." nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-returns-page-metadata ()
  "Pull result includes page metadata already present in the page response."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-metadata-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-api--get-page)
		   (lambda (_page-id _body-format)
		     (org-confluence-api-test--page-response
		      "123" "Remote Title" 12 "<p>Remote body.</p>"
		      '(:space "ENG"
			       :base-url "https://example.atlassian.net/wiki"
			       :webui "/spaces/ENG/pages/123/Remote+Title")))))
	  (let ((result (org-confluence-pull-to-file "123" source)))
	    (should (equal (plist-get result :status) 'created))
	    (should (equal (plist-get result :page-id) "123"))
	    (should (equal (plist-get result :file) source))
	    (should (equal (plist-get result :title) "Remote Title"))
	    (should (equal (plist-get result :space) "ENG"))
	    (should (equal (plist-get result :version) "12"))
	    (should (equal (plist-get result :remote-version) "12"))
	    (should (equal (plist-get result :web-url)
			   "https://example.atlassian.net/wiki/spaces/ENG/pages/123/Remote+Title"))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-resolves-page-mentions-before-import ()
  "Page pull resolves Confluence user mentions into readable Org links."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-mentions-" t))
	 (source (expand-file-name "article.org" dir))
	 (people (expand-file-name "confluence-people.org" dir))
	 requested)
    (unwind-protect
	(progn
	  (with-temp-file people
	    (insert "#+title: Confluence People\n\n"))
	  (cl-letf (((symbol-function 'org-confluence-api--get-page)
		     (lambda (_page-id _body-format)
		       (org-confluence-api-test--page-response
			"123" "Remote Title" 12
			"<p>Hello <ac:link><ri:user ri:account-id=\"acct-pull-1\" /></ac:link></p>")))
		    ((symbol-function 'org-confluence-api--users-bulk)
		     (lambda (account-ids &optional _config)
		       (setq requested account-ids)
		       '(:status 200 :body "{\"results\":[{\"accountId\":\"acct-pull-1\",\"displayName\":\"Alice Example\"}]}"))))
	    (org-confluence-pull-to-file "123" source)
	    (should (equal requested '("acct-pull-1")))
	    (with-temp-buffer
	      (insert-file-contents source)
	      (should (search-forward "[[confluence-user:acct-pull-1][@Alice Example]]" nil t)))
	    (with-temp-buffer
	      (insert-file-contents people)
	      (should (search-forward ":ORG_CONFLUENCE_ACCOUNT_ID: acct-pull-1" nil t))
	      (goto-char (point-min))
	      (should (search-forward "Alice Example" nil t)))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-omits-missing-optional-metadata ()
  "Missing optional page metadata is omitted from the pull result."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-missing-metadata-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-api--get-page)
		   (lambda (_page-id _body-format)
		     '(:status 200 :body "{\"id\":\"123\",\"body\":{\"storage\":{\"value\":\"<p>Remote body.</p>\"}}}"))))
	  (let ((result (org-confluence-pull-to-file "123" source)))
	    (should (equal (plist-get result :status) 'created))
	    (should (equal (plist-get result :page-id) "123"))
	    (should (equal (plist-get result :file) source))
	    (should-not (plist-member result :title))
	    (should-not (plist-member result :space))
	    (should-not (plist-member result :version))
	    (should-not (plist-member result :remote-version))
	    (should-not (plist-member result :web-url))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-list-child-pages-normalizes-metadata ()
  "List child pages as generic metadata plists."
  (cl-letf (((symbol-function 'org-confluence-api--list-children)
	     (lambda (content-type page-id)
	       (should (equal content-type "page"))
	       (should (equal page-id "123"))
	       '(:status 200
			 :body "{\"_links\":{\"base\":\"https://example.atlassian.net/wiki\"},\"results\":[{\"id\":\"c1\",\"type\":\"page\",\"title\":\"Child One\",\"space\":{\"key\":\"ENG\"},\"version\":{\"number\":3},\"_links\":{\"webui\":\"/spaces/ENG/pages/c1/Child+One\"}},{\"id\":\"c2\",\"type\":\"folder\",\"title\":\"Child Two\"}]}"))))
    (should (equal (org-confluence-list-child-pages "123")
		   '((:id "c1"
			  :type "page"
			  :title "Child One"
			  :space "ENG"
			  :version "3"
			  :remote-version "3"
			  :web-url "https://example.atlassian.net/wiki/spaces/ENG/pages/c1/Child+One")
		     (:id "c2" :type "folder" :title "Child Two"))))))

(ert-deftest org-confluence-list-child-pages-allows-empty-results ()
  "List child pages returns nil for a page without direct children."
  (cl-letf (((symbol-function 'org-confluence-api--list-children)
	     (lambda (_content-type _page-id)
	       '(:status 200 :body "{\"results\":[]}"))))
    (should-not (org-confluence-list-child-pages "123"))))

(ert-deftest org-confluence-pull-child-page-completes-pulls-and-opens-selection ()
  "Select one child page, pull it beside the current file, and open it."
  (let* ((dir (make-temp-file "hub-confluence-pull-child-one-" t))
	 (source (expand-file-name "parent.org" dir))
	 (target (expand-file-name "Child Two.org" dir))
	 pulled opened prompt collection require-match)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nParent.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-list-child-pages)
		       (lambda (page-id)
			 (should (equal page-id "123"))
			 '((:id "c1" :title "Child One" :space "ENG")
			   (:id "c2" :title "Child Two" :space "ENG"))))
		      ((symbol-function 'completing-read)
		       (lambda (prompt-arg collection-arg _predicate require-match-arg
					   &rest _args)
			 (setq prompt prompt-arg
			       collection collection-arg
			       require-match require-match-arg)
			 "📄 Child Two — ENG · c2"))
		      ((symbol-function 'org-confluence-pull-to-file)
		       (lambda (page-id file options)
			 (setq pulled (list page-id file options))
			 (with-temp-file file
			   (insert "#+CONFLUENCE_PAGE_ID: c2\n\nChild.\n"))
			 (list :status 'created :page-id page-id :file file)))
		      ((symbol-function 'find-file)
		       (lambda (file &rest _args)
			 (setq opened file)
			 (find-file-noselect file))))
	      (let ((result (org-confluence-pull-child-page nil '(:if-exists refresh))))
		(should (equal (plist-get result :status) 'created))
		(should (equal prompt "Pull descendant: "))
		(should require-match)
		(should (assoc "📄 Child One — ENG · c1" collection))
		(should (assoc "📄 Child Two — ENG · c2" collection))
		(should (equal pulled `("c2" ,target (:if-exists refresh))))
		(should (equal opened target))))))
      (dolist (file (list source target))
	(when-let* ((buffer (find-buffer-visiting file)))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-child-page-descends-through-folders ()
  "Selecting a folder continues browsing until a page is selected."
  (let* ((dir (make-temp-file "hub-confluence-pull-child-folder-" t))
	 (source (expand-file-name "parent.org" dir))
	 (target (expand-file-name "Nested Page.org" dir))
	 (choices '("📁 Folder/ — f1" "📄 Nested Page — p1"))
	 prompts pulled)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nParent.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-list-child-pages)
		       (lambda (_page-id)
			 '((:id "f1" :type "folder" :title "Folder"))))
		      ((symbol-function 'org-confluence-sync--list-folder-children)
		       (lambda (folder-id)
			 (should (equal folder-id "f1"))
			 '((:id "p1" :type "page" :title "Nested Page"))))
		      ((symbol-function 'completing-read)
		       (lambda (prompt _collection _predicate _require-match &rest _args)
			 (push prompt prompts)
			 (pop choices)))
		      ((symbol-function 'org-confluence-pull-to-file)
		       (lambda (page-id file _options)
			 (setq pulled (list page-id file))
			 (with-temp-file file
			   (insert "#+CONFLUENCE_PAGE_ID: p1\n"))
			 (list :status 'created :page-id page-id :file file)))
		      ((symbol-function 'find-file)
		       (lambda (file &rest _args) (find-file-noselect file))))
	      (org-confluence-pull-child-page)
	      (should (equal (nreverse prompts)
			     '("Pull descendant: " "Pull descendant from Folder: ")))
	      (should (equal pulled `("p1" ,target))))))
      (dolist (file (list source target))
	(when-let* ((buffer (find-buffer-visiting file)))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-child-page-errors-outside-visited-file ()
  "Require a visited file before pulling a selected child page."
  (with-temp-buffer
    (org-mode)
    (insert "#+CONFLUENCE_PAGE_ID: 123\n")
    (should-error (org-confluence-pull-child-page) :type 'user-error)))

(ert-deftest org-confluence-pull-child-page-errors-without-page-id ()
  "Require current Org buffer Confluence metadata for contextual child pulls."
  (let* ((dir (make-temp-file "hub-confluence-pull-child-no-id-" t))
	 (source (expand-file-name "parent.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "Parent.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (should-error (org-confluence-pull-child-page) :type 'user-error)))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-child-page-errors-without-children ()
  "Report an empty child list clearly."
  (let* ((dir (make-temp-file "hub-confluence-pull-child-empty-" t))
	 (source (expand-file-name "parent.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-list-child-pages)
		       (lambda (_page-id) nil)))
	      (should-error (org-confluence-pull-child-page) :type 'user-error))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-child-page-uses-bulk-collision-filenames ()
  "Use the same duplicate-title filename logic for selected child pulls."
  (let* ((dir (make-temp-file "hub-confluence-pull-child-collision-" t))
	 (source (expand-file-name "parent.org" dir))
	 (target (expand-file-name "Child-c2.org" dir))
	 pulled)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-list-child-pages)
		       (lambda (_page-id)
			 '((:id "c1" :title "Child")
			   (:id "c2" :title "Child"))))
		      ((symbol-function 'completing-read)
		       (lambda (&rest _args) "📄 Child — c2"))
		      ((symbol-function 'org-confluence-pull-to-file)
		       (lambda (page-id file _options)
			 (setq pulled (list page-id file))
			 (with-temp-file file
			   (insert "#+CONFLUENCE_PAGE_ID: c2\n"))
			 (list :status 'created :page-id page-id :file file)))
		      ((symbol-function 'find-file)
		       (lambda (file &rest _args) (find-file-noselect file))))
	      (org-confluence-pull-child-page)
	      (should (equal pulled `("c2" ,target))))))
      (dolist (file (list source target))
	(when-let* ((buffer (find-buffer-visiting file)))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-child-pages-materializes-direct-children ()
  "Pull direct child pages into title-derived Org files."
  (let* ((dir (make-temp-file "hub-confluence-pull-children-" t))
	 (first (expand-file-name "Child One.org" dir))
	 (second (expand-file-name "Child One-c2.org" dir))
	 calls)
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-list-child-pages)
		   (lambda (page-id)
		     (should (equal page-id "123"))
		     '((:id "c1" :title "Child One")
		       (:id "c2" :title "Child One"))))
		  ((symbol-function 'org-confluence-pull-to-file)
		   (lambda (page-id file options)
		     (push (list page-id file options) calls)
		     (with-temp-file file
		       (insert (format "#+CONFLUENCE_PAGE_ID: %s\n" page-id)))
		     (list :status 'created :page-id page-id :file file))))
	  (let ((result (org-confluence-pull-child-pages "123" dir '(:if-exists refresh))))
	    (should (equal (plist-get result :status) 'completed))
	    (should (equal (plist-get result :page-id) "123"))
	    (should (equal (plist-get result :directory) (file-name-as-directory dir)))
	    (should (equal (plist-get result :created) 2))
	    (should (equal (plist-get result :refreshed) 0))
	    (should (file-exists-p first))
	    (should (file-exists-p second))
	    (should (equal (nreverse calls)
			   `(("c1" ,first (:if-exists refresh))
			     ("c2" ,second (:if-exists refresh)))))))
      (dolist (file (list first second))
	(when-let* ((buffer (find-buffer-visiting file)))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-child-pages-uses-page-id-for-missing-title ()
  "Pull child pages with missing titles into page-id-derived Org files."
  (let* ((dir (make-temp-file "hub-confluence-pull-child-missing-title-" t))
	 (target (expand-file-name "c1.org" dir)))
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-list-child-pages)
		   (lambda (_page-id) '((:id "c1"))))
		  ((symbol-function 'org-confluence-pull-to-file)
		   (lambda (page-id file _options)
		     (should (equal page-id "c1"))
		     (should (equal file target))
		     (list :status 'refreshed :page-id page-id :file file))))
	  (let ((result (org-confluence-pull-child-pages "123" dir)))
	    (should (equal (plist-get result :created) 0))
	    (should (equal (plist-get result :refreshed) 1))))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-treats-empty-adf-as-empty-body ()
  "Pull an empty ADF page as an empty Org body for folder/index pages."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-empty-adf-" t))
	 (source (expand-file-name "folder.org" dir))
	 (empty-adf "{\"type\":\"doc\",\"content\":[{\"type\":\"paragraph\"}],\"version\":1}"))
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-api--get-page)
		   (lambda (_page-id _body-format)
		     (org-confluence-api-test--page-response
		      "123" "Folder Page" 12 empty-adf))))
	  (let ((result (org-confluence-pull-to-file "123" source)))
	    (should (equal (plist-get result :status) 'created)))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should (search-forward "#+TITLE: Folder Page" nil t))
	    (should (search-forward "#+CONFLUENCE_PAGE_ID: 123" nil t))
	    (should-not (search-forward "{\"type\":\"doc\"" nil t))
	    (goto-char (point-min))
	    (should-not (re-search-forward "^[^#[:space:]]" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-refuses-nonempty-adf ()
  "Do not silently write unsupported non-empty ADF bodies as Org."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-adf-" t))
	 (source (expand-file-name "page.org" dir))
	 (adf "{\"type\":\"doc\",\"content\":[{\"type\":\"paragraph\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}],\"version\":1}"))
    (unwind-protect
	(cl-letf (((symbol-function 'org-confluence-api--get-page)
		   (lambda (_page-id _body-format)
		     (org-confluence-api-test--page-response "123" "ADF Page" 12 adf))))
	  (should-error (org-confluence-pull-to-file "123" source)
			:type 'user-error)
	  (should-not (file-exists-p source)))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-refreshes-safe-existing-file ()
  "Refresh an unchanged local file for the same Confluence page."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-refresh-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+TITLE: Local Title\n#+FILETAGS: :local:cache:\n#+IDENTIFIER: local-id\n#+CONFLUENCE_PAGE_ID: 123\n\nOld body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (let ((hash (org-confluence-sync--local-org-hash)))
	      (goto-char (point-min))
	      (insert (format "#+CONFLUENCE_PAGE_VERSION: 7\n#+CONFLUENCE_LOCAL_ORG_HASH: %s\n" hash))
	      (save-buffer)))
	  (cl-letf (((symbol-function 'org-confluence-api--get-page)
		     (lambda (_page-id _body-format)
		       (org-confluence-api-test--page-response
			"123" "Remote Title" 12 "<p>Remote body.</p>"))))
	    (let ((result (org-confluence-pull-to-file "123" source)))
	      (should (equal (plist-get result :status) 'refreshed))
	      (should (equal (plist-get result :version) "12"))
	      (should (equal (plist-get result :remote-version) "12"))))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should (string-match-p "^#\\+TITLE: Local Title$" (buffer-string)))
	    (should (string-match-p "^#\\+FILETAGS: :local:cache:$" (buffer-string)))
	    (should (string-match-p "^#\\+IDENTIFIER: local-id$" (buffer-string)))
	    (should (string-match-p "^#\\+CONFLUENCE_PAGE_ID: 123$" (buffer-string)))
	    (should (string-match-p "^#\\+CONFLUENCE_PAGE_VERSION: 12$" (buffer-string)))
	    (should (= 1 (how-many "^#\\+TITLE:" (point-min) (point-max))))
	    (should (= 1 (how-many "^#\\+CONFLUENCE_PAGE_ID:" (point-min) (point-max))))
	    (should (search-forward "Remote body." nil t))
	    (should-not (search-forward "Old body." nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-adds-missing-page-id-for-empty-file ()
  "Refresh an existing metadata-only file and add missing page id."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-add-page-id-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+TITLE: Local Title\n#+FILETAGS: :cache:\n#+IDENTIFIER: local-id\n\n"))
	  (cl-letf (((symbol-function 'org-confluence-api--get-page)
		     (lambda (_page-id _body-format)
		       (org-confluence-api-test--page-response
			"123" "Remote Title" 12 "<p>Remote body.</p>"))))
	    (let ((result (org-confluence-pull-to-file "123" source)))
	      (should (equal (plist-get result :status) 'refreshed))))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should (string-match-p "^#\\+TITLE: Local Title$" (buffer-string)))
	    (should (string-match-p "^#\\+FILETAGS: :cache:$" (buffer-string)))
	    (should (string-match-p "^#\\+IDENTIFIER: local-id$" (buffer-string)))
	    (should (= 1 (how-many "^#\\+CONFLUENCE_PAGE_ID:" (point-min) (point-max))))
	    (should (search-forward "Remote body." nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-refuses-missing-page-id-with-local-body ()
  "Do not overwrite an unlinked existing file that already has body content."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-unlinked-body-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+TITLE: Local Title\n\nLocal body.\n"))
	  (should-error (org-confluence-pull-to-file "123" source)
			:type 'user-error))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-refuses-page-id-mismatch ()
  "Do not overwrite a file linked to a different Confluence page."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-mismatch-" t))
	 (source (expand-file-name "article.org" dir))
	 fetched)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 999\n\nBody.\n"))
	  (cl-letf (((symbol-function 'org-confluence-api--get-page)
		     (lambda (&rest _args) (setq fetched t))))
	    (should-error (org-confluence-pull-to-file "123" source)
			  :type 'user-error)
	    (should-not fetched)))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-refuses-modified-buffer ()
  "Do not overwrite an existing file when its visiting buffer is modified."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-modified-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (goto-char (point-max))
	    (insert "Unsaved edit.\n"))
	  (should-error (org-confluence-pull-to-file "123" source)
			:type 'user-error))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-refuses-missing-sync-metadata ()
  "Do not refresh existing same-page files without local hash metadata."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-no-metadata-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody.\n"))
	  (should-error (org-confluence-pull-to-file "123" source)
			:type 'user-error))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-to-file-refuses-local-edits ()
  "Do not refresh when current local content differs from its stored hash."
  (let* ((dir (make-temp-file "hub-confluence-pull-file-edited-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nOriginal body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (let ((hash (org-confluence-sync--local-org-hash)))
	      (goto-char (point-min))
	      (insert (format "#+CONFLUENCE_PAGE_VERSION: 7\n#+CONFLUENCE_LOCAL_ORG_HASH: %s\n" hash))
	      (goto-char (point-max))
	      (insert "Local edit.\n")
	      (save-buffer)))
	  (should-error (org-confluence-pull-to-file "123" source)
			:type 'user-error))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-sync-page-current-initializes-metadata ()
  "First page sync records remote version and local hashes without changing body."
  (let* ((dir (make-temp-file "hub-confluence-sync-init-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nLocal body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (cl-letf (((symbol-function 'org-confluence-api--get-page)
		       (lambda (_page-id _format)
			 '(:status 200 :body "{\"id\":\"123\",\"version\":{\"number\":7},\"body\":{\"storage\":{\"value\":\"<p>Remote body.</p>\"}}}"))))
	      (should (equal (plist-get (org-confluence-sync-page-current) :initialized) 1)))
	    (should (search-forward "#+CONFLUENCE_PAGE_VERSION: 7" nil t))
	    (should (search-forward "#+CONFLUENCE_PAGE_STORAGE_HASH: sha256:" nil t))
	    (should (search-forward "#+CONFLUENCE_LOCAL_ORG_HASH: sha256:" nil t))
	    (should (search-forward "Local body." nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-sync-page-current-pulls-safe-remote-change ()
  "Page sync fast-forwards remote content when local content is unchanged."
  (let* ((dir (make-temp-file "hub-confluence-sync-pull-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nOld body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (let ((old-hash (org-confluence-sync--local-org-hash)))
	      (goto-char (point-min))
	      (insert (format "#+CONFLUENCE_PAGE_VERSION: 7\n#+CONFLUENCE_LOCAL_ORG_HASH: %s\n" old-hash)))
	    (cl-letf (((symbol-function 'org-confluence-api--get-page)
		       (lambda (_page-id _format)
			 '(:status 200 :body "{\"id\":\"123\",\"version\":{\"number\":8},\"body\":{\"storage\":{\"value\":\"<p>Remote body.</p>\"}}}"))))
	      (should (equal (plist-get (org-confluence-sync-page-current) :pulled) 1)))
	    (goto-char (point-min))
	    (should (search-forward "#+CONFLUENCE_PAGE_VERSION: 8" nil t))
	    (should (search-forward "Remote body." nil t))
	    (should-not (search-forward "Old body." nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-sync-page-current-pushes-safe-local-change ()
  "Page sync pushes local content when remote version is unchanged."
  (let* ((dir (make-temp-file "hub-confluence-sync-push-" t))
	 (source (expand-file-name "article.org" dir))
	 (published nil)
	 responses)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nOriginal body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (let ((old-hash (org-confluence-sync--local-org-hash)))
	      (goto-char (point-min))
	      (insert (format "#+CONFLUENCE_PAGE_VERSION: 7\n#+CONFLUENCE_LOCAL_ORG_HASH: %s\n" old-hash))
	      (goto-char (point-max))
	      (insert "Local edit.\n"))
	    (setq responses
		  (list '(:status 200 :body "{\"id\":\"123\",\"version\":{\"number\":7},\"body\":{\"storage\":{\"value\":\"<p>Original body.</p>\"}}}")
			'(:status 200 :body "{\"id\":\"123\",\"version\":{\"number\":8},\"body\":{\"storage\":{\"value\":\"<p>Original body. Local edit.</p>\"}}}")))
	    (cl-letf (((symbol-function 'org-confluence-api--get-page)
		       (lambda (&rest _args) (pop responses)))
		      ((symbol-function 'org-confluence-publish)
		       (lambda (&rest _args) (setq published t) "123")))
	      (should (equal (plist-get (org-confluence-sync-page-current) :pushed) 1)))
	    (should published)
	    (goto-char (point-min))
	    (should (search-forward "#+CONFLUENCE_PAGE_VERSION: 8" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-sync-page-current-opens-conflict-buffer ()
  "Page sync opens a conflict buffer when local and remote both changed."
  (let* ((dir (make-temp-file "hub-confluence-sync-conflict-" t))
	 (source (expand-file-name "article.org" dir))
	 conflict)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n#+CONFLUENCE_PAGE_VERSION: 7\n#+CONFLUENCE_LOCAL_ORG_HASH: sha256:old\n\nLocal edited body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (cl-letf (((symbol-function 'org-confluence-api--get-page)
		       (lambda (_page-id _format)
			 '(:status 200 :body "{\"id\":\"123\",\"version\":{\"number\":8},\"body\":{\"storage\":{\"value\":\"<p>Remote edited body.</p>\"}}}"))))
	      (should (equal (plist-get (org-confluence-sync-page-current) :conflict) 1)))
	    (setq conflict (get-buffer "*Org Confluence Sync Conflict*"))
	    (should conflict)
	    (with-current-buffer conflict
	      (should (search-forward "Local edited body." nil t))
	      (should (search-forward "Remote edited body." nil t)))))
      (when (buffer-live-p conflict)
	(kill-buffer conflict))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-list-renders-diagnostic-buffer ()
  "Render remote comments in a diagnostic buffer without writing sidecars."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n\nBody"
   (lambda ()
     (let ((sidecar (concat (file-name-sans-extension (or buffer-file-name "article.org"))
			    ".comments.org"))
	   (opened nil)
	   (calls nil))
       (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		  (lambda (page-id kind body-format)
		    (push (list page-id kind body-format) calls)
		    (if (string= kind "footer-comments")
			'(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"resolutionStatus\":\"resolved\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Footer</p>\",\"representation\":\"storage\"}}}]}")
		      '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\",\"representation\":\"storage\"}}}]}") )))
		 ((symbol-function 'pop-to-buffer)
		  (lambda (buffer &rest _args)
		    (setq opened buffer))))
	 (org-confluence-comments-list)
	 (unwind-protect
	     (with-current-buffer opened
	       (should (derived-mode-p 'special-mode))
	       (should (string-match-p "Confluence comments for page 123" (buffer-string)))
	       (should (search-forward "footer-comments" nil t))
	       (should (search-forward "f1" nil t))
	       (should (search-forward "resolution: resolved" nil t))
	       (should (search-forward "resolution-fields: resolutionStatus=\"resolved\"" nil t))
	       (should (search-forward "status-fields: status=\"current\"" nil t))
	       (should (search-forward "<p>Footer</p>" nil t))
	       (should (search-forward "inline-comments" nil t))
	       (should (search-forward "i1" nil t))
	       (should (search-forward "resolution: open" nil t))
	       (should (search-forward "resolution-fields: resolutionStatus=\"open\"" nil t))
	       (should (save-excursion
			 (goto-char (point-min))
			 (search-forward "body-format: storage" nil t))))
	   (when (buffer-live-p opened)
	     (kill-buffer opened))))
       (should (equal (nreverse calls)
		      '(("123" "footer-comments" "storage")
			("123" "inline-comments" "storage"))))
       (should-not (file-exists-p sidecar))))))

(ert-deftest org-confluence-comments-remote-inline-target-text-reads-v2-properties ()
  "Read selected inline text from Confluence v2 properties payloads."
  (should (equal (org-confluence-comments-remote-inline-target-text
		  '((properties . ((inlineOriginalSelection . "ordered")
				   (inlineMarkerRef . "marker-1")))))
		 "ordered")))

(ert-deftest org-confluence-sync-status-mode-keeps-bepo-navigation-free ()
  "Keep c/t/s/r free in the sync status report keymap."
  (dolist (key '("c" "t" "s" "r"))
    (should-not (lookup-key org-confluence-sync-status-mode-map (kbd key)))))

(ert-deftest org-confluence-sync-status-writes-cache-and-summarizes-issues ()
  "Collect Confluence sync status into a hidden JSON cache."
  (let* ((dir (make-temp-file "hub-confluence-status-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\n[[./image-abcdef123456.png]]\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n")
	    (insert "* OPEN Remote\n:PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: remote-confluence-i1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: i1\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
	    (insert ":ORG_COMMENTS_TARGET_TEXT: repeated\n")
	    (insert ":ORG_COMMENTS_ANCHOR_STATE: ambiguous\n")
	    (insert ":END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-api--get-page)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p>Body</p>\"}}}"))))
	      (let ((status (org-confluence-sync-status--collect)))
		(should (equal (alist-get 'state status) "warning"))
		(should (string-match-p "comment issue" (alist-get 'summary status)))
		(should (string-match-p "attachment issue" (alist-get 'summary status)))
		(should (seq-some
			 (lambda (issue)
			   (when-let* ((link (alist-get 'link issue)))
			     (string-match-p "\\`\\[\\[org-comment:" link)))
			 (append (alist-get 'issues status) nil)))
		(org-confluence-sync-status--write-cache status source)
		(should (file-exists-p (expand-file-name ".article.confluence.cache" dir)))
		(should (string-match-p "Sync ⚠" (org-confluence-sync-status-marker-string source)))
		(let* ((org-confluence-sync-status-stale-after-days 3)
		       (old (copy-alist status)))
		  (setf (alist-get 'checkedAt old) "2000-01-01T00:00:00+0000")
		  (org-confluence-sync-status--write-cache old source)
		  (should (string-match-p "Sync \\? stale" (org-confluence-sync-status-marker-string source))))))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-inline-uses-storage-marker-occurrence ()
  "Anchor ambiguous imported inline comments using marker occurrence in storage."
  (let* ((dir (make-temp-file "hub-confluence-inline-marker-index-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nFirst coverage item and second coverage item and third coverage item.\n")
	  (save-buffer)
	  (org-mode)
	  (let ((org-confluence-comments-import-resolve-people nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (_page-id kind _body-format)
			 (should (equal kind "inline-comments"))
			 '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"body\":{\"storage\":{\"value\":\"<p>Remote body</p>\"}},\"properties\":{\"inlineOriginalSelection\":\"coverage\",\"inlineMarkerRef\":\"marker-1\"}}]}")))
		      ((symbol-function 'org-confluence-api--get-page)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p>First coverage item and second <ac:inline-comment-marker ac:ref=\\\"marker-1\\\">coverage</ac:inline-comment-marker> item and third coverage item.</p>\"}}}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 1 (org-confluence-comments-import-inline)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_COUNT: 3" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_INDEX: 1" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET: 59 67" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_ANCHOR_STATE: ambiguous" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-inline-uses-remote-match-index ()
  "Anchor ambiguous imported inline comments using Confluence match metadata."
  (let* ((dir (make-temp-file "hub-confluence-inline-index-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nFirst ordered item and second ordered item.\n")
	  (save-buffer)
	  (org-mode)
	  (let ((org-confluence-comments-import-resolve-people nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (_page-id kind _body-format)
			 (should (equal kind "inline-comments"))
			 '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"body\":{\"storage\":{\"value\":\"<p>Remote body</p>\"}},\"inlineCommentProperties\":{\"originalSelection\":\"ordered\",\"markerRef\":\"marker-1\",\"textSelectionMatchCount\":2,\"textSelectionMatchIndex\":1}}]}")))
		      ((symbol-function 'org-confluence-api--get-page)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p>First ordered item and second <ac:inline-comment-marker ac:ref=\\\"marker-1\\\">ordered</ac:inline-comment-marker> item.</p>\"}}}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 1 (org-confluence-comments-import-inline)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: ordered" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_MATCH_COUNT: 2" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_MATCH_INDEX: 1" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET: 58 65" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_ANCHOR_STATE: ambiguous" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-inline-backfills-v2-target-text ()
  "Backfill target text from existing remote inline comments' v2 properties."
  (let* ((dir (make-temp-file "hub-confluence-inline-target-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nFirst ordered item and second ordered item.\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Imported old inline\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: remote-confluence-i-old\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: i-old\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
	    (insert ":END:\n\n")
	    (insert "<p>Old body</p>\n"))
	  (let ((org-confluence-comments-import-resolve-people nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (_page-id kind _body-format)
			 (should (equal kind "inline-comments"))
			 '(:status 200 :body "{\"results\":[{\"id\":\"i-old\",\"body\":{\"storage\":{\"value\":\"<p>Remote body</p>\"}},\"properties\":{\"inlineOriginalSelection\":\"ordered\",\"inlineMarkerRef\":\"marker-1\"}}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (org-confluence-comments-import-inline)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: ordered" nil t))
	    (should (search-forward ":ORG_COMMENTS_ANCHOR_STATE: ambiguous" nil t))
	    (should (search-forward ":ORG_COMMENTS_ANCHOR_MATCH_COUNT: 2" nil t))
	    (should-not (search-forward "marker-1" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-inline-appends-unanchored-sidecar-entry ()
  "Import remote inline comments into sidecar as unanchored comments."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (cl-letf (((symbol-function 'org-confluence-people-store-cache-identity) #'ignore)
		    ((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (page-id kind body-format)
		       (should (equal page-id "123"))
		       (should (equal kind "inline-comments"))
		       (should (equal body-format "storage"))
		       '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"version\":{\"authorId\":\"acct-1\",\"createdAt\":\"2026-06-16T10:11:12.000Z\"},\"inlineCommentProperties\":{\"originalSelection\":\"selected text\",\"markerRef\":\"m1\"},\"body\":{\"storage\":{\"value\":\"<p>Inline body</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (org-confluence-comments-import-inline))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "* OPEN " nil t))
	    (should (search-forward "“selected text” — Inline body" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: remote-confluence-i1" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: i1" nil t))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: inline" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: selected text" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_TARGET_JSON:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_TARGET:" nil t))
	    (should (search-forward "<p>Inline body</p>" nil t)))
	  (let ((comments (org-comments-collect (current-buffer) t)))
	    (should (= 1 (length comments)))
	    (should (eq 'stale (plist-get (car comments) :anchor-state)))
	    (should (equal "inline" (plist-get (car comments) :sync-kind)))
	    (should (equal "selected text" (plist-get (car comments) :target-text)))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-inline-imports-replies-under-root ()
  "Import remote inline replies as child headings under the root comment."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (let ((org-confluence-comments-import-resolve-people nil)
		messages)
	    (cl-letf (((symbol-function 'org-confluence-people-store-cache-identity) #'ignore)
		      ((symbol-function 'message)
		       (lambda (format-string &rest args)
			 (push (apply #'format format-string args) messages)))
		      ((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (_page-id _kind _body-format)
			 '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"authorId\":\"acct-a\",\"body\":{\"storage\":{\"value\":\"<p>Root</p>\",\"representation\":\"storage\"}}}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (comment-id kind body-format)
			 (should (equal comment-id "i1"))
			 (should (equal kind "inline-comments"))
			 (should (equal body-format "storage"))
			 '(:status 200 :body "{\"results\":[{\"id\":\"r1\",\"parentCommentId\":\"i1\",\"authorId\":\"acct-b\",\"createdAt\":\"2026-06-10T14:31:00.000Z\",\"body\":{\"storage\":{\"value\":\"<p>Reply body</p>\",\"representation\":\"storage\"}}}]}"))))
	      (should (= 2 (org-confluence-comments-import-inline)))
	      (should (= 0 (org-confluence-comments-import-inline))))
	    (should (member "Imported roots: 1 (i1); replies: 1 (r1)" messages))
	    (should (member "Imported 0 Confluence inline comments" messages)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "* OPEN [1 reply] acct-a" nil t))
	    (should (search-forward "** Reply · acct-b" nil t))
	    (should (search-forward "Reply body" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: remote-confluence-r1" nil t))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: reply" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_PARENT_ID: i1" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_PARENT_ID:" nil t)))
	  (let ((comments (org-comments-collect (current-buffer) t)))
	    (should (= 1 (length comments)))
	    (should (= 1 (length (plist-get (car comments) :replies))))
	    (should (equal "r1" (plist-get (car (plist-get (car comments) :replies)) :remote-id)))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-imports-footer-and-inline ()
  "Broad comment import fetches both footer and inline comments."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (let (calls)
	    (cl-letf (((symbol-function 'org-confluence-people-store-cache-identity) #'ignore)
		      ((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (_page-id kind _body-format)
			 (push kind calls)
			 (if (string= kind "footer-comments")
			     '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Footer</p>\",\"representation\":\"storage\"}}}]}")
			   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\",\"representation\":\"storage\"}}}]}") ))))
	      (should (= 2 (org-confluence-comments-import)))
	      (should (equal (nreverse calls) '("footer-comments" "inline-comments"))))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-footer-appends-remote-sidecar-entry ()
  "Import remote footer comments into the sidecar without source edits."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (cl-letf (((symbol-function 'org-confluence-people-store-cache-identity) #'ignore)
		    ((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (page-id kind body-format)
		       (should (equal page-id "123"))
		       (should (equal kind "footer-comments"))
		       (should (equal body-format "storage"))
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"authorId\":\"acct-1\",\"author\":{\"displayName\":\"Alice Example\",\"accountId\":\"acct-1\"},\"createdAt\":\"2026-06-15T17:42:00.000Z\",\"body\":{\"storage\":{\"value\":\"<p>Footer</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (org-confluence-comments-import-footer))))
	  (should (equal (buffer-string) "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n"))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "#+todo: OPEN TODO | RESOLVED" nil t))
	    (should (search-forward "* OPEN Page" nil t))
	    (should (search-forward "Footer" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: remote-confluence-f1" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_AUTHOR:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_CREATED_AT:" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: f1" nil t))
	    (should (search-forward ":ORG_COMMENTS_SOURCE: confluence" nil t))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: footer" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_BODY_FORMAT: storage" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_AUTHOR_ID: acct-1" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME: Alice Example" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_CREATED_AT: 2026-06-15T17:42:00.000Z" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_TARGET:" nil t))
	    (should (search-forward "<p>Footer</p>" nil t))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-footer-caches-v2-author-id ()
  "Import caches v2 author IDs even when display names are absent."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (global-dir (make-temp-file "org-confluence-people-store-" t))
	 (org-confluence-people-store-directory global-dir))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"version\":{\"authorId\":\"acct-v2\",\"createdAt\":\"2026-06-16T10:11:12.000Z\"},\"body\":{\"storage\":{\"value\":\"<p>Body</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (org-confluence-comments-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents (org-confluence-people-store-global-file))
	    (should (search-forward "* acct-v2" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_ACCOUNT_ID: acct-v2" nil t))))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest org-confluence-comments-import-footer-backfills-v2-version-metadata ()
  "Re-import uses v2 comment version metadata when root author/date are absent."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Confluence footer comment f1\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: remote-confluence-f1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: f1\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
	    (insert ":ORG_COMMENTS_BODY_FORMAT: storage\n")
	    (insert ":END:\n\n")
	    (insert "Locally preserved body\n"))
	  (cl-letf (((symbol-function 'org-confluence-people-store-cache-identity) #'ignore)
		    ((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"version\":{\"authorId\":\"acct-v2\",\"createdAt\":\"2026-06-16T10:11:12.000Z\"},\"body\":{\"storage\":{\"value\":\"<p>Changed remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 0 (org-confluence-comments-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_AUTHOR:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_CREATED_AT:" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_AUTHOR_ID: acct-v2" nil t))
	    (should (search-forward "Locally preserved body" nil t))
	    (should-not (search-forward "Changed remote" nil t))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-footer-backfills-missing-remote-metadata ()
  "Re-import fills absent author/date metadata without overwriting the body."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Confluence footer comment f1\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: remote-confluence-f1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: f1\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
	    (insert ":ORG_COMMENTS_BODY_FORMAT: storage\n")
	    (insert ":END:\n\n")
	    (insert "Locally preserved body\n"))
	  (cl-letf (((symbol-function 'org-confluence-people-store-cache-identity) #'ignore)
		    ((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"authorId\":\"acct-1\",\"author\":{\"displayName\":\"Alice Example\",\"accountId\":\"acct-1\"},\"createdAt\":\"2026-06-15T17:42:00.000Z\",\"body\":{\"storage\":{\"value\":\"<p>Changed remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 0 (org-confluence-comments-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (= 1 (how-many ":ORG_COMMENTS_REMOTE_ID: f1" (point-min) (point-max))))
	    (should-not (search-forward ":ORG_COMMENTS_AUTHOR:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_CREATED_AT:" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_AUTHOR_ID: acct-1" nil t))
	    (should (search-forward "Locally preserved body" nil t))
	    (should-not (search-forward "Changed remote" nil t))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-footer-is-idempotent ()
  "Repeated footer imports do not duplicate or overwrite local sidecar bodies."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (org-confluence-comments-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (goto-char (point-min))
	    (search-forward "<p>Remote</p>")
	    (replace-match "Locally edited body" nil t)
	    (write-region (point-min) (point-max) sidecar nil 'silent))
	  (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Changed remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 0 (org-confluence-comments-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (= 1 (how-many ":ORG_COMMENTS_REMOTE_ID: f1" (point-min) (point-max))))
	    (should (search-forward "Locally edited body" nil t))
	    (should-not (search-forward "Changed remote" nil t))))
      (delete-directory dir t))))

(ert-deftest org-confluence-api--page-update-command-with-file ()
  "Build a cfl page update command that reads XHTML from a file."
  (should (equal (org-confluence-api--page-update-command "123" "/tmp/page.xhtml")
		 "cfl page edit 123 --file /tmp/page.xhtml --storage")))

(ert-deftest org-confluence-api--page-view-storage-command ()
  "Build a cfl page view command that returns raw storage XHTML only."
  (should (equal (org-confluence-api--page-view-storage-command "123")
		 "cfl page view 123 --raw --content-only")))

(ert-deftest org-confluence-api--page-create-command-with-parent ()
  "Build a cfl page create command with a parent page ID."
  (should (equal (org-confluence-api--page-create-command "ENG" "Roadmap" nil "456")
		 "cfl page create --space ENG --title Roadmap --parent 456 --storage")))

(ert-deftest org-confluence-api--page-create-missing-space ()
  "Signal an error when creating a page without a space key."
  (should-error (org-confluence-api--page-create-command nil "Roadmap") :type 'user-error))

(ert-deftest org-confluence-api--page-update-missing-id ()
  "Signal an error when updating a page without a page ID."
  (should-error (org-confluence-api--page-update-command nil) :type 'user-error))

(ert-deftest org-confluence-api--attachment-upload-command ()
  "Build a cfl attachment upload command string."
  (should (equal (org-confluence-api--attachment-upload-command "123" "/tmp/foo bar.png")
		 "cfl attachment upload --page 123 --file /tmp/foo\\ bar.png")))

(ert-deftest org-confluence-publish-from-export-dispatch-passes-options ()
  "Publish from Org export dispatch with the dispatcher subtree flag."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-publish-dwim)
	       (lambda (&rest args)
		 (setq received args))))
      (org-confluence-publish-from-export-dispatch nil t nil t)
      (should (equal received '(nil nil nil t nil t nil))))))

(ert-deftest org-confluence-publish-and-open-from-export-dispatch-opens-result ()
  "Publish from Org export dispatch and open the resulting page."
  (let ((opened nil))
    (cl-letf (((symbol-function 'org-confluence-publish-dwim)
	       (lambda (&rest _args) "789"))
	      ((symbol-function 'org-confluence-open-page)
	       (lambda (page-id space)
		 (setq opened (list page-id space)))))
      (let ((org-confluence-api-default-space "~personal"))
	(org-confluence-publish-and-open-from-export-dispatch nil t nil t))
      (should (equal opened '("789" "~personal"))))))

(ert-deftest org-confluence-publish-refuses-stale-remote-page ()
  "Do not update when Confluence changed since the last recorded pull."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n#+CONFLUENCE_PAGE_VERSION: 7\n\nLocal edit.\n"
   (lambda ()
     (let ((edited nil)
	   (uploaded nil))
       (cl-letf (((symbol-function 'org-confluence-api--get-page)
		  (lambda (_page-id _format)
		    '(:status 200 :body "{\"id\":\"123\",\"version\":{\"number\":8},\"body\":{\"storage\":{\"value\":\"<p>Remote edit.</p>\"}}}")))
		 ((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		 ((symbol-function 'org-confluence-publish-upload-assets)
		  (lambda (&rest _) (setq uploaded t)))
		 ((symbol-function 'org-confluence-process-run)
		  (lambda (&rest _) (setq edited t))))
	 (let ((org-confluence-publish--skip-inline-comment-preflight t))
	   (should-error (org-confluence-publish) :type 'user-error)))
       (should-not uploaded)
       (should-not edited)))))

(ert-deftest org-confluence-publish-force-bypasses-stale-remote-page ()
  "Allow explicit force publish when Confluence changed since last pull."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n#+CONFLUENCE_PAGE_VERSION: 7\n\nLocal edit.\n"
   (lambda ()
     (let ((commands nil)
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (progn
	     (cl-letf (((symbol-function 'org-confluence-api--get-page)
			(lambda (_page-id _format)
			  '(:status 200 :body "{\"id\":\"123\",\"version\":{\"number\":8},\"body\":{\"storage\":{\"value\":\"<p>Remote edit.</p>\"}}}")))
		       ((symbol-function 'org-confluence-export)
			(lambda (&rest _) "<p>forced local</p>"))
		       ((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		       ((symbol-function 'org-confluence-process-write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'org-confluence-process-run)
			(lambda (command) (push command commands) 0))
		       ((symbol-function 'org-confluence-publish--inline-comment-preflight)
			(lambda (&rest _) nil)))
	       (let ((org-confluence-publish--skip-inline-comment-preflight t)
		     (org-confluence-publish-preserve-inline-comments nil))
		 (should (equal (org-confluence-publish-force) "123")))
	       (should (equal commands (list (format "cfl page edit 123 --file %s --storage"
						     xhtml-file)))))
	     (when (file-exists-p xhtml-file)
	       (delete-file xhtml-file))))))))

(ert-deftest org-confluence-publish-uses-subtree-page-id-and-export ()
  "Publish a subtree selected through Org export dispatch."
  (org-confluence-api-test--with-org-buffer
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
		       ((symbol-function 'org-confluence-process-write-temp-xhtml) (lambda (_xhtml) xhtml-file))
		       ((symbol-function 'org-confluence-process-run) (lambda (command) (push command commands) 0)))
	       (org-confluence-publish nil t nil t nil)
	       (should (equal commands (list (format "cfl page edit 456 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(defun org-confluence-api-test--export-import (org)
  "Export ORG to Confluence storage and import it back to Org."
  (org-confluence-import-storage-to-org
   (with-temp-buffer
     (insert org)
     (org-mode)
     (org-confluence-export))))

(ert-deftest org-confluence-import-storage-to-org-normalized-page-semantics ()
  "Import real Confluence-normalized storage shapes without semantic loss."
  (let ((storage "<h2>Source block</h2><ac:structured-macro ac:name=\"code\" ac:schema-version=\"1\" ac:macro-id=\"m1\"><ac:parameter ac:name=\"language\">emacs-lisp</ac:parameter><ac:plain-text-body><![CDATA[(defun x ()\n  (message \"hi\"))]]></ac:plain-text-body></ac:structured-macro><h2>Subpage</h2><p><ac:link><ri:content-entity ri:content-id=\"456\" ri:version-at-save=\"3\" /><ac:link-body>Open subpage: Subpage</ac:link-body></ac:link></p><h2>Images</h2><ac:image ac:width=\"760\" ac:style=\"max-width: 100.0%;height: auto;\"><ri:attachment ri:filename=\"image-hash.png\" ri:version-at-save=\"1\" /></ac:image><ul><li><strong>Term 1:</strong> Definition</li></ul><hr /><p><ac:structured-macro ac:name=\"anchor\" ac:schema-version=\"1\" ac:macro-id=\"a1\"><ac:parameter ac:name=\"\">fn-1</ac:parameter></ac:structured-macro><strong>1.</strong> Footnote. <ac:link ac:anchor=\"fnref-1\"><ac:link-body>↩</ac:link-body></ac:link></p>"))
    (should (equal (org-confluence-import-storage-to-org storage)
		   "** Source block\n\n#+begin_src emacs-lisp\n(defun x ()\n  (message \"hi\"))\n#+end_src\n\n** Subpage\n:PROPERTIES:\n:CONFLUENCE_PAGE_ID: 456\n:END:\n\n** Images\n\n[[./image-hash.png]]\n\n- Term 1 :: Definition\n\n* Footnotes\n\n[fn:1] Footnote."))))

(ert-deftest org-confluence-import-storage-to-org-round-trips-typographic-semantics ()
  "Round-trip supported typographic Org semantics through Confluence storage."
  (let* ((org "* Title\n\nParagraph with /italic/, *bold*, _under_, +strike+, ~code~, and [[https://example.com][link]].\n\n- Bullet *bold*\n- Second\n\n1. One\n2. Two\n\n| Name | Role |\n|------+------|\n| Ada | *Eng* |\n\n#+begin_quote\nQuote /body/.\n#+end_quote\n\n#+ATTR_CALLOUT: :type warning :title \"Heads\"\n#+begin_callout\nCareful *body*.\n#+end_callout\n\n[[confluence-status:Purple][Medium]]\n\n#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n\nText[fn:1]\n\n* Footnotes\n\n[fn:1] Foot /note/.\n")
	 (imported (org-confluence-api-test--export-import org)))
    (dolist (expected '("* Title"
			"Paragraph with /italic/, *bold*, _under_, +strike+, ~code~, and [[https://example.com][link]]."
			"- Bullet *bold*"
			"1. One"
			"| Ada | *Eng* |"
			"#+begin_quote\nQuote /body/.\n#+end_quote"
			"#+ATTR_CALLOUT: :type warning :title \"Heads\""
			"Careful *body*."
			"[[confluence-status:Purple][Medium]]"
			"#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src"
			"Text[fn:1]"
			"* Footnotes"
			"[fn:1] Foot /note/."))
      (should (string-match-p (regexp-quote expected) imported)))))

(ert-deftest org-confluence-import-storage-to-org-basic ()
  "Convert basic Confluence storage XHTML to Org text."
  (should (equal (org-confluence-import-storage-to-org
		  "<h1>Title</h1><p>Hello <strong>world</strong> and <a href=\"https://example.com\">link</a>.</p><ul><li>One</li><li>Two</li></ul>")
		 "* Title\n\nHello *world* and [[https://example.com][link]].\n- One\n- Two")))

(ert-deftest org-confluence-import-storage-to-org-date-time ()
  "Convert Confluence time elements to Org date timestamps."
  (should (equal (org-confluence-import-storage-to-org
		  "<p>Due <time datetime=\"2026-07-02\" /> and <time datetime=\"2026-07-03\"></time>.</p>")
		 "Due [2026-07-02] and [2026-07-03].")))

(ert-deftest org-confluence-import-storage-to-org-nested-lists ()
  "Convert nested Confluence storage lists to nested Org lists."
  (should (equal (org-confluence-import-storage-to-org
		  "<ul><li>Parent<ul><li>Child</li></ul></li><li>Second</li></ul>")
		 "- Parent\n  - Child\n- Second")))

(ert-deftest org-confluence-import-storage-to-org-nested-ordered-lists ()
  "Convert nested ordered storage lists to nested Org lists."
  (should (equal (org-confluence-import-storage-to-org
		  "<ol><li><p>Parent</p><ol><li><p>Child</p></li></ol></li><li><p>Second</p></li></ol>")
		 "1. Parent\n  1. Child\n2. Second")))

(ert-deftest org-confluence-import-storage-to-org-table ()
  "Convert Confluence storage tables to Org tables."
  (should (equal (org-confluence-import-storage-to-org
		  "<table><tbody><tr><th><p>Name</p></th><th><p>Score</p></th></tr><tr><td><p>Ada</p></td><td><p>10</p></td></tr><tr><td><p>Bo</p></td><td><p>8</p></td></tr></tbody></table>")
		 "| Name | Score |\n|------+-------|\n| Ada | 10 |\n| Bo | 8 |")))

(ert-deftest org-confluence-import-storage-to-org-table-inline-markup ()
  "Preserve inline markup inside imported Org table cells."
  (should (equal (org-confluence-import-storage-to-org
		  "<table><tbody><tr><th>Item</th></tr><tr><td><strong>Bold</strong> and <a href=\"https://example.com\">link</a></td></tr></tbody></table>")
		 "| Item |\n|------|\n| *Bold* and [[https://example.com][link]] |")))

(ert-deftest org-confluence-import-storage-to-org-table-emoji-bold-spacing ()
  "Add Org emphasis boundary spacing after emoji in table cells."
  (should (equal (org-confluence-import-storage-to-org
		  "<table><tbody><tr><th><p>📆<strong>T ime</strong></p></th></tr></tbody></table>")
		 "| 📆 *T ime* |\n|-----------|")))

(ert-deftest org-confluence-import-storage-to-org-table-trims-bold-cell-text ()
  "Trim storage whitespace inside bold table cell text."
  (should (equal (org-confluence-import-storage-to-org
		  "<table><tbody><tr><th><p><strong> T ime</strong></p></th></tr></tbody></table>")
		 "| *T ime* |\n|---------|")))

(ert-deftest org-confluence-import-storage-to-org-structured-macro-body ()
  "Import unknown structured macro rich text body without macro parameters."
  (should (equal (org-confluence-import-storage-to-org
		  "<ac:structured-macro ac:name=\"custom\"><ac:parameter ac:name=\"bgColor\">#fff</ac:parameter><ac:rich-text-body><p>Body</p><ul><li><p>Point</p></li></ul></ac:rich-text-body></ac:structured-macro><table><tbody><tr><th>H</th></tr></tbody></table>")
		 "Body\n- Point\n\n| H |\n|---|")))

(ert-deftest org-confluence-import-storage-to-org-callout-macro ()
  "Import Confluence panel-like macros as semantic Org callouts."
  (should (equal (org-confluence-import-storage-to-org
		  "<ac:structured-macro ac:name=\"warning\"><ac:parameter ac:name=\"title\">Heads up</ac:parameter><ac:rich-text-body><p>Body</p></ac:rich-text-body></ac:structured-macro>")
		 "#+ATTR_CALLOUT: :type warning :title \"Heads up\"\n#+begin_callout\nBody\n#+end_callout")))

(ert-deftest org-confluence-import-storage-to-org-panel-macro-preserves-type ()
  "Import generic Confluence panels as callouts with panel type preserved."
  (should (equal (org-confluence-import-storage-to-org
		  "<ac:structured-macro ac:name=\"panel\"><ac:rich-text-body><p>Panel body</p></ac:rich-text-body></ac:structured-macro>")
		 "#+ATTR_CALLOUT: :type panel\n#+begin_callout\nPanel body\n#+end_callout")))

(ert-deftest org-confluence-import-storage-to-org-status-macro ()
  "Import a Confluence status macro as a status Org link."
  (should (equal (org-confluence-import-storage-to-org
		  "<p><ac:structured-macro ac:name=\"status\"><ac:parameter ac:name=\"title\">Medium</ac:parameter><ac:parameter ac:name=\"colour\">Purple</ac:parameter></ac:structured-macro></p>")
		 "[[confluence-status:Purple][Medium]]")))

(ert-deftest org-confluence-import-storage-to-org-emoji-fallback ()
  "Import Confluence emoticons as their Unicode fallback."
  (should (equal (org-confluence-import-storage-to-org
		  "<p><ac:emoticon ac:name=\"blue-star\" ac:emoji-shortname=\":calendar:\" ac:emoji-id=\"1f4c6\" ac:emoji-fallback=\"📆\" /> Plan</p>")
		 "📆 Plan")))

(ert-deftest org-confluence-import-storage-to-org-emoji-shortname-fallback ()
  "Fallback to emoji shortname when Unicode fallback is absent."
  (should (equal (org-confluence-import-storage-to-org
		  "<p><ac:emoticon ac:name=\"unknown\" ac:emoji-shortname=\":unknown:\" /> Info</p>")
		 ":unknown: Info")))

(ert-deftest org-confluence-import-storage-to-org-atlassian-info-emoji ()
  "Map Atlassian info emoticon metadata to a Unicode emoji."
  (should (equal (org-confluence-import-storage-to-org
		  "<h2><ac:emoticon ac:name=\"information\" ac:emoji-shortname=\":info:\" ac:emoji-id=\"atlassian-info\" ac:emoji-fallback=\":info:\" /> Context</h2>")
		 "** ℹ️ Context")))

(ert-deftest org-confluence-import-storage-to-org-document-emoji ()
  "Map Atlassian document emoticon metadata to a Unicode emoji."
  (should (equal (org-confluence-import-storage-to-org
		  "<h2><ac:emoticon ac:name=\"blue-star\" ac:emoji-shortname=\":document:\" ac:emoji-fallback=\":document:\" /> Related Material</h2>")
		 "** 📄 Related Material")))

(ert-deftest org-confluence-import-storage-to-org-user-mention-with-body ()
  "Confluence user mentions preserve visible handle text in Org links."
  (should (equal (org-confluence-import-storage-to-org
		  "<p>Hello <ac:link><ri:user ri:account-id=\"acct-visible-1\" /><ac:plain-text-link-body><![CDATA[@Alice]]></ac:plain-text-link-body></ac:link></p>")
		 "Hello [[confluence-user:acct-visible-1][@Alice]]")))

(ert-deftest org-confluence-import-storage-to-org-user-mention-fallback ()
  "Confluence user mentions without visible text preserve account IDs."
  (should (equal (org-confluence-import-storage-to-org
		  "<p>Hello <ac:link><ri:user ri:account-id=\"acct-fallback-1\" /></ac:link></p>")
		 "Hello [[confluence-user:acct-fallback-1][@acct-fallback-1]]")))

(ert-deftest org-confluence-import-storage-to-org-user-mentions-keep-spacing ()
  "Adjacent Confluence user mentions remain separate Org links."
  (should (equal (org-confluence-import-storage-to-org
		  "<p><ac:link><ri:user ri:account-id=\"acct-space-1\" /></ac:link> <ac:link><ri:user ri:account-id=\"acct-space-2\" /></ac:link> (OoO)</p>")
		 "[[confluence-user:acct-space-1][@acct-space-1]] [[confluence-user:acct-space-2][@acct-space-2]] (OoO)")))

(ert-deftest org-confluence-import-storage-to-org-user-mention-prefers-people-handle ()
  "People store handles take precedence over Confluence visible labels."
  (let* ((dir (make-temp-file "hub-confluence-mentions-" t))
	 (people (expand-file-name "confluence-people.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file people
	    (insert "#+title: Confluence People\n\n")
	    (insert "* Alice Example\n:PROPERTIES:\n:ORG_CONFLUENCE_ACCOUNT_ID: acct-1\n:ORG_CONFLUENCE_HANDLE: alice\n:END:\n"))
	  (should (equal (org-confluence-import-storage-to-org
			  "<p>Hello <ac:link><ri:user ri:account-id=\"acct-1\" /><ac:plain-text-link-body><![CDATA[@Remote Alice]]></ac:plain-text-link-body></ac:link></p>"
			  dir)
			 "Hello [[confluence-user:acct-1][@alice]]")))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-refreshes-current-file ()
  "The legacy pull command now refreshes the current file instead of previewing."
  (let* ((dir (make-temp-file "hub-confluence-pull-current-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+TITLE: Old\n#+CONFLUENCE_PAGE_ID: 123\n\nOld body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((hash (org-confluence-sync--local-org-hash)))
	      (goto-char (point-min))
	      (insert (format "#+CONFLUENCE_PAGE_VERSION: 7\n#+CONFLUENCE_LOCAL_ORG_HASH: %s\n" hash))
	      (save-buffer))
	    (cl-letf (((symbol-function 'org-confluence-api--get-page)
		       (lambda (_page-id _body-format)
			 (org-confluence-api-test--page-response
			  "123" "Remote Title" 12 "<p>Remote body.</p>")))
		      ((symbol-function 'pop-to-buffer)
		       (lambda (&rest _)
			 (ert-fail "org-confluence-pull should not open a preview buffer"))))
	      (let ((result (org-confluence-pull)))
		(should (equal (plist-get result :status) 'refreshed))))
	    (goto-char (point-min))
	    (should (search-forward "#+TITLE: Old" nil t))
	    (should (search-forward "Remote body." nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-pull-prefix-includes-comments ()
  "A prefix argument makes interactive pull include comments."
  (let* ((dir (make-temp-file "hub-confluence-pull-prefix-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 received)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-pull-to-file)
		       (lambda (&rest args)
			 (setq received args)
			 '(:status refreshed)))
		      ((symbol-function 'revert-buffer) (lambda (&rest _) nil)))
	      (let ((current-prefix-arg '(4)))
		(call-interactively #'org-confluence-pull)))
	    (should (equal received (list "123" source :include-comments t)))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-page-created-page-id ()
  "Parse created page IDs from cfl output."
  (should (equal (org-confluence-page-created-page-id "ID: 789") "789"))
  (should (equal (org-confluence-page-created-page-id "{\"id\":\"456\"}") "456"))
  (should (equal (org-confluence-page-created-page-id "https://example/wiki/spaces/X/pages/123/Page") "123")))

(ert-deftest org-confluence-publish-dwim-records-created-page-metadata ()
  "Record page ID and space in the Org buffer after create flow succeeds."
  (org-confluence-api-test--with-org-buffer
   "#+TITLE: Created Page\n\nBody"
   (lambda ()
     (let ((org-confluence-api-default-space "~personal")
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (cl-letf (((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>Body</p>"))
		     ((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		     ((symbol-function 'org-confluence-process-write-temp-xhtml) (lambda (_xhtml) xhtml-file))
		     ((symbol-function 'org-confluence-process-run-output)
		      (lambda (command)
			(should (string-match-p (regexp-quote "--space") command))
			(should (string-match-p (regexp-quote "~personal") command))
			"Created page\nID: 789"))
		     ((symbol-function 'org-confluence-process-run) (lambda (_command) 0)))
	     (org-confluence-publish-dwim)
	     (should (equal (buffer-string)
			    "#+TITLE: Created Page\n#+CONFLUENCE_PAGE_ID: 789\n#+CONFLUENCE_SPACE: ~personal\n\nBody")))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(ert-deftest org-comments-open-link-opens-sidecar-fallback ()
  "Opening org-comment links can locate a source file's sidecar comment."
  (let* ((dir (make-temp-file "hub-org-comment-link-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 opened)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "Body without original anchor.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Inline\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-inline-1\n:ORG_COMMENTS_SYNC_KIND: inline\n:ORG_COMMENTS_TARGET_TEXT: missing text\n:END:\n\nBody\n"))
	  (cl-letf (((symbol-function 'pop-to-buffer)
		     (lambda (buffer &rest _args) (setq opened buffer))))
	    (org-comments-open-link (format "%s::local-inline-1" source)))
	  (should (equal (buffer-file-name opened) sidecar))
	  (with-current-buffer opened
	    (should (equal (org-entry-get nil "ORG_COMMENTS_ID") "local-inline-1"))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-blocks-unpreservable-active-inline-comments ()
  "Publishing aborts when active inline comments lack marker metadata."
  (let* ((dir (make-temp-file "hub-confluence-publish-preflight-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (commands nil))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((org-confluence-publish--skip-inline-comment-preflight nil)
		  (org-confluence-comments-import-resolve-people nil))
	      (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
			 (lambda (_page-id kind _body-format)
			   (should (equal kind "inline-comments"))
			   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\"}}}]}")))
			((symbol-function 'org-confluence-api--list-comment-children)
			 (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
			((symbol-function 'org-confluence-process-run)
			 (lambda (command) (push command commands))))
		(should-error (org-confluence-publish) :type 'user-error))))
	  (should-not commands)
	  (with-current-buffer "*Org Confluence Publish Preflight*"
	    (should (derived-mode-p 'org-mode))
	    (should (search-forward "[[org-comment:" nil t))
	    (should (search-forward "::remote-confluence-i1][comment i1]]" nil t)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: i1" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-force-reports-but-continues ()
  "Force publishing imports/report blockers but still updates the page."
  (let* ((dir (make-temp-file "hub-confluence-publish-force-" t))
	 (source (expand-file-name "article.org" dir))
	 (commands nil))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((org-confluence-publish--skip-inline-comment-preflight nil)
		  (org-confluence-comments-import-resolve-people nil))
	      (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
			 (lambda (_page-id kind _body-format)
			   (should (equal kind "inline-comments"))
			   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"m1\",\"inlineOriginalSelection\":\"selected text\"},\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\"}}}]}")))
			((symbol-function 'org-confluence-api--list-comment-children)
			 (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
			((symbol-function 'org-confluence-api--get-page)
			 (lambda (&rest _args)
			   '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p>Alpha <ac:inline-comment-marker ac:ref=\\\"m1\\\">selected text</ac:inline-comment-marker> omega.</p>\"}}}")))
			((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
			((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
			((symbol-function 'org-confluence-process-write-temp-xhtml)
			 (lambda (_xhtml) (make-temp-file "org-confluence-test-" nil ".xhtml")))
			((symbol-function 'org-confluence-process-run)
			 (lambda (command) (push command commands) 0)))
		(should (equal (org-confluence-publish-force) "123")))))
	  (should (= 1 (length commands)))
	  (should (string-match-p "page edit 123" (car commands))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-does-not-block-dangling-inline-comments ()
  "Already dangling inline comments are imported and reported but do not block."
  (let* ((dir (make-temp-file "hub-confluence-publish-dangling-" t))
	 (source (expand-file-name "article.org" dir))
	 (commands nil))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((org-confluence-publish--skip-inline-comment-preflight nil)
		  (org-confluence-comments-import-resolve-people nil))
	      (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
			 (lambda (_page-id kind _body-format)
			   (should (equal kind "inline-comments"))
			   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"dangling\",\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\"}}}]}")))
			((symbol-function 'org-confluence-api--list-comment-children)
			 (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
			((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
			((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
			((symbol-function 'org-confluence-process-write-temp-xhtml)
			 (lambda (_xhtml) (make-temp-file "org-confluence-test-" nil ".xhtml")))
			((symbol-function 'org-confluence-process-run)
			 (lambda (command) (push command commands) 0)))
		(should (equal (org-confluence-publish) "123")))))
	  (should (= 1 (length commands))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-prompts-to-repair-missing-inline-marker ()
  "Interactive publish repairs safe missing inline markers before updating."
  (let* ((dir (make-temp-file "hub-confluence-publish-repair-" t))
	 (source (expand-file-name "article.org" dir))
	 (commands nil)
	 (repairs nil))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((org-confluence-publish--skip-inline-comment-preflight nil)
		  (org-confluence-comments-import-resolve-people nil))
	      (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
			 (lambda (_page-id kind _body-format)
			   (should (equal kind "inline-comments"))
			   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"m1\",\"inlineOriginalSelection\":\"selected text\"},\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\"}}}]}")))
			((symbol-function 'org-confluence-api--list-comment-children)
			 (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
			((symbol-function 'org-confluence-api--get-page)
			 (lambda (&rest _args)
			   '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p>Alpha selected text omega.</p>\"}}}")))
			((symbol-function 'org-confluence-publish--interactive-repair-p) (lambda () t))
			((symbol-function 'yes-or-no-p) (lambda (&rest _args) t))
			((symbol-function 'org-confluence-inline-repair-comment-anchors)
			 (lambda (page-id apply) (push (list page-id apply) repairs)))
			((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
			((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
			((symbol-function 'org-confluence-process-write-temp-xhtml)
			 (lambda (_xhtml) (make-temp-file "org-confluence-test-" nil ".xhtml")))
			((symbol-function 'org-confluence-process-run)
			 (lambda (command) (push command commands) 0)))
		(should (equal (org-confluence-publish) "123")))))
	  (should (equal repairs '(("123" t))))
	  (should (= 1 (length commands))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-does-not-block-remote-missing-inline-comments ()
  "Remote-missing inline comments are reported but do not block publishing."
  (let* ((dir (make-temp-file "hub-confluence-publish-missing-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (commands nil))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Missing inline\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-i1\n:ORG_COMMENTS_REMOTE_ID: i1\n:ORG_COMMENTS_SYNC_KIND: inline\n:ORG_COMMENTS_REMOTE_STATE: missing\n:ORG_COMMENTS_TARGET_TEXT: selected text\n:END:\n\nInline\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((org-confluence-publish--skip-inline-comment-preflight nil)
		  (org-confluence-comments-import-resolve-people nil))
	      (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
			 (lambda (_page-id kind _body-format)
			   (should (equal kind "inline-comments"))
			   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"m1\",\"inlineOriginalSelection\":\"selected text\"},\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\"}}}]}")))
			((symbol-function 'org-confluence-comments-import-remote-comments)
			 (lambda (&rest _args) (list :imported 0)))
			((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
			((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
			((symbol-function 'org-confluence-process-write-temp-xhtml)
			 (lambda (_xhtml) (make-temp-file "org-confluence-test-" nil ".xhtml")))
			((symbol-function 'org-confluence-process-run)
			 (lambda (command) (push command commands) 0)))
		(should (equal (org-confluence-publish) "123")))))
	  (should (= 1 (length commands)))
	  (with-current-buffer "*Org Confluence Publish Preflight*"
	    (should (search-forward "missing" nil t))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-recursive-child-does-not-clear-parent-preservation ()
  "Child subpage publish does not clear parent inline marker preservation state."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n** Child\n:PROPERTIES:\n:CONFLUENCE_PAGE_ID: 456\n:END:\nChild body."
   (lambda ()
     (let ((published-xhtml nil)
	   (commands nil)
	   (org-confluence-publish--skip-inline-comment-preflight nil)
	   (org-confluence-comments-import-resolve-people nil))
       (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		  (lambda (page-id kind _body-format)
		    (should (equal kind "inline-comments"))
		    (if (equal page-id "123")
			'(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"m1\",\"inlineOriginalSelection\":\"selected text\"},\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\"}}}]}" )
		      '(:status 200 :body "{\"results\":[]}"))))
		 ((symbol-function 'org-confluence-api--list-comment-children)
		  (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
		 ((symbol-function 'org-confluence-comments-import-remote-comments)
		  (lambda (&rest _args) (list :imported 0)))
		 ((symbol-function 'org-confluence-api--get-page)
		  (lambda (&rest _args)
		    '(:status 200 :body "{\"body\":{\"storage\":{\"value\":\"<p>Alpha <ac:inline-comment-marker ac:ref=\\\"m1\\\">selected text</ac:inline-comment-marker> omega.</p>\"}}}")))
		 ((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		 ((symbol-function 'org-confluence-export)
		  (lambda (&rest _args) "<p>Alpha selected text omega.</p>"))
		 ((symbol-function 'org-confluence-process-write-temp-xhtml)
		  (lambda (xhtml)
		    (push xhtml published-xhtml)
		    (make-temp-file "org-confluence-test-" nil ".xhtml")))
		 ((symbol-function 'org-confluence-process-run)
		  (lambda (command) (push command commands) 0)))
	 (org-confluence-publish)
	 (should (= 2 (length commands)))
	 (should (string-match-p "<ac:inline-comment-marker ac:ref=\"m1\">selected text</ac:inline-comment-marker>"
				 (car published-xhtml))))))))

(ert-deftest org-confluence-publish-recursively-publishes-subpages-first ()
  "Publishing a parent updates child subpages before the parent page."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n\n* Parent body\n** Child\n:PROPERTIES:\n:CONFLUENCE_PAGE_ID: 456\n:END:\nChild body."
   (lambda ()
     (let ((published nil))
       (cl-letf (((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		 ((symbol-function 'org-confluence-export)
		  (lambda (&rest args)
		    (push (list (org-confluence-api--page-id-from-buffer (cadr args))
				(cadr args)
				(plist-get (car (last args)) :confluence-omit-root-heading))
			  published)
		    "<p>x</p>"))
		 ((symbol-function 'org-confluence-process-write-temp-xhtml)
		  (lambda (_xhtml) (make-temp-name "org-confluence-test")))
		 ((symbol-function 'org-confluence-process-run) (lambda (_command) 0)))
	 (org-confluence-publish)
	 (should (equal (nreverse published)
			'(("456" t t) ("123" nil nil)))))))))

(ert-deftest org-confluence-publish-aborts-parent-when-subpage-fails ()
  "Do not update a parent page after a child subpage publish failure."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n\n* Parent body\n** Child\n:PROPERTIES:\n:CONFLUENCE_PAGE_ID: 456\n:END:\nChild body."
   (lambda ()
     (let ((commands nil))
       (cl-letf (((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		 ((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>x</p>"))
		 ((symbol-function 'org-confluence-process-write-temp-xhtml)
		  (lambda (_xhtml) (make-temp-name "org-confluence-test")))
		 ((symbol-function 'org-confluence-process-run)
		  (lambda (command)
		    (push command commands)
		    (when (string-match-p "page edit 456" command)
		      (user-error "child failed")))))
	 (should-error (org-confluence-publish) :type 'user-error)
	 (should (= 1 (length commands)))
	 (should (string-match-p "page edit 456" (car commands))))))))

(ert-deftest org-confluence-publish-uploads-images-before-page-edit ()
  "Upload all referenced images before editing the Confluence page."
  (org-confluence-api-test--with-org-buffer
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
		       ((symbol-function 'org-confluence-process-write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'org-confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'org-confluence-process-run)
			(lambda (command) (push command commands) 0)))
	       (org-confluence-publish)
	       (should (equal (nreverse commands)
			      (list "upload:123:foo-hash.png"
				    (format "cfl page edit 123 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p source-file)
	   (delete-file source-file))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(ert-deftest org-confluence-publish-image-assets-reuses-missing-hashed-attachment ()
  "Treat missing hash-named image links as existing Confluence attachments."
  (let* ((dir (make-temp-file "hub-confluence-image-reuse-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\n[[./image-abcdef123456.png]]\n")
	  (save-buffer)
	  (org-mode)
	  (let ((assets (org-confluence-image-assets)))
	    (should (= 1 (length assets)))
	    (should (equal (plist-get (car assets) :filename)
			   "image-abcdef123456.png"))
	    (should (plist-get (car assets) :missing-source))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-image-assets-does-not-rehash-existing-hashed-attachment ()
  "Do not add another hash suffix to already hash-named image links."
  (let* ((dir (make-temp-file "hub-confluence-image-hashed-" t))
	 (source (expand-file-name "article.org" dir))
	 (image (expand-file-name "image-abcdef123456.png" dir)))
    (unwind-protect
	(progn
	  (with-temp-file image (insert "image bytes"))
	  (with-current-buffer (find-file-noselect source)
	    (erase-buffer)
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\n[[./image-abcdef123456.png]]\n")
	    (save-buffer)
	    (org-mode)
	    (let ((assets (org-confluence-image-assets)))
	      (should (= 1 (length assets)))
	      (should (equal (plist-get (car assets) :filename)
			     "image-abcdef123456.png"))
	      (should-not (plist-get (car assets) :missing-source)))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-publish-upload-assets-skips-missing-sources ()
  "Reuse existing Confluence attachments for missing hash-named sources."
  (let ((commands nil))
    (cl-letf (((symbol-function 'org-confluence-process-run)
	       (lambda (command) (push command commands))))
      (org-confluence-publish-upload-assets
       "123" '((:filename "image-abcdef123456.png" :missing-source t)))
      (should-not commands))))

(ert-deftest org-confluence-publish-upload-assets-deduplicates-filenames ()
  "Upload repeated image assets with the same attachment filename only once."
  (let ((commands nil)
	(source-file (make-temp-file "org-confluence-source-" nil ".png")))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "png"))
	  (cl-letf (((symbol-function 'org-confluence-api--attachment-upload-command)
		     (lambda (page-id file-path)
		       (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		    ((symbol-function 'org-confluence-process-run)
		     (lambda (command) (push command commands) 0)))
	    (org-confluence-publish-upload-assets
	     "123"
	     (list (list :source-path source-file :filename "foo-hash.png")
		   (list :source-path source-file :filename "foo-hash.png")))
	    (should (equal commands (list "upload:123:foo-hash.png")))))
      (when (file-exists-p source-file)
	(delete-file source-file)))))

(ert-deftest org-confluence-publish-dwim-creates-then-uploads-images ()
  "Create a page before uploading image assets in create flow."
  (org-confluence-api-test--with-org-buffer
   "#+CONFLUENCE_SPACE: ENG\n#+TITLE: Page\n\n[[./foo.png]]"
   (lambda ()
     (let ((events nil)
	   (source-file (make-temp-file "org-confluence-source-" nil ".png"))
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (progn
	     (with-temp-file source-file (insert "png"))
	     (cl-letf (((symbol-function 'org-confluence-image-assets)
			(lambda (&rest _)
			  (list (list :path source-file
				      :source-path source-file
				      :source-link "./foo.png"
				      :filename "foo-hash.png"))))
		       ((symbol-function 'org-confluence-export)
			(lambda (&rest args)
			  (should (member '("./foo.png" . "foo-hash.png")
					  (plist-get (car (last args)) :confluence-image-filenames)))
			  "<p>x</p>"))
		       ((symbol-function 'org-confluence-process-write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'org-confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'org-confluence-process-run-output)
			(lambda (command)
			  (push command events)
			  "ID: 789"))
		       ((symbol-function 'org-confluence-process-run)
			(lambda (command) (push command events) 0)))
	       (org-confluence-publish-dwim)
	       (should (equal (nreverse events)
			      (list (format "cfl page create --space ENG --title Page --file %s --storage" xhtml-file)
				    "upload:789:foo-hash.png"
				    (format "cfl page edit 789 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p source-file)
	   (delete-file source-file))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(ert-deftest org-confluence-process-run-reports-command-output ()
  "Report command output explicitly when a cfl command fails."
  (cl-letf (((symbol-function 'org-confluence-api--cfl-available-p) (lambda () t))
	    ((symbol-function 'process-file)
	     (lambda (_program _infile buffer _display &rest _args)
	       (with-current-buffer buffer
		 (insert "explicit failure"))
	       1)))
    (should-error (org-confluence-process-run "cfl fail") :type 'user-error)))

(ert-deftest org-confluence-publish-continues-when-hashed-attachment-exists ()
  "Continue publishing when uploading an already-present hashed attachment."
  (org-confluence-api-test--with-org-buffer
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
		       ((symbol-function 'org-confluence-process-write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'org-confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'org-confluence-process-run)
			(lambda (command)
			  (push command commands)
			  (when (string-prefix-p "upload:" command)
			    (user-error "Cannot add a new attachment with same file name as an existing attachment: foo-hash.png"))
			  0)))
	       (org-confluence-publish)
	       (should (equal (nreverse commands)
			      (list "upload:123:foo-hash.png"
				    (format "cfl page edit 123 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p source-file)
	   (delete-file source-file))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

(ert-deftest org-confluence-publish-cleans-temp-xhtml-on-upload-failure ()
  "Delete temporary XHTML when an image upload fails."
  (org-confluence-api-test--with-org-buffer
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
		       ((symbol-function 'org-confluence-process-write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'org-confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'org-confluence-process-run)
			(lambda (command)
			  (push command commands)
			  (user-error "upload failed"))))
	       (should-error (org-confluence-publish) :type 'user-error)
	       (should-not (file-exists-p xhtml-file))
	       (should (equal commands (list "upload:123:foo-hash.png")))))
	 (when (file-exists-p source-file)
	   (delete-file source-file)))))))

(ert-deftest org-confluence-comments-import-marks-missing-for-fetched-kind-only ()
  "Import marks absent remote-linked comments missing only for fetched kind."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Inline linked\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-i-old\n:ORG_COMMENTS_REMOTE_ID: i-old\n:ORG_COMMENTS_SYNC_KIND: inline\n:END:\n\nBody\n")
	    (insert "* TODO Footer linked\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-f-old\n:ORG_COMMENTS_REMOTE_ID: f-old\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nBody\n"))
	  (let ((org-confluence-comments-import-resolve-people nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (org-confluence-comments-import-footer)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: f-old" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: missing" nil t))
	    (org-mode)
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: i-old" nil t))
	    (org-back-to-heading t)
	    (let ((inline-end (save-excursion (org-end-of-subtree t t))))
	      (should-not (save-excursion
			    (re-search-forward ":ORG_COMMENTS_REMOTE_STATE: missing" inline-end t))))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-derives-local-status-from-remote ()
  "Import derives remote-linked root TODO status with local TODO exception."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* TODO Working open\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-f-open\n:ORG_COMMENTS_REMOTE_ID: f-open\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nBody\n")
	    (insert "* TODO Working resolved\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-f-resolved\n:ORG_COMMENTS_REMOTE_ID: f-resolved\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nBody\n")
	    (insert "* RESOLVED Reopened\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-f-reopen\n:ORG_COMMENTS_REMOTE_ID: f-reopen\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nBody\n"))
	  (let ((org-confluence-comments-import-resolve-people nil)
		(reported nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"results\":[{\"id\":\"f-open\",\"resolutionStatus\":\"open\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Open</p>\"}}},{\"id\":\"f-resolved\",\"resolutionStatus\":\"resolved\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Done</p>\"}}},{\"id\":\"f-reopen\",\"resolutionStatus\":\"open\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Open</p>\"}}}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
		      ((symbol-function 'message)
		       (lambda (format-string &rest args)
			 (setq reported (apply #'format format-string args)))))
	      (should (= 0 (org-confluence-comments-import-footer)))
	      (should (string-match-p "Remote resolved locally: 1" reported))
	      (should (string-match-p "Remote reopened locally: 1" reported))
	      (should (string-match-p "Local TODO closed by remote resolution: 1" reported))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (org-mode)
	    (dolist (case '(("f-open" . "TODO")
			    ("f-resolved" . "RESOLVED")
			    ("f-reopen" . "OPEN")))
	      (goto-char (point-min))
	      (should (search-forward (concat ":ORG_COMMENTS_REMOTE_ID: " (car case)) nil t))
	      (org-back-to-heading t)
	      (should (equal (cdr case) (org-get-todo-state)))))
	  (should (get-buffer "*Confluence Comment Sync Report*")))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-ignores-content-status-for-resolution ()
  "Import ignores Confluence content status when resolutionStatus is absent."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* TODO Working\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-f-status\n:ORG_COMMENTS_REMOTE_ID: f-status\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nBody\n"))
	  (let ((org-confluence-comments-import-resolve-people nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"results\":[{\"id\":\"f-status\",\"status\":\"resolved\",\"body\":{\"storage\":{\"value\":\"<p>Status only</p>\"}}}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (org-confluence-comments-import-footer)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: f-status" nil t))
	    (org-back-to-heading t)
	    (should (equal "TODO" (org-get-todo-state)))
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS:" nil t))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-restores-remote-present-again ()
  "Import clears missing metadata when a remote-linked comment reappears."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Was missing\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-f1\n:ORG_COMMENTS_REMOTE_ID: f1\n:ORG_COMMENTS_SYNC_KIND: footer\n:ORG_COMMENTS_REMOTE_STATE: missing\n:ORG_COMMENTS_REMOTE_MISSING_AT: 2026-01-01T00:00:00+0000\n:END:\n\nBody\n"))
	  (let ((org-confluence-comments-import-resolve-people nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Back</p>\"}}}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (org-confluence-comments-import-footer)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_STATE:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_MISSING_AT:" nil t))))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-import-restores-inline-remote-present-again ()
  "Inline import clears remote-missing metadata after marker repair."
  (let* ((dir (make-temp-file "hub-confluence-inline-present-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n")
	  (save-buffer)
	  (org-mode)
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Was missing\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-i1\n:ORG_COMMENTS_REMOTE_ID: i1\n:ORG_COMMENTS_SYNC_KIND: inline\n:ORG_COMMENTS_SOURCE: confluence\n:ORG_COMMENTS_TARGET_TEXT: selected text\n:ORG_COMMENTS_REMOTE_STATE: missing\n:ORG_COMMENTS_REMOTE_MISSING_AT: 2026-01-01T00:00:00+0000\n:END:\n\nBody\n"))
	  (let ((org-confluence-comments-import-resolve-people nil))
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"m1\",\"inlineOriginalSelection\":\"selected text\"},\"body\":{\"storage\":{\"value\":\"<p>Back</p>\"}}}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (org-confluence-comments-import-inline)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_STATE:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_MISSING_AT:" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: open" nil t))))
      (delete-directory dir t))))

(ert-deftest org-confluence-api--create-footer-comment-posts-storage-payload ()
  "Create footer comments through REST without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"id\":\"f123\"}"))))
      (should (equal (org-confluence-api--create-footer-comment
		      "123" "<p>Hello</p>" '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"id\":\"f123\"}")))
      (cl-destructuring-bind (method url body config) received
	(should (equal method "POST"))
	(should (equal url "https://example.atlassian.net/wiki/api/v2/footer-comments"))
	(should (equal config '(:base-url "https://example.atlassian.net")))
	(should (equal (json-parse-string body :object-type 'alist)
		       '((pageId . "123")
			 (body . ((representation . "storage")
				  (value . "<p>Hello</p>"))))))))))

(ert-deftest org-confluence-api--create-inline-comment-posts-selection-payload ()
  "Create inline comments through REST without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"id\":\"i123\"}"))))
      (should (equal (org-confluence-api--create-inline-comment
		      "123" "<p>Hello</p>"
		      '((textSelection . "selected")
			(textSelectionMatchCount . 2)
			(textSelectionMatchIndex . 1))
		      '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"id\":\"i123\"}")))
      (cl-destructuring-bind (method url body _config) received
	(should (equal method "POST"))
	(should (equal url "https://example.atlassian.net/wiki/api/v2/inline-comments"))
	(should (equal (json-parse-string body :object-type 'alist)
		       '((pageId . "123")
			 (body . ((representation . "storage")
				  (value . "<p>Hello</p>")))
			 (inlineCommentProperties . ((textSelection . "selected")
						     (textSelectionMatchCount . 2)
						     (textSelectionMatchIndex . 1))))))))))

(ert-deftest org-confluence-api--update-comment-puts-next-version-payload ()
  "Update comments through REST without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"id\":\"f123\"}"))))
      (should (equal (org-confluence-api--update-comment
		      "footer-comments" "f123" "<p>Updated</p>" 4
		      '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"id\":\"f123\"}")))
      (cl-destructuring-bind (method url body _config) received
	(should (equal method "PUT"))
	(should (equal url "https://example.atlassian.net/wiki/api/v2/footer-comments/f123"))
	(should (equal (json-parse-string body :object-type 'alist)
		       '((version . ((number . 4)))
			 (body . ((representation . "storage")
				  (value . "<p>Updated</p>"))))))))))

(ert-deftest org-confluence-api--create-comment-reply-posts-parent-payload ()
  "Create replies through the matching comment endpoint without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'org-confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"id\":\"r123\"}"))))
      (should (equal (org-confluence-api--create-comment-reply
		      "inline-comments" "i123" "<p>Reply</p>"
		      '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"id\":\"r123\"}")))
      (cl-destructuring-bind (method url body _config) received
	(should (equal method "POST"))
	(should (equal url "https://example.atlassian.net/wiki/api/v2/inline-comments"))
	(should (equal (json-parse-string body :object-type 'alist)
		       '((parentCommentId . "i123")
			 (body . ((representation . "storage")
				  (value . "<p>Reply</p>"))))))))))

(ert-deftest org-confluence-comments-push-body-to-storage-uses-plain-paragraphs ()
  "Convert sidecar Org comment bodies to storage paragraphs."
  (should (equal (org-confluence-comments-push-body-to-storage
		  "This needs <clarity>.\nStill same paragraph.\n\nSecond & final.")
		 "<p>This needs &lt;clarity&gt;. Still same paragraph.</p><p>Second &amp; final.</p>")))

(ert-deftest org-confluence-comments-push-body-to-storage-supports-basic-org ()
  "Convert basic Org inline markup and lists to Confluence storage XHTML."
  (should (equal (org-confluence-comments-push-body-to-storage
		  "*Bold* /italic/ ~code~ =verb= [[https://example.test][link]]\n\n- one\n- two")
		 "<p><strong>Bold</strong> <em>italic</em> <code>code</code> <code>verb</code> <a href=\"https://example.test\">link</a></p><ul><li>one</li><li>two</li></ul>"))
  (should (equal (org-confluence-comments-push-body-to-storage
		  "1. first\n2. second")
		 "<ol><li>first</li><li>second</li></ol>")))

(ert-deftest org-confluence-comments-push-current-pushes-local-footer-comment ()
  "Push the current sidecar footer comment and stamp remote metadata."
  (let* ((dir (make-temp-file "hub-confluence-push-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (calls nil)
	 (org-confluence-api-base-url "https://example.atlassian.net"))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nSource body stays clean.\n"))
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* TODO Page · Alice — Needs clarification\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-1\n")
	    (insert ":ORG_COMMENTS_AUTHOR: Alice\n")
	    (insert ":ORG_COMMENTS_CREATED_AT: 2026-01-01T00:00:00+0000\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
	    (insert ":END:\n\n")
	    (insert "Needs <clarification>.\n\nAdd owner.\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (erase-buffer)
	    (insert-file-contents sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Needs clarification")
	    (cl-letf (((symbol-function 'org-confluence-api--create-footer-comment)
		       (lambda (page-id storage &optional _config)
			 (push (list page-id storage) calls)
			 '(:status 200 :body "{\"id\":\"f123\",\"authorId\":\"acct-1\",\"author\":{\"displayName\":\"Alice Remote\"},\"createdAt\":\"2026-01-02T00:00:00.000Z\",\"resolutionStatus\":\"open\"}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-01-03T00:00:00+0000")))
	      (should (equal (org-confluence-comments-push-current)
			     (list :created 1 :remote-id "f123" :sync-kind "footer"
				   :sidecar-file sidecar
				   :url "https://example.atlassian.net/wiki/pages/123?focusedCommentId=f123")))))
	  (should (equal (current-kill 0 t)
			 "https://example.atlassian.net/wiki/pages/123?focusedCommentId=f123"))
	  (should (equal calls '(("123" "<p>Needs &lt;clarification&gt;.</p><p>Add owner.</p>"))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "* TODO" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: local-1" nil t))
	    (should (search-forward ":ORG_COMMENTS_AUTHOR: Alice" nil t))
	    (should (search-forward ":ORG_COMMENTS_CREATED_AT: 2026-01-01T00:00:00+0000" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: f123" nil t))
	    (should (search-forward ":ORG_COMMENTS_SOURCE: confluence" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_LAST_SEEN_AT: 2026-01-03T00:00:00+0000" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_AUTHOR_ID: acct-1" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME: Alice Remote" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_CREATED_AT: 2026-01-02T00:00:00.000Z" nil t))
	    (should (search-forward "Needs <clarification>." nil t)))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should-not (search-forward "ORG_COMMENTS_REMOTE_ID" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-updates-authored-linked-comment ()
  "Push local edits to a remote comment only when current user authored it."
  (let* ((dir (make-temp-file "hub-confluence-update-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (calls nil)
	 (org-confluence-api-base-url "https://example.atlassian.net"))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nSource body.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Linked\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:ORG_COMMENTS_SYNC_KIND: footer\n:ORG_COMMENTS_REMOTE_ID: f1\n:ORG_COMMENTS_REMOTE_AUTHOR_ID: acct-me\n:ORG_COMMENTS_LOCAL_UPDATED_AT: 2026-06-19T20:00:00+0000\n:END:\n\nUpdated body.\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Linked")
	    (cl-letf (((symbol-function 'org-confluence-api--get-comment)
		       (lambda (kind remote-id body-format)
			 (push (list :get kind remote-id body-format) calls)
			 '(:status 200 :body "{\"id\":\"f1\",\"authorId\":\"acct-me\",\"version\":{\"number\":3}}")))
		      ((symbol-function 'org-confluence-api--current-user)
		       (lambda () '(:status 200 :body "{\"accountId\":\"acct-me\"}")))
		      ((symbol-function 'org-confluence-api--update-comment)
		       (lambda (kind remote-id storage version)
			 (push (list :put kind remote-id storage version) calls)
			 '(:status 200 :body "{\"id\":\"f1\",\"authorId\":\"acct-me\",\"version\":{\"number\":4,\"createdAt\":\"2026-06-19T20:01:00.000Z\"},\"resolutionStatus\":\"open\"}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-06-19T20:02:00+0000")))
	      (should (equal (org-confluence-comments-push-current)
			     (list :updated 1 :remote-id "f1" :sync-kind "footer"
				   :sidecar-file sidecar
				   :url "https://example.atlassian.net/wiki/pages/123?focusedCommentId=f1")))))
	  (should (equal (nreverse calls)
			 '((:get "footer-comments" "f1" "storage")
			   (:put "footer-comments" "f1" "<p>Updated body.</p>" 4))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_LOCAL_UPDATED_AT:" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_UPDATED_AT: 2026-06-19T20:01:00.000Z" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_LAST_SEEN_AT: 2026-06-19T20:02:00+0000" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-refuses-non-authored-linked-comment ()
  "Refuse remote update when the authenticated user is not the comment author."
  (let* ((dir (make-temp-file "hub-confluence-update-author-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nSource body.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Linked\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:ORG_COMMENTS_SYNC_KIND: footer\n:ORG_COMMENTS_REMOTE_ID: f1\n:ORG_COMMENTS_LOCAL_UPDATED_AT: 2026-06-19T20:00:00+0000\n:END:\n\nUpdated body.\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Linked")
	    (cl-letf (((symbol-function 'org-confluence-api--get-comment)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"id\":\"f1\",\"authorId\":\"acct-other\",\"version\":{\"number\":3}}")))
		      ((symbol-function 'org-confluence-api--current-user)
		       (lambda () '(:status 200 :body "{\"accountId\":\"acct-me\"}")))
		      ((symbol-function 'org-confluence-api--update-comment)
		       (lambda (&rest _args) (error "Should not update"))))
	      (should-error (org-confluence-comments-push-current) :type 'user-error))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-refuses-already-linked-comments ()
  "Do not create duplicate remote comments for linked sidecar entries."
  (let* ((dir (make-temp-file "hub-confluence-push-linked-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Linked\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:ORG_COMMENTS_SYNC_KIND: footer\n:ORG_COMMENTS_REMOTE_ID: f1\n:END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Linked")
	    (cl-letf (((symbol-function 'org-confluence-api--create-footer-comment)
		       (lambda (&rest _args) (error "Should not call create"))))
	      (should-error (org-confluence-comments-push-current "123") :type 'user-error))))
      (when-let* ((buffer (find-buffer-visiting sidecar)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-refuses-non-footer-comment ()
  "Require explicit footer sync kind before publishing a local comment."
  (let* ((dir (make-temp-file "hub-confluence-push-kind-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Ambiguous\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Ambiguous")
	    (should-error (org-confluence-comments-push-current "123") :type 'user-error)))
      (when-let* ((buffer (find-buffer-visiting sidecar)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-pushes-local-inline-comment ()
  "Push the current sidecar inline comment with text selection properties."
  (let* ((dir (make-temp-file "hub-confluence-push-inline-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (calls nil)
	 (org-confluence-api-base-url "https://example.atlassian.net"))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega selected text.\n"))
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Alice · “selected text” — Inline body\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-inline-1\n")
	    (insert ":ORG_COMMENTS_AUTHOR: Alice\n")
	    (insert ":ORG_COMMENTS_CREATED_AT: 2026-01-01T00:00:00+0000\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
	    (insert ":ORG_COMMENTS_TARGET: 34 47\n")
	    (insert ":ORG_COMMENTS_TARGET_TEXT: selected text\n")
	    (insert ":END:\n\n")
	    (insert "Inline body\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Inline body")
	    (cl-letf (((symbol-function 'org-confluence-api--create-inline-comment)
		       (lambda (page-id storage properties &optional _config)
			 (push (list page-id storage properties) calls)
			 '(:status 200 :body "{\"id\":\"i123\",\"resolutionStatus\":\"open\"}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-01-03T00:00:00+0000")))
	      (should (equal (org-confluence-comments-push-current)
			     (list :created 1 :remote-id "i123" :sync-kind "inline"
				   :sidecar-file sidecar
				   :url "https://example.atlassian.net/wiki/pages/123?focusedCommentId=i123")))))
	  (should (equal calls
			 '(("123" "<p>Inline body</p>"
			    ((textSelection . "selected text")
			     (textSelectionMatchCount . 2)
			     (textSelectionMatchIndex . 0))))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: i123" nil t))
	    (should (search-forward ":ORG_COMMENTS_SOURCE: confluence" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: inline" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_LAST_SEEN_AT: 2026-01-03T00:00:00+0000" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_MATCH_COUNT: 2" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_MATCH_INDEX: 0" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ANCHOR_STATE: unconfirmed" nil t))
	    (should (search-forward "Inline body" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-pushes-local-reply ()
  "Push a local reply child heading under an existing remote inline thread."
  (let* ((dir (make-temp-file "hub-confluence-push-reply-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (calls nil)
	 (org-confluence-api-base-url "https://example.atlassian.net"))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nSource body.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Remote root\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-root\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: i123\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
	    (insert ":END:\n\nRoot body\n")
	    (insert "** Reply · Alice — Local reply\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-reply-1\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	    (insert ":ORG_COMMENTS_AUTHOR: Alice\n")
	    (insert ":ORG_COMMENTS_CREATED_AT: 2026-01-01T00:00:00+0000\n")
	    (insert ":END:\n\nLocal reply\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Local reply")
	    (cl-letf (((symbol-function 'org-confluence-api--create-comment-reply)
		       (lambda (endpoint parent-id storage &optional _config)
			 (push (list endpoint parent-id storage) calls)
			 '(:status 200 :body "{\"id\":\"r123\",\"authorId\":\"acct-1\",\"createdAt\":\"2026-01-02T00:00:00.000Z\"}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-01-03T00:00:00+0000")))
	      (should (equal (org-confluence-comments-push-current)
			     (list :created 1 :remote-id "r123" :sync-kind "reply"
				   :sidecar-file sidecar
				   :url "https://example.atlassian.net/wiki/pages/123?focusedCommentId=r123")))))
	  (should (equal calls '(("inline-comments" "i123" "<p>Local reply</p>"))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "* OPEN [1 reply]" nil t))
	    (should (search-forward "** Reply" nil t))
	    (should-not (looking-at-p "[[:space:]]+OPEN"))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: r123" nil t))
	    (should (search-forward ":ORG_COMMENTS_SOURCE: confluence" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: reply" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_PARENT_ID: i123" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_LAST_SEEN_AT: 2026-01-03T00:00:00+0000" nil t))
	    (should (search-forward "Local reply" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-refuses-reply-parent-mismatch ()
  "Reply push refuses explicit parent metadata that conflicts with the root."
  (let* ((dir (make-temp-file "hub-confluence-push-reply-mismatch-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Root\n:PROPERTIES:\n:ORG_COMMENTS_ID: root\n:ORG_COMMENTS_REMOTE_ID: i123\n:ORG_COMMENTS_SYNC_KIND: inline\n:END:\n\nRoot\n")
	    (insert "** Reply · Alice\n:PROPERTIES:\n:ORG_COMMENTS_ID: reply\n:ORG_COMMENTS_SYNC_KIND: reply\n:ORG_COMMENTS_REMOTE_PARENT_ID: other\n:END:\n\nReply\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Reply · Alice")
	    (should-error (org-confluence-comments-push-current) :type 'user-error)))
      (when-let* ((buffer (find-buffer-visiting sidecar)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-refuses-stale-inline-anchor ()
  "Do not call Confluence when a local inline anchor is stale."
  (let* ((dir (make-temp-file "hub-confluence-push-stale-inline-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha changed text omega.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Inline\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-inline-1\n:ORG_COMMENTS_SYNC_KIND: inline\n:ORG_COMMENTS_TARGET: 34 47\n:ORG_COMMENTS_TARGET_TEXT: selected text\n:END:\n\nInline body\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Inline")
	    (cl-letf (((symbol-function 'org-confluence-api--create-inline-comment)
		       (lambda (&rest _args) (error "Should not call create"))))
	      (should-error (org-confluence-comments-push-current) :type 'user-error)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_ID:" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-keeps-inline-unlinked-on-api-error ()
  "Inline push failures do not stamp remote metadata."
  (let* ((dir (make-temp-file "hub-confluence-push-inline-error-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Inline\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-inline-1\n:ORG_COMMENTS_SYNC_KIND: inline\n:ORG_COMMENTS_TARGET: 34 47\n:ORG_COMMENTS_TARGET_TEXT: selected text\n:END:\n\nInline body\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Inline")
	    (cl-letf (((symbol-function 'org-confluence-api--create-inline-comment)
		       (lambda (&rest _args) (user-error "Confluence rejected inline target"))))
	      (should-error (org-confluence-comments-push-current) :type 'user-error)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_ID:" nil t))
	    (should (search-forward "Inline body" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-inline-round-trips-without-duplication ()
  "A pushed inline comment is recognized by subsequent import, not duplicated."
  (let* ((dir (make-temp-file "hub-confluence-push-inline-roundtrip-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (org-confluence-comments-import-resolve-people nil)
	 (org-confluence-api-base-url "https://example.atlassian.net"))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega.\n"))
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* TODO Alice · “selected text” — Local inline draft\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-inline-1\n")
	    (insert ":ORG_COMMENTS_AUTHOR: Alice\n")
	    (insert ":ORG_COMMENTS_CREATED_AT: 2026-01-01T00:00:00+0000\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
	    (insert ":ORG_COMMENTS_TARGET: 34 47\n")
	    (insert ":ORG_COMMENTS_TARGET_TEXT: selected text\n")
	    (insert ":END:\n\n")
	    (insert "Local inline draft\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Local inline draft")
	    (cl-letf (((symbol-function 'org-confluence-api--create-inline-comment)
		       (lambda (_page-id _storage _properties &optional _config)
			 '(:status 200 :body "{\"id\":\"i123\",\"authorId\":\"acct-1\",\"createdAt\":\"2026-01-02T00:00:00.000Z\",\"resolutionStatus\":\"open\"}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-01-03T00:00:00+0000")))
	      (org-confluence-comments-push-current)))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (_page-id kind _body-format)
			 (should (equal kind "inline-comments"))
			 '(:status 200 :body "{\"results\":[{\"id\":\"i123\",\"authorId\":\"acct-1\",\"body\":{\"storage\":{\"value\":\"<p>Remote inline copy</p>\",\"representation\":\"storage\"}},\"resolutionStatus\":\"open\",\"properties\":{\"inlineMarkerRef\":\"marker-1\",\"inlineOriginalSelection\":\"selected text\"}}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-01-04T00:00:00+0000")))
	      (should (= 0 (org-confluence-comments-import-inline)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (let ((remote-count 0)
		  (heading-count 0))
	      (goto-char (point-min))
	      (while (re-search-forward org-heading-regexp nil t)
		(setq heading-count (1+ heading-count)))
	      (goto-char (point-min))
	      (while (search-forward ":ORG_COMMENTS_REMOTE_ID: i123" nil t)
		(setq remote-count (1+ remote-count)))
	      (should (= heading-count 1))
	      (should (= remote-count 1)))
	    (goto-char (point-min))
	    (should (search-forward "* TODO" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: local-inline-1" nil t))
	    (should (search-forward ":ORG_COMMENTS_SYNC_KIND: inline" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: selected text" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_LAST_SEEN_AT: 2026-01-04T00:00:00+0000" nil t))
	    (should (search-forward "Local inline draft" nil t))
	    (should-not (search-forward "Remote inline copy" nil t))
	    (should-not (search-forward "marker-1" nil t)))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should-not (search-forward "ORG_COMMENTS_REMOTE_ID" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-confluence-comments-push-current-round-trips-without-duplication ()
  "A pushed footer comment is recognized by subsequent import, not duplicated."
  (let* ((dir (make-temp-file "hub-confluence-push-roundtrip-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (org-confluence-comments-import-resolve-people nil)
	 (org-confluence-api-base-url "https://example.atlassian.net"))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nSource body stays clean.\n"))
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* TODO Page · Alice — Local draft body\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-1\n")
	    (insert ":ORG_COMMENTS_AUTHOR: Alice\n")
	    (insert ":ORG_COMMENTS_CREATED_AT: 2026-01-01T00:00:00+0000\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
	    (insert ":END:\n\n")
	    (insert "Local draft body\n"))
	  (with-current-buffer (find-file-noselect sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (re-search-forward "Local draft body")
	    (cl-letf (((symbol-function 'org-confluence-api--create-footer-comment)
		       (lambda (_page-id _storage &optional _config)
			 '(:status 200 :body "{\"id\":\"f123\",\"authorId\":\"acct-1\",\"createdAt\":\"2026-01-02T00:00:00.000Z\",\"resolutionStatus\":\"open\"}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-01-03T00:00:00+0000")))
	      (org-confluence-comments-push-current)))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-confluence-api--list-page-comments)
		       (lambda (_page-id kind _body-format)
			 (should (equal kind "footer-comments"))
			 '(:status 200 :body "{\"results\":[{\"id\":\"f123\",\"authorId\":\"acct-1\",\"body\":{\"storage\":{\"value\":\"<p>Remote copy</p>\",\"representation\":\"storage\"}},\"resolutionStatus\":\"open\"}]}")))
		      ((symbol-function 'org-confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
		      ((symbol-function 'org-confluence-comments-import-sync-timestamp)
		       (lambda () "2026-01-04T00:00:00+0000")))
	      (should (= 0 (org-confluence-comments-import-footer)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (let ((remote-count 0)
		  (heading-count 0))
	      (goto-char (point-min))
	      (while (re-search-forward org-heading-regexp nil t)
		(setq heading-count (1+ heading-count)))
	      (goto-char (point-min))
	      (while (search-forward ":ORG_COMMENTS_REMOTE_ID: f123" nil t)
		(setq remote-count (1+ remote-count)))
	      (should (= heading-count 1))
	      (should (= remote-count 1)))
	    (goto-char (point-min))
	    (should (search-forward "* TODO" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: local-1" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_LAST_SEEN_AT: 2026-01-04T00:00:00+0000" nil t))
	    (should (search-forward "Local draft body" nil t))
	    (should-not (search-forward "Remote copy" nil t)))
	  (with-temp-buffer
	    (insert-file-contents source)
	    (should-not (search-forward "ORG_COMMENTS_REMOTE_ID" nil t))))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(provide 'org-confluence-api-test)
;;; org-confluence-api-test.el ends here
