;;; org-confluence-api-test.el --- Tests for Org Confluence cfl API wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavior tests for Confluence publish command construction.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

;; Ensure repo modules are reachable for isolated batch test runners.
(let ((root (file-name-as-directory
	     (locate-dominating-file (or load-file-name buffer-file-name)
				     "domains.yaml"))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root))
  (add-to-list 'load-path (expand-file-name "packages/org-confluence" root))
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
     (let ((hub/confluence-api-default-space "~personal"))
       (should (equal (hub/confluence-api--space-from-buffer) "ENG"))))))

(ert-deftest hub/confluence-api--space-from-default ()
  "Read default Confluence space when Org buffer has no space keyword."
  (hub/confluence-api-test--with-org-buffer
   "* Title"
   (lambda ()
     (let ((hub/confluence-api-default-space "~personal"))
       (should (equal (hub/confluence-api--space-from-buffer) "~personal"))))))

(ert-deftest hub/confluence-api--page-url ()
  "Build browser URLs from Confluence page metadata."
  (let ((hub/confluence-api-base-url "https://example.atlassian.net/"))
    (should (equal (hub/confluence-api--page-url "123" "ENG")
		   "https://example.atlassian.net/wiki/spaces/ENG/pages/123"))))

(ert-deftest hub/confluence-api--page-url-strips-wiki-suffix ()
  "Accept a Confluence base URL that already includes /wiki."
  (let ((hub/confluence-api-base-url "https://example.atlassian.net/wiki"))
    (should (equal (hub/confluence-api--page-url "123")
		   "https://example.atlassian.net/wiki/pages/123"))))

(ert-deftest hub/confluence-api--read-cfl-config ()
  "Read Confluence Cloud REST credentials from a cfl config file."
  (let ((config-file (make-temp-file "hub-cfl-config-" nil ".yml")))
    (unwind-protect
	(progn
	  (with-temp-file config-file
	    (insert "base_url: https://example.atlassian.net/wiki\n")
	    (insert "cloud_id: cloud-123\n")
	    (insert "email: author@example.com\n")
	    (insert "api_token: secret-token\n"))
	  (should (equal (hub/confluence-api--read-cfl-config config-file)
			 '(:base-url "https://example.atlassian.net"
				     :cloud-id "cloud-123"
				     :email "author@example.com"
				     :api-token "secret-token"))))
      (delete-file config-file))))

(ert-deftest hub/confluence-api--read-cfl-config-cloud-id-optional ()
  "Read cfl configs that omit cloud ID when base URL auth is available."
  (let ((config-file (make-temp-file "hub-cfl-config-" nil ".yml")))
    (unwind-protect
	(progn
	  (with-temp-file config-file
	    (insert "base_url: https://example.atlassian.net\n")
	    (insert "email: author@example.com\n")
	    (insert "api_token: secret-token\n"))
	  (should (equal (hub/confluence-api--read-cfl-config config-file)
			 '(:base-url "https://example.atlassian.net"
				     :cloud-id nil
				     :email "author@example.com"
				     :api-token "secret-token"))))
      (delete-file config-file))))

(ert-deftest hub/confluence-api--read-cfl-config-missing-required-field-redacts-token ()
  "Report missing required cfl config fields without leaking secrets."
  (let ((config-file (make-temp-file "hub-cfl-config-" nil ".yml")))
    (unwind-protect
	(progn
	  (with-temp-file config-file
	    (insert "base_url: https://example.atlassian.net\n")
	    (insert "cloud_id: cloud-123\n")
	    (insert "api_token: secret-token\n"))
	  (should-error (hub/confluence-api--read-cfl-config config-file)
			:type 'user-error)
	  (condition-case error
	      (hub/confluence-api--read-cfl-config config-file)
	    (user-error
	     (should (string-match-p "email" (error-message-string error)))
	     (should-not (string-match-p "secret-token" (error-message-string error))))))
      (delete-file config-file))))

(ert-deftest hub/confluence-api--comment-list-url ()
  "Build Confluence Cloud REST URLs for page comments."
  (let ((config '(:base-url "https://example.atlassian.net/wiki")))
    (should (equal (hub/confluence-api--comment-list-url
		    "123" "inline-comments" "storage" config)
		   "https://example.atlassian.net/wiki/api/v2/pages/123/inline-comments?body-format=storage"))))

(ert-deftest hub/confluence-api--rest-request-uses-authenticated-transport ()
  "Send REST requests through the injected transport without network access."
  (let* ((received nil)
	 (hub/confluence-api--rest-transport
	  (lambda (method url headers body)
	    (setq received (list method url headers body))
	    '(:status 200 :body "{}"))))
    (should (equal (hub/confluence-api--rest-request
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

(ert-deftest hub/confluence-api--users-bulk-uses-rest-request ()
  "Look up users through Confluence users-bulk without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'hub/confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"results\":[]}"))))
      (should (equal (hub/confluence-api--users-bulk
		      '("acct-1" "acct-2") '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"results\":[]}")))
      (should (equal received
		     '("POST"
		       "https://example.atlassian.net/wiki/api/v2/users-bulk"
		       "{\"accountIds\":[\"acct-1\",\"acct-2\"]}"
		       (:base-url "https://example.atlassian.net")))))))

(ert-deftest hub/confluence-api--list-page-comments-uses-read-only-rest-request ()
  "List page comments through the REST wrapper without network access."
  (let ((received nil))
    (cl-letf (((symbol-function 'hub/confluence-api--rest-request)
	       (lambda (method url body config)
		 (setq received (list method url body config))
		 '(:status 200 :body "{\"results\":[]}"))))
      (should (equal (hub/confluence-api--list-page-comments
		      "123" "footer-comments" "storage" '(:base-url "https://example.atlassian.net"))
		     '(:status 200 :body "{\"results\":[]}")))
      (should (equal received
		     '("GET"
		       "https://example.atlassian.net/wiki/api/v2/pages/123/footer-comments?body-format=storage"
		       nil
		       (:base-url "https://example.atlassian.net")))))))

(ert-deftest hub/confluence-api--rest-request-normalizes-url-buffer-response ()
  "Normalize url.el response buffers into plist responses."
  (let* ((url-buffer (generate-new-buffer " *hub-url-response-test*"))
	 (hub/confluence-api--rest-transport
	  (lambda (&rest _args)
	    url-buffer)))
    (with-current-buffer url-buffer
      (setq-local url-http-response-status 200)
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"results\":[]}"))
    (should (equal (hub/confluence-api--rest-request
		    "GET" "https://example.test" nil
		    '(:email "author@example.com" :api-token "secret-token"))
		   '(:status 200 :body "{\"results\":[]}")))
    (should-not (buffer-live-p url-buffer))))

(ert-deftest hub/confluence-people-resolve-updates-unresolved-entries ()
  "Resolve cached Confluence account IDs through users-bulk."
  (let* ((dir (make-temp-file "hub-confluence-people-resolve-" t))
	 (source (expand-file-name "article.org" dir))
	 (global-dir (make-temp-file "hub-confluence-people-global-" t))
	 (org-directory global-dir)
	 (people-file (hub/confluence-people-global-file)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (hub/confluence-people-cache-identity "acct-1" nil)
	  (cl-letf (((symbol-function 'hub/confluence-api--users-bulk)
		     (lambda (account-ids &optional _config)
		       (should (equal account-ids '("acct-1")))
		       '(:status 200 :body "{\"results\":[{\"accountId\":\"acct-1\",\"displayName\":\"Alice Example\",\"email\":\"alice@example.com\",\"accountStatus\":\"active\",\"timeZone\":\"Europe/Tallinn\"}]}") )))
	    (should (= 1 (hub/confluence-people-resolve))))
	  (with-temp-buffer
	    (insert-file-contents people-file)
	    (should (search-forward ":HUB_PERSON_DISPLAY_NAME: Alice Example" nil t))
	    (should (search-forward ":HUB_PERSON_EMAIL: alice@example.com" nil t))
	    (should (search-forward ":HUB_PERSON_ACCOUNT_STATUS: active" nil t))
	    (should (search-forward ":HUB_PERSON_TIME_ZONE: Europe/Tallinn" nil t))))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest hub/confluence-people-resolve-reports-no-unresolved-people ()
  "Resolving people without unresolved entries only reports a message."
  (let* ((global-dir (make-temp-file "hub-confluence-people-global-" t))
	 (org-directory global-dir)
	 (reported nil))
    (unwind-protect
	(progn
	  (with-temp-file (hub/confluence-people-global-file)
	    (insert "#+title: Confluence People\n\n"))
	  (cl-letf (((symbol-function 'message)
		     (lambda (format-string &rest args)
		       (setq reported (apply #'format format-string args))))
		    ((symbol-function 'hub/confluence-api--users-bulk)
		     (lambda (&rest _args)
		       (error "Should not call users-bulk without unresolved people"))))
	    (should (= 0 (hub/confluence-people-resolve)))
	    (should (equal reported "No unresolved Confluence people"))))
      (delete-directory global-dir t))))

(ert-deftest hub/confluence-comment-list-renders-diagnostic-buffer ()
  "Render remote comments in a diagnostic buffer without writing sidecars."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n\nBody"
   (lambda ()
     (let ((sidecar (concat (file-name-sans-extension (or buffer-file-name "article.org"))
			    ".comments.org"))
	   (opened nil)
	   (calls nil))
       (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		  (lambda (page-id kind body-format)
		    (push (list page-id kind body-format) calls)
		    (if (string= kind "footer-comments")
			'(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"resolutionStatus\":\"resolved\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Footer</p>\",\"representation\":\"storage\"}}}]}")
		      '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"resolutionStatus\":\"open\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\",\"representation\":\"storage\"}}}]}") )))
		 ((symbol-function 'pop-to-buffer)
		  (lambda (buffer &rest _args)
		    (setq opened buffer))))
	 (hub/confluence-comment-list)
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

(ert-deftest hub/confluence-comment-import-inline-appends-unanchored-sidecar-entry ()
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
	  (cl-letf (((symbol-function 'hub/confluence-people-cache-identity) #'ignore)
		    ((symbol-function 'hub/confluence-api--list-page-comments)
		     (lambda (page-id kind body-format)
		       (should (equal page-id "123"))
		       (should (equal kind "inline-comments"))
		       (should (equal body-format "storage"))
		       '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"version\":{\"authorId\":\"acct-1\",\"createdAt\":\"2026-06-16T10:11:12.000Z\"},\"inlineCommentProperties\":{\"originalSelection\":\"selected text\",\"markerRef\":\"m1\"},\"body\":{\"storage\":{\"value\":\"<p>Inline body</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (hub/confluence-comment-import-inline))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "* OPEN " nil t))
	    (should (search-forward "“selected text” — Inline body" nil t))
	    (should (search-forward ":HUB_COMMENT_ID: remote-confluence-i1" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_ID: i1" nil t))
	    (should (search-forward ":HUB_COMMENT_SYNC_KIND: inline" nil t))
	    (should (search-forward ":HUB_COMMENT_TARGET_TEXT: selected text" nil t))
	    (should-not (search-forward ":HUB_COMMENT_REMOTE_TARGET_JSON:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_TARGET:" nil t))
	    (should (search-forward "<p>Inline body</p>" nil t)))
	  (let ((comments (hub/org-comment-collect (current-buffer) t)))
	    (should (= 1 (length comments)))
	    (should (eq 'stale (plist-get (car comments) :anchor-state)))
	    (should (equal "inline" (plist-get (car comments) :sync-kind)))
	    (should (equal "selected text" (plist-get (car comments) :target-text)))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-inline-imports-replies-under-root ()
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
	  (let ((hub/confluence-comment-import-resolve-people nil))
	    (cl-letf (((symbol-function 'hub/confluence-people-cache-identity) #'ignore)
		      ((symbol-function 'hub/confluence-api--list-page-comments)
		       (lambda (_page-id _kind _body-format)
			 '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"authorId\":\"acct-a\",\"body\":{\"storage\":{\"value\":\"<p>Root</p>\",\"representation\":\"storage\"}}}]}")))
		      ((symbol-function 'hub/confluence-api--list-comment-children)
		       (lambda (comment-id kind body-format)
			 (should (equal comment-id "i1"))
			 (should (equal kind "inline-comments"))
			 (should (equal body-format "storage"))
			 '(:status 200 :body "{\"results\":[{\"id\":\"r1\",\"parentCommentId\":\"i1\",\"authorId\":\"acct-b\",\"createdAt\":\"2026-06-10T14:31:00.000Z\",\"body\":{\"storage\":{\"value\":\"<p>Reply body</p>\",\"representation\":\"storage\"}}}]}"))))
	      (should (= 2 (hub/confluence-comment-import-inline)))
	      (should (= 0 (hub/confluence-comment-import-inline)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "* OPEN [1 reply] acct-a" nil t))
	    (should (search-forward "** Reply · acct-b" nil t))
	    (should (search-forward "Reply body" nil t))
	    (should (search-forward ":HUB_COMMENT_ID: remote-confluence-r1" nil t))
	    (should (search-forward ":HUB_COMMENT_SYNC_KIND: reply" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_PARENT_ID: i1" nil t))
	    (should-not (search-forward ":HUB_COMMENT_PARENT_ID:" nil t)))
	  (let ((comments (hub/org-comment-collect (current-buffer) t)))
	    (should (= 1 (length comments)))
	    (should (= 1 (length (plist-get (car comments) :replies))))
	    (should (equal "r1" (plist-get (car (plist-get (car comments) :replies)) :remote-id)))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-imports-footer-and-inline ()
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
	    (cl-letf (((symbol-function 'hub/confluence-people-cache-identity) #'ignore)
		      ((symbol-function 'hub/confluence-api--list-page-comments)
		       (lambda (_page-id kind _body-format)
			 (push kind calls)
			 (if (string= kind "footer-comments")
			     '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Footer</p>\",\"representation\":\"storage\"}}}]}")
			   '(:status 200 :body "{\"results\":[{\"id\":\"i1\",\"body\":{\"storage\":{\"value\":\"<p>Inline</p>\",\"representation\":\"storage\"}}}]}") ))))
	      (should (= 2 (hub/confluence-comment-import)))
	      (should (equal (nreverse calls) '("footer-comments" "inline-comments"))))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-footer-appends-remote-sidecar-entry ()
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
	  (cl-letf (((symbol-function 'hub/confluence-people-cache-identity) #'ignore)
		    ((symbol-function 'hub/confluence-api--list-page-comments)
		     (lambda (page-id kind body-format)
		       (should (equal page-id "123"))
		       (should (equal kind "footer-comments"))
		       (should (equal body-format "storage"))
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"authorId\":\"acct-1\",\"author\":{\"displayName\":\"Alice Example\",\"accountId\":\"acct-1\"},\"createdAt\":\"2026-06-15T17:42:00.000Z\",\"body\":{\"storage\":{\"value\":\"<p>Footer</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (hub/confluence-comment-import-footer))))
	  (should (equal (buffer-string) "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n"))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "#+todo: OPEN TODO | RESOLVED" nil t))
	    (should (search-forward "* OPEN Page" nil t))
	    (should (search-forward "Footer" nil t))
	    (should (search-forward ":HUB_COMMENT_ID: remote-confluence-f1" nil t))
	    (should-not (search-forward ":HUB_COMMENT_AUTHOR:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_CREATED_AT:" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_ID: f1" nil t))
	    (should (search-forward ":HUB_COMMENT_SOURCE: confluence" nil t))
	    (should (search-forward ":HUB_COMMENT_SYNC_KIND: footer" nil t))
	    (should-not (search-forward ":HUB_COMMENT_BODY_FORMAT: storage" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_AUTHOR_ID: acct-1" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME: Alice Example" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_CREATED_AT: 2026-06-15T17:42:00.000Z" nil t))
	    (should-not (search-forward ":HUB_COMMENT_TARGET:" nil t))
	    (should (search-forward "<p>Footer</p>" nil t))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-footer-caches-v2-author-id ()
  "Import caches v2 author IDs even when display names are absent."
  (let* ((dir (make-temp-file "hub-confluence-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (global-dir (make-temp-file "hub-confluence-people-" t))
	 (org-directory global-dir))
    (unwind-protect
	(with-current-buffer (find-file-noselect source)
	  (erase-buffer)
	  (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
	  (save-buffer)
	  (org-mode)
	  (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"version\":{\"authorId\":\"acct-v2\",\"createdAt\":\"2026-06-16T10:11:12.000Z\"},\"body\":{\"storage\":{\"value\":\"<p>Body</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (hub/confluence-comment-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents (hub/confluence-people-global-file))
	    (should (search-forward "* acct-v2" nil t))
	    (should (search-forward ":HUB_CONFLUENCE_ACCOUNT_ID: acct-v2" nil t))))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest hub/confluence-comment-import-footer-backfills-v2-version-metadata ()
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
	    (insert ":HUB_COMMENT_ID: remote-confluence-f1\n")
	    (insert ":HUB_COMMENT_REMOTE_ID: f1\n")
	    (insert ":HUB_COMMENT_SOURCE: confluence\n")
	    (insert ":HUB_COMMENT_SYNC_KIND: footer\n")
	    (insert ":HUB_COMMENT_BODY_FORMAT: storage\n")
	    (insert ":END:\n\n")
	    (insert "Locally preserved body\n"))
	  (cl-letf (((symbol-function 'hub/confluence-people-cache-identity) #'ignore)
		    ((symbol-function 'hub/confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"version\":{\"authorId\":\"acct-v2\",\"createdAt\":\"2026-06-16T10:11:12.000Z\"},\"body\":{\"storage\":{\"value\":\"<p>Changed remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 0 (hub/confluence-comment-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":HUB_COMMENT_AUTHOR:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_CREATED_AT:" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_AUTHOR_ID: acct-v2" nil t))
	    (should (search-forward "Locally preserved body" nil t))
	    (should-not (search-forward "Changed remote" nil t))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-footer-backfills-missing-remote-metadata ()
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
	    (insert ":HUB_COMMENT_ID: remote-confluence-f1\n")
	    (insert ":HUB_COMMENT_REMOTE_ID: f1\n")
	    (insert ":HUB_COMMENT_SOURCE: confluence\n")
	    (insert ":HUB_COMMENT_SYNC_KIND: footer\n")
	    (insert ":HUB_COMMENT_BODY_FORMAT: storage\n")
	    (insert ":END:\n\n")
	    (insert "Locally preserved body\n"))
	  (cl-letf (((symbol-function 'hub/confluence-people-cache-identity) #'ignore)
		    ((symbol-function 'hub/confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"authorId\":\"acct-1\",\"author\":{\"displayName\":\"Alice Example\",\"accountId\":\"acct-1\"},\"createdAt\":\"2026-06-15T17:42:00.000Z\",\"body\":{\"storage\":{\"value\":\"<p>Changed remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 0 (hub/confluence-comment-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (= 1 (how-many ":HUB_COMMENT_REMOTE_ID: f1" (point-min) (point-max))))
	    (should-not (search-forward ":HUB_COMMENT_AUTHOR:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_CREATED_AT:" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_AUTHOR_ID: acct-1" nil t))
	    (should (search-forward "Locally preserved body" nil t))
	    (should-not (search-forward "Changed remote" nil t))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-footer-is-idempotent ()
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
	  (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 1 (hub/confluence-comment-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (goto-char (point-min))
	    (search-forward "<p>Remote</p>")
	    (replace-match "Locally edited body" nil t)
	    (write-region (point-min) (point-max) sidecar nil 'silent))
	  (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		     (lambda (&rest _args)
		       '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Changed remote</p>\",\"representation\":\"storage\"}}}]}") )))
	    (should (= 0 (hub/confluence-comment-import-footer))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (= 1 (how-many ":HUB_COMMENT_REMOTE_ID: f1" (point-min) (point-max))))
	    (should (search-forward "Locally edited body" nil t))
	    (should-not (search-forward "Changed remote" nil t))))
      (delete-directory dir t))))

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
    (cl-letf (((symbol-function 'hub/confluence-publish-dwim)
	       (lambda (&rest args)
		 (setq received args))))
      (hub/confluence-publish-from-export-dispatch nil t nil t)
      (should (equal received '(nil nil nil t nil t nil))))))

(ert-deftest hub/confluence-publish-and-open-from-export-dispatch-opens-result ()
  "Publish from Org export dispatch and open the resulting page."
  (let ((opened nil))
    (cl-letf (((symbol-function 'hub/confluence-publish-dwim)
	       (lambda (&rest _args) "789"))
	      ((symbol-function 'hub/confluence-open-page)
	       (lambda (page-id space)
		 (setq opened (list page-id space)))))
      (let ((hub/confluence-api-default-space "~personal"))
	(hub/confluence-publish-and-open-from-export-dispatch nil t nil t))
      (should (equal opened '("789" "~personal"))))))

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

(ert-deftest hub/confluence-import-storage-to-org-table-emoji-bold-spacing ()
  "Add Org emphasis boundary spacing after emoji in table cells."
  (should (equal (hub/confluence-import-storage-to-org
		  "<table><tbody><tr><th><p>📆<strong>T ime</strong></p></th></tr></tbody></table>")
		 "| 📆 *T ime* |\n|-----------|")))

(ert-deftest hub/confluence-import-storage-to-org-table-trims-bold-cell-text ()
  "Trim storage whitespace inside bold table cell text."
  (should (equal (hub/confluence-import-storage-to-org
		  "<table><tbody><tr><th><p><strong> T ime</strong></p></th></tr></tbody></table>")
		 "| *T ime* |\n|---------|")))

(ert-deftest hub/confluence-import-storage-to-org-structured-macro-body ()
  "Import unknown structured macro rich text body without macro parameters."
  (should (equal (hub/confluence-import-storage-to-org
		  "<ac:structured-macro ac:name=\"custom\"><ac:parameter ac:name=\"bgColor\">#fff</ac:parameter><ac:rich-text-body><p>Body</p><ul><li><p>Point</p></li></ul></ac:rich-text-body></ac:structured-macro><table><tbody><tr><th>H</th></tr></tbody></table>")
		 "Body\n- Point\n| H |\n|---|")))

(ert-deftest hub/confluence-import-storage-to-org-callout-macro ()
  "Import Confluence panel-like macros as semantic Org callouts."
  (should (equal (hub/confluence-import-storage-to-org
		  "<ac:structured-macro ac:name=\"warning\"><ac:parameter ac:name=\"title\">Heads up</ac:parameter><ac:rich-text-body><p>Body</p></ac:rich-text-body></ac:structured-macro>")
		 "#+ATTR_CALLOUT: :type warning :title \"Heads up\"\n#+begin_callout\nBody\n#+end_callout")))

(ert-deftest hub/confluence-import-storage-to-org-panel-macro-preserves-type ()
  "Import generic Confluence panels as callouts with panel type preserved."
  (should (equal (hub/confluence-import-storage-to-org
		  "<ac:structured-macro ac:name=\"panel\"><ac:rich-text-body><p>Panel body</p></ac:rich-text-body></ac:structured-macro>")
		 "#+ATTR_CALLOUT: :type panel\n#+begin_callout\nPanel body\n#+end_callout")))

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

(ert-deftest hub/confluence-import-storage-to-org-document-emoji ()
  "Map Atlassian document emoticon metadata to a Unicode emoji."
  (should (equal (hub/confluence-import-storage-to-org
		  "<h2><ac:emoticon ac:name=\"blue-star\" ac:emoji-shortname=\":document:\" ac:emoji-fallback=\":document:\" /> Related Material</h2>")
		 "** 📄 Related Material")))

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

(ert-deftest hub/confluence-commands--created-page-id ()
  "Parse created page IDs from cfl output."
  (should (equal (hub/confluence-commands--created-page-id "ID: 789") "789"))
  (should (equal (hub/confluence-commands--created-page-id "{\"id\":\"456\"}") "456"))
  (should (equal (hub/confluence-commands--created-page-id "https://example/wiki/spaces/X/pages/123/Page") "123")))

(ert-deftest hub/confluence-publish-dwim-records-created-page-metadata ()
  "Record page ID and space in the Org buffer after create flow succeeds."
  (hub/confluence-api-test--with-org-buffer
   "#+TITLE: Created Page\n\nBody"
   (lambda ()
     (let ((hub/confluence-api-default-space "~personal")
	   (xhtml-file (make-temp-file "org-confluence-test-" nil ".xhtml")))
       (unwind-protect
	   (cl-letf (((symbol-function 'org-confluence-export) (lambda (&rest _) "<p>Body</p>"))
		     ((symbol-function 'org-confluence-image-assets) (lambda (&rest _) nil))
		     ((symbol-function 'hub/confluence-commands--write-temp-xhtml) (lambda (_xhtml) xhtml-file))
		     ((symbol-function 'hub/confluence-commands--run-output)
		      (lambda (command)
			(should (string-match-p (regexp-quote "--space") command))
			(should (string-match-p (regexp-quote "~personal") command))
			"Created page\nID: 789"))
		     ((symbol-function 'hub/confluence-commands--run) (lambda (_command) 0)))
	     (hub/confluence-publish-dwim)
	     (should (equal (buffer-string)
			    "#+TITLE: Created Page\n#+CONFLUENCE_PAGE_ID: 789\n#+CONFLUENCE_SPACE: ~personal\n\nBody")))
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

(ert-deftest hub/confluence-commands--upload-assets-deduplicates-filenames ()
  "Upload repeated image assets with the same attachment filename only once."
  (let ((commands nil)
	(source-file (make-temp-file "org-confluence-source-" nil ".png")))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "png"))
	  (cl-letf (((symbol-function 'hub/confluence-api--attachment-upload-command)
		     (lambda (page-id file-path)
		       (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		    ((symbol-function 'hub/confluence-commands--run)
		     (lambda (command) (push command commands) 0)))
	    (hub/confluence-commands--upload-assets
	     "123"
	     (list (list :source-path source-file :filename "foo-hash.png")
		   (list :source-path source-file :filename "foo-hash.png")))
	    (should (equal commands (list "upload:123:foo-hash.png")))))
      (when (file-exists-p source-file)
	(delete-file source-file)))))

(ert-deftest hub/confluence-publish-dwim-creates-then-uploads-images ()
  "Create a page before uploading image assets in create flow."
  (hub/confluence-api-test--with-org-buffer
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
		       ((symbol-function 'hub/confluence-commands--write-temp-xhtml)
			(lambda (_xhtml) xhtml-file))
		       ((symbol-function 'hub/confluence-api--attachment-upload-command)
			(lambda (page-id file-path)
			  (format "upload:%s:%s" page-id (file-name-nondirectory file-path))))
		       ((symbol-function 'hub/confluence-commands--run-output)
			(lambda (command)
			  (push command events)
			  "ID: 789"))
		       ((symbol-function 'hub/confluence-commands--run)
			(lambda (command) (push command events) 0)))
	       (hub/confluence-publish-dwim)
	       (should (equal (nreverse events)
			      (list (format "cfl page create --space ENG --title Page --file %s --storage" xhtml-file)
				    "upload:789:foo-hash.png"
				    (format "cfl page edit 789 --file %s --storage" xhtml-file))))))
	 (when (file-exists-p source-file)
	   (delete-file source-file))
	 (when (file-exists-p xhtml-file)
	   (delete-file xhtml-file)))))))

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

(ert-deftest hub/confluence-comment-import-marks-missing-for-fetched-kind-only ()
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
	    (insert "* OPEN Inline linked\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-i-old\n:HUB_COMMENT_REMOTE_ID: i-old\n:HUB_COMMENT_SYNC_KIND: inline\n:END:\n\nBody\n")
	    (insert "* TODO Footer linked\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-f-old\n:HUB_COMMENT_REMOTE_ID: f-old\n:HUB_COMMENT_SYNC_KIND: footer\n:END:\n\nBody\n"))
	  (let ((hub/confluence-comment-import-resolve-people nil))
	    (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (hub/confluence-comment-import-footer)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":HUB_COMMENT_REMOTE_ID: f-old" nil t))
	    (should (search-forward ":HUB_COMMENT_REMOTE_STATE: missing" nil t))
	    (org-mode)
	    (goto-char (point-min))
	    (should (search-forward ":HUB_COMMENT_REMOTE_ID: i-old" nil t))
	    (org-back-to-heading t)
	    (let ((inline-end (save-excursion (org-end-of-subtree t t))))
	      (should-not (save-excursion
			    (re-search-forward ":HUB_COMMENT_REMOTE_STATE: missing" inline-end t))))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-derives-local-status-from-remote ()
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
	    (insert "* TODO Working open\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-f-open\n:HUB_COMMENT_REMOTE_ID: f-open\n:HUB_COMMENT_SYNC_KIND: footer\n:END:\n\nBody\n")
	    (insert "* TODO Working resolved\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-f-resolved\n:HUB_COMMENT_REMOTE_ID: f-resolved\n:HUB_COMMENT_SYNC_KIND: footer\n:END:\n\nBody\n")
	    (insert "* RESOLVED Reopened\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-f-reopen\n:HUB_COMMENT_REMOTE_ID: f-reopen\n:HUB_COMMENT_SYNC_KIND: footer\n:END:\n\nBody\n"))
	  (let ((hub/confluence-comment-import-resolve-people nil)
		(reported nil))
	    (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"results\":[{\"id\":\"f-open\",\"resolutionStatus\":\"open\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Open</p>\"}}},{\"id\":\"f-resolved\",\"resolutionStatus\":\"resolved\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Done</p>\"}}},{\"id\":\"f-reopen\",\"resolutionStatus\":\"open\",\"status\":\"current\",\"body\":{\"storage\":{\"value\":\"<p>Open</p>\"}}}]}")))
		      ((symbol-function 'hub/confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}")))
		      ((symbol-function 'message)
		       (lambda (format-string &rest args)
			 (setq reported (apply #'format format-string args)))))
	      (should (= 0 (hub/confluence-comment-import-footer)))
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
	      (should (search-forward (concat ":HUB_COMMENT_REMOTE_ID: " (car case)) nil t))
	      (org-back-to-heading t)
	      (should (equal (cdr case) (org-get-todo-state)))))
	  (should (get-buffer "*Confluence Comment Sync Report*")))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-ignores-content-status-for-resolution ()
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
	    (insert "* TODO Working\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-f-status\n:HUB_COMMENT_REMOTE_ID: f-status\n:HUB_COMMENT_SYNC_KIND: footer\n:END:\n\nBody\n"))
	  (let ((hub/confluence-comment-import-resolve-people nil))
	    (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"results\":[{\"id\":\"f-status\",\"status\":\"resolved\",\"body\":{\"storage\":{\"value\":\"<p>Status only</p>\"}}}]}")))
		      ((symbol-function 'hub/confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (hub/confluence-comment-import-footer)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (should (search-forward ":HUB_COMMENT_REMOTE_ID: f-status" nil t))
	    (org-back-to-heading t)
	    (should (equal "TODO" (org-get-todo-state)))
	    (should-not (search-forward ":HUB_COMMENT_REMOTE_RESOLUTION_STATUS:" nil t))))
      (delete-directory dir t))))

(ert-deftest hub/confluence-comment-import-restores-remote-present-again ()
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
	    (insert "* OPEN Was missing\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-f1\n:HUB_COMMENT_REMOTE_ID: f1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_STATE: missing\n:HUB_COMMENT_REMOTE_MISSING_AT: 2026-01-01T00:00:00+0000\n:END:\n\nBody\n"))
	  (let ((hub/confluence-comment-import-resolve-people nil))
	    (cl-letf (((symbol-function 'hub/confluence-api--list-page-comments)
		       (lambda (&rest _args)
			 '(:status 200 :body "{\"results\":[{\"id\":\"f1\",\"body\":{\"storage\":{\"value\":\"<p>Back</p>\"}}}]}")))
		      ((symbol-function 'hub/confluence-api--list-comment-children)
		       (lambda (&rest _args) '(:status 200 :body "{\"results\":[]}"))))
	      (should (= 0 (hub/confluence-comment-import-footer)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":HUB_COMMENT_REMOTE_STATE:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_REMOTE_MISSING_AT:" nil t))))
      (delete-directory dir t))))

(provide 'org-confluence-api-test)
;;; org-confluence-api-test.el ends here
