;;; org-google-docs-live-conformance-test.el --- Live Google Docs conformance tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in live tests for API-normalized semantic conformance.  These tests create
;; real Google Docs and compare canonical local IR against fetched Docs JSON IR.
;; Run with ORG_GOOGLE_DOCS_LIVE_TESTS=1 from an Emacs session/config that has
;; gdocs loaded and authenticated.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-comments"
					  (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-sync"
					  (file-name-directory load-file-name)))

(require 'org-google-docs)

(defconst org-google-docs-live-conformance--test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the live conformance test file.")

(defvar org-google-docs-live-conformance-timeout 90
  "Seconds to wait for live Google Docs callbacks in conformance tests.")

(defvar org-google-docs-live-conformance-debug-file
  (expand-file-name "org-google-docs-live-conformance-last.el"
		    temporary-file-directory)
  "File where the last live conformance mismatch is written.")

(defun org-google-docs-live-conformance--enabled-p ()
  "Return non-nil when live conformance tests should run."
  (getenv "ORG_GOOGLE_DOCS_LIVE_TESTS"))

(defun org-google-docs-live-conformance--account ()
  "Return the Google Docs account for live conformance tests."
  (or (getenv "ORG_GOOGLE_DOCS_LIVE_ACCOUNT") "personal"))

(defun org-google-docs-live-conformance--configure-env-account ()
  "Configure gdocs credentials from live-test environment variables.
This enables non-interactive automation without depending on personal Emacs
modules.  Set `ORG_GOOGLE_DOCS_CLIENT_ID', `ORG_GOOGLE_DOCS_CLIENT_SECRET', and
optionally `ORG_GOOGLE_DOCS_REFRESH_TOKEN'.  When a refresh token is present,
write a token file so batch API calls can refresh without launching OAuth."
  (when-let* ((client-id (getenv "ORG_GOOGLE_DOCS_CLIENT_ID"))
	      (client-secret (getenv "ORG_GOOGLE_DOCS_CLIENT_SECRET")))
    (let ((account (org-google-docs-live-conformance--account))
	  (refresh-token (getenv "ORG_GOOGLE_DOCS_REFRESH_TOKEN")))
      (setq gdocs-accounts
	    (list (cons account
			`((client-id . ,client-id)
			  (client-secret . ,client-secret)))))
      (when (and refresh-token (fboundp 'gdocs-auth--write-token-file))
	(gdocs-auth--write-token-file
	 account
	 `((access_token . "")
	   (refresh_token . ,refresh-token)
	   (expires_at . 0)
	   (token_type . "Bearer")
	   (client_id . ,client-id)
	   (client_secret . ,client-secret)))))))

(defun org-google-docs-live-conformance--configure-accounts ()
  "Configure live-test Google account credentials when possible."
  (org-google-docs-live-conformance--configure-env-account)
  (when (and (not (and (boundp 'gdocs-accounts) gdocs-accounts))
	     (fboundp 'hub/org-google-docs-configure-accounts-from-auth-source))
    (hub/org-google-docs-configure-accounts-from-auth-source 'noerror))
  (and (boundp 'gdocs-accounts) gdocs-accounts))

(defun org-google-docs-live-conformance--require-live ()
  "Skip unless live Google Docs conformance testing is configured."
  (unless (org-google-docs-live-conformance--enabled-p)
    (ert-skip "Set ORG_GOOGLE_DOCS_LIVE_TESTS=1 to run live conformance tests"))
  (unless (and (fboundp 'gdocs-api-create-document)
	       (fboundp 'gdocs-api-batch-update)
	       (fboundp 'gdocs-api-get-document)
	       (fboundp 'gdocs-convert-org-buffer-to-ir)
	       (fboundp 'gdocs-convert-docs-json-to-ir))
    (org-google-docs-ensure-gdocs-loaded))
  (unless (org-google-docs-live-conformance--configure-accounts)
    (ert-skip "Configure gdocs-accounts or ORG_GOOGLE_DOCS_CLIENT_ID/SECRET")))

(defun org-google-docs-live-conformance--await (starter)
  "Run async STARTER and wait for its callback value.
STARTER receives two callbacks: success and error."
  (let ((done nil)
	value
	error-value)
    (funcall starter
	     (lambda (result)
	       (setq value result
		     done t))
	     (lambda (error)
	       (setq error-value error
		     done t)))
    (let ((deadline (+ (float-time)
		       org-google-docs-live-conformance-timeout)))
      (while (and (not done) (< (float-time) deadline))
	(accept-process-output nil 0.2)))
    (when error-value
      (signal (car error-value) (cdr error-value)))
    (unless done
      (error "Timed out waiting for Google Docs callback"))
    value))

(defun org-google-docs-live-conformance--create-document (title account)
  "Create a live Google Doc titled TITLE using ACCOUNT."
  (org-google-docs-live-conformance--await
   (lambda (success _error)
     (gdocs-api-create-document title success account))))

(defun org-google-docs-live-conformance--batch-update (document-id requests account)
  "Send REQUESTS to DOCUMENT-ID using ACCOUNT and wait for completion."
  (org-google-docs-live-conformance--await
   (lambda (success error)
     (gdocs-api-batch-update document-id requests success account error))))

(defun org-google-docs-live-conformance--get-document (document-id account)
  "Fetch DOCUMENT-ID using ACCOUNT and wait for the parsed JSON."
  (org-google-docs-live-conformance--await
   (lambda (success error)
     (gdocs-api-get-document document-id success account error))))

(defun org-google-docs-live-conformance--trash-document (document-id account)
  "Move live conformance DOCUMENT-ID to Drive trash using ACCOUNT."
  (when (and document-id (fboundp 'gdocs-api--request))
    (ignore-errors
      (org-google-docs-live-conformance--await
       (lambda (success error)
	 (gdocs-api--request
	  'patch
	  (concat gdocs-api--drive-base-url "/" document-id)
	  success
	  :account account
	  :body (json-encode '((trashed . t)))
	  :on-error error))))))

(defun org-google-docs-live-conformance--fixture-path (name)
  "Return absolute path to fixture NAME."
  (expand-file-name name
		    (expand-file-name
		     "fixtures"
		     org-google-docs-live-conformance--test-directory)))

(defun org-google-docs-live-conformance--canonical-run (run)
  "Return semantic conformance shape for text RUN."
  (let ((footnote-label (plist-get run :footnote-label))
	(date-element (plist-get run :date-element)))
    (list :text (if (or footnote-label date-element)
		    ""
		  (substring-no-properties (or (plist-get run :text) "")))
	  :bold (and (plist-get run :bold) t)
	  :italic (and (plist-get run :italic) t)
	  :underline (and (plist-get run :underline) t)
	  :strikethrough (and (plist-get run :strikethrough) t)
	  :code (and (plist-get run :code) t)
	  :link (plist-get run :link)
	  :date (when date-element
		  (list :timestamp (plist-get date-element :timestamp)
			:active (and (plist-get date-element :active) t)))
	  :footnote-label footnote-label)))

(defun org-google-docs-live-conformance--same-run-shape-p (left right)
  "Return non-nil when LEFT and RIGHT have same non-text semantic shape."
  (and (eq (plist-get left :bold) (plist-get right :bold))
       (eq (plist-get left :italic) (plist-get right :italic))
       (eq (plist-get left :underline) (plist-get right :underline))
       (eq (plist-get left :strikethrough) (plist-get right :strikethrough))
       (eq (plist-get left :code) (plist-get right :code))
       (equal (plist-get left :link) (plist-get right :link))
       (equal (plist-get left :date) (plist-get right :date))
       (equal (plist-get left :footnote-label)
	      (plist-get right :footnote-label))))

(defun org-google-docs-live-conformance--merge-canonical-runs (runs)
  "Merge adjacent canonical RUNS with the same semantic shape."
  (let (result)
    (dolist (run runs (nreverse result))
      (if (and result
	       (org-google-docs-live-conformance--same-run-shape-p
		(car result) run))
	  (plist-put (car result) :text
		     (concat (plist-get (car result) :text)
			     (plist-get run :text)))
	(push (copy-sequence run) result)))))

(defun org-google-docs-live-conformance--canonical-runs (runs)
  "Return semantic conformance shape for RUNS."
  (org-google-docs-live-conformance--merge-canonical-runs
   (mapcar #'org-google-docs-live-conformance--canonical-run runs)))

(defun org-google-docs-live-conformance--canonical-list (list-info)
  "Return canonical list shape for LIST-INFO."
  (when list-info
    (list :type (plist-get list-info :type)
	  :level (or (plist-get list-info :level) 0)
	  :checked (plist-get list-info :checked))))

(defun org-google-docs-live-conformance--canonical-body-elements (elements)
  "Return canonical conformance shape for nested body ELEMENTS."
  (mapcar #'org-google-docs-live-conformance--canonical-element elements))

(defun org-google-docs-live-conformance--canonical-element (element)
  "Return semantic conformance shape for IR ELEMENT."
  (pcase (plist-get element :type)
    ('paragraph
     (list :type 'paragraph
	   :style (or (plist-get element :style) 'normal)
	   :runs (org-google-docs-live-conformance--canonical-runs
		  (plist-get element :contents))
	   :list (org-google-docs-live-conformance--canonical-list
		  (plist-get element :list))))
    ('quote-block
     (list :type 'quote-block
	   :paragraphs
	   (mapcar #'org-google-docs-live-conformance--canonical-runs
		   (plist-get element :paragraphs))))
    ('source-block
     (list :type 'source-block
	   :language (or (plist-get element :language) "")
	   :lines (plist-get element :lines)))
    ('example-block
     (list :type 'example-block
	   :lines (plist-get element :lines)))
    ('callout-block
     (list :type 'callout-block
	   :callout-type (or (plist-get element :callout-type) "info")
	   :title (or (plist-get element :title) "")
	   :body (org-google-docs-live-conformance--canonical-body-elements
		  (gdocs-convert--callout-body-elements-from-paragraphs
		   element))))
    ('table
     (list :type 'table
	   :rows (mapcar
		  (lambda (row)
		    (mapcar #'org-google-docs-live-conformance--canonical-runs
			    row))
		  (plist-get element :rows))))
    ('image
     (list :type 'image
	   :path (plist-get element :path)
	   :caption (or (plist-get element :caption) "")))
    ('footnote
     (list :type 'footnote
	   :label (plist-get element :label)
	   :runs (org-google-docs-live-conformance--canonical-runs
		  (plist-get element :contents))))
    (type (list :type type))))

(defun org-google-docs-live-conformance--canonical-ir (ir)
  "Return canonical semantic conformance shape for IR."
  (mapcar #'org-google-docs-live-conformance--canonical-element
	  (gdocs-sync--filter-empty-paragraphs
	   (gdocs-sync--filter-title ir))))

(defun org-google-docs-live-conformance--write-debug (data)
  "Write conformance debug DATA to `org-google-docs-live-conformance-debug-file'."
  (when org-google-docs-live-conformance-debug-file
    (with-temp-file org-google-docs-live-conformance-debug-file
      (pp data (current-buffer)))))

(defun org-google-docs-live-conformance--diff-message (expected actual)
  "Return readable EXPECTED versus ACTUAL conformance diff message."
  (with-temp-buffer
    (insert "Expected canonical IR:\n")
    (pp expected (current-buffer))
    (insert "\nActual canonical IR:\n")
    (pp actual (current-buffer))
    (buffer-string)))

(defun org-google-docs-live-conformance--prepare-native-push (account)
  "Prepare the current fixture buffer for native push using ACCOUNT."
  (org-google-docs-live-conformance--await
   (lambda (success error)
     (condition-case err
	 (let ((image-plan (org-google-docs--preflight-images-for-push)))
	   (org-google-docs-images-begin-push
	    image-plan
	    (lambda ()
	      (condition-case inner-err
		  (progn
		    (org-google-docs--prepare-footnotes-for-body-write)
		    (funcall success t))
		((error quit)
		 (funcall error inner-err))))
	    account))
       ((error quit)
	(funcall error err))))))

(defun org-google-docs-live-conformance--local-ir-from-file (path)
  "Return local IR for Org fixture PATH."
  (with-temp-buffer
    (setq default-directory (file-name-directory path))
    (insert-file-contents path)
    (org-mode)
    (gdocs-convert-org-buffer-to-ir)))

(defun org-google-docs-live-conformance--local-ir-from-file-appending
    (path text)
  "Return local IR for Org fixture PATH with TEXT appended."
  (with-temp-buffer
    (setq default-directory (file-name-directory path))
    (insert-file-contents path)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert text)
    (org-mode)
    (gdocs-convert-org-buffer-to-ir)))

(defun org-google-docs-live-conformance--native-footnote-ir ()
  "Return expected native footnote IR for the active footnote push session."
  (when (and (boundp 'org-google-docs-footnotes--push-session)
	     org-google-docs-footnotes--push-session)
    (mapcar (lambda (reference)
	      (list :type 'footnote
		    :label (plist-get reference :label)
		    :contents (plist-get reference :body-runs)))
	    (org-google-docs-footnotes--session-reference-list
	     org-google-docs-footnotes--push-session))))

(defun org-google-docs-live-conformance--create-fetch (path account)
  "Create a document from fixture PATH using ACCOUNT and return local/remote data."
  (with-temp-buffer
    (setq default-directory (file-name-directory path))
    (insert-file-contents path)
    (org-mode)
    (let ((raw-ir (gdocs-convert-org-buffer-to-ir)))
      (org-google-docs-live-conformance--prepare-native-push account)
      (let* ((body-ir (gdocs-convert-org-buffer-to-ir))
	     (native-footnotes
	      (org-google-docs-live-conformance--native-footnote-ir))
	     (local-ir (append (org-google-docs-footnotes-filter-native-footnote-ir
				raw-ir)
			       native-footnotes))
	     (create-response
	      (org-google-docs-live-conformance--create-document
	       (format "ORG GDOCS CONFORMANCE %s"
		       (format-time-string "%Y%m%dT%H%M%S"))
	       account))
	     (document-id (alist-get 'documentId create-response))
	     (requests (gdocs-convert-ir-to-docs-requests
			(gdocs-sync--filter-title body-ir)))
	     remote-json remote-ir)
	(org-google-docs-live-conformance--batch-update
	 document-id requests account)
	(setq remote-json
	      (org-google-docs-live-conformance--get-document
	       document-id account)
	      remote-ir (gdocs-convert-docs-json-to-ir remote-json))
	(list :document-id document-id
	      :local-ir local-ir
	      :remote-json remote-json
	      :remote-ir remote-ir)))))

(defun org-google-docs-live-conformance--create-fetch-canonical-ir (path account)
  "Create a document from fixture PATH using ACCOUNT and return fetched IR."
  (let ((result (org-google-docs-live-conformance--create-fetch path account)))
    (list :document-id (plist-get result :document-id)
	  :ir (plist-get result :remote-ir))))

(defun org-google-docs-live-conformance--assert-create-fetch (fixture-name)
  "Assert that live create/fetch canonicalization holds for FIXTURE-NAME."
  (org-google-docs-live-conformance--require-live)
  (let* ((fixture (org-google-docs-live-conformance--fixture-path fixture-name))
	 (account (org-google-docs-live-conformance--account))
	 result)
    (unwind-protect
	(let (expected actual)
	  (setq result
		(org-google-docs-live-conformance--create-fetch
		 fixture account)
		expected (org-google-docs-live-conformance--canonical-ir
			  (plist-get result :local-ir))
		actual (org-google-docs-live-conformance--canonical-ir
			(plist-get result :remote-ir)))
	  (unless (equal expected actual)
	    (org-google-docs-live-conformance--write-debug
	     (list :fixture fixture-name :expected expected :actual actual)))
	  (should (equal expected actual))
	  (message "Created conformance document: %s"
		   (plist-get result :document-id)))
      (org-google-docs-live-conformance--trash-document
       (plist-get result :document-id)
       account))))

(defun org-google-docs-live-conformance--requests-for-local-ir (data local-ir)
  "Return diff requests for live DATA compared with LOCAL-IR."
  (let* ((remote-full (plist-get data :remote-ir))
	 (remote-ir (gdocs-sync--filter-empty-paragraphs
		     (gdocs-sync--filter-title remote-full))))
    (gdocs-diff-generate remote-ir
			 (gdocs-sync--filter-title local-ir)
			 (gdocs-sync--body-start-index remote-full))))

(defun org-google-docs-live-conformance--assert-immediate-diff (fixture-name)
  "Assert that live create/fetch has no immediate diff for FIXTURE-NAME."
  (org-google-docs-live-conformance--require-live)
  (let* ((fixture (org-google-docs-live-conformance--fixture-path fixture-name))
	 (account (org-google-docs-live-conformance--account))
	 result)
    (unwind-protect
	(let* ((data (setq result
			   (org-google-docs-live-conformance--create-fetch
			    fixture account)))
	       (requests (org-google-docs-live-conformance--requests-for-local-ir
			  data (plist-get data :local-ir))))
	  (unless (null requests)
	    (org-google-docs-live-conformance--write-debug
	     (list :fixture fixture-name
		   :document-id (plist-get data :document-id)
		   :local-ir (gdocs-sync--filter-title (plist-get data :local-ir))
		   :remote-ir (gdocs-sync--filter-empty-paragraphs
			       (gdocs-sync--filter-title
				(plist-get data :remote-ir)))
		   :requests requests)))
	  (should (null requests)))
      (org-google-docs-live-conformance--trash-document
       (plist-get result :document-id)
       account))))

(defun org-google-docs-live-conformance--assert-append-diff (fixture-name)
  "Assert that appending one paragraph to FIXTURE-NAME emits a tiny safe diff."
  (org-google-docs-live-conformance--require-live)
  (let* ((fixture (org-google-docs-live-conformance--fixture-path fixture-name))
	 (account (org-google-docs-live-conformance--account))
	 (append-text "\nA tiny appended paragraph.\n")
	 result)
    (unwind-protect
	(let* ((data (setq result
			   (org-google-docs-live-conformance--create-fetch
			    fixture account)))
	       (local-ir
		(org-google-docs-live-conformance--local-ir-from-file-appending
		 fixture append-text))
	       (requests (org-google-docs-live-conformance--requests-for-local-ir
			  data local-ir))
	       (request-kinds (mapcar #'caar requests)))
	  (unless (and (<= (length requests) 8)
		       (not (memq 'deleteContentRange request-kinds)))
	    (org-google-docs-live-conformance--write-debug
	     (list :fixture fixture-name
		   :document-id (plist-get data :document-id)
		   :local-ir (gdocs-sync--filter-title local-ir)
		   :remote-ir (gdocs-sync--filter-empty-paragraphs
			       (gdocs-sync--filter-title
				(plist-get data :remote-ir)))
		   :request-kinds request-kinds
		   :requests requests)))
	  (should (<= (length requests) 8))
	  (should-not (memq 'deleteContentRange request-kinds)))
      (org-google-docs-live-conformance--trash-document
       (plist-get result :document-id)
       account))))

(ert-deftest org-google-docs-live-conformance-plain-heading-list-create-fetch ()
  "Plain prose, headings, and lists survive create/fetch canonicalization."
  (org-google-docs-live-conformance--assert-create-fetch
   "plain-heading-list.org"))

(ert-deftest org-google-docs-live-conformance-plain-heading-list-immediate-diff ()
  "Freshly created plain/heading/list fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "plain-heading-list.org"))

(ert-deftest org-google-docs-live-conformance-plain-heading-list-append-diff ()
  "Appending one trailing paragraph to a fresh fixture emits a tiny local diff."
  (org-google-docs-live-conformance--assert-append-diff
   "plain-heading-list.org"))

(ert-deftest org-google-docs-live-conformance-lists-create-fetch ()
  "Nested, numbered, and checkbox lists survive create/fetch canonicalization."
  (org-google-docs-live-conformance--assert-create-fetch
   "list-conformance.org"))

(ert-deftest org-google-docs-live-conformance-lists-immediate-diff ()
  "Freshly created list fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "list-conformance.org"))

(ert-deftest org-google-docs-live-conformance-lists-append-diff ()
  "Appending one trailing paragraph to list fixture emits a tiny local diff."
  (org-google-docs-live-conformance--assert-append-diff
   "list-conformance.org"))

(ert-deftest org-google-docs-live-conformance-inline-create-fetch ()
  "Inline text formatting survives create/fetch canonicalization."
  (org-google-docs-live-conformance--assert-create-fetch
   "inline-conformance.org"))

(ert-deftest org-google-docs-live-conformance-inline-immediate-diff ()
  "Freshly created inline fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "inline-conformance.org"))

(ert-deftest org-google-docs-live-conformance-inline-append-diff ()
  "Appending one trailing paragraph to inline fixture emits a tiny local diff."
  (org-google-docs-live-conformance--assert-append-diff
   "inline-conformance.org"))

(ert-deftest org-google-docs-live-conformance-blocks-create-fetch ()
  "Semantic blocks survive create/fetch canonicalization."
  (org-google-docs-live-conformance--assert-create-fetch
   "block-conformance.org"))

(ert-deftest org-google-docs-live-conformance-blocks-immediate-diff ()
  "Freshly created semantic block fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "block-conformance.org"))

(ert-deftest org-google-docs-live-conformance-blocks-append-diff ()
  "Appending one trailing paragraph to block fixture emits a tiny local diff."
  (org-google-docs-live-conformance--assert-append-diff
   "block-conformance.org"))

(ert-deftest org-google-docs-live-conformance-tables-create-fetch ()
  "Tables survive create/fetch canonicalization."
  (org-google-docs-live-conformance--assert-create-fetch
   "table-conformance.org"))

(ert-deftest org-google-docs-live-conformance-tables-immediate-diff ()
  "Freshly created table fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "table-conformance.org"))

(ert-deftest org-google-docs-live-conformance-tables-append-diff ()
  "Appending one trailing paragraph to table fixture emits a tiny local diff."
  (org-google-docs-live-conformance--assert-append-diff
   "table-conformance.org"))

(ert-deftest org-google-docs-live-conformance-images-create-fetch ()
  "Images and captions survive native create/fetch canonicalization."
  (org-google-docs-live-conformance--assert-create-fetch
   "image-conformance.org"))

(ert-deftest org-google-docs-live-conformance-images-immediate-diff ()
  "Freshly created image fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "image-conformance.org"))

(ert-deftest org-google-docs-live-conformance-images-append-diff ()
  "Appending one trailing paragraph to image fixture emits a tiny local diff."
  (org-google-docs-live-conformance--assert-append-diff
   "image-conformance.org"))

(ert-deftest org-google-docs-live-conformance-footnotes-create-fetch ()
  "Native footnotes survive create/fetch canonicalization."
  (org-google-docs-live-conformance--assert-create-fetch
   "footnote-conformance.org"))

(ert-deftest org-google-docs-live-conformance-footnotes-immediate-diff ()
  "Freshly created native footnote fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "footnote-conformance.org"))

(ert-deftest org-google-docs-live-conformance-date-footnote-quote-create-fetch ()
  "Dates before native footnotes do not desynchronize later block semantics."
  (org-google-docs-live-conformance--assert-create-fetch
   "date-footnote-quote-conformance.org"))

(ert-deftest org-google-docs-live-conformance-date-footnote-quote-immediate-diff ()
  "Freshly created date/footnote/quote fixture has no immediate push diff."
  (org-google-docs-live-conformance--assert-immediate-diff
   "date-footnote-quote-conformance.org"))

(provide 'org-google-docs-live-conformance-test)
;;; org-google-docs-live-conformance-test.el ends here
