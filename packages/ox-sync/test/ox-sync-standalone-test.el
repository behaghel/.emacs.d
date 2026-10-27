;;; ox-sync-standalone-test.el --- Standalone export-sync tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify the package namespace and compatibility with existing tracking data.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'package)

(ert-deftest ox-sync-standalone-loads-without-old-namespace ()
  "The entrypoint uses the new namespace without loading the unrelated package."
  (require 'ox-sync)
  (should-not (featurep 'org-sync))
  (should-not (fboundp 'org-sync-status))
  (should (eq (plist-get (ox-sync-context-panel-provider) :name) 'org-sync))
  (with-temp-buffer
    (insert-file-contents (locate-library "ox-sync"))
    (should (equal (mapcar #'car (package-desc-reqs (package-buffer-info)))
		   '(emacs org context-panels magit-section transient org-comments)))))

(ert-deftest ox-sync-standalone-preserves-existing-tracking-and-cache-paths ()
  "Renaming the library does not rename sidecars, their schema, or asset paths."
  (require 'ox-sync)
  (let* ((root (make-temp-file "ox-sync-legacy-" t))
	 (source (expand-file-name "source.org" root))
	 (sidecar (expand-file-name "source.sync.org" root))
	 (data '(:provider (:kind confluence :remote-id "123")
			   :domains ((content :fetched-remote-ref (:version "42"))))))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+TITLE: Org Sync Tracking\n"
		    "#+PROPERTY: org_sync_sidecar_version 1\n\n"
		    "* Tracking\n#+BEGIN_SRC emacs-lisp\n")
	    (prin1 data (current-buffer))
	    (insert "\n#+END_SRC\n"))
	  (should (equal (ox-sync-store-path source) sidecar))
	  (should (equal (ox-sync-store-read source) data))
	  (ox-sync-store-write source data)
	  (should (equal (ox-sync-store-read source) data))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "org_sync_sidecar_version 1" nil t)))
	  (should (equal ox-sync-assets-cache-directory "assets/org-sync")))
      (delete-directory root t))))

(defmacro ox-sync-standalone-test--with-demo (&rest body)
  "Run BODY with a disposable source and the offline provider."
  (declare (indent 0) (debug t))
  `(progn
     (require 'ox-sync)
     (load (expand-file-name "examples/ox-sync-demo.el"
			     (file-name-directory (locate-library "ox-sync"))) nil t)
     (let* ((root (make-temp-file "ox-sync-demo-test-" t))
	    (source-file (expand-file-name "source.org" root))
	    (ox-sync-providers nil)
	    source panel)
       (unwind-protect
	   (save-window-excursion
	     (with-temp-file source-file (insert "#+title: Offline demo\n\nOriginal text.\n"))
	     (find-file source-file)
	     (setq source (current-buffer))
	     (ox-sync-demo-start)
	     (setq panel (get-buffer ox-sync-status-buffer-name))
	     ,@body)
	 (when (buffer-live-p panel) (kill-buffer panel))
	 (when (buffer-live-p source)
	   (with-current-buffer source (set-buffer-modified-p nil))
	   (kill-buffer source))
	 (delete-directory root t)))))

(ert-deftest ox-sync-standalone-push-stays-clean-after-fetching-new-remote-revision ()
  "Real status keys publish a local edit and retain clean status after refetch."
  (ox-sync-standalone-test--with-demo
   (should (eq (current-buffer) panel))
   (should (derived-mode-p 'magit-section-mode))
   (should (= 0 (plist-get (buffer-local-value 'ox-sync-demo--state source) :fetches)))
   (execute-kbd-macro (kbd "g"))
   (should (= 0 (plist-get (buffer-local-value 'ox-sync-demo--state source) :fetches)))
   (let ((before (with-current-buffer source (buffer-string))))
     (execute-kbd-macro (kbd "f"))
     (should (equal before (with-current-buffer source (buffer-string)))))
   (execute-kbd-macro (kbd "B"))
   (with-current-buffer source
     (goto-char (point-max))
     (insert "Local edit.\n")
     (save-buffer))
   (execute-kbd-macro (kbd "g"))
   (should (string-match-p "Content[[:space:]]+ahead" (buffer-string)))
   (execute-kbd-macro (kbd "c"))
   (execute-kbd-macro (kbd "f"))
   (should (string-match-p "Head: ⇅ clean" (buffer-string)))
   (let ((entry (ox-sync-domain-entry (ox-sync-store-read source-file) 'content)))
     (should (equal (plist-get (plist-get entry :base-remote-ref) :version) "2")))
   (ox-sync-demo-remote-edit "Remote addition.")
   (execute-kbd-macro (kbd "f"))
   (should (string-match-p "Content[[:space:]]+behind" (buffer-string)))
   (execute-kbd-macro (kbd "C"))
   (should (string-match-p "Remote addition\\." (with-current-buffer source (buffer-string))))
   (should (string-match-p "Head: ⇅ clean" (buffer-string)))))

(ert-deftest ox-sync-standalone-push-needs-refetch-before-remote-mutation ()
  "A provider unable to confirm published refs is rejected before publishing."
  (ox-sync-standalone-test--with-demo
   (ox-sync-fetch)
   (ox-sync-baseline)
   (with-current-buffer source (goto-char (point-max)) (insert "Local edit.\n"))
   (setf (plist-get (ox-sync-provider 'demo) :fetch) nil)
   (should-error (ox-sync-push-content) :type 'user-error)
   (should (= 1 (plist-get (buffer-local-value 'ox-sync-demo--state source) :version)))))

(ert-deftest ox-sync-standalone-identity-mismatch-blocks-operations ()
  "An unrelated sidecar blocks fetch, baseline, push, and pull without mutation."
  (dolist (mismatch '((:kind . another-provider)
		      (:remote-id . "another-document")
		      (:account . "another-account")))
    (dolist (operation '(ox-sync-fetch ox-sync-baseline ox-sync-push-content ox-sync-pull-content))
      (ox-sync-standalone-test--with-demo
       (ox-sync-fetch)
       (ox-sync-baseline)
       (if (eq operation 'ox-sync-pull-content)
	   (progn (ox-sync-demo-remote-edit "Remote edit.") (ox-sync-fetch))
	 (with-current-buffer source (goto-char (point-max)) (insert "Local edit.\n")))
       (let ((tracking (ox-sync-store-read source-file)))
	 (setf (plist-get (plist-get tracking :provider) (car mismatch)) (cdr mismatch))
	 (ox-sync-store-write source-file tracking))
       (let ((before (ox-sync-store-read source-file))
	     (local (with-current-buffer source (buffer-string)))
	     (fetches (plist-get (buffer-local-value 'ox-sync-demo--state source) :fetches))
	     (version (plist-get (buffer-local-value 'ox-sync-demo--state source) :version)))
	 (should-error (funcall operation) :type 'user-error)
	 (should (equal before (ox-sync-store-read source-file)))
	 (should (equal local (with-current-buffer source (buffer-string))))
	 (should (= fetches (plist-get (buffer-local-value 'ox-sync-demo--state source) :fetches)))
	 (should (= version (plist-get (buffer-local-value 'ox-sync-demo--state source) :version))))))))

(ert-deftest ox-sync-standalone-fetch-reports-time-outcome-and-refresh-is-local ()
  "Fetch reports success even when unchanged; refresh leaves receipt time alone."
  (ox-sync-standalone-test--with-demo
   (should (string-match-p "Last successful fetch: not recorded" (buffer-string)))
   (let ((clock (encode-time 0 0 12 27 10 2026 0))
	 messages)
     (cl-letf (((symbol-function 'current-time) (lambda () clock))
	       ((symbol-function 'message)
		(lambda (fmt &rest args)
		  (when fmt (push (apply #'format fmt args) messages)))))
       (execute-kbd-macro (kbd "f"))
       (let ((first (plist-get (ox-sync-store-read source-file) :fetched-at)))
	 (should (equal first (format-time-string "%FT%T%z" clock)))
	 (should (string-match-p (regexp-quote first) (buffer-string)))
	 (should (cl-some (lambda (text)
			    (string-prefix-p "Fetch complete: remote refs updated" text))
			  messages))
	 (setq clock (time-add clock 5) messages nil)
	 (execute-kbd-macro (kbd "f"))
	 (let ((second (plist-get (ox-sync-store-read source-file) :fetched-at)))
	   (should (equal second (format-time-string "%FT%T%z" clock)))
	   (should-not (equal first second))
	   (should (string-match-p "no remote changes" (buffer-string)))
	   (should (cl-some (lambda (text)
			      (string-prefix-p "Fetch complete: no remote changes" text))
			    messages))
	   (setq clock (time-add clock 5))
	   (let ((before (ox-sync-store-read source-file))
		 (fetches (plist-get (buffer-local-value 'ox-sync-demo--state source) :fetches)))
	     (execute-kbd-macro (kbd "g"))
	     (should (equal before (ox-sync-store-read source-file)))
	     (should (= fetches (plist-get (buffer-local-value 'ox-sync-demo--state source) :fetches)))
	     (should (string-match-p (regexp-quote second) (buffer-string))))))))))

(ert-deftest ox-sync-standalone-failed-fetch-does-not-report-success ()
  "A provider error preserves the previous successful receipt and tracking refs."
  (ox-sync-standalone-test--with-demo
   (ox-sync-fetch)
   (let ((before (ox-sync-store-read source-file))
	 messages)
     (should (plist-get before :fetched-at))
     (setf (plist-get (ox-sync-provider 'demo) :fetch)
	   (lambda (&rest _) (error "Demo fetch failed")))
     (cl-letf (((symbol-function 'message)
		(lambda (fmt &rest args)
		  (when fmt (push (apply #'format fmt args) messages)))))
       (should (string-match-p "Demo fetch failed"
			       (error-message-string (should-error (ox-sync-fetch))))))
     (should-not (cl-some (lambda (text) (string-prefix-p "Fetch complete:" text)) messages))
     (should (equal before (ox-sync-store-read source-file)))
     (ox-sync-refresh)
     (should (string-match-p (regexp-quote (plist-get before :fetched-at)) (buffer-string))))))

(ert-deftest ox-sync-standalone-empty-fetch-result-is-not-a-success ()
  "An adapter returning no remote references cannot create a success receipt."
  (ox-sync-standalone-test--with-demo
   (ox-sync-fetch)
   (let ((before (ox-sync-store-read source-file)))
     (setf (plist-get (ox-sync-provider 'demo) :fetch) (lambda (&rest _) nil))
     (should (string-match-p
	      "no usable remote references"
	      (error-message-string (should-error (ox-sync-fetch) :type 'user-error))))
     (should (equal before (ox-sync-store-read source-file))))))

(ert-deftest ox-sync-standalone-panel-shows-local-fetched-and-baseline-refs ()
  "The panel explains baseline movement rather than merely claiming clean."
  (ox-sync-standalone-test--with-demo
   (ox-sync-fetch)
   (ox-sync-baseline)
   (let ((base (substring (plist-get (ox-sync-local-ref 'content source) :hash) 0 8)))
     (should (string-match-p (regexp-quote (format "Baseline: local %s ↔ remote v1" base))
			     (buffer-string)))
     (with-current-buffer source (goto-char (point-max)) (insert "Local edit.\n"))
     (ox-sync-refresh)
     (should (string-match-p "Content[[:space:]]+ahead" (buffer-string)))
     (should (string-match-p (regexp-quote (format "Baseline: local %s ↔ remote v1" base))
			     (buffer-string)))
     (ox-sync-push-content)
     (let ((local (substring (plist-get (ox-sync-local-ref 'content source) :hash) 0 8)))
       (should (string-match-p (regexp-quote (format "local %s | fetched v2" local))
			       (buffer-string)))
       (should (string-match-p (regexp-quote (format "Baseline: local %s ↔ remote v2" local))
			       (buffer-string)))
       (should (string-match-p "remote refs updated (content)" (buffer-string)))
       (ox-sync-fetch)
       (should (string-match-p "no remote changes" (buffer-string)))
       (should (string-match-p (regexp-quote (format "Baseline: local %s ↔ remote v2" local))
			       (buffer-string)))))))

(ert-deftest ox-sync-standalone-panel-shows-opaque-revisions-and-short-hashes ()
  "Provider revision IDs remain readable and long hashes are abbreviated."
  (ox-sync-standalone-test--with-demo
   (setf (plist-get (ox-sync-provider 'demo) :fetch)
	 (lambda (&rest _)
	   '(:domains ((content :remote-ref (:revision-id "opaque-revision-123"))
		       (comments :remote-ref (:hash "fedcba9876543210"))))))
   (ox-sync-fetch)
   (ox-sync-baseline)
   (should (string-match-p "fetched opaque-revision-123" (buffer-string)))
   (should (string-match-p "remote opaque-revision-123" (buffer-string)))
   (should (string-match-p "fetched fedcba98" (buffer-string)))
   (should-not (string-match-p "fedcba9876543210" (buffer-string)))))

(ert-deftest ox-sync-standalone-legacy-references-do-not-imply-never-fetched ()
  "Existing references without a receipt time show unknown history, read-only."
  (ox-sync-standalone-test--with-demo
   (ox-sync-store-write
    source-file
    '(:provider (:kind demo :remote-id "offline-document")
		:domains ((content :fetched-remote-ref (:version "1")))))
   (let ((before (ox-sync-store-read source-file)))
     (ox-sync-refresh)
     (should (string-match-p "Last successful fetch: not recorded" (buffer-string)))
     (should (string-match-p "fetched v1" (buffer-string)))
     (should (equal before (ox-sync-store-read source-file)))
     (should (zerop (plist-get (buffer-local-value 'ox-sync-demo--state source) :fetches))))))

(provide 'ox-sync-standalone-test)
;;; ox-sync-standalone-test.el ends here
