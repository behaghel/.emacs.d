;;; org-sync-status-test.el --- Org sync status tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the generic Org sync status bottom panel.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-sync)
(require 'context-panels)

(defmacro org-sync-status-test--with-providers (providers &rest body)
  "Bind org-sync status PROVIDERS while running BODY."
  (declare (indent 1))
  `(let ((org-sync-providers ,providers))
     ,@body))

(defun org-sync-status-test--fake-provider (&optional detect-result)
  "Return a fake org-sync provider with DETECT-RESULT."
  (list :kind 'fake
	:detect (lambda (_source-buffer)
		  (or detect-result
		      (list :kind 'fake :remote-id "doc-1" :title "Fake Doc")))))

(ert-deftest org-sync-detects-single-provider ()
  "Provider detection returns the one matching document descriptor."
  (with-temp-buffer
    (org-mode)
    (org-sync-status-test--with-providers
     (list (org-sync-status-test--fake-provider))
     (let ((descriptor (org-sync-detect-document (current-buffer))))
       (should (equal (plist-get descriptor :kind) 'fake))
       (should (equal (plist-get descriptor :remote-id) "doc-1"))))))

(ert-deftest org-sync-errors-without-provider ()
  "Provider detection fails clearly when no adapter matches."
  (with-temp-buffer
    (org-mode)
    (org-sync-status-test--with-providers nil
					  (should-error (org-sync-detect-document (current-buffer))
							:type 'user-error))))

(ert-deftest org-sync-status-opens-bottom-view ()
  "Status opens a generic context-panels bottom view for the source buffer."
  (with-temp-buffer
    (org-mode)
    (org-sync-status-test--with-providers
     (list (org-sync-status-test--fake-provider))
     (let ((source (current-buffer)))
       (org-sync-status)
       (let ((panel (buffer-local-value 'context-panels-bottom-panel-buffer
					source)))
	 (should (buffer-live-p panel))
	 (with-current-buffer panel
	   (should (derived-mode-p 'org-sync-status-mode))
	   (should (eq context-panels-source-buffer source))
	   (should (string-match-p "Org Sync: fake doc-1" (buffer-string)))
	   (should (string-match-p "Content[[:space:]]+unknown" (buffer-string)))
	   (should (string-match-p "Comments[[:space:]]+unknown" (buffer-string)))))))))

(ert-deftest org-sync-baseline-records-base-refs-and-renders-clean ()
  "Baselining stores current/fetched ref pairs and renders clean domains."
  (let* ((directory (make-temp-file "org-sync-status" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "Body\n"))
	  (org-sync-store-write
	   source-file
	   '(:provider (:kind fake :remote-id "doc-1")
		       :domains ((content :fetched-remote-ref (:version "1"))
				 (comments :fetched-remote-ref (:hash "remote-comments")))))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-sync-status-test--with-providers
	     (list (org-sync-status-test--fake-provider))
	     (org-sync-status)
	     (org-sync-baseline)
	     (let* ((tracking (org-sync-store-read source-file))
		    (content (alist-get 'content (plist-get tracking :domains)))
		    (comments (alist-get 'comments (plist-get tracking :domains)))
		    (panel (get-buffer org-sync-status-buffer-name)))
	       (should (plist-get content :base-local-ref))
	       (should (plist-get content :base-remote-ref))
	       (should (plist-get comments :base-local-ref))
	       (should (plist-get comments :base-remote-ref))
	       (with-current-buffer panel
		 (should (string-match-p "Content[[:space:]]+clean" (buffer-string)))
		 (should (string-match-p "Comments[[:space:]]+clean" (buffer-string))))))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(provide 'org-sync-status-test)
;;; org-sync-status-test.el ends here
