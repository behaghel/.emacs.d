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

(provide 'org-sync-status-test)
;;; org-sync-status-test.el ends here
