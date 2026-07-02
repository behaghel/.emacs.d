;;; org-comments-context-panel-test.el --- Comments context provider tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the org-comments provider layer over org-context-panel primitives.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-comments)

(defun org-comments-context-panel-test--write-sidecar (source-file body)
  "Write BODY to SOURCE-FILE's comments sidecar."
  (let ((sidecar-file (org-comments-sidecar-path source-file)))
    (make-directory (file-name-directory sidecar-file) t)
    (with-temp-file sidecar-file
      (insert body))
    sidecar-file))

(ert-deftest org-comments-context-panel-refresh-creates-provider-overlays ()
  "The comments provider creates range overlays and page markers."
  (let* ((directory (make-temp-file "org-comments-context-panel" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+TITLE: Source\n\nAlpha beta gamma\n"))
	  (org-comments-context-panel-test--write-sidecar
	   source-file
	   "#+SOURCE: source.org\n\n* OPEN Page note\n:PROPERTIES:\n:ORG_COMMENTS_ID: p1\n:ORG_COMMENTS_SOURCE_FILE: source.org\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nPage body\n")
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (goto-char (point-min))
	    (search-forward "Alpha")
	    (org-comments-append-to-sidecar
	     (org-comments-create-record source-file
					 (match-beginning 0)
					 (match-end 0)
					 "Body" "c1" "Alice" "now"))
	    (org-comments-context-panel-refresh)
	    (should (overlayp org-comments-page-comment-overlay))
	    (should (seq-some (lambda (overlay)
				(overlay-get overlay 'org-comments-comment))
			      org-comments-overlays))
	    (org-comments-context-panel-delete-overlays)
	    (should-not org-comments-overlays)
	    (should-not (overlayp org-comments-page-comment-overlay))))
      (delete-directory directory t))))

(ert-deftest org-comments-context-panel-provider-exposes-collection-functions ()
  "The comments provider descriptor exposes collection entry points."
  (let ((provider (org-comments-context-panel-provider)))
    (should (eq (plist-get provider :name) 'comments))
    (should (eq (plist-get provider :collect-side-items)
		#'org-comments-context-panel-collect-side-items))
    (should (eq (plist-get provider :collect-top-markers)
		#'org-comments-context-panel-collect-top-markers))
    (should (eq (plist-get provider :refresh-source-overlays)
		#'org-comments-context-panel-refresh-source-overlays))))

(ert-deftest org-comments-mode-enables-context-panel-provider ()
  "`org-comments-mode' enables the comments provider through context-panel mode."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'org-context-panel-refresh-source-overlays)
	       (lambda ())))
      (org-comments-mode 1)
      (should org-comments-mode)
      (should org-context-panel-mode)
      (should (org-context-panel-registered-provider 'comments))
      (org-comments-mode -1)
      (should-not org-comments-mode)
      (should-not org-context-panel-mode)
      (should-not (org-context-panel-registered-provider 'comments)))))

(ert-deftest org-comments-overlays-enable-registers-provider ()
  "Overlay activation registers and unregisters the comments provider."
  (with-temp-buffer
    (org-mode)
    (let ((buffer-file-name nil))
      (cl-letf (((symbol-function 'org-context-panel-refresh-source-overlays)
		 (lambda ())))
	(org-comments-overlays-enable)
	(should (org-context-panel-registered-provider 'comments))
	(org-comments-overlays-disable)
	(should-not (org-context-panel-registered-provider 'comments))))))

(ert-deftest org-comments-overlays-refresh-delegates-through-context-registry ()
  "The public overlay refresh facade delegates through the provider registry."
  (with-temp-buffer
    (org-mode)
    (let (called)
      (cl-letf (((symbol-function 'org-context-panel-refresh-source-overlays)
		 (lambda ()
		   (setq called t))))
	(org-comments-overlays-refresh))
      (should called)
      (should (org-context-panel-registered-provider 'comments)))))

(provide 'org-comments-context-panel-test)
;;; org-comments-context-panel-test.el ends here
