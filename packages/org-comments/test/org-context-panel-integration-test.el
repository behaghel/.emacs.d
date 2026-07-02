;;; org-context-panel-integration-test.el --- Context panel provider integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for composed org-context-panel providers.

;;; Code:

(require 'ert)
(require 'org-comments)
(require 'org-marginalia-context-panel)

(ert-deftest org-context-panel-composes-comments-and-marginalia-with-generic-ret ()
  "Comments and marginalia share one side panel and generic RET dispatch."
  (let* ((directory (make-temp-file "org-context-panel-integration" t))
	 (source-file (expand-file-name "source.org" directory))
	 source-buffer
	 panel-buffer
	 reference-start
	 reference-end)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "Alpha [fn:one]\nBeta\n\n[fn:one] Marginal note.\n"))
	  (setq source-buffer (find-file-noselect source-file))
	  (with-current-buffer source-buffer
	    (org-mode)
	    (goto-char (point-min))
	    (search-forward "[fn:one]")
	    (setq reference-start (match-beginning 0))
	    (setq reference-end (match-end 0)))
	  (with-current-buffer source-buffer
	    (org-comments-append-to-sidecar
	     (org-comments-create-record source-file
					 reference-start
					 reference-end
					 "Comment body"
					 "c1"
					 "Alice"
					 "now"))
	    (org-comments-context-panel-enable)
	    (org-marginalia-context-panel-mode 1))
	  (setq panel-buffer (generate-new-buffer " *org context integration panel*"))
	  (with-current-buffer panel-buffer
	    (org-context-panel-buffer-mode)
	    (setq-local org-context-panel-source-buffer source-buffer)
	    (org-context-panel-render-side-panel source-buffer)
	    (let* ((starts (org-context-panel-item-starts))
		   (items (mapcar #'org-context-panel-item-at-position starts)))
	      (should (= (length items) 2))
	      (should (equal (mapcar (lambda (item) (plist-get item :provider)) items)
			     '(comments marginalia)))
	      (should (equal (mapcar (lambda (item) (plist-get item :id)) items)
			     '("c1" "one")))
	      (goto-char (nth 0 starts))
	      (org-context-panel-jump-at-point)
	      (should (eq (current-buffer) source-buffer))
	      (should (= (point) reference-start))
	      (with-current-buffer panel-buffer
		(goto-char (nth 1 starts))
		(org-context-panel-jump-at-point))
	      (should (eq (current-buffer) source-buffer))
	      (should (= (point) reference-start)))))
      (when (buffer-live-p panel-buffer)
	(kill-buffer panel-buffer))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer))
      (when (file-directory-p directory)
	(delete-directory directory t)))))

(provide 'org-context-panel-integration-test)
;;; org-context-panel-integration-test.el ends here
