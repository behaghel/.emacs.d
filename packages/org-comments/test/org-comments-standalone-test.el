;;; org-comments-standalone-test.el --- Standalone comments workflow tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Check dependency metadata and the local create/reopen/navigate workflow
;; without personal activation, provider adapters, or repository test helpers.

;;; Code:

(require 'ert)
(require 'package)
(require 'org-comments)

(ert-deftest org-comments-standalone-declares-context-panels-dependency ()
  "The entrypoint declares the panel package required by its public workflow."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "org-comments.el"
		       (file-name-directory (locate-library "org-comments"))))
    (let* ((descriptor (package-buffer-info))
	   (dependency (assq 'context-panels (package-desc-reqs descriptor))))
      (should dependency)
      (should (version-list-<= '(0 1 0) (cadr dependency))))))

(ert-deftest org-comments-standalone-create-reopen-and-navigate ()
  "A local comment survives reopening and its panel jumps to the source text."
  (let* ((directory (make-temp-file "org-comments-standalone" t))
	 (source-file (expand-file-name "draft.org" directory))
	 (sidecar-file (org-comments-sidecar-path source-file))
	 (content "Alpha selected text omega.\n")
	 source panel)
    (unwind-protect
	(save-window-excursion
	  (with-temp-file source-file (insert content))
	  (setq source (find-file-noselect source-file))
	  (switch-to-buffer source)
	  (org-comments-mode 1)
	  (org-comments-create 7 20 "Review this.")
	  (should (file-exists-p sidecar-file))
	  (with-current-buffer source
	    (should (equal (buffer-string) content))
	    (should-not (buffer-modified-p))
	    (org-comments-mode -1))
	  (kill-buffer source)
	  (kill-buffer (find-buffer-visiting sidecar-file))
	  (setq source (find-file-noselect source-file))
	  (switch-to-buffer source)
	  (org-comments-mode 1)
	  (let ((comments (org-comments-collect source)))
	    (should (= (length comments) 1))
	    (should (equal (plist-get (car comments) :body) "Review this."))
	    (should (equal (plist-get (car comments) :target-text)
			   "selected text")))
	  (let ((highlights
		 (with-current-buffer source
		   (goto-char 7)
		   (org-comments-context-panel-follow-point)
		   (mapcar (lambda (overlay)
			     (list (overlay-start overlay)
				   (overlay-end overlay)
				   (overlay-get overlay 'face)))
			   org-comments-overlays))))
	    (should highlights)
	    (org-comments-open)
	    (context-panels-update-active-highlights source)
	    (with-current-buffer source
	      (dolist (overlay (append org-comments-overlays
				       context-panels-active-source-overlays))
		(should (member (list (overlay-start overlay)
				      (overlay-end overlay)
				      (overlay-get overlay 'face))
				highlights)))))
	  (setq panel context-panels-side-panel-buffer)
	  (should (buffer-live-p panel))
	  (with-current-buffer panel
	    (goto-char (point-min))
	    (search-forward "Review this.")
	    (context-panels-jump-at-point))
	  (should (eq (window-buffer (selected-window)) source))
	  (with-current-buffer source
	    (should (= (point) 7))
	    (should (equal (buffer-string) content))))
      (when (buffer-live-p source)
	(with-current-buffer source
	  (org-comments-mode -1))
	(kill-buffer source))
      (when (buffer-live-p panel) (kill-buffer panel))
      (when-let* ((sidecar (find-buffer-visiting sidecar-file)))
	(kill-buffer sidecar))
      (delete-directory directory t))))

(provide 'org-comments-standalone-test)
;;; org-comments-standalone-test.el ends here
