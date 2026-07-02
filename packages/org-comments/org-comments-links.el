;;; org-comments-links.el --- Org links for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; `org-comment:' link helpers for the initial Org comments extraction.  Names
;; remain in the legacy `org-comments-*' namespace until the public API
;; migration slice.

;;; Code:

(require 'org)
(require 'ol)
(require 'org-comments-core)
(require 'org-comments-sidecar)
(require 'org-comments-ui)
(require 'seq)
(require 'subr-x)

(defun org-comments--project-root (&optional file)
  "Return project-like root for FILE, or nil."
  (let ((directory (file-name-directory (or file default-directory))))
    (or (locate-dominating-file directory "domains.yaml")
	(locate-dominating-file directory ".git"))))

(defun org-comments-link-source-path (source-file)
  "Return portable source path for SOURCE-FILE in `org-comment:' links."
  (let ((root (org-comments--project-root source-file)))
    (if root
	(file-relative-name source-file root)
      (abbreviate-file-name source-file))))

(defun org-comments--link-description (description fallback)
  "Return DESCRIPTION sanitized for use as an Org link label.
FALLBACK is used when DESCRIPTION is nil or empty."
  (let ((label (or (org-comments--present-string description) fallback)))
    (replace-regexp-in-string "[][]" "" label)))

(defun org-comments-make-link (source-file comment-id &optional description)
  "Return an Org link to COMMENT-ID belonging to SOURCE-FILE."
  (format "[[org-comment:%s::%s][%s]]"
	  (org-comments-link-source-path source-file)
	  comment-id
	  (org-comments--link-description description comment-id)))

(defun org-comments--resolve-source-file (path)
  "Resolve org-comment source PATH to an absolute file name."
  (cond
   ((file-name-absolute-p path) (expand-file-name path))
   ((let* ((root (org-comments--project-root default-directory))
	   (candidate (and root (expand-file-name path root))))
      (and candidate (file-exists-p candidate) candidate)))
   (t (expand-file-name path default-directory))))

(defun org-comments--parse-link-path (path)
  "Parse org-comment PATH into source file and comment id."
  (if (string-match "\\`\\(.+\\)::\\(.+\\)\\'" path)
      (cons (org-comments--resolve-source-file (match-string 1 path))
	    (match-string 2 path))
    (cons (or buffer-file-name
	      (when (and (string-suffix-p ".comments.org" (or buffer-file-name "")))
		(org-comments-source-file-from-sidecar buffer-file-name)))
	  path)))

(defun org-comments-parse-link-path (path)
  "Parse org-comment PATH into source file and comment id."
  (org-comments--parse-link-path path))

(defun org-comments-open-link (path &optional _arg)
  "Open org-comment link PATH."
  (pcase-let* ((`(,source-file . ,comment-id) (org-comments--parse-link-path path))
	       (sidecar-file (and source-file (org-comments-sidecar-path source-file))))
    (unless (and source-file (file-exists-p source-file))
      (user-error "Cannot resolve org-comment source file: %s" (or source-file "<none>")))
    (unless (and sidecar-file (file-exists-p sidecar-file))
      (user-error "Cannot find comments sidecar for %s" source-file))
    (let ((source-buffer (find-file-noselect source-file)))
      (with-current-buffer source-buffer
	(org-mode))
      (with-current-buffer (find-file-noselect sidecar-file)
	(org-mode)
	(unless (org-comments-goto-id comment-id)
	  (user-error "Cannot find org comment %s" comment-id))
	(let* ((sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
	       (reply-parent
		(when (equal sync-kind "reply")
		  (save-excursion
		    (when (org-up-heading-safe)
		      (list :id (org-entry-get nil "ORG_COMMENTS_ID")
			    :sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))))))
	       (lookup-id (or (plist-get reply-parent :id) comment-id))
	       (inline-record (when (equal (or (plist-get reply-parent :sync-kind) sync-kind) "inline")
				(seq-find (lambda (record)
					    (equal lookup-id (plist-get record :id)))
					  (with-current-buffer source-buffer
					    (org-comments-collect source-buffer nil))))))
	  (cond
	   ((and (equal (or (plist-get reply-parent :sync-kind) sync-kind) "footer")
		 org-comments-ui-page-open-comment-function)
	    (pop-to-buffer source-buffer)
	    (org-comments-ui-page-open-comment comment-id))
	   ((and inline-record org-comments-ui-open-comment-function)
	    (pop-to-buffer source-buffer)
	    (org-comments-ui-open-comment
	     comment-id (plist-get inline-record :jump-pos)))
	   (t
	    (pop-to-buffer (current-buffer)))))))))

(defun org-comments-store-link ()
  "Store an `org-comment:' link for the sidecar comment at point."
  (when (and buffer-file-name
	     (string-suffix-p ".comments.org" buffer-file-name)
	     (derived-mode-p 'org-mode)
	     (not (org-before-first-heading-p)))
    (org-back-to-heading t)
    (when-let* ((comment-id (org-entry-get nil "ORG_COMMENTS_ID")))
      (let* ((source-file (org-comments-source-file-from-sidecar buffer-file-name))
	     (heading (org-get-heading t t t t))
	     (link (org-comments-make-link source-file comment-id heading)))
	(org-link-store-props :type "org-comment"
			      :link link
			      :description heading)
	link))))

(org-link-set-parameters "org-comment"
			 :follow #'org-comments-open-link
			 :store #'org-comments-store-link)

(provide 'org-comments-links)
;;; org-comments-links.el ends here
