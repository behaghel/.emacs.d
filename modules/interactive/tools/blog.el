;;; blog.el --- Blogging helpers (ox-hugo, capture) -*- lexical-binding: t; -*-

;;; Commentary:
;; Blogging workflow helpers: post creation, org-capture integration, ox-hugo.

;;; Code:

(require 'hub-utils)

(defgroup hub/blog nil
  "Blogging helpers and paths."
  :group 'external)

(defcustom hub/blog-root "~/ws/blog.behaghel.org"
  "Root directory of the blog workspace."
  :type 'directory
  :group 'hub/blog)

(defcustom hub/blog-posts-dir (expand-file-name "content-org" hub/blog-root)
  "Directory where standalone blog posts are stored."
  :type 'directory
  :group 'hub/blog)

(defcustom hub/blog-all-posts-file (expand-file-name "content-org/all-posts.org" hub/blog-root)
  "Org file where captured posts are appended under the All Blog Posts heading."
  :type 'file
  :group 'hub/blog)

(defun hub/create-post (title)
  "Create a new blog post with TITLE under content-org using template.post."
  (interactive "sTitle: ")
  (let ((slug (hub/sluggify title))
	(today (format-time-string "%Y-%m-%d"))
	(template (expand-file-name "template.post" auto-insert-directory)))
    (remove-hook 'find-file-hooks 'auto-insert)
    (find-file (format "%s/%s-%s.org" hub/blog-posts-dir today slug))
    (insert-file-contents template)
    (hub/autoinsert-yas-expand `((title ,title)))
    (add-hook 'find-file-hooks 'auto-insert)))
(define-key evil-normal-state-map (kbd ",np") 'hub/create-post)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Return an org-capture template string for a new Hugo post."
    (let* ((title (read-from-minibuffer "Post Title: "))
	   (fname (org-hugo-slug title)))
      (mapconcat #'identity
		 (list (concat "* TODO " title)
		       ":PROPERTIES:" (concat ":EXPORT_FILE_NAME: " fname) ":END:" "%?\n")
		 "\n")))
  (add-to-list 'org-capture-templates
	       `( "b" "Blog post (all-posts.org)" entry
		  (file+olp ,hub/blog-all-posts-file "All Blog Posts")
		  (function org-hugo-new-subtree-post-capture-template))))

(add-to-list 'org-capture-templates
	     '("B" "Blog post (standalone)" plain (file denote-last-path) #'denote-org-capture))

(defun blog-post-hook ()
  (let ((current-path (or (buffer-file-name) "")))
    (when (string-match hub/blog-posts-dir current-path)
      (ispell-change-dictionary "en_GB")
      (setq-local ispell-check-comments nil)
      (writeroom-mode)
      (artbollocks-mode)
      (font-lock-fontify-buffer))))
(add-hook 'org-mode-hook 'blog-post-hook)

(use-package ox-hugo :commands (org-hugo-auto-export-mode) :after (ox))

(provide 'tools/blog)
;;; blog.el ends here
