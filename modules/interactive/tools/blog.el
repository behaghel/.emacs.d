;;; blog.el --- Static-site publishing compatibility entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal static-site workflow now lives in reusable `hb-static-site'.  This
;; entrypoint keeps the historical `tools/blog' feature and old command aliases
;; available for interactive configuration.

;;; Code:

(require 'hb-static-site)

(use-package ox-hugo
  :commands (org-hugo-auto-export-mode org-hugo-export-wim-to-md)
  :after (ox))

(defgroup hub/blog nil
  "Compatibility group for historical blog helpers."
  :group 'hb-static-site)

(defcustom hub/blog-root nil
  "Deprecated static-site root override.

Prefer project-local `org-hugo-base-dir' in each site's .dir-locals.el."
  :type '(choice (const :tag "Use project-local configuration" nil) directory)
  :group 'hub/blog)

(defcustom hub/blog-posts-dir nil
  "Deprecated posts directory override.

Prefer `hb-static-site-posts-subdirectory' plus project-local `denote-directory'."
  :type '(choice (const :tag "Use hb-static-site defaults" nil) directory)
  :group 'hub/blog)

(defun hub/create-post (title)
  "Compatibility wrapper creating a Denote static-site post with TITLE."
  (interactive "sPost title: ")
  (hb-static-site-create-post title nil))

(defalias 'hub/blog-enable #'hb-static-site-enable)
(defalias 'hub/blog-create-post #'hb-static-site-create-post)
(defalias 'hub/blog-create-page #'hb-static-site-create-page)
(defalias 'hub/blog-find-page #'hb-static-site-find-page)
(defalias 'hub/blog-export-buffer #'hb-static-site-export-buffer)
(defalias 'hub/blog-validate-buffer #'hb-static-site-validate-buffer)
(defalias 'hub/blog-validate-and-export-buffer #'hb-static-site-validate-and-export-buffer)

(with-eval-after-load 'evil
  (when (boundp 'evil-normal-state-map)
    (define-key evil-normal-state-map (kbd ",np") #'hb-static-site-create-post)))

(provide 'tools/blog)
;;; blog.el ends here
