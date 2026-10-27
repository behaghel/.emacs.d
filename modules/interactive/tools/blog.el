;;; blog.el --- Static-site publishing compatibility entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Reusable mechanics live in `hb-static-site'.  Personal prose activation,
;; bindings, discovery labels, and historical command aliases stay here.

;;; Code:

(require 'hb-static-site)

(declare-function writing/enable-basics "writing/writing")

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

(defun hub/blog--enable-prose ()
  "Apply personal prose setup when enabling static-site mode."
  (when (and hb-static-site-mode (fboundp 'writing/enable-basics))
    (writing/enable-basics)))

(add-hook 'hb-static-site-mode-hook #'hub/blog--enable-prose)

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w n") #'hb-static-site-create-post
		     (kbd ",w s") #'hb-static-site-create-section
		     (kbd ",w p") #'hb-static-site-create-page
		     (kbd ",w P") #'hb-static-site-create-basic-page
		     (kbd ",w o") #'hb-static-site-find-page
		     (kbd ",w e") #'hb-static-site-export-buffer
		     (kbd ",w E") #'hb-static-site-export-all
		     (kbd ",w v") #'hb-static-site-validate-buffer))
  (when (boundp 'evil-normal-state-map)
    (define-key evil-normal-state-map (kbd ",np") #'hb-static-site-create-post)))

(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements "C-c w" "static site")))

(provide 'tools/blog)
;;; blog.el ends here
