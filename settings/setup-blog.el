;; Blogging

(setq blog-root "~/ws/blog.behaghel.org")
(setq blog-posts-dir (expand-file-name "content-org" blog-root))

;; FIXME: just wrap denote-new by setting the denote note dir to the
;; blog dir
(defun hub/create-post (title)
  "Start editing a new file whose TITLE is following this pattern %Y-%m-%d-[title].orp in the posts directory."
  (interactive "sTitle: ")
  (let ((slug (hub/sluggify title))
        (today (format-time-string "%Y-%m-%d"))
        (template (expand-file-name "template.post" auto-insert-directory)))
    (remove-hook 'find-file-hooks 'auto-insert) ; to prevent normal .org insertion
    (find-file (format "%s/%s-%s.org" blog-posts-dir today slug))
    (insert-file-contents template)
    (hub/autoinsert-yas-expand `((title ,title)))
    (add-hook 'find-file-hooks 'auto-insert)))
(define-key evil-normal-state-map (kbd ",nb") 'hub/create-post)

;; stolen from https://ox-hugo.scripter.co/doc/org-capture-setup/
;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("b"                ;`org-capture' binding + h
                 "Blog post (all-posts.org)"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "~/ws/blog.behaghel.org/content-org/all-posts.org" "All Blog Posts")
                 (function org-hugo-new-subtree-post-capture-template))))
  (add-to-list 'org-capture-templates
               '("B"                ;`org-capture' binding + h
                 "Blog post (standalone)"
                 plain
                 (file denote-last-path)
                 ;; #'org-hugo-new-subtree-post-capture-template
                 #'denote-org-capture
                 ))

(defun blog-post-hook ()
  (let ((current-path (or (buffer-file-name) "don't know")))
    (cond ((string-match blog-posts-dir current-path)
           (ispell-change-dictionary "uk")
           (setq-local ispell-check-comments nil)
           (writeroom-mode)
           (artbollocks-mode)
           (font-lock-fontify-buffer)))))

(add-hook 'org-mode-hook 'blog-post-hook)

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  ;; :pin melpa
  :commands (org-hugo-auto-export-mode)
  :after (ox))

(provide 'setup-blog)
