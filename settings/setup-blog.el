;; Blogging

(setq blog-root "~/Documents/org/le-carnet")
(setq blog-posts-dir (expand-file-name "posts" blog-root))

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

(defun blog-post-hook ()
  (let ((current-path (or (buffer-file-name) "don't know")))
    (cond ((string-match blog-posts-dir current-path)
           (ispell-change-dictionary "uk")
           (setq-local ispell-check-comments nil)
           (writeroom-mode)
           (artbollocks-mode)))))

(add-hook 'org-mode-hook 'blog-post-hook)

(defun hub/sluggify (title)
  "Transform the TITLE of an article into a slug suitable for an URL."
  (let* ((lc-title (downcase title))
         (no-space (replace-regexp-in-string " +" "-" lc-title))
         (striped (replace-regexp-in-string "[',!?.:/()_;\"<>«»@#]" "" no-space))
         (asciified (xah-asciify-string striped)))
    asciified))

(defun xah-asciify-region (&optional φfrom φto)
  "Change European language characters into equivalent ASCII ones, ⁖ “café” ⇒ “cafe”.
This command does not transcode all Unicode chars such as Greek, math symbols. They remains.
When called interactively, work on text selection or current line.
URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2014-10-20"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((ξcharMap [
                   ["á\\|à\\|â\\|ä\\|ã\\|å" "a"]
                   ["é\\|è\\|ê\\|ë" "e"]
                   ["í\\|ì\\|î\\|ï" "i"]
                   ["ó\\|ò\\|ô\\|ö\\|õ\\|ø" "o"]
                   ["ú\\|ù\\|û\\|ü"     "u"]
                   ["Ý\\|ý\\|ÿ"     "y"]
                   ["ñ" "n"]
                   ["ç" "c"]
                   ["ð" "d"]
                   ["þ" "th"]
                   ["ß" "ss"]
                   ["æ" "ae"]
                   ]))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region φfrom φto)
        (mapc
         (lambda (ξpair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt ξpair 0) (point-max) t)
             (replace-match (elt ξpair 1))))
         ξcharMap)))))

(defun xah-asciify-string (φstring)
  "Returns a new string. European language chars are changed ot ASCII ones ⁖ “café” ⇒ “cafe”.
See `xah-asciify-region'
Version 2014-10-20"
  (with-temp-buffer
    (insert φstring)
    (xah-asciify-region (point-min) (point-max))
    (buffer-string)))

(provide 'setup-blog)