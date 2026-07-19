;;; hb-static-site.el --- Personal static-site authoring helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal reusable workflow glue for Hugo static sites authored as Org files,
;; exported with ox-hugo, and named with Denote.
;;
;; Intended project-local pattern:
;;
;;   ((nil . ((denote-directory . "content-org")
;;            (denote-prompts . (subdirectory title keywords))
;;            (org-hugo-base-dir . ".")))
;;    ("content-org/" . ((org-mode . ((eval . (hb-static-site-mode)))))))
;;
;; Keep site-specific paths in .dir-locals.el.  This library derives posts and
;; pages from the active project/local variables and does not hardcode any one
;; Hugo repository.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar denote-directory nil
  "External Denote variable naming the active note directory.")
(defvar denote-org-front-matter nil
  "External Denote variable containing Org front matter template.")
(defvar denote-prompts nil
  "External Denote variable controlling note creation prompts.")
(defvar org-hugo-auto-export-mode nil
  "External ox-hugo auto-export mode variable.")
(defvar org-hugo-base-dir nil
  "External ox-hugo variable naming the active Hugo project root.")

(defun hb-static-site--safe-string-or-nil-p (value)
  "Return non-nil when VALUE is nil or a string."
  (or (null value) (stringp value)))

(defgroup hb-static-site nil
  "Personal Hugo/Org/Denote static-site workflow."
  :group 'applications)

(defcustom hb-static-site-content-org-directory nil
  "Directory containing Org sources for the current static site.

When nil, derive the directory from `denote-directory', then from
`org-hugo-base-dir'/content-org, then from the current project root/content-org."
  :type '(choice (const :tag "Derive from project/local variables" nil)
		 directory)
  :safe #'hb-static-site--safe-string-or-nil-p
  :group 'hb-static-site)

(defcustom hb-static-site-posts-subdirectory "posts"
  "Subdirectory of the content Org directory used for posts."
  :type 'string
  :safe #'stringp
  :group 'hb-static-site)

(defcustom hb-static-site-pages-subdirectory "pages"
  "Subdirectory of the content Org directory used for pages."
  :type 'string
  :safe #'stringp
  :group 'hb-static-site)

(defcustom hb-static-site-enable-auto-export t
  "Non-nil means `hb-static-site-mode' tries to enable ox-hugo auto export."
  :type 'boolean
  :safe #'booleanp
  :group 'hb-static-site)

(defcustom hb-static-site-org-front-matter "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+hugo_draft: true

"
  "Default Denote Org front matter for Hugo-authored site files."
  :type 'string
  :safe #'stringp
  :group 'hb-static-site)

(put 'denote-directory 'safe-local-variable #'stringp)
(put 'denote-prompts 'safe-local-variable #'listp)
(put 'denote-org-front-matter 'safe-local-variable #'stringp)
(put 'org-hugo-base-dir 'safe-local-variable #'stringp)
(put 'hb-static-site-mode 'safe-local-eval-function t)
(put 'hb-static-site-enable 'safe-local-eval-function t)

(defun hb-static-site--expand-directory (directory)
  "Return expanded DIRECTORY with a trailing slash, or nil."
  (when (and (stringp directory) (not (string-empty-p directory)))
    (file-name-as-directory (expand-file-name directory))))

(defun hb-static-site--project-root ()
  "Return the current project root, or `default-directory' when unavailable."
  (or (when (require 'project nil 'noerror)
	(when-let* ((project (project-current nil)))
	  (car (project-roots project))))
      default-directory))

(defun hb-static-site-hugo-base-dir ()
  "Return the active Hugo base directory for the current buffer."
  (hb-static-site--expand-directory
   (cond
    ((and (boundp 'org-hugo-base-dir) (stringp org-hugo-base-dir))
     org-hugo-base-dir)
    (t (hb-static-site--project-root)))))

(defun hb-static-site-content-org-directory ()
  "Return the active Org source directory for the current static site."
  (hb-static-site--expand-directory
   (cond
    ((stringp hb-static-site-content-org-directory)
     hb-static-site-content-org-directory)
    ((and (boundp 'denote-directory) (stringp denote-directory))
     denote-directory)
    ((hb-static-site-hugo-base-dir)
     (expand-file-name "content-org" (hb-static-site-hugo-base-dir))))))

(defun hb-static-site--content-org-directory-or-error ()
  "Return the active content Org directory or signal a user error."
  (or (hb-static-site-content-org-directory)
      (user-error "No static-site content Org directory configured")))

(defun hb-static-site-posts-directory ()
  "Return the active Denote posts directory."
  (expand-file-name hb-static-site-posts-subdirectory
		    (hb-static-site--content-org-directory-or-error)))

(defun hb-static-site-pages-directory ()
  "Return the active Denote pages directory."
  (expand-file-name hb-static-site-pages-subdirectory
		    (hb-static-site--content-org-directory-or-error)))

(defun hb-static-site--org-file-p (file)
  "Return non-nil when FILE names an Org file."
  (and (stringp file) (string-equal (file-name-extension file) "org")))

(defun hb-static-site--buffer-under-content-p ()
  "Return non-nil when the current buffer file is under content-org."
  (when-let* ((file (buffer-file-name))
	      (content (hb-static-site-content-org-directory)))
    (file-in-directory-p (file-truename file) (file-truename content))))

(defun hb-static-site--require-denote ()
  "Load Denote or signal a helpful user error."
  (unless (require 'denote nil 'noerror)
    (user-error "Denote is not available; install/load denote to create site files")))

(defun hb-static-site--require-ox-hugo ()
  "Load ox-hugo or signal a helpful user error."
  (unless (require 'ox-hugo nil 'noerror)
    (user-error "ox-hugo is not available; install/load ox-hugo to export Hugo content")))

(defun hb-static-site--read-keywords ()
  "Read Denote keywords, degrading when Denote prompt helpers are unavailable."
  (if (fboundp 'denote-keywords-prompt)
      (denote-keywords-prompt)
    (split-string (read-string "Keywords (comma-separated): ") "[, ]+" t)))

(defun hb-static-site--create-denote-file (directory title keywords)
  "Create a Denote Org file in DIRECTORY with TITLE and KEYWORDS."
  (hb-static-site--require-denote)
  (let* ((content (hb-static-site--content-org-directory-or-error))
	 (directory (file-name-as-directory (expand-file-name directory))))
    (make-directory directory t)
    (let ((denote-directory content)
	  (denote-prompts '(title keywords))
	  (denote-org-front-matter hb-static-site-org-front-matter))
      (find-file (denote title keywords 'org directory)))))

;;;###autoload
(defun hb-static-site-create-post (title keywords)
  "Create a Denote Hugo post under content-org/posts.

The content root is derived from .dir-locals.el variables such as
`denote-directory' and `org-hugo-base-dir'."
  (interactive (progn
		 (hb-static-site--require-denote)
		 (list (read-string "Post title: ")
		       (hb-static-site--read-keywords))))
  (hb-static-site--create-denote-file
   (hb-static-site-posts-directory) title keywords))

;;;###autoload
(defun hb-static-site-create-page (title keywords)
  "Create a Denote Hugo page under content-org/pages."
  (interactive (progn
		 (hb-static-site--require-denote)
		 (list (read-string "Page title: ")
		       (hb-static-site--read-keywords))))
  (hb-static-site--create-denote-file
   (hb-static-site-pages-directory) title keywords))

;;;###autoload
(defun hb-static-site-find-page ()
  "Find an existing Org page from content-org/pages."
  (interactive)
  (let* ((directory (hb-static-site-pages-directory))
	 (files (when (file-directory-p directory)
		  (seq-filter #'hb-static-site--org-file-p
			      (directory-files-recursively directory "\\.org\\'"))))
	 (file (if files
		   (completing-read "Page: " files nil t)
		 (user-error "No Org pages found in %s" directory))))
    (find-file file)))

(defun hb-static-site--validation-errors ()
  "Return a list of validation errors for the current static-site buffer."
  (delq nil
	(list
	 (unless (derived-mode-p 'org-mode) "Current buffer is not in Org mode")
	 (unless (buffer-file-name) "Current buffer is not visiting a file")
	 (unless (hb-static-site-content-org-directory) "No content Org directory configured")
	 (unless (hb-static-site-hugo-base-dir) "No Hugo base directory configured")
	 (unless (and (hb-static-site-content-org-directory)
		      (file-directory-p (hb-static-site-content-org-directory)))
	   (format "Content Org directory does not exist: %s"
		   (or (hb-static-site-content-org-directory) "<nil>")))
	 (unless (and (hb-static-site-hugo-base-dir)
		      (file-directory-p (hb-static-site-hugo-base-dir)))
	   (format "Hugo base directory does not exist: %s"
		   (or (hb-static-site-hugo-base-dir) "<nil>")))
	 (unless (hb-static-site--buffer-under-content-p)
	   "Current buffer is not under the content Org directory"))))

;;;###autoload
(defun hb-static-site-validate-buffer ()
  "Validate that the current buffer looks like a configured static-site source."
  (interactive)
  (let ((errors (hb-static-site--validation-errors)))
    (if errors
	(user-error "Static-site buffer is not exportable: %s"
		    (string-join errors "; "))
      (message "Static-site buffer OK: %s" (buffer-file-name))
      t)))

;;;###autoload
(defun hb-static-site-export-buffer (&optional all-subtrees)
  "Export the current Org buffer with ox-hugo.

With prefix argument ALL-SUBTREES, ask ox-hugo to export all valid Hugo
subtrees in the buffer."
  (interactive "P")
  (hb-static-site-validate-buffer)
  (hb-static-site--require-ox-hugo)
  (let ((org-hugo-base-dir (hb-static-site-hugo-base-dir)))
    (org-hugo-export-wim-to-md all-subtrees)))

;;;###autoload
(defun hb-static-site-validate-and-export-buffer (&optional all-subtrees)
  "Validate, then export the current Org buffer with ox-hugo."
  (interactive "P")
  (hb-static-site-export-buffer all-subtrees))

(defun hb-static-site--enable-auto-export ()
  "Enable ox-hugo auto export when available."
  (when hb-static-site-enable-auto-export
    (if (require 'ox-hugo nil 'noerror)
	(when (fboundp 'org-hugo-auto-export-mode)
	  (org-hugo-auto-export-mode 1))
      (message "[hb-static-site] ox-hugo unavailable; auto export not enabled"))))

;;;###autoload
(define-minor-mode hb-static-site-mode
  "Minor mode for editing an Org source buffer in a Hugo static site.

The mode is intentionally small: it uses project-local variables, enables
pleasant prose editing when those helpers exist, sets local Denote/Hugo defaults,
and gracefully skips optional packages that are not loaded."
  :lighter " Site"
  (if hb-static-site-mode
      (progn
	(setq-local denote-directory (hb-static-site-content-org-directory))
	(setq-local denote-prompts '(subdirectory title keywords))
	(setq-local denote-org-front-matter hb-static-site-org-front-matter)
	(when (fboundp 'visual-line-mode) (visual-line-mode 1))
	(when (fboundp 'writing/enable-basics) (writing/enable-basics))
	(hb-static-site--enable-auto-export))
    (when (and (boundp 'org-hugo-auto-export-mode) org-hugo-auto-export-mode)
      (org-hugo-auto-export-mode -1))))

;;;###autoload
(defun hb-static-site-enable ()
  "Enable static-site behavior in the current buffer."
  (interactive)
  (hb-static-site-mode 1))

(provide 'hb-static-site)
;;; hb-static-site.el ends here
