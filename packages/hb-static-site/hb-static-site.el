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

(require 'autoinsert)
(require 'org)
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
(defvar org-hugo-front-matter-format nil
  "External ox-hugo variable naming the front matter format.")

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

(defcustom hb-static-site-post-org-front-matter "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+hugo_section: posts
#+hugo_draft: true

"
  "Denote Org front matter for Hugo posts."
  :type 'string
  :safe #'stringp
  :group 'hb-static-site)

(put 'denote-directory 'safe-local-variable #'stringp)
(put 'denote-prompts 'safe-local-variable #'listp)
(put 'denote-org-front-matter 'safe-local-variable #'stringp)
(put 'org-hugo-base-dir 'safe-local-variable #'stringp)
(put 'org-hugo-front-matter-format 'safe-local-variable #'stringp)
(put 'hb-static-site-mode 'safe-local-eval-function t)
(put 'hb-static-site-enable 'safe-local-eval-function t)

(defvar hb-static-site-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c w n") #'hb-static-site-create-post)
    (define-key map (kbd "C-c w s") #'hb-static-site-create-section)
    (define-key map (kbd "C-c w p") #'hb-static-site-create-page)
    (define-key map (kbd "C-c w P") #'hb-static-site-create-basic-page)
    (define-key map (kbd "C-c w o") #'hb-static-site-find-page)
    (define-key map (kbd "C-c w e") #'hb-static-site-export-buffer)
    (define-key map (kbd "C-c w E") #'hb-static-site-export-all)
    (define-key map (kbd "C-c w v") #'hb-static-site-validate-buffer)
    map)
  "Keymap for `hb-static-site-mode'.")

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w n") #'hb-static-site-create-post)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w s") #'hb-static-site-create-section)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w p") #'hb-static-site-create-page)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w P") #'hb-static-site-create-basic-page)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w o") #'hb-static-site-find-page)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w e") #'hb-static-site-export-buffer)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w E") #'hb-static-site-export-all)
    (evil-define-key 'normal hb-static-site-mode-map
		     (kbd ",w v") #'hb-static-site-validate-buffer)))

(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements "C-c w" "static site")))

(defun hb-static-site--expand-directory (directory &optional base)
  "Return expanded DIRECTORY with a trailing slash, or nil.

Relative directories are expanded from BASE when provided.  This matters for
project-local values like \"content-org\" and \".\" from .dir-locals.el: they
must stay anchored at the site root, not at the currently visited Org file's
subdirectory."
  (when (and (stringp directory) (not (string-empty-p directory)))
    (file-name-as-directory (expand-file-name directory base))))

(defun hb-static-site--site-root ()
  "Return the current static-site root directory."
  (file-name-as-directory
   (or (locate-dominating-file default-directory "hugo.toml")
       (locate-dominating-file default-directory ".dir-locals.el")
       (hb-static-site--project-root))))

(defun hb-static-site--project-root ()
  "Return the current project root, or `default-directory' when unavailable."
  (or (when (require 'project nil 'noerror)
	(when-let* ((project (project-current nil)))
	  (car (project-roots project))))
      default-directory))

(defun hb-static-site-hugo-base-dir ()
  "Return the active Hugo base directory for the current buffer."
  (let ((root (hb-static-site--site-root)))
    (hb-static-site--expand-directory
     (cond
      ((and (boundp 'org-hugo-base-dir) (stringp org-hugo-base-dir))
       org-hugo-base-dir)
      (t root))
     root)))

(defun hb-static-site-content-org-directory ()
  "Return the active Org source directory for the current static site."
  (let ((root (hb-static-site-hugo-base-dir)))
    (hb-static-site--expand-directory
     (cond
      ((stringp hb-static-site-content-org-directory)
       hb-static-site-content-org-directory)
      ((and (boundp 'denote-directory) (stringp denote-directory))
       denote-directory)
      (root (expand-file-name "content-org" root)))
     root)))

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

(defun hb-static-site--slugify (text)
  "Return a Hugo-friendly slug derived from TEXT."
  (let* ((downcased (downcase (string-trim text)))
	 (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" downcased)))
    (string-trim slug "-+" "-+")))

(defun hb-static-site--titleize-slug (slug)
  "Return a human title derived from SLUG."
  (string-join
   (mapcar #'capitalize (split-string (replace-regexp-in-string "[-_]" " " slug)))
   " "))

(defun hb-static-site--read-slug (prompt)
  "Read a static-site slug with PROMPT."
  (let ((slug (hb-static-site--slugify (read-string prompt))))
    (if (string-empty-p slug)
	(user-error "Slug cannot be empty")
      slug)))

(defun hb-static-site--read-title (prompt default)
  "Read a title with PROMPT and DEFAULT."
  (read-string (format "%s (default %s): " prompt default) nil nil default))

(defun hb-static-site-section-names ()
  "Return existing first-level Hugo section names under content-org.

A directory counts as a section when it contains `_index.org'.  The root
section `/` is always available for ordinary root pages."
  (let* ((content (hb-static-site--content-org-directory-or-error))
	 (sections (when (file-directory-p content)
		     (seq-filter
		      (lambda (name)
			(and (not (member name '("." ".." "pages" "posts")))
			     (file-directory-p (expand-file-name name content))
			     (file-exists-p (expand-file-name "_index.org"
							      (expand-file-name name content)))))
		      (directory-files content nil "^[^.].*")))))
    (cons "/" (sort sections #'string<))))

(defun hb-static-site--read-page-args ()
  "Read section, slug and title for page creation."
  (let* ((section (completing-read "Section: " (hb-static-site-section-names) nil t nil nil "/"))
	 (slug (hb-static-site--read-slug "Page slug: "))
	 (title (hb-static-site--read-title "Page title" (hb-static-site--titleize-slug slug)))
	 (path (if (string= section "/") slug (concat section "/" slug))))
    (list path title)))

(defun hb-static-site--expand-template (template)
  "Insert TEMPLATE, using YASnippet when available."
  (if (fboundp 'yas-expand-snippet)
      (yas-expand-snippet template (point-min) (point-max))
    (insert (replace-regexp-in-string "\\(?:\n\\)?\\$0" "" template t t))))

(defun hb-static-site--auto-insert-template (file template)
  "Visit FILE and expand TEMPLATE through `auto-insert'."
  (make-directory (file-name-directory file) t)
  (find-file file)
  (when (= (buffer-size) 0)
    (let ((auto-insert-query nil)
	  (auto-insert-alist
	   `(("\\.org\\'"
	      . [(lambda () (hb-static-site--expand-template ,template))]))))
      (auto-insert)))
  (current-buffer))

(defun hb-static-site--page-template (title section slug base-dir &optional bundle)
  "Return an Org/YAS template for page TITLE in Hugo SECTION with SLUG.

BASE-DIR is the relative `org-hugo-base-dir' value for the generated file.
When BUNDLE is non-nil, add `hugo_bundle' for a Hugo leaf bundle."
  (format "#+title: %s
#+hugo_base_dir: %s
#+hugo_section: %s
#+hugo_slug: %s
%s
$0
" title base-dir section slug (if bundle (format "#+hugo_bundle: %s\n" slug) "")))

(defun hb-static-site--section-template (title section)
  "Return an Org/YAS template for section TITLE and SECTION."
  (format "#+title: %s
#+hugo_base_dir: ../..
#+hugo_section: %s

$0
" title section))

(defun hb-static-site--create-denote-file (directory title keywords front-matter)
  "Create a Denote Org file in DIRECTORY with TITLE, KEYWORDS and FRONT-MATTER."
  (hb-static-site--require-denote)
  (let* ((content (hb-static-site--content-org-directory-or-error))
	 (directory (file-name-as-directory (expand-file-name directory))))
    (make-directory directory t)
    (let ((denote-directory content)
	  (denote-prompts '(title keywords))
	  (denote-org-front-matter front-matter))
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
   (hb-static-site-posts-directory) title keywords
   hb-static-site-post-org-front-matter))

;;;###autoload
(defun hb-static-site-create-section (slug title)
  "Create a Hugo section index Org file under content-org/SLUG/_index.org."
  (interactive
   (let* ((slug (hb-static-site--read-slug "Section slug: "))
	  (title (hb-static-site--read-title
		  "Section title" (hb-static-site--titleize-slug slug))))
     (list slug title)))
  (let ((file (expand-file-name
	       (concat (file-name-as-directory slug) "_index.org")
	       (hb-static-site--content-org-directory-or-error))))
    (hb-static-site--auto-insert-template
     file (hb-static-site--section-template title slug))))

(defun hb-static-site--page-path-parts (path)
  "Return normalized path parts for page PATH."
  (let ((parts (split-string (string-remove-prefix "/" path) "/" t)))
    (unless parts
      (user-error "Page path cannot be empty"))
    parts))

(defun hb-static-site--page-section (parts)
  "Return Hugo section for page PARTS."
  (if (= (length parts) 1) "/" (car parts)))

(defun hb-static-site--page-slug (parts)
  "Return Hugo slug for page PARTS."
  (hb-static-site--slugify (file-name-base (car (last parts)))))

(defun hb-static-site--page-directory (parts &optional bundle)
  "Return Org directory for page PARTS.

When BUNDLE is non-nil, return the leaf-bundle directory."
  (let* ((slug (hb-static-site--page-slug parts))
	 (parent (if (= (length parts) 1)
		     (hb-static-site-pages-directory)
		   (expand-file-name (car parts)
				     (hb-static-site--content-org-directory-or-error)))))
    (if bundle (expand-file-name slug parent) parent)))

;;;###autoload
(defun hb-static-site-create-basic-page (path title)
  "Create a basic Hugo page Org file.

PATH without a slash creates content-org/pages/PATH.org and exports to the
site root.  PATH with a slash, such as notes/first-note, creates a page inside
that Hugo section.  Prefer `hb-static-site-create-page' for new pages because
it creates a Hugo leaf bundle that can own local page resources."
  (interactive (hb-static-site--read-page-args))
  (let* ((parts (hb-static-site--page-path-parts path))
	 (slug (hb-static-site--page-slug parts))
	 (section (hb-static-site--page-section parts))
	 (directory (hb-static-site--page-directory parts))
	 (file (expand-file-name (concat slug ".org") directory)))
    (hb-static-site--auto-insert-template
     file (hb-static-site--page-template title section slug "../.."))))

;;;###autoload
(defun hb-static-site-create-page (path title)
  "Create a Hugo leaf-bundle page Org file.

PATH without a slash creates content-org/pages/PATH/index.org and exports to
the site root.  PATH with a slash, such as notes/first-note, creates a bundle
inside that Hugo section.  Leaf bundles can own page-local resources such as
images."
  (interactive (hb-static-site--read-page-args))
  (let* ((parts (hb-static-site--page-path-parts path))
	 (slug (hb-static-site--page-slug parts))
	 (section (hb-static-site--page-section parts))
	 (directory (hb-static-site--page-directory parts t))
	 (file (expand-file-name "index.org" directory)))
    (hb-static-site--auto-insert-template
     file (hb-static-site--page-template title section slug "../../.." t))))

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

(defun hb-static-site-org-files (&optional directory)
  "Return Org source files under DIRECTORY or the active content Org root."
  (let ((root (file-name-as-directory
	       (or directory (hb-static-site--content-org-directory-or-error)))))
    (seq-filter #'hb-static-site--org-file-p
		(directory-files-recursively root "\\.org\\'"))))

;;;###autoload
(defun hb-static-site-export-all (&optional directory)
  "Export every Org source file under DIRECTORY or the active content Org root."
  (interactive)
  (hb-static-site--require-ox-hugo)
  (let* ((root (file-name-as-directory
		(or directory (hb-static-site--content-org-directory-or-error))))
	 (files (hb-static-site-org-files root))
	 (exported nil)
	 (failures nil))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
	(condition-case err
	    (progn
	      (org-mode)
	      (hb-static-site-export-buffer t)
	      (push file exported))
	  (error
	   (push (format "%s: %s" file (error-message-string err)) failures)))))
    (if failures
	(user-error "Static-site export failed for %d/%d files: %s"
		    (length failures) (length files) (string-join (nreverse failures) "; "))
      (message "Static-site exported %d Org files from %s" (length exported) root)
      (nreverse exported))))

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
pleasant prose editing when those helpers exist, sets local Denote/Hugo
defaults, and gracefully skips optional packages that are not loaded."
  :lighter " Site"
  :keymap hb-static-site-mode-map
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
