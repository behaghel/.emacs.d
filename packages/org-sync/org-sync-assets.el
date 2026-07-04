;;; org-sync-assets.el --- Shared Org sync asset planning -*- lexical-binding: t; -*-

;;; Commentary:
;; Provider-neutral Org asset helpers for bidirectional sync packages.
;; Provider packages own remote upload/download APIs; this library owns local Org
;; image detection, path resolution, generated filenames, and missing-source
;; classification.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup org-sync-assets nil
  "Shared Org synchronization asset helpers."
  :group 'org)

(defcustom org-sync-assets-image-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg")
  "Local image extensions considered by Org sync providers."
  :type '(repeat string)
  :group 'org-sync-assets)

(defun org-sync-assets-diagnostic (code message &rest properties)
  "Return an asset diagnostic with CODE, MESSAGE, and PROPERTIES."
  (append (list :code code :message message) properties))

(defun org-sync-assets-blank-string-p (object)
  "Return non-nil when OBJECT is only whitespace text."
  (and (stringp object) (string-blank-p object)))

(defun org-sync-assets-image-link-p (object)
  "Return non-nil when OBJECT is an undescribed local image link."
  (and (not (stringp object))
       (eq (org-element-type object) 'link)
       (string= (org-element-property :type object) "file")
       (null (org-element-contents object))
       (member (downcase (or (file-name-extension
			      (org-element-property :path object))
			     ""))
	       org-sync-assets-image-extensions)))

(defun org-sync-assets-standalone-image-link (paragraph)
  "Return PARAGRAPH's standalone image link, or nil."
  (let ((objects (seq-remove #'org-sync-assets-blank-string-p
			     (org-element-contents paragraph))))
    (when (and (= (length objects) 1)
	       (org-sync-assets-image-link-p (car objects)))
      (car objects))))

(defun org-sync-assets-caption (paragraph)
  "Return PARAGRAPH caption text, or nil."
  (when-let* ((caption (org-element-property :caption paragraph)))
    (string-trim (org-element-interpret-data caption))))

(defun org-sync-assets-absolute-path (path &optional base-directory require-buffer-file)
  "Return absolute file PATH using BASE-DIRECTORY or current buffer context.
When REQUIRE-BUFFER-FILE is non-nil, relative paths in unsaved buffers signal a
`user-error' instead of falling back to `default-directory'."
  (cond
   ((file-name-absolute-p path) (expand-file-name path))
   (base-directory (expand-file-name path base-directory))
   (buffer-file-name (expand-file-name path (file-name-directory buffer-file-name)))
   (require-buffer-file
    (user-error "Cannot resolve relative image path in unsaved Org buffer: %s" path))
   (t (expand-file-name path default-directory))))

(defun org-sync-assets-resolve-local-image (path &optional base-directory require-buffer-file)
  "Resolve local image PATH or signal a clear `user-error'."
  (let ((absolute-path (org-sync-assets-absolute-path
			path base-directory require-buffer-file)))
    (unless (file-readable-p absolute-path)
      (user-error "Image file is not readable: %s" path))
    absolute-path))

(defun org-sync-assets-maybe-resolve-local-image (path &optional base-directory require-buffer-file)
  "Resolve local image PATH, or return nil when it is unreadable."
  (condition-case nil
      (org-sync-assets-resolve-local-image path base-directory require-buffer-file)
    (user-error nil)))

(defun org-sync-assets-hashed-filename-p (filename)
  "Return non-nil when FILENAME looks like a generated sync asset name."
  (string-match-p "-[0-9a-f]\\{12\\}\\.[^.]+\\'" filename))

(defun org-sync-assets-hashed-filename (path)
  "Return a stable generated asset filename for PATH.
This intentionally matches the existing Confluence filename scheme for
incremental migration compatibility."
  (let* ((extension (file-name-extension path t))
	 (stem (file-name-sans-extension (file-name-nondirectory path)))
	 (hash (substring (secure-hash 'sha256 path) 0 12)))
    (format "%s-%s%s" stem hash extension)))

(defun org-sync-assets-entry (paragraph link &optional options)
  "Return an image asset entry for PARAGRAPH and LINK.
OPTIONS is a plist.  `:reuse-imported-missing-source' marks generated/imported
filenames with missing local sources as reusable instead of failing."
  (let* ((source-link (org-element-property :path link))
	 (source-path (org-sync-assets-maybe-resolve-local-image
		       source-link nil
		       (plist-get options :require-buffer-file-for-relative)))
	 (basename (file-name-nondirectory source-link))
	 (imported-p (org-sync-assets-hashed-filename-p basename))
	 (reuse-imported (plist-get options :reuse-imported-missing-source))
	 (filename (cond
		    (imported-p basename)
		    (source-path (org-sync-assets-hashed-filename source-path))
		    (t basename)))
	 (entry (list :source-link source-link
		      :path source-path
		      :source-path source-path
		      :absolute-path (or source-path
					 (org-sync-assets-absolute-path
					  source-link nil
					  (plist-get options
						     :require-buffer-file-for-relative)))
		      :filename filename
		      :begin (org-element-property :begin paragraph)
		      :end (org-element-property :end paragraph))))
    (when-let* ((caption (org-sync-assets-caption paragraph)))
      (setq entry (append entry (list :caption caption))))
    (when (and imported-p reuse-imported (not source-path))
      (setq entry (append entry (list :missing-source t))))
    entry))

(defun org-sync-assets-missing-diagnostics (assets)
  "Return missing-file diagnostics for ASSETS."
  (cl-loop for asset in assets
	   unless (or (plist-get asset :missing-source)
		      (file-readable-p (plist-get asset :absolute-path)))
	   collect (org-sync-assets-diagnostic
		    :missing-image-file
		    (format "Image file is not readable: %s"
			    (plist-get asset :source-link))
		    :path (plist-get asset :source-link)
		    :absolute-path (plist-get asset :absolute-path))))

;;;###autoload
(defun org-sync-assets-plan-buffer (&optional options)
  "Return standalone image asset plan for the current Org buffer.
OPTIONS is passed to `org-sync-assets-entry'.  When `:subtreep' is non-nil,
inspect only the current Org subtree."
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (save-restriction
    (when (plist-get options :subtreep)
      (unless (org-at-heading-p)
	(org-back-to-heading))
      (org-narrow-to-subtree))
    (let* ((tree (org-element-parse-buffer))
	   (assets (org-element-map tree 'paragraph
				    (lambda (paragraph)
				      (when-let* ((link (org-sync-assets-standalone-image-link
							 paragraph)))
					(org-sync-assets-entry paragraph link options)))))
	   (diagnostics (org-sync-assets-missing-diagnostics assets)))
      (list :ready-p (null diagnostics)
	    :assets assets
	    :diagnostics diagnostics))))

(provide 'org-sync-assets)
;;; org-sync-assets.el ends here
