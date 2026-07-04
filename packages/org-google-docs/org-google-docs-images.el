;;; org-google-docs-images.el --- Google Docs image planning -*- lexical-binding: t; -*-

;;; Commentary:
;; Preflight standalone Org image links before Google Docs push.  Native image
;; insertion needs an accessible image URI, so image-bearing pushes must never
;; silently degrade to literal file paths or no-op image IR.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup org-google-docs-images nil
  "Google Docs image semantic planning."
  :group 'org-google-docs)

(defcustom org-google-docs-images-supported-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg")
  "Local image extensions considered for native Google Docs image push."
  :type '(repeat string)
  :group 'org-google-docs-images)

(defun org-google-docs-images--diagnostic (code message &rest properties)
  "Return an image diagnostic with CODE, MESSAGE, and PROPERTIES."
  (append (list :code code :message message) properties))

(defun org-google-docs-images--blank-string-p (object)
  "Return non-nil when OBJECT is only whitespace text."
  (and (stringp object) (string-blank-p object)))

(defun org-google-docs-images--standalone-link (paragraph)
  "Return the standalone image link in PARAGRAPH, or nil."
  (let ((objects (seq-remove #'org-google-docs-images--blank-string-p
			     (org-element-contents paragraph))))
    (when (and (= (length objects) 1)
	       (org-google-docs-images--image-link-p (car objects)))
      (car objects))))

(defun org-google-docs-images--image-link-p (object)
  "Return non-nil when OBJECT is an undescribed local image link."
  (and (not (stringp object))
       (eq (org-element-type object) 'link)
       (string= (org-element-property :type object) "file")
       (null (org-element-contents object))
       (member (downcase (or (file-name-extension
			      (org-element-property :path object))
			     ""))
	       org-google-docs-images-supported-extensions)))

(defun org-google-docs-images--absolute-path (path)
  "Return absolute file PATH for an Org image link."
  (if (file-name-absolute-p path)
      path
    (expand-file-name path (or (and buffer-file-name
				    (file-name-directory buffer-file-name))
			       default-directory))))

(defun org-google-docs-images--caption (paragraph)
  "Return PARAGRAPH caption text, or nil."
  (when-let* ((caption (org-element-property :caption paragraph)))
    (string-trim (org-element-interpret-data caption))))

(defun org-google-docs-images--entry (paragraph link)
  "Return image plan entry for PARAGRAPH and LINK."
  (let* ((path (org-element-property :path link))
	 (absolute-path (org-google-docs-images--absolute-path path)))
    (append (list :path path
		  :absolute-path absolute-path
		  :begin (org-element-property :begin paragraph)
		  :end (org-element-property :end paragraph))
	    (when-let* ((caption (org-google-docs-images--caption paragraph)))
	      (list :caption caption)))))

(defun org-google-docs-images--missing-diagnostics (images)
  "Return missing-file diagnostics for IMAGES."
  (cl-loop for image in images
	   unless (file-readable-p (plist-get image :absolute-path))
	   collect (org-google-docs-images--diagnostic
		    :missing-image-file
		    (format "Image file is not readable: %s"
			    (plist-get image :path))
		    :path (plist-get image :path)
		    :absolute-path (plist-get image :absolute-path))))

;;;###autoload
(defun org-google-docs-images-plan-buffer ()
  "Return a native Google Docs image push plan for the current Org buffer."
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (let* ((tree (org-element-parse-buffer))
	 (images (org-element-map tree 'paragraph
				  (lambda (paragraph)
				    (when-let* ((link (org-google-docs-images--standalone-link
						       paragraph)))
				      (org-google-docs-images--entry paragraph link))))))
    (list :ready-p (null (org-google-docs-images--missing-diagnostics images))
	  :images images
	  :diagnostics (org-google-docs-images--missing-diagnostics images))))

(provide 'org-google-docs-images)
;;; org-google-docs-images.el ends here
