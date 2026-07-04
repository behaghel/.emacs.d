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

(declare-function gdocs-api-upload-image "gdocs-api"
		  (file-path callback &optional account folder-id))
(declare-function gdocs-api-create-anyone-reader-permission "gdocs-api"
		  (file-id callback &optional account))
(declare-function gdocs-convert-org-buffer-to-ir "gdocs-convert" ())

(defvar org-google-docs-images--push-session nil
  "Active native Google Docs image push session, or nil.")

(defgroup org-google-docs-images nil
  "Google Docs image semantic planning."
  :group 'org-google-docs)

(defcustom org-google-docs-images-supported-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg")
  "Local image extensions considered for native Google Docs image push."
  :type '(repeat string)
  :group 'org-google-docs-images)

(defcustom org-google-docs-images-make-uploaded-files-public t
  "Whether uploaded images should receive an anyone-reader Drive permission.
Google Docs `insertInlineImage' fetches image bytes from a URI, so uploaded
Drive images need a fetchable URI.  The default favors reliable publishing;
set this to nil only if your environment provides another fetchable URI path."
  :type 'boolean
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
  (expand-file-name path (or (and buffer-file-name
				  (file-name-directory buffer-file-name))
			     default-directory)))

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

(defun org-google-docs-images--drive-download-uri (file-id)
  "Return a direct Drive download URI for FILE-ID."
  (format "https://drive.google.com/uc?export=download&id=%s" file-id))

(defun org-google-docs-images--uploaded-file-id (metadata)
  "Return uploaded Drive file id from METADATA."
  (or (alist-get 'id metadata)
      (plist-get metadata :id)))

(defun org-google-docs-images--image-by-path (images path)
  "Return image entry from IMAGES matching PATH."
  (seq-find (lambda (image)
	      (equal (plist-get image :path) path))
	    images))

(defun org-google-docs-images--enrich-image-ir (element images)
  "Return image IR ELEMENT enriched with uploaded URI from IMAGES."
  (if (not (eq (plist-get element :type) 'image))
      element
    (if-let* ((image (org-google-docs-images--image-by-path
		      images (plist-get element :path)))
	      (uri (plist-get image :uri)))
	(plist-put (copy-sequence element) :uri uri)
      element)))

(defun org-google-docs-images--around-org-buffer-to-ir (orig)
  "Advise ORIG `gdocs-convert-org-buffer-to-ir' during native image push."
  (let ((ir (funcall orig)))
    (if org-google-docs-images--push-session
	(prog1
	    (mapcar (lambda (element)
		      (org-google-docs-images--enrich-image-ir
		       element
		       (plist-get org-google-docs-images--push-session :images)))
		    ir)
	  (setq org-google-docs-images--push-session nil))
      ir)))

(defun org-google-docs-images-enable-conversion-advice ()
  "Enable push-time image URI enrichment around gdocs conversion."
  (when (fboundp 'gdocs-convert-org-buffer-to-ir)
    (advice-add 'gdocs-convert-org-buffer-to-ir :around
		#'org-google-docs-images--around-org-buffer-to-ir)))

(defun org-google-docs-images--upload-one (image account callback)
  "Upload IMAGE using ACCOUNT, then call CALLBACK with enriched image entry."
  (gdocs-api-upload-image
   (plist-get image :absolute-path)
   (lambda (metadata)
     (let* ((file-id (org-google-docs-images--uploaded-file-id metadata))
	    (enriched (plist-put (copy-sequence image)
				 :uri (org-google-docs-images--drive-download-uri
				       file-id))))
       (unless file-id
	 (user-error "Google Drive upload response did not include an image file id"))
       (if org-google-docs-images-make-uploaded-files-public
	   (gdocs-api-create-anyone-reader-permission
	    file-id (lambda (_permission) (funcall callback enriched)) account)
	 (funcall callback enriched))))
   account))

(defun org-google-docs-images--upload-all (images account callback)
  "Upload IMAGES using ACCOUNT, then call CALLBACK with enriched images."
  (let ((remaining images)
	(uploaded nil))
    (cl-labels ((next ()
		  (if (null remaining)
		      (funcall callback (nreverse uploaded))
		    (let ((image (pop remaining)))
		      (org-google-docs-images--upload-one
		       image account
		       (lambda (enriched)
			 (push enriched uploaded)
			 (next)))))))
      (next))))

(defun org-google-docs-images-begin-push (plan callback &optional account)
  "Upload images from PLAN, activate conversion session, then call CALLBACK."
  (let ((images (plist-get plan :images)))
    (if (null images)
	(funcall callback)
      (org-google-docs-images--upload-all
       images account
       (lambda (uploaded)
	 (setq org-google-docs-images--push-session (list :images uploaded))
	 (org-google-docs-images-enable-conversion-advice)
	 (funcall callback))))))

(defun org-google-docs-images-deactivate-session ()
  "Deactivate active native image push session."
  (setq org-google-docs-images--push-session nil))

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
