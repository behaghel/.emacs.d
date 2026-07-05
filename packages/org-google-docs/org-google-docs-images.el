;;; org-google-docs-images.el --- Google Docs image planning -*- lexical-binding: t; -*-

;;; Commentary:
;; Preflight standalone Org image links before Google Docs push.  Native image
;; insertion needs an accessible image URI, so image-bearing pushes must never
;; silently degrade to literal file paths or no-op image IR.

;;; Code:

(require 'cl-lib)
(require 'org-sync-assets)
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

(defcustom org-google-docs-images-cache-directory "assets/google-docs"
  "Directory, relative to the Org file, used to cache pulled remote images."
  :type 'directory
  :group 'org-google-docs-images)

(defcustom org-google-docs-images-make-uploaded-files-public t
  "Whether uploaded images should receive an anyone-reader Drive permission.
Google Docs `insertInlineImage' fetches image bytes from a URI, so uploaded
Drive images need a fetchable URI.  The default favors reliable publishing;
set this to nil only if your environment provides another fetchable URI path."
  :type 'boolean
  :group 'org-google-docs-images)

(defun org-google-docs-images--absolute-path (path)
  "Return absolute file PATH for an Org image link."
  (org-sync-assets-absolute-path path))

(defun org-google-docs-images--document-cache-directory ()
  "Return local cache directory for the current Google Docs buffer."
  (let ((base (if buffer-file-name
		  (file-name-directory buffer-file-name)
		default-directory))
	(doc-id (and (boundp 'gdocs-sync--document-id)
		     (symbol-value 'gdocs-sync--document-id))))
    (expand-file-name (if doc-id
			  (expand-file-name doc-id org-google-docs-images-cache-directory)
			org-google-docs-images-cache-directory)
		      base)))

(defun org-google-docs-images--remote-image-paragraphs ()
  "Return standalone remote image paragraph/link pairs in reverse buffer order."
  (let ((tree (org-element-parse-buffer))
	entries)
    (org-element-map tree 'paragraph
		     (lambda (paragraph)
		       (when-let* ((link (org-sync-assets-standalone-remote-link paragraph)))
			 (push (cons paragraph link) entries))))
    entries))

(defun org-google-docs-images--replace-link-with-local-file (link local-path)
  "Replace remote LINK with LOCAL-PATH relative to current buffer."
  (let* ((begin (org-element-property :begin link))
	 (end (org-element-property :end link))
	 (relative (file-relative-name
		    local-path
		    (or (and buffer-file-name (file-name-directory buffer-file-name))
			default-directory))))
    (goto-char begin)
    (delete-region begin end)
    (insert (format "[[file:%s]]" relative))))

;;;###autoload
(defun org-google-docs-images-cache-remote-images ()
  "Cache standalone remote image links in the current Org buffer locally.
Pulled Google Docs images are initially represented as remote Org links.  This
command downloads those links into `org-google-docs-images-cache-directory' and
rewrites them as local `file:' links so Org inline image display and later push
use the normal local-asset path."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (let ((directory (org-google-docs-images--document-cache-directory))
	(count 0))
    (save-excursion
      (dolist (entry (org-google-docs-images--remote-image-paragraphs))
	(let* ((link (cdr entry))
	       (url (org-element-property :raw-link link))
	       (local-path (org-sync-assets-cache-remote-url url directory)))
	  (org-google-docs-images--replace-link-with-local-file link local-path)
	  (cl-incf count))))
    (message "Cached %d Google Docs remote image(s)" count)
    count))

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
  (let* ((asset-plan (org-sync-assets-plan-buffer))
	 (images (mapcar (lambda (asset)
			   (append (list :path (plist-get asset :source-link))
				   asset))
			 (plist-get asset-plan :assets))))
    (list :ready-p (plist-get asset-plan :ready-p)
	  :images images
	  :diagnostics (plist-get asset-plan :diagnostics))))

(provide 'org-google-docs-images)
;;; org-google-docs-images.el ends here
