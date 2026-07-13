;;; org-confluence.el --- Org Confluence export package facade -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-comments "0.1.0"))
;; Keywords: outlines, tools, hypermedia
;; URL: https://github.com/behaghel/org-confluence

;;; Commentary:
;; Package-style entrypoint for the Org -> Confluence exporter.
;;
;; Requiring `org-confluence' loads the exporter commands, the Org export
;; backend, and Confluence comment integration.  User activation happens through
;; `org-confluence-mode' or `org-confluence-mode-maybe'.

;;; Code:

(require 'org)
(require 'org-comments)
(require 'subr-x)
(require 'transient nil 'noerror)
(require 'org-confluence-api)
(require 'org-confluence-comments-backend)
(require 'org-confluence-commands)
(require 'org-confluence-people-store)
(require 'org-confluence-sync-status-marker)
(require 'org-sync nil 'noerror)

(declare-function org-confluence-comments-import "org-confluence-comments-import" (&optional page-id body-format))
(declare-function org-confluence-open-page "org-confluence-publish" ())
(declare-function org-confluence-publish "org-confluence-publish" (&optional page-id subtreep visible-only body-only ext-plist))
(declare-function org-confluence-pull "org-confluence-import" (&optional page-id))
(declare-function org-confluence-pull-child-page "org-confluence-sync" (&optional page-id))
(declare-function org-confluence-sync-current "org-confluence-sync" ())
(declare-function org-confluence-sync-status "org-confluence-sync-status" (&optional refresh))

(defcustom org-confluence-enable-org-comments-mode t
  "Whether `org-confluence-mode' also enables `org-comments-mode'."
  :type 'boolean
  :group 'org-confluence-api)

(defcustom org-confluence-show-empty-page-notice t
  "Whether `org-confluence-mode' marks linked Org buffers with empty bodies."
  :type 'boolean
  :group 'org-confluence-api)

(defcustom org-confluence-keymap-prefix "C-c C-x C"
  "Prefix key used by `org-confluence-mode' for `org-confluence-dispatch'.
Set this to nil to leave `org-confluence-mode-map' without a default binding."
  :type '(choice (const :tag "No default binding" nil)
		 string)
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (when (fboundp 'org-confluence-rebuild-mode-map)
	   (org-confluence-rebuild-mode-map)))
  :group 'org-confluence-api)

(defface org-confluence-empty-page-notice-face
  '((t :inherit shadow :slant italic))
  "Face for the Confluence empty page notice."
  :group 'org-confluence-api)

(defvar-local org-confluence--empty-page-overlay nil
  "Overlay displaying the Confluence empty page notice.")

(defvar-local org-confluence--enabled-org-comments-mode nil
  "Non-nil when `org-confluence-mode' enabled `org-comments-mode'.")

(defvar-local org-confluence--installed-account-id-resolver nil
  "Non-nil when `org-confluence-mode' installed the Org comments resolver.")

(defun org-confluence-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an Org buffer linked to a Confluence page."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'org-mode)
	 (org-confluence-api--page-id-from-buffer))))

(defun org-confluence--metadata-end-position ()
  "Return position after leading Org metadata keywords and blank lines."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
		(looking-at-p "^[ \t]*#\\+[^:\n]+:.*$"))
      (forward-line 1))
    (while (and (not (eobp))
		(looking-at-p "^[ \t]*$"))
      (forward-line 1))
    (point)))

(defun org-confluence--body-empty-p ()
  "Return non-nil when the current Org buffer body has no content."
  (save-excursion
    (goto-char (org-confluence--metadata-end-position))
    (string-empty-p (string-trim (buffer-substring-no-properties (point) (point-max))))))

(defun org-confluence--delete-empty-page-notice ()
  "Delete the Confluence empty page notice overlay."
  (when (overlayp org-confluence--empty-page-overlay)
    (delete-overlay org-confluence--empty-page-overlay))
  (setq org-confluence--empty-page-overlay nil))

;;;###autoload
(defun org-confluence-refresh-empty-page-notice ()
  "Refresh the Confluence empty page notice in the current buffer."
  (interactive)
  (org-confluence--delete-empty-page-notice)
  (when (and org-confluence-show-empty-page-notice
	     (derived-mode-p 'org-mode)
	     (org-confluence-buffer-p)
	     (org-confluence--body-empty-p))
    (let* ((position (org-confluence--metadata-end-position))
	   (overlay (make-overlay position position nil t nil)))
      (overlay-put overlay 'priority -10)
      (overlay-put overlay 'after-string
		   (concat
		    (propertize "[Confluence page has no body content; it may be a folder/index page]"
				'face 'org-confluence-empty-page-notice-face)
		    "\n"))
      (setq org-confluence--empty-page-overlay overlay))))

(defconst org-confluence--dispatch-entries
  '((?s "Status: Sync status" org-confluence-sync-status)
    (?y "Sync: Sync page and comments" org-confluence-sync-current)
    (?p "Publish: Publish page" org-confluence-publish)
    (?P "Pull: Pull current file" org-confluence-pull)
    (?o "Open: Open remote page" org-confluence-open-page)
    (?i "Comments: Import comments" org-confluence-comments-import)
    (?d "Navigate: Pull descendant" org-confluence-pull-child-page))
  "Dispatch entries for `org-confluence-dispatch'.")

(defun org-confluence--dispatch-command (key)
  "Return command associated with dispatch KEY."
  (nth 2 (assq key org-confluence--dispatch-entries)))

(defun org-confluence--dispatch-fallback ()
  "Show a simple fallback dispatch menu for Confluence commands."
  (let* ((prompt "Confluence: [s] status  [y] sync  [p] publish  [P] pull  [o] open  [i] comments  [d] descendant  [q] quit ")
	 (key (read-char-choice prompt '(?s ?y ?p ?P ?o ?i ?d ?q))))
    (unless (eq key ?q)
      (call-interactively (org-confluence--dispatch-command key)))))

(when (featurep 'transient)
  (transient-define-prefix org-confluence--dispatch-transient ()
			   "Show Confluence commands for the current Org buffer."
			   [["Confluence"
			     ("s" "Status: sync status" org-confluence-sync-status)
			     ("y" "Sync: page and comments" org-confluence-sync-current)
			     ("p" "Publish: page" org-confluence-publish)
			     ("P" "Pull: current file" org-confluence-pull)
			     ("o" "Open: remote page" org-confluence-open-page)
			     ("i" "Comments: import" org-confluence-comments-import)
			     ("d" "Navigate: pull descendant" org-confluence-pull-child-page)]]))

;;;###autoload
(defun org-confluence-dispatch ()
  "Show Confluence commands for the current Org buffer."
  (interactive)
  (if (and (featurep 'transient) (fboundp 'org-confluence--dispatch-transient))
      (call-interactively #'org-confluence--dispatch-transient)
    (org-confluence--dispatch-fallback)))

(defvar org-confluence-mode-map)

(defun org-confluence--keymap-prefix-vector ()
  "Return `org-confluence-keymap-prefix' as a key vector, or nil."
  (when org-confluence-keymap-prefix
    (kbd org-confluence-keymap-prefix)))

(defun org-confluence-rebuild-mode-map ()
  "Rebuild `org-confluence-mode-map' from `org-confluence-keymap-prefix'."
  (setq org-confluence-mode-map (make-sparse-keymap))
  (when-let* ((prefix (org-confluence--keymap-prefix-vector)))
    (define-key org-confluence-mode-map prefix #'org-confluence-dispatch))
  org-confluence-mode-map)

(defvar org-confluence-mode-map
  (let ((map (make-sparse-keymap)))
    (when-let* ((prefix (org-confluence--keymap-prefix-vector)))
      (define-key map prefix #'org-confluence-dispatch))
    map)
  "Keymap used by `org-confluence-mode'.")

;;;###autoload
(defun org-confluence-mode-maybe ()
  "Enable `org-confluence-mode' for Org buffers with Confluence metadata."
  (when (org-confluence-buffer-p)
    (org-confluence-mode 1)))

;;;###autoload
(define-minor-mode org-confluence-mode
  "Enable Confluence integration for the current Org buffer.
This package-level mode registers the Confluence `org-comments' backend and,
when `org-confluence-enable-org-comments-mode' is non-nil, enables
`org-comments-mode' for local comment commands and overlays.  The mode does not
install personal leader, Evil, or layout-specific bindings."
  :lighter " Cfl"
  (cond
   (org-confluence-mode
    (unless (derived-mode-p 'org-mode)
      (setq org-confluence-mode nil)
      (user-error "org-confluence-mode only works in Org buffers"))
    (org-confluence-comments-backend-register)
    (when (null org-comments-resolve-account-id-function)
      (setq-local org-comments-resolve-account-id-function
		  #'org-confluence-people-resolve-account-id)
      (setq org-confluence--installed-account-id-resolver t))
    (when (and org-confluence-enable-org-comments-mode
	       (not (bound-and-true-p org-comments-mode)))
      (org-comments-mode 1)
      (setq org-confluence--enabled-org-comments-mode t))
    (add-hook 'after-save-hook #'org-confluence-refresh-empty-page-notice nil t)
    (org-confluence-refresh-empty-page-notice)
    (setq org-confluence-sync-status-refresh-source-marker-function
	  #'org-confluence-sync-status-refresh-source-marker-in-buffer)
    (org-confluence-sync-status-source-marker-setup))
   (t
    (when org-confluence--installed-account-id-resolver
      (kill-local-variable 'org-comments-resolve-account-id-function)
      (setq org-confluence--installed-account-id-resolver nil))
    (remove-hook 'after-save-hook #'org-confluence-refresh-empty-page-notice t)
    (org-confluence--delete-empty-page-notice)
    (org-confluence-sync-status-source-marker-teardown)
    (when (and org-confluence--enabled-org-comments-mode
	       (bound-and-true-p org-comments-mode))
      (org-comments-mode -1))
    (setq org-confluence--enabled-org-comments-mode nil))))

(defun org-confluence-remote--url-title (url)
  "Return a Confluence-ish title from URL."
  (when (string-match "/pages/[0-9]+/\\([^?#]+\\)" url)
    (string-trim (url-unhex-string (replace-regexp-in-string "+" " " (match-string 1 url))))))

(defun org-confluence-remote-describe-url (url)
  "Return org-sync descriptor for Confluence URL, or nil."
  (let ((trimmed (string-trim url)))
    (when (string-match "/pages/\\([0-9]+\\)" trimmed)
      (list :kind "confluence"
	    :id (match-string 1 trimmed)
	    :url trimmed
	    :title (org-confluence-remote--url-title trimmed)
	    :supports '(:pull t :activity t :comments t)))))

(defun org-confluence-remote-pull (page-id local-path &rest options)
  "Pull Confluence PAGE-ID to LOCAL-PATH and return normalized metadata."
  (require 'org-confluence-sync)
  (let ((result (apply #'org-confluence-pull-to-file page-id local-path options)))
    (list :kind "confluence"
	  :id page-id
	  :local-path (plist-get result :file)
	  :baseline (list :version (plist-get result :remote-version)
			  :pulled-at (format-time-string "%FT%TZ" nil t))
	  :raw result)))

(defun org-confluence-remote-scan (page-id baseline &rest _options)
  "Scan Confluence PAGE-ID against BASELINE and return normalized activity."
  (require 'org-confluence-sync)
  (let* ((page (org-confluence-api--get-page page-id "storage"))
	 (remote-version (org-confluence-sync--page-version-value page))
	 (local-version (or (plist-get baseline :version)
			    (plist-get baseline :remote-version)))
	 (updated-at (or (alist-get 'createdAt (alist-get 'version page))
			 (alist-get 'created-at (alist-get 'version page))))
	 (author (or (alist-get 'displayName (alist-get 'author (alist-get 'version page)))
		     (alist-get 'publicName (alist-get 'author (alist-get 'version page)))
		     "unknown"))
	 (signals nil))
    (when (and remote-version local-version (> remote-version (string-to-number (format "%s" local-version))))
      (setq signals
	    (list (list :id (format "confluence-page:%s:v%s" page-id remote-version)
			:kind "remote-update"
			:remote-at (or updated-at "")
			:author author
			:summary (format "Remote page version %s is newer than local version %s."
					 remote-version local-version)))))
    (list :kind "confluence"
	  :id page-id
	  :remote (list :version remote-version :updated-at updated-at :author author)
	  :signals signals)))

(when (fboundp 'org-sync-remote-register-provider)
  (org-sync-remote-register-provider
   :kind "confluence"
   :describe-url #'org-confluence-remote-describe-url
   :pull #'org-confluence-remote-pull
   :scan #'org-confluence-remote-scan))

(provide 'org-confluence)
;;; org-confluence.el ends here
