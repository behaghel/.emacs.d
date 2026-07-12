;;; google-docs-themes.el --- Google Docs style themes -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme registry and token resolution for Google Docs publishing styles.
;; Visual policy is organized as palette tokens and semantic roles so concrete
;; style definitions can be derived without scattering literal colors through
;; the renderer bridge.

;;; Code:

(require 'subr-x)

(defgroup hub/org-google-docs-themes nil
  "Google Docs publishing theme policy."
  :group 'org)

(defcustom hub/org-google-docs-active-theme 'legacy-neutral
  "Default Google Docs publishing theme.
Documents may override this with `#+GDOCS_THEME: THEME-ID'."
  :type 'symbol
  :group 'hub/org-google-docs-themes)

(defconst hub/org-google-docs-theme-registry
  '((legacy-neutral
     :palette ((neutral-plain . "#ffffff")
	       (neutral-subtle . "#f5f5f5")
	       (neutral-muted . "#666666"))
     :roles ((body-font . "Inter")
	     (body-text . neutral-plain)
	     (heading-font . "Inter")
	     (heading-brand . neutral-plain)
	     (heading-text . neutral-plain)
	     (caption-text . neutral-muted)
	     (code-font . "Roboto Mono")
	     (code-surface . neutral-subtle)
	     (quote-surface . neutral-subtle)
	     (quote-indent-start . 18)
	     (quote-border-padding . 6)
	     (callout-surface . neutral-subtle)
	     (callout-border-padding . 6))))
  "Registered Google Docs publishing themes.
Each entry is (THEME-ID . PLIST).  Theme plists may contain :extends,
:palette, and :roles.  Palette entries map token symbols to hex strings; role
entries map semantic role symbols to palette token symbols or literal values.")

(defun hub/org-google-docs-theme--plist-merge (parent child)
  "Return PARENT plist overridden by CHILD plist."
  (let ((result (copy-sequence parent)))
    (while child
      (setq result (plist-put result (pop child) (pop child))))
    result))

(defun hub/org-google-docs-theme--alist-merge (parent child)
  "Return PARENT alist overridden by CHILD alist keys."
  (let ((result (copy-sequence parent)))
    (dolist (entry child)
      (setq result (assq-delete-all (car entry) result))
      (push entry result))
    (nreverse result)))

(defun hub/org-google-docs-theme--definition (theme-id)
  "Return raw theme definition for THEME-ID, or nil."
  (cdr (assq theme-id hub/org-google-docs-theme-registry)))

(defun hub/org-google-docs-theme-resolve (theme-id &optional seen)
  "Return fully resolved theme for THEME-ID.
Signals `user-error' for unknown themes or inheritance cycles."
  (let ((theme (hub/org-google-docs-theme--definition theme-id)))
    (unless theme
      (user-error "Unknown Google Docs theme `%s'; valid themes: %s"
		  theme-id
		  (mapconcat (lambda (entry) (symbol-name (car entry)))
			     hub/org-google-docs-theme-registry ", ")))
    (when (memq theme-id seen)
      (user-error "Cyclic Google Docs theme inheritance involving `%s'" theme-id))
    (if-let* ((parent-id (plist-get theme :extends)))
	(let* ((parent (hub/org-google-docs-theme-resolve parent-id
							  (cons theme-id seen)))
	       (palette (hub/org-google-docs-theme--alist-merge
			 (plist-get parent :palette)
			 (plist-get theme :palette)))
	       (roles (hub/org-google-docs-theme--alist-merge
		       (plist-get parent :roles)
		       (plist-get theme :roles))))
	  (hub/org-google-docs-theme--plist-merge
	   parent
	   (list :id theme-id :palette palette :roles roles)))
      (hub/org-google-docs-theme--plist-merge theme (list :id theme-id)))))

(defun hub/org-google-docs-theme-role (theme role)
  "Return ROLE value from resolved THEME."
  (alist-get role (plist-get theme :roles)))

(defun hub/org-google-docs-theme-color (theme role)
  "Return the color value for ROLE in resolved THEME.
If the role value is a palette token, resolve it through THEME's palette;
otherwise return the literal role value."
  (let* ((value (hub/org-google-docs-theme-role theme role))
	 (palette (plist-get theme :palette)))
    (or (and (symbolp value) (alist-get value palette))
	value)))

(defun hub/org-google-docs--buffer-theme-keywords ()
  "Return all `#+GDOCS_THEME:' values in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (values)
      (while (re-search-forward "^[	]*#\\+GDOCS_THEME:[	]*\\(.+?\\)[	]*$" nil t)
	(push (string-trim (match-string 1)) values))
      (nreverse values))))

(defun hub/org-google-docs-buffer-theme-id ()
  "Return the current buffer's Google Docs theme id.
If no `#+GDOCS_THEME:' keyword is present, return
`hub/org-google-docs-active-theme'.  Duplicate keywords or unknown themes signal
`user-error'."
  (let ((values (hub/org-google-docs--buffer-theme-keywords)))
    (when (> (length values) 1)
      (user-error "Duplicate #+GDOCS_THEME keywords"))
    (let ((theme-id (if values
			(intern (car values))
		      hub/org-google-docs-active-theme)))
      (hub/org-google-docs-theme-resolve theme-id)
      theme-id)))

(defun hub/org-google-docs-current-theme (&optional theme-id)
  "Return resolved Google Docs theme for THEME-ID or current buffer/default."
  (hub/org-google-docs-theme-resolve
   (or theme-id (hub/org-google-docs-buffer-theme-id))))

(provide 'org/google-docs-themes)
;;; google-docs-themes.el ends here
