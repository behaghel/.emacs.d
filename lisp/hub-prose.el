;;; hub-prose.el --- Prose editing display helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared helpers for prose buffers that should display as visually filled
;; without inserting hard line breaks into the file.

;;; Code:

(require 'hub-utils)

(defgroup hub/prose nil
  "Customizations for prose editing display."
  :group 'editing)

(defcustom hub/prose-visual-fill-column 90
  "Visual fill width for prose buffers."
  :type 'integer
  :group 'hub/prose)

(defcustom hub/prose-org-table-horizontal-scroll t
  "Whether Org tables should scroll horizontally instead of visually wrapping.
This uses `org-phscroll-mode' when the optional phscroll package is available."
  :type 'boolean
  :group 'hub/prose)

(defcustom hub/prose-org-sync-indent-prefixes t
  "Whether prose wrapping should synchronously initialize Org indent prefixes.
Org normally initializes `org-indent-mode' prefixes lazily.  Visual wrapping can
be displayed before that lazy pass has happened, causing continuation lines to
ignore Org section indentation."
  :type 'boolean
  :group 'hub/prose)

(defun hub/prose--org-indented-p ()
  "Return non-nil when the current buffer uses Org virtual indentation."
  (and (derived-mode-p 'org-mode)
       (bound-and-true-p org-indent-mode)))

(defun hub/prose--enable-adaptive-wrap-p ()
  "Return non-nil when adaptive wrap should manage continuation prefixes."
  (not (hub/prose--org-indented-p)))

(defun hub/prose--enable-org-table-horizontal-scroll ()
  "Enable horizontal scrolling for Org tables when available."
  (when (and hub/prose-org-table-horizontal-scroll
	     (derived-mode-p 'org-mode)
	     (require 'org-phscroll nil t))
    (org-phscroll-mode 1)))

(defun hub/prose--ensure-org-indent-prefixes ()
  "Synchronously initialize Org indentation prefixes when needed."
  (when (and hub/prose-org-sync-indent-prefixes
	     (hub/prose--org-indented-p)
	     (fboundp 'org-indent-add-properties))
    (save-restriction
      (widen)
      (org-indent-add-properties (point-min) (point-max)))))

(defun hub/prose-visual-fill-mode ()
  "Enable virtual autofill for the current prose buffer.

This disables hard autofill, enables word wrapping, and uses
`adaptive-wrap-prefix-mode' and `visual-fill-column-mode' when
available so wrapping happens at `hub/prose-visual-fill-column'
without changing the file contents."
  (auto-fill-mode -1)
  (visual-line-mode 1)
  (setq-local comment-auto-fill-only-comments nil
	      truncate-lines nil
	      word-wrap t)
  (if (and (hub/prose--enable-adaptive-wrap-p)
	   (require 'adaptive-wrap nil t))
      (progn
	(setq-local adaptive-wrap-extra-indent 0)
	(adaptive-wrap-prefix-mode 1))
    (when (bound-and-true-p adaptive-wrap-prefix-mode)
      (adaptive-wrap-prefix-mode -1)))
  (hub/prose--ensure-org-indent-prefixes)
  (hub/prose--enable-org-table-horizontal-scroll)
  (when (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-width hub/prose-visual-fill-column
		visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

(provide 'hub-prose)
;;; hub-prose.el ends here
