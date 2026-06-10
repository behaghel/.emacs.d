;;; treesit.el --- Generic Tree-sitter development defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic Tree-sitter defaults and diagnostics.  Language-specific grammar
;; sources and major-mode remaps live under modules/lang/*.

;;; Code:

(require 'hub-utils)
(require 'seq)

(defvar hub/treesit--configured nil
  "Non-nil once generic Tree-sitter defaults have been configured.")

(defun hub/treesit-configure-for-prog-buffer ()
  "Configure generic Tree-sitter defaults for programming buffers."
  (when (and (not hub/treesit--configured)
	     (require 'treesit nil t))
    (setq hub/treesit--configured t)
    (setq treesit-font-lock-level 4
	  treesit-font-lock-settings t
	  treesit-simple-indent-rules t
	  treesit-defun-type-regexp t
	  treesit-defun-name-function t)
    (treesit-major-mode-setup)))

(add-hook 'prog-mode-hook #'hub/treesit-configure-for-prog-buffer)

(defun hub/treesit-install-missing (&optional langs)
  "Install missing Tree-sitter grammars for LANGS or known sources."
  (interactive)
  (unless (require 'treesit nil t)
    (user-error "treesit not available in this Emacs"))
  (let* ((known (mapcar #'car treesit-language-source-alist))
	 (targets (or langs known))
	 (missing (seq-filter (lambda (l) (not (treesit-language-available-p l))) targets)))
    (if (null missing)
	(message "All Tree-sitter grammars present: %s" targets)
      (dolist (lang missing)
	(condition-case err
	    (progn (message "Installing grammar: %s" lang)
		   (treesit-install-language-grammar lang))
	  (error (message "Failed installing %s: %S" lang err)))))))

(defun hub/treesit-report (&optional lang)
  "Report Tree-sitter setup; optionally focus on LANG."
  (interactive)
  (unless (require 'treesit nil t)
    (user-error "treesit not available in this Emacs"))
  (let* ((lang (or lang 'scala)))
    (message "treesit available: %s" (treesit-available-p))
    (message "extra load path: %S" treesit-extra-load-path)
    (message "source for %s: %S" lang (alist-get lang treesit-language-source-alist))
    (message "grammar installed for %s: %s" lang (treesit-language-available-p lang))))

(defcustom hub/treesit-auto-install t
  "When non-nil, automatically install missing Tree-sitter grammars on demand."
  :type 'boolean)

(defvar hub/treesit-mode-language-alist
  '((js-ts-mode . javascript)
    (typescriptreact-ts-mode . tsx))
  "Mapping from ts major modes to Tree-sitter language symbols for special cases.")

(defun hub/treesit-language-for-mode (mode)
  "Best-effort mapping from MODE (a symbol) to a Tree-sitter language symbol."
  (or (alist-get mode hub/treesit-mode-language-alist)
      (let* ((name (symbol-name mode)))
	(when (string-match "^\([^-]+\)-ts-mode$" name)
	  (intern (match-string 1 name))))))

(defun hub/treesit-ensure-for-current-mode ()
  "Ensure Tree-sitter grammar for current ts-mode is installed, lazily."
  (when (and hub/treesit-auto-install (not noninteractive) (featurep 'treesit))
    (let* ((lang (hub/treesit-language-for-mode major-mode))
	   (src (and lang (alist-get lang treesit-language-source-alist))))
      (when (and lang src (not (treesit-language-available-p lang)))
	(condition-case err
	    (progn
	      (message "Installing missing Tree-sitter grammar: %s" lang)
	      (treesit-install-language-grammar lang)
	      (message "Installed grammar: %s" lang))
	  (error (message "Failed installing grammar %s: %S" lang err)))))))

(add-hook 'after-change-major-mode-hook #'hub/treesit-ensure-for-current-mode)

(provide 'dev/treesit)
;;; treesit.el ends here
