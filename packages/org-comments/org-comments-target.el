;;; org-comments-target.el --- Target helpers for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Target text normalization, hashing, and source-position helpers for the
;; initial Org comments extraction.  Names remain in the legacy
;; `org-comments-*' namespace until the public API migration slice.

;;; Code:

(require 'subr-x)

(defun org-comments-normalize-target-text (text)
  "Return TEXT trimmed with whitespace runs collapsed to one space."
  (string-trim (replace-regexp-in-string "[[:space:]]+" " " text)))

(defun org-comments-target-hash (target-text)
  "Return sha256 hash string for normalized TARGET-TEXT."
  (concat "sha256:" (secure-hash 'sha256 (org-comments-normalize-target-text target-text))))

(defun org-comments--line-column-at (position)
  "Return cons cell of one-based line and zero-based column at POSITION."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos position t) (current-column))))

(provide 'org-comments-target)
;;; org-comments-target.el ends here
