;;; hub-denote.el --- Shared Denote file helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight predicates for code that needs Denote file hygiene without
;; loading the full notes configuration.

;;; Code:

(require 'hub-noise)
(require 'seq)
(require 'subr-x)

(defgroup hub/notes nil
  "Notes configuration and shared Denote helpers."
  :group 'convenience)

(defcustom hub/denote-directory (expand-file-name "~/ws/blog.behaghel.org/content-org/journal/")
  "Default directory for Denote notes."
  :type 'directory
  :group 'hub/notes)

(defcustom hub/denote-work-directory (expand-file-name "~/ws/veriff/my-docs/")
  "Directory for work Denote notes."
  :type 'directory
  :group 'hub/notes)

(defconst hub/denote--source-note-regexp
  (rx string-start
      (= 8 digit) "T" (= 6 digit)
      (? "==" (+ (not (any "/" "."))))
      "--" (+ nonl)
      ".org"
      string-end)
  "Regexp matching canonical Denote Org source note filenames.")

(defun hub/denote--hidden-path-component-p (path)
  "Return non-nil when PATH contains a hidden file or directory component."
  (seq-some (lambda (component)
	      (and (not (member component '("" "." "..")))
		   (string-prefix-p "." component)))
	    (split-string (expand-file-name path) "/")))

(defun hub/denote-source-note-file-p (path)
  "Return non-nil when PATH is a canonical Denote Org source note.

This excludes generated or auxiliary siblings such as comment sidecars,
exports, recovery files, hidden files, caches, and files with extensions other
than exactly `.org'.  It intentionally does not check whether the note has a
readable title; consumers such as dashboards can layer that display policy on
top."
  (and (stringp path)
       (not (hub/denote--hidden-path-component-p path))
       (not (hub/noise-file-p path))
       (string-match-p hub/denote--source-note-regexp
		       (file-name-nondirectory path))))

(provide 'hub-denote)
;;; hub-denote.el ends here
