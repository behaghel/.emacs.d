;;; hub-denote-test.el --- Tests for Denote helper predicates -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for shared Denote file hygiene helpers.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "lisp" default-directory))

(require 'hub-denote)

(ert-deftest hub/denote-source-note-file-p-accepts-canonical-org-note ()
  "Canonical Denote Org source notes are accepted."
  (should (hub/denote-source-note-file-p
	   "/notes/20260426T085331--bâtisseurs-de-paradis__faith.org")))

(ert-deftest hub/denote-source-note-file-p-rejects-generated-siblings ()
  "Generated, sidecar, hidden, and recovery files are rejected."
  (dolist (path '("/notes/20260426T085331--bâtisseurs-de-paradis__faith.tex"
		  "/notes/20260426T085331--bâtisseurs-de-paradis__faith.comments.org"
		  "/notes/20260426T085331--bâtisseurs-de-paradis__faith.org.lost-20260614"
		  "/notes/.20260426T085331--bâtisseurs-de-paradis__faith.org.~undo-tree~"
		  "/notes/.devenv/20260426T085331--bâtisseurs-de-paradis__faith.org"
		  "/notes/journal.org"))
    (should-not (hub/denote-source-note-file-p path))))

(provide 'hub-denote-test)
;;; hub-denote-test.el ends here
