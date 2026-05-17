;;; denote-note-test.el --- Denote note editing defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression test for Denote note creation behavior.

;;; Code:

(require 'ert)
(require 'seq)

(let ((user-emacs-directory (file-name-as-directory default-directory)))
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/interactive" user-emacs-directory))
  (require 'core-packages)
  (require 'editing/evil)
  (require 'org/core)
  (require 'notes/brain))

(ert-deftest hub/denote-note-keeps-org-editing-experience ()
  "A new Denote note opens as Org and preserves key editing behaviors."
  (require 'denote)
  (let* ((tmp-dir (make-temp-file "hub-denote-test-" t))
	 (denote-directory tmp-dir)
	 (path (denote "ERT denote note" '("emacs") nil denote-directory nil nil nil nil)))
    (unwind-protect
	(with-current-buffer (find-file-noselect path)
	  (should (derived-mode-p 'org-mode))
	  (should (featurep 'org-tempo))
	  (should-not
	   (let* ((keys (append (mapcar #'car org-structure-template-alist)
				(mapcar #'car org-tempo-keywords-alist)))
		  (unique-keys (seq-uniq keys #'equal)))
	     (/= (length keys) (length unique-keys))))
	  (should (equal (alist-get "c" org-structure-template-alist nil nil #'equal)
			 "comment"))
	  (dolist (template '(("sf" . "standfirst")
			      ("ep" . "epigraph")
			      ("pq" . "pullquote")
			      ("co" . "callout")
			      ("me" . "metrics")
			      ("mi" . "metric")
			      ("pi" . "pillars")
			      ("pa" . "pillar")
			      ("gr" . "graph")))
	    (should (equal (alist-get (car template) org-structure-template-alist nil nil #'equal)
			   (cdr template))))
	  (evil-insert-state)
	  (should (eq (key-binding (kbd "<escape>")) 'evil-normal-state))
	  (erase-buffer)
	  (insert "<c")
	  (org-cycle)
	  (goto-char (point-min))
	  (should (search-forward "#+begin_comment" nil t))
	  (erase-buffer)
	  (insert "<ep")
	  (org-cycle)
	  (goto-char (point-min))
	  (should (search-forward "#+begin_epigraph" nil t)))
      (ignore-errors
	(when-let* ((buf (find-buffer-visiting path)))
	  (kill-buffer buf))
	(when (file-exists-p path)
	  (delete-file path))
	(when (file-directory-p tmp-dir)
	  (delete-directory tmp-dir t))))))

(ert-deftest hub/denote-brain-configures-note-directory ()
  "Loading the notes module configures the Denote note directory."
  (load (expand-file-name "modules/interactive/notes/brain.el" default-directory) nil t)
  (should (fboundp 'denote))
  (should (equal denote-directory hub/denote-directory)))

(ert-deftest hub/org-tab-advances-active-yasnippet-field ()
  "Org TAB should move through Yasnippet fields before cycling headings."
  (unless (require 'yasnippet nil 'noerror)
    (ert-skip "Yasnippet is unavailable in this isolated test environment"))
  (with-temp-buffer
    (org-mode)
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:first} ${2:second} $0")
    (let ((before (point)))
      (hub/org-tab-dwim)
      (should (> (point) before)))))

(ert-deftest hub/org-veriff-template-inserts-authoring-scaffold ()
  "The Veriff authoring template inserts required export metadata."
  (with-temp-buffer
    (org-mode)
    (hub/org-insert-veriff-template)
    (goto-char (point-min))
    (should (search-forward "#+LATEX_CLASS: veriff" nil t))
    (should (search-forward "#+LATEX_VARIANT:" nil t))
    (should (search-forward "#+EXPORT_EYEBROW:" nil t))
    (should (search-forward "#+EXPORT_FOOTER_NOTE:" nil t))
    (should (search-forward "#+begin_standfirst" nil t))
    (should (search-forward "#+begin_epigraph" nil t))))

(ert-deftest hub/org-generic-template-keeps-metadata-fields ()
  "The generic Org template keeps metadata in Yasnippet field order."
  (let ((template (with-temp-buffer
		    (insert-file-contents
		     (expand-file-name "insert/template.org" default-directory))
		    (buffer-string))))
    (dolist (fragment '("#+TITLE: $1"
			"#+SUBTITLE: ${2:}"
			"#+DATE: ${3:`(format-time-string \"%Y-%m-%d\")`}"
			"#+EXPORT_EYEBROW: ${4:}"
			"#+EXPORT_FOOTER_NOTE: ${5:}"
			"$0"))
      (should (string-match-p (regexp-quote fragment) template)))))

(provide 'denote-note-test)
;;; denote-note-test.el ends here
