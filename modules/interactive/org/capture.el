;;; capture.el --- Org mode: capture and refile configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture templates and refile behavior for Org mode.

;;; Code:

(require 'hub-utils)

(defcustom hub/org-veriff-template-file
  (expand-file-name "insert/template.veriff.org" user-emacs-directory)
  "Yasnippet-compatible template used for new Veriff Org articles."
  :type 'file
  :group 'hub/org)

(defun hub/org-insert-veriff-template ()
  "Insert the Veriff article template at point.
When Yasnippet is available, expand fields in the inserted template."
  (interactive)
  (let ((template (with-temp-buffer
		    (insert-file-contents hub/org-veriff-template-file)
		    (buffer-string))))
    (if (fboundp 'yas-expand-snippet)
	(yas-expand-snippet template)
      (insert template))))

(defun hub/org-setup-capture ()
  "Configure Org capture templates and refile targets."
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (require 'org-protocol)
  (setq org-capture-templates
	'(("i" "inbox" entry (file org-default-notes-file) "* TODO %?" :prepend t)
	  ("f" "follow-up" entry (file org-default-notes-file) "* TODO %? %a\n  %i" :prepend t)
	  ("r" "respond to email (mu4e)" entry (file org-default-notes-file)
	   "* TODO REPLY to [[mailto:%:fromaddress][%:fromname]] on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%U\n\n"
	   :immediate-finish t :prepend t)
	  ("l" "link" entry (file+headline org-default-notes-file "Browsing")
	   "* TODO %(org-cliplink-capture)" :immediate-finish t :prepend t)
	  ("c" "org-protocol-capture" entry (file+headline org-default-notes-file "Browsing")
	   "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t :prepend t)
	  ("p" "Philosophy" entry (file "faith.org") "* %?\nEntered on %U\n  %i\n  %a")
	  ("m" "Meeting Minutes" entry (file org-default-notes-file)
	   "* Meeting Minutes\n** Present at meeting\n- [X] Peter\n- [ ] Sarah - [X] Lucy\n ** Agenda\n- item 1\n- item 2\n- item 3\n** Notes\n*** Last meeting minutes are approved                              :decision:\n*** Discussion\n**** TODO Topic 1                                      :@Fred:\n**** TODO Topic 2                                    :@Sara:\n**** DONE Topic 2.1                                      :@Lucy:@Ted:\nDEADLINE: <2020-03-01 So>\n**** Another sub-topic                                    :decision:\n* Actions\n#+BEGIN: columnview :id global :match \"/TODO|DONE\" :format \"%ITEM(What) %TAGS(Who) %DEADLINE(When) %TODO(State)\"\n#+END:\n\n* Decisions\n#+BEGIN: columnview :id global :match \"decision\" :format \"%ITEM(decisions)\"\n#+END:")))

  (require 'tools/blog)
  (setq org-outline-path-complete-in-steps nil
	org-refile-use-outline-path 'file
	org-refile-allow-creating-parent-nodes 'confirm
	org-reverse-note-order t
	org-refile-targets '(("veriff.org" :maxlevel . 4)
			     ("faith.org" :maxlevel . 2)
			     ("hubert.org" :maxlevel . 2)
			     ("family.org" :maxlevel . 2))))

(provide 'org/capture)
;;; capture.el ends here
