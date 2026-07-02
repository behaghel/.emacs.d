;;; org-confluence-sync-status-render.el --- Sync status rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Rendering helpers for Confluence sync status reports and compact markers.

;;; Code:

(require 'button)
(require 'magit-section nil 'noerror)
(require 'org)
(require 'seq)
(require 'subr-x)

(require 'org-confluence-sync-status-cache)
(require 'org-confluence-sync-status-collect)

(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-section-body "magit-section")
(declare-function sync-actions "org-confluence-sync-status")
(declare-function sync-category "org-confluence-sync-status")
(declare-function sync-issue "org-confluence-sync-status")

(defface org-confluence-sync-status-clean-face
  '((t :inherit success :weight bold))
  "Face for clean Confluence sync status elements."
  :group 'org-confluence-api)

(defface org-confluence-sync-status-warning-face
  '((t :inherit warning :weight bold))
  "Face for warning Confluence sync status elements."
  :group 'org-confluence-api)

(defface org-confluence-sync-status-blocking-face
  '((t :inherit error :weight bold))
  "Face for blocking Confluence sync status elements."
  :group 'org-confluence-api)

(defface org-confluence-sync-status-heading-face
  '((t :inherit font-lock-function-name-face :weight bold :height 1.18))
  "Face for Confluence sync status report headings."
  :group 'org-confluence-api)

(defface org-confluence-sync-status-section-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for Confluence sync status section names."
  :group 'org-confluence-api)

(defface org-confluence-sync-status-field-face
  '((t :inherit font-lock-keyword-face :weight semi-bold))
  "Face for Confluence sync status metadata fields."
  :group 'org-confluence-api)

(defface org-confluence-sync-status-value-face
  '((t :inherit font-lock-string-face))
  "Face for Confluence sync status metadata values."
  :group 'org-confluence-api)

(defface org-confluence-sync-status-muted-face
  '((t :inherit shadow :slant italic))
  "Face for subdued Confluence sync status report text."
  :group 'org-confluence-api)

(defun org-confluence-sync-status--state-symbol (state)
  "Return display symbol for sync STATE."
  (pcase state
    ("clean" "✓")
    ("warning" "⚠")
    ("blocking" "×")
    (_ "?")))

(defun org-confluence-sync-status--state-face (state)
  "Return face for sync STATE."
  (pcase state
    ("clean" 'org-confluence-sync-status-clean-face)
    ("warning" 'org-confluence-sync-status-warning-face)
    ("blocking" 'org-confluence-sync-status-blocking-face)
    (_ 'org-confluence-sync-status-muted-face)))

(defun org-confluence-sync-status--face (text face)
  "Return TEXT using FACE for both display face properties.
Magit-section buffers primarily use `font-lock-face'.  Setting both `face' and
`font-lock-face' keeps this report visible across special-mode, magit-section,
and theme/font-lock combinations."
  (propertize text 'face face 'font-lock-face face))

(defun org-confluence-sync-status--state-chip (state)
  "Return propertized state symbol for STATE."
  (org-confluence-sync-status--face
   (org-confluence-sync-status--state-symbol state)
   (org-confluence-sync-status--state-face state)))

(defun org-confluence-sync-status-marker-string (&optional source-file)
  "Return compact Confluence sync marker string for SOURCE-FILE."
  (let* ((status (org-confluence-sync-status--cache-state source-file))
	 (state (or (org-confluence-sync-status--string-value
		     (alist-get 'state status))
		    "unknown"))
	 (summary (org-confluence-sync-status--string-value (alist-get 'summary status)))
	 (symbol (org-confluence-sync-status--state-symbol state)))
    (if (equal state "clean")
	(format "[Sync %s]" symbol)
      (format "[Sync %s %s]" symbol (or summary "unknown")))))

(defun org-confluence-sync-status--open-link-button (button)
  "Open the Org comment link stored on BUTTON."
  (org-link-open-from-string (button-get button 'org-confluence-link)))

(defun org-confluence-sync-status--link-label (link)
  "Return display label for Org comment LINK."
  (let ((link (org-confluence-sync-status--string-value link)))
    (if (and link (string-match "\\`\\[\\[[^]]+\\]\\[\\(.*\\)\\]\\]\\'" link))
	(match-string 1 link)
      "Open linked comment")))

(defun org-confluence-sync-status--insert-issue-link (link)
  "Insert clickable sync issue LINK when non-nil."
  (when-let* ((link (org-confluence-sync-status--string-value link)))
    (insert "\n  ↪ ")
    (insert-text-button (format "Open comment: %s"
				(org-confluence-sync-status--link-label link))
			'face 'link
			'follow-link t
			'action #'org-confluence-sync-status--open-link-button
			'org-confluence-link link
			'help-echo "Open linked Org comment")))

(defun org-confluence-sync-status--message-lines (issue)
  "Return display lines for ISSUE message."
  (let ((link (org-confluence-sync-status--string-value (alist-get 'link issue))))
    (seq-remove
     (lambda (line)
       (and link (string-prefix-p "sidecar: " line)))
     (split-string (or (org-confluence-sync-status--string-value
			(alist-get 'message issue))
		       "")
		   "; " t))))

(defun org-confluence-sync-status--insert-issue (issue)
  "Insert one sync ISSUE in a readable multi-line shape."
  (let* ((lines (org-confluence-sync-status--message-lines issue))
	 (summary (or (car lines) "Unknown issue"))
	 (details (cdr lines)))
    (insert (format "%s %s%s"
		    (pcase (alist-get 'severity issue)
		      ("blocking" "×")
		      ("warning" "⚠")
		      (_ "?"))
		    summary
		    (if-let* ((action (org-confluence-sync-status--string-value
				       (alist-get 'action issue))))
			(format " [%s]" action)
		      "")))
    (dolist (detail details)
      (insert (format "\n  · %s" detail)))
    (org-confluence-sync-status--insert-issue-link (alist-get 'link issue))
    (insert "\n")))

(defun org-confluence-sync-status--insert-section (title issues)
  "Insert sync status section TITLE containing ISSUES."
  (insert (format "* %s\n" title))
  (if issues
      (dolist (issue issues)
	(org-confluence-sync-status--insert-issue issue))
    (insert "✓ No issues.\n"))
  (insert "\n"))

(defun org-confluence-sync-status--section-shortcut (title)
  "Return jump shortcut label for section TITLE."
  (pcase title
    ("Page" "p")
    ("Comments" "c")
    ("Attachments" "a")
    ("People" "u")
    (_ nil)))

(defun org-confluence-sync-status--section-summary (title issues)
  "Return one-line summary for section TITLE and ISSUES."
  (let* ((state (if issues "warning" "clean"))
	 (face (org-confluence-sync-status--state-face state))
	 (shortcut (org-confluence-sync-status--section-shortcut title)))
    (concat
     (org-confluence-sync-status--face (if issues "⚠" "✓") face)
     " "
     (org-confluence-sync-status--face
      title 'org-confluence-sync-status-section-face)
     (when shortcut
       (org-confluence-sync-status--face
	(format " (%s)" shortcut) 'org-confluence-sync-status-muted-face))
     " "
     (if issues
	 (org-confluence-sync-status--face
	  (format "— %s" (org-confluence-sync-status--plural
			  (length issues) "issue"))
	  face)
       (org-confluence-sync-status--face
	"— clean" 'org-confluence-sync-status-clean-face)))))

(defun org-confluence-sync-status--render-magit-issue (issue)
  "Render ISSUE as a Magit section."
  (magit-insert-section (sync-issue issue)
			(let* ((lines (org-confluence-sync-status--message-lines issue))
			       (summary (or (car lines) "Unknown issue"))
			       (severity (alist-get 'severity issue))
			       (face (pcase severity
				       ("blocking" 'org-confluence-sync-status-blocking-face)
				       ("warning" 'org-confluence-sync-status-warning-face)
				       (_ 'org-confluence-sync-status-muted-face))))
			  (insert (format "  %s %s%s\n"
					  (org-confluence-sync-status--face
					   (pcase severity
					     ("blocking" "×")
					     ("warning" "⚠")
					     (_ "?"))
					   face)
					  summary
					  (if-let* ((action (org-confluence-sync-status--string-value
							     (alist-get 'action issue))))
					      (org-confluence-sync-status--face
					       (format "  [%s]" action)
					       'org-confluence-sync-status-muted-face)
					    ""))))
			(magit-insert-section-body
			 (dolist (detail (cdr (org-confluence-sync-status--message-lines issue)))
			   (insert (format "    %s %s\n"
					   (org-confluence-sync-status--face
					    "·" 'org-confluence-sync-status-muted-face)
					   detail)))
			 (when-let* ((link (org-confluence-sync-status--string-value
					    (alist-get 'link issue))))
			   (insert "    ↪ ")
			   (insert-text-button (format "Open comment: %s"
						       (org-confluence-sync-status--link-label link))
					       'face 'link
					       'font-lock-face 'link
					       'follow-link t
					       'action #'org-confluence-sync-status--open-link-button
					       'org-confluence-link link
					       'help-echo "Open linked Org comment")
			   (insert "\n")))))

(defun org-confluence-sync-status--render-magit-section (title issues)
  "Render TITLE and ISSUES as a collapsible Magit section."
  (magit-insert-section (sync-category title (not issues))
			(insert (org-confluence-sync-status--section-summary title issues) "\n")
			(magit-insert-section-body
			 (if issues
			     (dolist (issue issues)
			       (org-confluence-sync-status--render-magit-issue issue))
			   (insert "  " (org-confluence-sync-status--face
					 "✓" 'org-confluence-sync-status-clean-face)
				   " " (org-confluence-sync-status--face
					"No issues." 'org-confluence-sync-status-muted-face) "\n")))))

(defun org-confluence-sync-status--insert-metadata (field value)
  "Insert metadata FIELD and VALUE with distinct faces."
  (insert (org-confluence-sync-status--face
	   (concat field ": ") 'org-confluence-sync-status-field-face)
	  (org-confluence-sync-status--face
	   (or value "<none>") 'org-confluence-sync-status-value-face)
	  "\n"))

(defun org-confluence-sync-status--insert-action-help (text)
  "Insert subdued action help TEXT."
  (insert (org-confluence-sync-status--face
	   text 'org-confluence-sync-status-muted-face)))

(defun org-confluence-sync-status-render (status source-buffer)
  "Render STATUS for SOURCE-BUFFER in the current buffer."
  (let* ((state (or (org-confluence-sync-status--string-value
		     (alist-get 'state status))
		    "unknown"))
	 (issues (append (alist-get 'issues status) nil))
	 (page (seq-filter (lambda (issue) (equal "page" (alist-get 'category issue))) issues))
	 (comments (seq-filter (lambda (issue) (equal "comment" (alist-get 'category issue))) issues))
	 (attachments (seq-filter (lambda (issue) (equal "attachment" (alist-get 'category issue))) issues))
	 (people (seq-filter (lambda (issue) (equal "people" (alist-get 'category issue))) issues)))
    (insert (org-confluence-sync-status--face
	     "Org ↔ Confluence Sync Status"
	     'org-confluence-sync-status-heading-face) "\n")
    (org-confluence-sync-status--insert-metadata
     "Source" (or (buffer-file-name source-buffer) "<none>"))
    (org-confluence-sync-status--insert-metadata
     "Page" (or (alist-get 'pageId status) "<none>"))
    (org-confluence-sync-status--insert-metadata
     "Checked" (or (alist-get 'checkedAt status) "<unknown>"))
    (insert (org-confluence-sync-status--face
	     "Overall: " 'org-confluence-sync-status-field-face)
	    (org-confluence-sync-status--face
	     "Sync " 'org-confluence-sync-status-value-face)
	    (org-confluence-sync-status--state-chip state))
    (when-let* ((summary (org-confluence-sync-status--string-value
			  (alist-get 'summary status)))
		(_ (not (string-empty-p summary))))
      (insert (org-confluence-sync-status--face
	       (format " — %s" summary)
	       (org-confluence-sync-status--state-face state))))
    (insert "\n\n")
    (org-confluence-sync-status--render-magit-section "Page" page)
    (org-confluence-sync-status--render-magit-section "Comments" comments)
    (org-confluence-sync-status--render-magit-section "Attachments" attachments)
    (org-confluence-sync-status--render-magit-section "People" people)
    (insert "\n\n")
    (magit-insert-section (sync-actions nil t)
			  (org-confluence-sync-status--insert-action-help
			   "Actions  (? opens action menu)\n")
			  (magit-insert-section-body
			   (org-confluence-sync-status--insert-action-help
			    "  Content: g refresh · y sync page · f fetch page · p publish · ! force publish · o open page\n")
			   (org-confluence-sync-status--insert-action-help
			    "  Comments: Y sync page+comments · F fetch page+comments · i import · C sidecar · L list remote\n")
			   (org-confluence-sync-status--insert-action-help
			    "  At point/tools: RET open · x delete local when recommended · a reanchor · A repair anchors · u resolve people · q quit\n")))))

(provide 'org-confluence-sync-status-render)
;;; org-confluence-sync-status-render.el ends here
