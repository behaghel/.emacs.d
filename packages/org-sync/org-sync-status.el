;;; org-sync-status.el --- Org sync status panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic bottom-panel status UI for provider-neutral Org sync state.

;;; Code:

(require 'context-panels)
(require 'magit-section)
(require 'org)
(require 'org-sync-provider)

(defgroup org-sync nil
  "Provider-neutral Org synchronization status."
  :group 'org)

(defcustom org-sync-status-buffer-name "*Org Sync*"
  "Buffer name used for Org sync status bottom panels."
  :type 'string
  :group 'org-sync)

(defcustom org-sync-status-height 14
  "Window height for Org sync status bottom panels."
  :type 'natnum
  :group 'org-sync)

(defvar-local org-sync-current-document nil
  "Detected org-sync document descriptor for the current source buffer.")

(defvar-local org-sync-status-source-buffer nil
  "Source buffer associated with the current Org sync status panel.")

(defvar org-sync-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'org-sync-close)
    map)
  "Keymap for `org-sync-status-mode'.")

(define-derived-mode org-sync-status-mode special-mode "Org Sync"
  "Major mode for Org sync status bottom panels.")

(defun org-sync--domain-statuses ()
  "Return initial v1 domain statuses for an unknown tracking state."
  '((content . unknown)
    (comments . unknown)))

(defun org-sync--document-label (document)
  "Return display label for DOCUMENT."
  (format "%s %s"
	  (plist-get document :kind)
	  (or (plist-get document :remote-id)
	      (plist-get document :id)
	      "<unknown>")))

(defun org-sync-status-render (source-buffer _view)
  "Render Org sync status for SOURCE-BUFFER."
  (setq org-sync-status-source-buffer source-buffer)
  (let ((document (with-current-buffer source-buffer
		    (or org-sync-current-document
			(setq org-sync-current-document
			      (org-sync-detect-document source-buffer))))))
    (insert (format "Org Sync: %s\n" (org-sync--document-label document)))
    (insert "\nUnknown\n")
    (dolist (entry (org-sync--domain-statuses))
      (insert (format "  %-8s %s\n"
		      (capitalize (symbol-name (car entry)))
		      (cdr entry))))
    (insert "\nActions: g refresh, f fetch, p push, F pull, B baseline, R reset, ? help, q close\n")))

(defun org-sync--bottom-views (_source-buffer)
  "Return Org sync bottom view descriptors."
  (list (list :id 'org-sync-status
	      :label "Org Sync"
	      :buffer-name org-sync-status-buffer-name
	      :height org-sync-status-height
	      :mode #'org-sync-status-mode
	      :render #'org-sync-status-render)))

(defun org-sync-context-panel-provider ()
  "Return context-panels provider descriptor for Org sync."
  (list :name 'org-sync
	:icon "⇅"
	:collect-bottom-views #'org-sync--bottom-views
	:priority 20))

;;;###autoload
(define-minor-mode org-sync-mode
  "Enable provider-neutral Org sync status integration."
  :lighter " Sync"
  (if org-sync-mode
      (progn
	(setq org-sync-current-document (org-sync-detect-document (current-buffer)))
	(context-panels-mode 1)
	(context-panels-register-provider (org-sync-context-panel-provider)))
    (context-panels-unregister-provider 'org-sync)
    (setq org-sync-current-document nil)))

;;;###autoload
(defun org-sync-status ()
  "Open the Org sync status bottom panel for the current source buffer."
  (interactive)
  (unless org-sync-mode
    (org-sync-mode 1))
  (context-panels-open-bottom-view 'org-sync-status (current-buffer) t))

;;;###autoload
(defun org-sync-close ()
  "Close the current Org sync status bottom panel."
  (interactive)
  (context-panels-close-bottom-view))

(provide 'org-sync-status)
;;; org-sync-status.el ends here
