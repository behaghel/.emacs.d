;;; org-copilot-llm.el --- LLM adapter protocol for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Adapter-neutral review request construction for Org Copilot.  The core
;; package calls `org-copilot-review-function' and installs the normalized AI
;; comments it returns.  Concrete adapters such as gptel live in separate files.

;;; Code:

(require 'org)
(require 'org-copilot-context-panel)
(require 'org-copilot-session)

(defcustom org-copilot-review-function nil
  "Function used to request an AI review.
The function receives one plist request and returns a list of normalized or
normalizable AI comment plists.  Adapter packages set this variable."
  :type '(choice (const :tag "No adapter" nil)
		 function)
  :group 'org-copilot)

(defun org-copilot--region-active-p ()
  "Return non-nil when the current buffer has an active region."
  (and mark-active
       (mark t)))

(defun org-copilot--subtree-bounds ()
  "Return current Org subtree bounds, or whole buffer bounds outside headings."
  (save-excursion
    (if (org-before-first-heading-p)
	(cons (point-min) (point-max))
      (org-back-to-heading t)
      (let ((start (point)))
	(org-end-of-subtree t t)
	(cons start (point))))))

(defun org-copilot-review-bounds-dwim ()
  "Return review bounds as a plist for the active region or current subtree."
  (if (org-copilot--region-active-p)
      (let ((start (region-beginning))
	    (end (region-end)))
	(list :scope 'region :start start :end end))
    (pcase-let ((`(,start . ,end) (org-copilot--subtree-bounds)))
      (list :scope 'subtree :start start :end end))))

(defun org-copilot-review-request-dwim ()
  "Return a review request plist for the current Org buffer context."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org Copilot review needs an Org source buffer"))
  (let* ((bounds (org-copilot-review-bounds-dwim))
	 (start (plist-get bounds :start))
	 (end (plist-get bounds :end)))
    (append bounds
	    (list :source-buffer (current-buffer)
		  :buffer-name (buffer-name)
		  :text (buffer-substring-no-properties start end)))))

(defun org-copilot--request-review (request)
  "Request AI review for REQUEST through `org-copilot-review-function'."
  (unless org-copilot-review-function
    (user-error "No Org Copilot review adapter configured"))
  (funcall org-copilot-review-function request))

(defun org-copilot-install-review-comments (comments)
  "Install normalized review COMMENTS into the current Org Copilot session."
  (dolist (comment comments)
    (org-copilot-add-comment comment))
  (org-copilot-mode 1)
  (when (fboundp 'org-copilot-refresh-overlays)
    (org-copilot-refresh-overlays))
  (org-copilot-comments))

;;;###autoload
(defun org-copilot-review-dwim ()
  "Review the active region or current Org subtree with the configured adapter."
  (interactive)
  (let* ((request (org-copilot-review-request-dwim))
	 (comments (org-copilot--request-review request)))
    (org-copilot-install-review-comments comments)))

(provide 'org-copilot-llm)
;;; org-copilot-llm.el ends here
