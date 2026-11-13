;;; org-copilot-demo.el --- Scripted offline Copilot walkthrough -*- lexical-binding: t; -*-

;;; Commentary:
;; No model or authentication is configured.  A small scripted adapter queues
;; replies for explicit delivery through the real optional adapter's parser and
;; artifact installer.  The fixture is loaded only by the standalone launcher.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org-copilot)
(require 'org-copilot-gptel)
(require 'org-suggestions)

(defvar org-copilot-demo--pending nil
  "FIFO queue of captured requests and scripted JSON replies.")

(defvar org-copilot-demo--counter 0
  "Counter for distinct suggestion identifiers in this disposable session.")

(defun org-copilot-demo--chat (request)
  "Queue a scripted reply for REQUEST without invoking any model."
  (let* ((edit (equal (downcase (string-trim (plist-get request :message))) "edit"))
	 (id (format "offline-thread-%d" (cl-incf org-copilot-demo--counter)))
	 (reply
	  (if edit
	      (list :intent "edit" :message "Offline proposal ready; source is unchanged until acceptance."
		    :suggestion_threads
		    (vector (list :id id :intent "rewrite_section"
				  :summary "Make the Intro explicitly reviewable"
				  :suggestions
				  (vector (list :id (concat id ".1")
						:hunks
						(vector (list :kind "section-replace" :primary t
							      :section_title "Intro"
							      :replacement "Bépo keeps edits reviewable.\nUnchanged second line.\n")))))))
	    (list :intent "answer" :message "Offline answer: proposals stay separate from source until you accept them."))))
    (setq org-copilot-demo--pending
	  (nconc org-copilot-demo--pending (list (cons request (json-encode reply)))))
    nil))

(defun org-copilot-demo-respond ()
  "Deliver the oldest scripted reply to its captured source, not this buffer."
  (interactive)
  (unless org-copilot-demo--pending
    (user-error "Send a chat prompt first; use answer or edit"))
  (let* ((entry (pop org-copilot-demo--pending))
	 (request (car entry))
	 (source (plist-get request :source-buffer))
	 (org-copilot-gptel-defer-chat-postprocessing nil))
    (unless (buffer-live-p source)
      (user-error "The queued reply's source buffer was closed"))
    (org-copilot-gptel--handle-chat-response
     (cdr entry) source (plist-get request :focus-comment-id)
     (plist-get request :context-id) request)))

(defun org-copilot-demo-undo ()
  "Undo the latest accepted candidate using the real org-suggestions API.
This helper does not implement the retired Copilot comment-local undo path."
  (interactive)
  (let* ((source (org-copilot--active-source-buffer))
	 (file (buffer-file-name source))
	 (candidate
	  (cl-loop for thread in (reverse (org-suggestions-load-sidecar file))
		   thereis (cl-find-if
			    (lambda (item) (eq (plist-get item :status) 'accepted))
			    (reverse (plist-get thread :candidates))))))
    (unless candidate
      (user-error "Accept a demo suggestion before undoing it"))
    (org-suggestions-undo-accepted-candidate source candidate)))

(defun org-copilot-demo-start ()
  "Activate scripted offline chat for the current disposable Org source."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name)
    (user-error "Visit the disposable Org source before starting the demo"))
  (setq-local org-copilot-chat-function #'org-copilot-demo--chat
	      org-copilot-review-function nil)
  (setq org-export-use-babel nil)
  (org-copilot-open-panels)
  (message "Offline Copilot: send answer or edit, then M-x org-copilot-demo-respond"))

(provide 'org-copilot-demo)
;;; org-copilot-demo.el ends here
