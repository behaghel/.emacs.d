;;; ox-sync-demo.el --- Offline provider for the standalone walkthrough -*- lexical-binding: t; -*-

;;; Commentary:
;; Explicitly activated demonstration only.  The remote is buffer-local memory;
;; no accounts, credentials, HTTP calls, or real provider packages are involved.

;;; Code:

(require 'cl-lib)
(require 'ox-sync)

(defvar-local ox-sync-demo--state nil
  "In-memory remote content, revision, and fetch count for this sample source.")

(defun ox-sync-demo--detect (source)
  "Return the demonstration descriptor for SOURCE when explicitly activated."
  (with-current-buffer source
    (when ox-sync-demo--state
      (list :kind 'demo :remote-id "offline-document"
	    :source-buffer source :source-file buffer-file-name))))

(defun ox-sync-demo--fetch (document domains)
  "Return DOCUMENT's in-memory remote references for DOMAINS."
  (with-current-buffer (plist-get document :source-buffer)
    (cl-incf (plist-get ox-sync-demo--state :fetches))
    (list :provider 'demo :remote-id "offline-document"
	  :domains
	  (mapcar
	   (lambda (domain)
	     (cons domain
		   (list :remote-ref
			 (if (eq domain 'content)
			     (list :version (number-to-string
					     (plist-get ox-sync-demo--state :version))
				   :hash (secure-hash
					  'sha1 (plist-get ox-sync-demo--state :content)))
			   (list :hash (secure-hash 'sha1 "nil") :count 0)))))
	   domains))))

(defun ox-sync-demo--push (document _domain _tracking)
  "Publish DOCUMENT's buffer to the in-memory remote."
  (with-current-buffer (plist-get document :source-buffer)
    (setf (plist-get ox-sync-demo--state :content) (buffer-string))
    (cl-incf (plist-get ox-sync-demo--state :version))))

(defun ox-sync-demo--pull (document _domain _tracking)
  "Replace DOCUMENT's source text with the in-memory remote text."
  (with-current-buffer (plist-get document :source-buffer)
    (erase-buffer)
    (insert (plist-get ox-sync-demo--state :content))))

(defun ox-sync-demo-remote-edit (text)
  "Append TEXT to the in-memory remote without changing the source."
  (interactive "sAppend to simulated remote: ")
  (let ((source (if (derived-mode-p 'ox-sync-status-mode)
		    context-panels-source-buffer
		  (current-buffer))))
    (with-current-buffer source
      (unless ox-sync-demo--state
	(user-error "This buffer has no active offline demo"))
      (setf (plist-get ox-sync-demo--state :content)
	    (concat (plist-get ox-sync-demo--state :content) "\n" text "\n"))
      (cl-incf (plist-get ox-sync-demo--state :version))
      (message "Simulated remote changed; fetch to inspect it"))))

(defun ox-sync-demo-start ()
  "Start the offline status walkthrough for the current file-visiting source."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name)
    (user-error "Visit the disposable Org source before starting the demo"))
  (setq-local ox-sync-demo--state (list :content (buffer-string) :version 1 :fetches 0))
  (ox-sync-register-provider
   :kind 'demo :detect #'ox-sync-demo--detect :fetch #'ox-sync-demo--fetch
   :push-content #'ox-sync-demo--push :pull-content #'ox-sync-demo--pull)
  (ox-sync-status))

(provide 'ox-sync-demo)
;;; ox-sync-demo.el ends here
