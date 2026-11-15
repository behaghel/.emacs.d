;;; org-copilot-narrowing-test.el --- Narrowed proposal integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise request capture, delayed provider response, durable installation,
;; restored acceptance and undo with duplicate titles and identical bodies.

;;; Code:

(require 'ert)
(require 'org-copilot)
(require 'org-copilot-chat)
(require 'org-copilot-gptel)
(require 'org-suggestions)

(defun org-copilot-narrowing-test--run (final-newline context &optional change)
  "Exercise a narrowed request with FINAL-NEWLINE, CONTEXT and optional CHANGE."
  (let* ((directory (make-temp-file "copilot-narrowing" t))
	 (file (expand-file-name "draft.org" directory))
	 (original (concat "* Parent\n** Intro\nOriginal\n* Intro\nOriginal"
			   (when final-newline "\n")))
	 (org-copilot-gptel-defer-chat-postprocessing nil)
	 (org-suggestions--rollback-data nil)
	 source callback request prompt)
    (unwind-protect
	(progn
	  (with-temp-file file (insert original))
	  (setq source (find-file-noselect file))
	  (with-current-buffer source
	    (org-mode)
	    (goto-char (point-min))
	    (search-forward "** Intro")
	    (beginning-of-line)
	    (org-narrow-to-subtree)
	    (setq org-copilot-chat-context
		  (if (eq context 'section)
		      (org-copilot-chat--section-context-at-point)
		    '(:type full-document)))
	    (setq request (org-copilot-chat--request source "Rewrite Intro"))
	    (should (buffer-narrowed-p))
	    (should (equal (plist-get request :source-content)
			   "** Intro\nOriginal"))
	    (cl-letf (((symbol-function 'gptel-request)
		       (lambda (text &rest args)
			 (setq prompt text callback (plist-get args :callback)))))
	      (org-copilot-gptel-chat request))
	    ;; The prompt must not acquire hidden document text.
	    (should-not (string-match-p "\\* Parent" prompt))
	    (widen)
	    (when change
	      (goto-char (point-min))
	      (search-forward "Original")
	      (replace-match "Human edit" t t)))
	  (funcall callback
		   (concat "{\"intent\":\"edit\",\"message\":\"Drafted.\","
			   "\"suggestion_threads\":[{\"summary\":\"Rewrite Intro\","
			   "\"suggestions\":[{\"id\":\"narrow-1\",\"hunks\":[{"
			   "\"kind\":\"section-replace\",\"section_title\":\"Intro\","
			   "\"replacement\":\"Proposed\"}]}]}]}")
		   '(:status success))
	  (let* ((thread (car (org-suggestions-load-sidecar file)))
		 (candidate (car (plist-get thread :candidates)))
		 (before (with-current-buffer source (buffer-string))))
	    (should candidate)
	    (should (equal (plist-get (car (plist-get candidate :hunks)) :section-path)
			   '("Parent" "Intro")))
	    (should (equal (plist-get (car (plist-get candidate :hunks)) :original)
			   "Original\n"))
	    (if change
		(progn
		  (should-error (org-suggestions-accept-candidate-id source "narrow-1")
				:type 'user-error)
		  (should (equal before (with-current-buffer source (buffer-string)))))
	      (let ((accepted (org-suggestions-accept-candidate-id source "narrow-1")))
		(should (equal (with-current-buffer source (buffer-string))
			       (concat "* Parent\n** Intro\nProposed\n* Intro\nOriginal"
				       (when final-newline "\n"))))
		(org-suggestions-undo-accepted-candidate source accepted)
		(should (equal before (with-current-buffer source (buffer-string))))))))
      (dolist (buffer (buffer-list))
	(when-let* ((name (buffer-file-name buffer)))
	  (when (file-in-directory-p name directory)
	    (with-current-buffer buffer (set-buffer-modified-p nil))
	    (kill-buffer buffer))))
      (delete-directory directory t))))

(ert-deftest org-copilot-narrowing-no-document-final-newline ()
  "A partial path must never select the other, identical top-level Intro."
  (dolist (context '(section full-document))
    (org-copilot-narrowing-test--run nil context)))

(ert-deftest org-copilot-narrowing-with-document-final-newline ()
  "An unchanged section is not stale when narrowing omits its final newline."
  (dolist (context '(section full-document))
    (org-copilot-narrowing-test--run t context)))

(ert-deftest org-copilot-narrowing-intervening-edit-conflicts ()
  "A delayed response cannot bind to mutable callback-time source text."
  (org-copilot-narrowing-test--run t 'section t))

(provide 'org-copilot-narrowing-test)
;;; org-copilot-narrowing-test.el ends here
