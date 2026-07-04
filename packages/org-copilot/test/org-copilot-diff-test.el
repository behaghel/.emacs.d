;;; org-copilot-diff-test.el --- Diff tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot suggestion diff preview buffers.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-diff)

(ert-deftest org-copilot-diff-mode-defines-action-keys ()
  "Org Copilot diff mode exposes accept and close keys."
  (should (eq (lookup-key org-copilot-diff-mode-map (kbd "a"))
	      #'org-copilot-accept-at-point))
  (should (eq (lookup-key org-copilot-diff-mode-map (kbd "q"))
	      #'org-copilot-close-diff)))

(ert-deftest org-copilot-diff-buffer-is-read-only ()
  "Diff preview buffers are read-only."
  (let ((source (generate-new-buffer " *org copilot diff source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (let* ((comment (org-copilot-add-comment
			   (list :id "ai-1"
				 :source-start (point-min)
				 :source-end (line-end-position)
				 :target-text "Alpha sentence."
				 :body "Tighten wording."
				 :suggestion "Alpha."
				 :status 'active)))
		 (buffer (org-copilot-diff-open source comment)))
	    (with-current-buffer buffer
	      (should buffer-read-only))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-diff-buffer-shows-old-and-new-text ()
  "Diff preview buffers show reviewed text and proposed replacement."
  (let ((source (generate-new-buffer " *org copilot diff source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (let* ((comment (org-copilot-add-comment
			   (list :id "ai-1"
				 :source-start (point-min)
				 :source-end (line-end-position)
				 :target-text "Alpha sentence."
				 :body "Tighten wording."
				 :suggestion "Alpha."
				 :status 'active)))
		 (buffer (org-copilot-diff-open source comment)))
	    (with-current-buffer buffer
	      (should (string-match-p "^-Alpha sentence\\." (buffer-string)))
	      (should (string-match-p "^+Alpha\\." (buffer-string))))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-diff-buffer-is-context-panel-associated ()
  "Diff preview buffers keep source association for context-panel follow logic."
  (let ((source (generate-new-buffer " *org copilot diff source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (let* ((comment (org-copilot-add-comment
			   (list :id "ai-1"
				 :source-start (point-min)
				 :source-end (+ (point-min) (length "Alpha sentence."))
				 :target-text "Alpha sentence."
				 :body "Tighten wording."
				 :suggestion "Alpha."
				 :status 'active)))
		 (buffer (org-copilot-diff-open source comment)))
	    (with-current-buffer buffer
	      (should (eq org-context-panel-source-buffer source))
	      (should (eq org-context-panel-view-id 'copilot-diff)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-accept-from-diff-kills-diff-buffer ()
  "Accepting from a diff preview kills the diff buffer after applying."
  (let ((source (generate-new-buffer " *org copilot diff source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (let* ((comment (org-copilot-add-comment
			   (list :id "ai-1"
				 :source-start (point-min)
				 :source-end (+ (point-min) (length "Alpha sentence."))
				 :target-text "Alpha sentence."
				 :body "Tighten wording."
				 :suggestion "Alpha."
				 :status 'active)))
		 (buffer (org-copilot-diff-open source comment)))
	    (with-current-buffer buffer
	      (org-copilot-accept-at-point))
	    (should-not (buffer-live-p buffer))
	    (should (equal (buffer-string) "Alpha.\n"))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-view-diff-errors-without-suggestion ()
  "Viewing a diff errors when the AI comment has no suggestion."
  (with-temp-buffer
    (org-copilot-panel-mode)
    (let ((inhibit-read-only t)
	  (comment (list :id "ai-1"
			 :body "Clarify this."
			 :status 'active)))
      (insert "AI [active] Clarify this.\n")
      (add-text-properties (point-min) (point-max)
			   `(org-context-panel-item ,comment))
      (goto-char (point-min))
      (should-error (org-copilot-view-diff-at-point) :type 'user-error))))

(provide 'org-copilot-diff-test)
;;; org-copilot-diff-test.el ends here
