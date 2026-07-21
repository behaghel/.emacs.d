;;; org-copilot-suggestion-test.el --- Suggestion tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot document and section suggestion previews.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-suggestion)

(ert-deftest org-copilot-suggestion-open-renders-left-preview ()
  "Suggestion previews render read-only Org buffers."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (unwind-protect
	  (let ((buffer (org-copilot-suggestion-open
			 source "🌐 Full document" "Try this."
			 'full-document)))
	    (with-current-buffer buffer
	      (should (derived-mode-p 'org-copilot-suggestion-mode))
	      (should buffer-read-only)
	      (should (eq org-copilot-suggestion-source-buffer source))
	      (should (string-match-p
		       "Org Copilot Suggestion — 🌐 Full document · preview only"
		       (buffer-string)))
	      (should (string-match-p "Try this" (buffer-string)))))
	(when (get-buffer org-copilot-suggestion-buffer-name)
	  (kill-buffer org-copilot-suggestion-buffer-name))))))

(ert-deftest org-copilot-suggestion-open-preserves-selected-window ()
  "Generated suggestion previews do not steal focus."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(progn
	  (set-window-buffer (selected-window) source)
	  (org-copilot-suggestion-open source "🌐 Full document" "Try this."
				       'full-document)
	  (should (eq (window-buffer (selected-window)) source)))
      (when (buffer-live-p source)
	(kill-buffer source))
      (when (get-buffer org-copilot-suggestion-buffer-name)
	(kill-buffer org-copilot-suggestion-buffer-name)))))

(ert-deftest org-copilot-suggestion-open-can-select-preview ()
  "Explicit preview commands can select the suggestion window."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(progn
	  (set-window-buffer (selected-window) source)
	  (let ((buffer (org-copilot-suggestion-open
			 source "🌐 Full document" "Try this."
			 'full-document nil t)))
	    (should (eq (window-buffer (selected-window)) buffer))))
      (when (buffer-live-p source)
	(kill-buffer source))
      (when (get-buffer org-copilot-suggestion-buffer-name)
	(kill-buffer org-copilot-suggestion-buffer-name)))))

(ert-deftest org-copilot-suggestion-mode-uses-shared-close-key ()
  "Suggestion previews bind q through the shared context auxiliary map."
  (should (eq (lookup-key org-copilot-suggestion-mode-map (kbd "q"))
	      #'org-context-panel-close-current-window)))

(ert-deftest org-copilot-suggestion-window-is-deletable-from-source ()
  "Suggestion previews do not resist `delete-other-windows' from source."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (set-window-buffer (selected-window) source)
	  (org-copilot-suggestion-open source "🌐 Full document" "Try this."
				       'full-document)
	  (should (get-buffer-window org-copilot-suggestion-buffer-name t))
	  (select-window (get-buffer-window source t))
	  (delete-other-windows)
	  (should-not (get-buffer-window org-copilot-suggestion-buffer-name t)))
      (delete-other-windows)
      (when (buffer-live-p source)
	(kill-buffer source))
      (when (get-buffer org-copilot-suggestion-buffer-name)
	(kill-buffer org-copilot-suggestion-buffer-name)))))

(ert-deftest org-copilot-view-suggestion-at-point-reopens-section-preview ()
  "Section suggestion rows can reopen the left preview."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer))
	  (comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :type 'scope
			  :status 'active
			  :suggestion "New body.\n"
			  :section-title "Intro"))))
      (with-temp-buffer
	(setq org-context-panel-source-buffer source)
	(insert "§✏️ Rewrite\n")
	(add-text-properties (point-min) (point-max)
			     `(org-context-panel-item ,comment))
	(goto-char (point-min))
	(let ((buffer (org-copilot-view-suggestion-at-point nil)))
	  (with-current-buffer buffer
	    (should (string-match-p "§ Section: Intro" (buffer-string)))
	    (should (string-match-p "New body" (buffer-string)))))))))

(ert-deftest org-copilot-toggle-suggestion-panel-opens-single-section-suggestion ()
  "The toggle command opens the only active section suggestion from source."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (unwind-protect
	  (progn
	    (org-copilot-add-comment
	     (list :id "ai-1"
		   :type 'scope
		   :status 'active
		   :suggestion "New body.\n"
		   :section-title "Intro"))
	    (org-copilot-toggle-suggestion-panel)
	    (should (get-buffer-window org-copilot-suggestion-buffer-name t))
	    (org-copilot-toggle-suggestion-panel)
	    (should-not (get-buffer-window org-copilot-suggestion-buffer-name t)))
	(when (get-buffer org-copilot-suggestion-buffer-name)
	  (kill-buffer org-copilot-suggestion-buffer-name))
	(should (buffer-live-p source))))))

(ert-deftest org-copilot-view-suggestion-at-point-errors-for-inline-suggestion ()
  "Inline suggestion rows do not use the left suggestion preview."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer))
	  (comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :type 'inline
			  :status 'active
			  :suggestion "Short."))))
      (with-temp-buffer
	(setq org-context-panel-source-buffer source)
	(insert "✏️ Rewrite\n")
	(add-text-properties (point-min) (point-max)
			     `(org-context-panel-item ,comment))
	(goto-char (point-min))
	(should-error (org-copilot-view-suggestion-at-point nil)
		      :type 'user-error)))))

(ert-deftest org-copilot-suggestion-resolves-unique-section-title ()
  "Section anchors resolve a unique exact title."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nBody.\n* Methods\nMethod body.\n")
    (let ((section (org-copilot-suggestion-resolve-section
		    (current-buffer)
		    (list :section-title "Methods")
		    '(:type full-document))))
      (should section)
      (should (equal (plist-get section :section-title) "Methods")))))

(ert-deftest org-copilot-suggestion-refuses-duplicate-section-title ()
  "Ambiguous section titles do not resolve silently."
  (with-temp-buffer
    (org-mode)
    (insert "* Details\nOne.\n* Details\nTwo.\n")
    (should-not
     (org-copilot-suggestion-resolve-section
      (current-buffer)
      (list :section-title "Details")
      '(:type full-document)))))

(ert-deftest org-copilot-suggestion-installs-current-section-comment ()
  "Section-context suggestions become accept-capable AI comments."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nOriginal body.\n")
    (goto-char (point-min))
    (let* ((context (org-copilot-chat--section-context-at-point))
	   (comment (org-copilot-suggestion-install-section
		     (current-buffer)
		     (list :suggestion "New body.")
		     context
		     "Rewrite section.")))
      (should comment)
      (should (equal (plist-get comment :target-text) "Original body.\n"))
      (should (equal (plist-get comment :suggestion) "New body.\n"))
      (should (equal (plist-get comment :section-title) "Intro"))
      (with-current-buffer (get-buffer org-copilot-suggestion-buffer-name)
	(should (string-match-p
		 "Org Copilot Suggestion — § Section: Intro · can accept"
		 (buffer-string)))))))

(provide 'org-copilot-suggestion-test)
;;; org-copilot-suggestion-test.el ends here
