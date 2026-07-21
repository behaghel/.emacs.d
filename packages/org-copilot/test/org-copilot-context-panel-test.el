;;; org-copilot-context-panel-test.el --- Context panel tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for rendering Org Copilot AI comments through org-context-panel.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-context-panel)
(require 'org-copilot-suggestion)

(ert-deftest org-copilot-highlight-faces-keep-foreground-unspecified ()
  "Org Copilot highlight faces only force background colors."
  (dolist (face '(org-copilot-target-face
		  org-copilot-target-dim-face
		  org-copilot-panel-focused-face))
    (should (eq (face-attribute face :foreground nil t) 'unspecified))
    (should (eq (face-attribute face :inherit nil t) 'unspecified))
    (should (stringp (face-attribute face :background nil t)))))

(ert-deftest org-copilot-context-panel-registers-provider ()
  "Enabling Org Copilot registers a context-panel provider."
  (with-temp-buffer
    (org-mode)
    (org-copilot-mode 1)
    (should (org-context-panel-registered-provider 'copilot))
    (org-copilot-mode -1)
    (should-not (org-context-panel-registered-provider 'copilot))))

(ert-deftest org-copilot-context-panel-provider-exposes-chat-bottom-view ()
  "Org Copilot chat is exposed as a context-panel bottom view with height."
  (with-temp-buffer
    (org-mode)
    (let ((org-copilot-chat-window-height 18))
      (org-copilot-mode 1)
      (let ((view (org-context-panel-bottom-view 'copilot-chat (current-buffer))))
	(should view)
	(should (eq (plist-get view :provider) 'copilot))
	(should (eq (plist-get view :mode) 'org-copilot-chat-mode))
	(should (= (plist-get view :height) 18))))))

(ert-deftest org-copilot-context-panel-collects-visible-items ()
  "The context-panel provider exposes active AI comments as side items."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\nBeta sentence.\n")
    (let ((start (point-min))
	  (end (save-excursion
		 (goto-char (point-min))
		 (search-forward "Alpha")
		 (point))))
      (org-copilot-mode 1)
      (org-copilot-add-comment
       (list :id "ai-1"
	     :type 'inline
	     :status 'active
	     :source-start start
	     :source-end end
	     :target-text "Alpha"
	     :body "Clarify this."))
      (let ((items (org-context-panel-collect-side-items (current-buffer))))
	(should (= (length items) 1))
	(should (equal (plist-get (car items) :id) "ai-1"))
	(should (eq (plist-get (car items) :provider) 'copilot))))))

(ert-deftest org-copilot-context-panel-renders-unanchored-scope-items ()
  "The side panel surfaces scope AI comments without source anchors."
  (let ((source-buffer (generate-new-buffer " *org copilot scope source*"))
	(panel-buffer (generate-new-buffer " *org copilot scope panel*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (set-window-buffer (selected-window) source-buffer)
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'scope
		 :status 'active
		 :summary "Clarifier le pacte de lecture"
		 :body "Clarifier."))
	  (with-current-buffer panel-buffer
	    (org-copilot-panel-mode)
	    (org-context-panel-render-side-panel
	     source-buffer (get-buffer-window source-buffer t))
	    (should (string-match-p "Clarifier le pacte de lecture"
				    (buffer-string)))))
      (when (buffer-live-p panel-buffer)
	(kill-buffer panel-buffer))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-copilot-context-panel-jump-unanchored-scope-focuses-comment ()
  "Jumping to an unanchored scope row focuses the comment without error."
  (let ((source-buffer (generate-new-buffer " *org copilot jump scope source*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'scope
		 :status 'active
		 :summary "Scope note"
		 :body "Verbose scope note."))
	  (org-copilot-context-panel-jump-side-item
	   source-buffer (car (org-copilot-comments)))
	  (should (equal org-copilot-chat-focus-comment-id "ai-1")))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-copilot-context-panel-ret-opens-section-suggestion ()
  "Jumping to a section suggestion row opens its preview."
  (let ((source-buffer (generate-new-buffer " *org copilot jump suggestion source*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "* Intro\nBody.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'scope
		 :status 'active
		 :summary "Section suggestion"
		 :body "Try this."
		 :suggestion "New body.\n"
		 :section-title "Intro"))
	  (org-copilot-context-panel-jump-side-item
	   source-buffer (car (org-copilot-comments)))
	  (should (get-buffer org-copilot-suggestion-buffer-name)))
      (when (get-buffer org-copilot-suggestion-buffer-name)
	(kill-buffer org-copilot-suggestion-buffer-name))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-copilot-refresh-overlays-creates-dim-target-overlay-by-default ()
  "Refreshing overlays dims anchored AI comment target ranges by default."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha"))
	   :target-text "Alpha"
	   :status 'active))
    (org-copilot-refresh-overlays)
    (let ((overlays (overlays-at (point-min))))
      (should (= (length org-copilot--overlays) 1))
      (should (cl-some (lambda (overlay)
			 (eq (overlay-get overlay 'face) 'org-copilot-target-dim-face))
		       overlays)))))

(ert-deftest org-copilot-refresh-overlays-highlights-scope-title-only ()
  "Scope comments highlight only the section title line."
  (with-temp-buffer
    (org-mode)
    (insert "* Section\nBody line.\nMore body.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :type 'scope
	   :source-start (point-min)
	   :source-end (point-max)
	   :target-text (buffer-string)
	   :status 'active))
    (org-copilot-refresh-overlays)
    (let ((overlay (car org-copilot--overlays)))
      (should (= (overlay-start overlay) (point-min)))
      (should (= (overlay-end overlay)
		 (save-excursion
		   (goto-char (point-min))
		   (line-end-position)))))))

(ert-deftest org-copilot-refresh-overlays-highlights-only-focused-target ()
  "Refreshing overlays highlights focused target and leaves others dimmed."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\nBeta sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha"))
	   :target-text "Alpha"
	   :status 'active))
    (org-copilot-add-comment
     (list :id "ai-2"
	   :source-start (save-excursion
			   (goto-char (point-min))
			   (search-forward "Beta")
			   (match-beginning 0))
	   :source-end (save-excursion
			 (goto-char (point-min))
			 (search-forward "Beta")
			 (match-end 0))
	   :target-text "Beta"
	   :status 'active))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (org-copilot-refresh-overlays)
    (let* ((faces (mapcar (lambda (overlay)
			    (overlay-get overlay 'face))
			  org-copilot--overlays)))
      (should (= (cl-count 'org-copilot-target-face faces) 1))
      (should (memq 'org-copilot-target-dim-face faces)))))

(ert-deftest org-copilot-refresh-overlays-skips-dismissed-comments ()
  "Dismissed AI comments are not rendered as target overlays."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha"))
	   :target-text "Alpha"
	   :status 'dismissed))
    (org-copilot-refresh-overlays)
    (should-not org-copilot--overlays)))

(ert-deftest org-copilot-clear-session-removes-overlays ()
  "Clearing a session removes Org Copilot source overlays."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha"))
	   :target-text "Alpha"
	   :status 'active))
    (org-copilot-refresh-overlays)
    (should org-copilot--overlays)
    (org-copilot-clear-session)
    (should-not org-copilot--overlays)))

(ert-deftest org-copilot-clear-session-refreshes-focused-panel-row ()
  "Clearing a session removes focused side-panel row background state."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :source-start (point-min)
		 :source-end (+ (point-min) (length "Alpha"))
		 :target-text "Alpha"
		 :summary "Focused row"
		 :status 'active))
	  (setq org-copilot-chat-focus-comment-id "ai-1")
	  (with-current-buffer (get-buffer-create org-copilot-panel-buffer-name)
	    (org-copilot-panel-mode)
	    (setq org-context-panel-source-buffer source))
	  (org-context-panel-refresh)
	  (with-current-buffer org-copilot-panel-buffer-name
	    (should (string-match-p "Focused row" (buffer-string))))
	  (org-copilot-clear-session)
	  (with-current-buffer org-copilot-panel-buffer-name
	    (should-not (string-match-p "Focused row" (buffer-string)))))
      (when (get-buffer org-copilot-panel-buffer-name)
	(kill-buffer org-copilot-panel-buffer-name))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-panel-mode-defines-action-keys ()
  "Org Copilot panel mode exposes package-default row action keys."
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "RET"))
	      #'org-context-panel-jump-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "d"))
	      #'org-copilot-view-diff-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "v"))
	      #'org-copilot-view-suggestion-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "a"))
	      #'org-copilot-accept-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "x"))
	      #'org-copilot-dismiss-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "c"))
	      #'org-copilot-chat))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / a"))
	      #'org-copilot-chat-accept-focused-suggestion-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / d"))
	      #'org-copilot-chat-dismiss-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / n"))
	      #'org-copilot-chat-focus-next-comment))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / p"))
	      #'org-copilot-chat-focus-previous-comment))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / u"))
	      #'org-copilot-chat-undo-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / g"))
	      #'org-copilot-chat-full-document))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / s"))
	      #'org-copilot-chat-section))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "C-c C-x / o"))
	      #'org-copilot-open-panels))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "G"))
	      #'org-copilot-chat-full-document))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "g"))
	      #'org-copilot-refresh))
  (should (eq (lookup-key org-copilot-panel-mode-map (kbd "q"))
	      #'org-copilot-close)))

(ert-deftest org-copilot-mode-defines-prefixed-source-keys ()
  "Org Copilot source mode uses the Org-friendly prefix keymap."
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / g"))
	      #'org-copilot-chat-full-document))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / s"))
	      #'org-copilot-chat-section))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / c"))
	      #'org-copilot-chat))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / n"))
	      #'org-context-panel-next-item))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / p"))
	      #'org-context-panel-previous-item))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / o"))
	      #'org-copilot-open-panels)))

(ert-deftest org-copilot-selected-source-ignores-minibuffer ()
  "Minibuffer selection does not count as a source-buffer switch."
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (cl-letf (((symbol-function 'selected-window)
		 (lambda () 'fake-window))
		((symbol-function 'window-buffer)
		 (lambda (_window) buffer))
		((symbol-function 'minibufferp)
		 (lambda (_buffer) t)))
	(should-not (org-copilot--selected-source-buffer))))))

(ert-deftest org-copilot-retargets-visible-panels-between-copilot-sources ()
  "Visible Org Copilot panels retarget when switching Copilot source buffers."
  (let ((source-a (generate-new-buffer " *org copilot source a*"))
	(source-b (generate-new-buffer " *org copilot source b*")))
    (unwind-protect
	(progn
	  (with-current-buffer source-a
	    (org-mode)
	    (insert "* A\n")
	    (org-copilot-mode 1)
	    (org-copilot-add-comment (list :id "ai-a" :summary "From A")))
	  (with-current-buffer source-b
	    (org-mode)
	    (insert "* B\n")
	    (org-copilot-mode 1)
	    (org-copilot-add-comment (list :id "ai-b" :summary "From B")))
	  (let ((panel (get-buffer-create org-copilot-panel-buffer-name))
		(chat (org-copilot-chat--buffer source-a)))
	    (with-current-buffer panel
	      (org-copilot-panel-mode)
	      (setq org-context-panel-source-buffer source-a)
	      (org-context-panel-render-side-panel source-a))
	    (cl-letf (((symbol-function 'get-buffer-window)
		       (lambda (buffer &optional _all-frames)
			 (and (memq buffer (list panel chat)) t))))
	      (org-copilot--retarget-visible-panels source-b))
	    (with-current-buffer panel
	      (should (eq org-context-panel-source-buffer source-b))
	      (should (string-match-p "From B" (buffer-string)))
	      (should-not (string-match-p "From A" (buffer-string))))
	    (with-current-buffer chat
	      (should (eq org-copilot-chat-source-buffer source-b))
	      (should (string-match-p "source b" (buffer-string))))))
      (when (buffer-live-p source-a)
	(kill-buffer source-a))
      (when (buffer-live-p source-b)
	(kill-buffer source-b))
      (when (get-buffer org-copilot-panel-buffer-name)
	(kill-buffer org-copilot-panel-buffer-name))
      (when (get-buffer org-copilot-chat-buffer-name)
	(kill-buffer org-copilot-chat-buffer-name)))))

(ert-deftest org-copilot-suggestion-buffer-is-auxiliary-not-source ()
  "Suggestion previews do not become workspace source buffers."
  (let ((source (generate-new-buffer " *org copilot source*"))
	(suggestion (generate-new-buffer " *org copilot suggestion*")))
    (unwind-protect
	(progn
	  (with-current-buffer source
	    (org-mode)
	    (org-copilot-mode 1))
	  (with-current-buffer suggestion
	    (org-copilot-suggestion-mode)
	    (setq org-copilot-suggestion-source-buffer source)
	    (should (org-copilot--auxiliary-buffer-p (current-buffer)))
	    (should (eq (org-copilot--active-source-buffer) source))))
      (when (buffer-live-p source)
	(kill-buffer source))
      (when (buffer-live-p suggestion)
	(kill-buffer suggestion)))))

(ert-deftest org-copilot-open-panels-from-suggestion-keeps-source-chat ()
  "Opening panels from a suggestion preview uses its source buffer."
  (let ((source (generate-new-buffer " *org copilot source*"))
	(suggestion (generate-new-buffer " *org copilot suggestion*"))
	called-source)
    (unwind-protect
	(progn
	  (with-current-buffer source
	    (org-mode)
	    (insert "* Source\n"))
	  (with-current-buffer suggestion
	    (org-copilot-suggestion-mode)
	    (setq org-copilot-suggestion-source-buffer source)
	    (cl-letf (((symbol-function 'org-context-panel-open)
		       (lambda (buffer) (setq called-source buffer)))
		      ((symbol-function 'org-copilot-chat-full-document)
		       (lambda ()
			 (setq called-source
			       (cons called-source (current-buffer))))))
	      (org-copilot-open-panels)))
	  (should (equal called-source (cons source source)))
	  (should (buffer-local-value 'org-copilot-mode source))
	  (should-not (buffer-local-value 'org-copilot-mode suggestion)))
      (when (buffer-live-p source)
	(kill-buffer source))
      (when (buffer-live-p suggestion)
	(kill-buffer suggestion)))))

(ert-deftest org-copilot-closes-visible-panels-for-non-copilot-source ()
  "Org Copilot panels close when selected source lacks Copilot mode."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(progn
	  (with-current-buffer source
	    (org-mode)
	    (org-copilot-mode 1))
	  (display-buffer (get-buffer-create org-copilot-panel-buffer-name))
	  (display-buffer (org-copilot-chat--buffer source))
	  (org-copilot--close-auxiliary-panels)
	  (should-not (get-buffer org-copilot-panel-buffer-name))
	  (should-not (get-buffer org-copilot-chat-buffer-name)))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-context-panel-renders-compact-summary ()
  "AI comment rows show compact lifecycle marker and summary text."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'inline
		 :status 'active
		 :source-start (point-min)
		 :source-end (+ (point-min) 5)
		 :target-text "Alpha"
		 :summary "Why should I believe this?"
		 :body "Clarify this sentence."))
	  (with-temp-buffer
	    (org-context-panel-render-side-panel source)
	    (should (string-match-p "💬" (buffer-string)))
	    (should-not (string-match-p "AI" (buffer-string)))
	    (should-not (string-match-p "active" (buffer-string)))
	    (should (string-match-p "Why should I believe this" (buffer-string)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-panel-focus-at-point-highlights-source-and-row ()
  "Moving point onto a side-panel comment focuses its source target and row."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'inline
		 :status 'active
		 :source-start (point-min)
		 :source-end (+ (point-min) 5)
		 :target-text "Alpha"
		 :summary "Focused row"))
	  (let ((panel (get-buffer-create org-copilot-panel-buffer-name)))
	    (unwind-protect
		(with-current-buffer panel
		  (org-copilot-panel-mode)
		  (setq org-context-panel-source-buffer source)
		  (org-context-panel-render-side-panel source)
		  (goto-char (point-min))
		  (org-copilot-panel-focus-at-point)
		  (with-current-buffer source
		    (should (equal org-copilot-chat-focus-comment-id "ai-1"))
		    (should (eq (overlay-get (car org-copilot--overlays) 'face)
				'org-copilot-target-face)))
		  (let ((face (get-text-property (point) 'face)))
		    (should (if (listp face)
				(memq 'org-copilot-panel-focused-face face)
			      (eq face 'org-copilot-panel-focused-face)))))
	      (when (buffer-live-p panel)
		(kill-buffer panel)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-context-panel-tints-focused-comment-background ()
  "AI comment rows tint the background of the comment focused by chat."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'inline
		 :status 'active
		 :source-start (point-min)
		 :source-end (+ (point-min) 5)
		 :target-text "Alpha"
		 :summary "Focused row"))
	  (setq org-copilot-chat-focus-comment-id "ai-1")
	  (with-temp-buffer
	    (org-context-panel-render-side-panel source)
	    (goto-char (point-min))
	    (let ((face (get-text-property (point) 'face)))
	      (should (if (listp face)
			  (memq 'org-copilot-panel-focused-face face)
			(eq face 'org-copilot-panel-focused-face))))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-context-panel-marks-section-suggestions ()
  "AI comment rows distinguish section suggestions."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "* Intro\nBody.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'scope
		 :status 'active
		 :source-start (point-min)
		 :source-end (point-max)
		 :target-text "Body."
		 :summary "Rewrite section"
		 :suggestion "New body."
		 :section-title "Intro"))
	  (with-temp-buffer
	    (org-context-panel-render-side-panel source)
	    (should (string-match-p "§✏️" (buffer-string)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-context-panel-marks-suggested-comments ()
  "AI comment rows distinguish comments with replacement suggestions."
  (let ((source (generate-new-buffer " *org copilot source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :type 'inline
		 :status 'active
		 :source-start (point-min)
		 :source-end (+ (point-min) 5)
		 :target-text "Alpha"
		 :summary "Tighten this"
		 :suggestion "Alpha."))
	  (with-temp-buffer
	    (org-context-panel-render-side-panel source)
	    (should (string-match-p "✏️" (buffer-string)))
	    (should-not (string-match-p "💬" (buffer-string)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(provide 'org-copilot-context-panel-test)
;;; org-copilot-context-panel-test.el ends here
