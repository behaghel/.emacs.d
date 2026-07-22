;;; org-context-panel-test.el --- Context panel primitive tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for reusable Org context panel primitives.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-context-panel)

(defvar-local org-context-panel-test--marker-overlay nil
  "Top marker overlay used by context-panel tests.")

(ert-deftest org-context-panel-protect-window-defends-panel-buffer ()
  "Protected panel windows restore their panel and redirect document buffers."
  (let ((source (generate-new-buffer " *org context source*"))
	(panel (generate-new-buffer " *org context panel*"))
	(document (generate-new-buffer " *org context document*"))
	panel-window)
    (unwind-protect
	(progn
	  (set-window-buffer (selected-window) source)
	  (setq panel-window (split-window-right))
	  (set-window-buffer panel-window panel)
	  (org-context-panel-protect-window panel-window panel source)
	  (should (window-dedicated-p panel-window))
	  (should (window-parameter panel-window 'no-other-window))
	  (should (window-parameter panel-window 'no-delete-other-windows))
	  (set-window-dedicated-p panel-window nil)
	  (set-window-buffer panel-window document)
	  (org-context-panel--repair-protected-window panel-window)
	  (should (eq (window-buffer panel-window) panel))
	  (should (eq (window-buffer (selected-window)) document))
	  (should (window-dedicated-p panel-window)))
      (when (window-live-p panel-window)
	(delete-window panel-window))
      (dolist (buffer (list source panel document))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(ert-deftest org-context-panel-registers-buffer-local-providers ()
  "Providers are registered buffer-locally by name."
  (with-temp-buffer
    (let ((provider-a (list :name 'comments :value 1))
	  (provider-b (list :name 'comments :value 2)))
      (should (eq (org-context-panel-register-provider provider-a) provider-a))
      (should (eq (org-context-panel-registered-provider 'comments) provider-a))
      (org-context-panel-register-provider provider-b)
      (should (equal (org-context-panel-registered-providers)
		     (list provider-b)))
      (org-context-panel-unregister-provider 'comments)
      (should-not (org-context-panel-registered-provider 'comments))))
  (with-temp-buffer
    (should-not (org-context-panel-registered-providers))))

(ert-deftest org-context-panel-collects-provider-side-items ()
  "Side item collection annotates provider ownership."
  (with-temp-buffer
    (org-context-panel-register-provider
     (list :name 'test
	   :collect-side-items
	   (lambda (_source-buffer)
	     (list (list :id "a" :source-start 1)
		   (list :id "b" :source-start 2)))))
    (let ((items (org-context-panel-collect-side-items)))
      (should (equal (mapcar (lambda (item) (plist-get item :id)) items)
		     '("a" "b")))
      (should (cl-every (lambda (item) (eq (plist-get item :provider) 'test))
			items)))))

(ert-deftest org-context-panel-items-for-window-adds-viewport-rows ()
  "Visible side items are copied with viewport row metadata."
  (let ((buffer (generate-new-buffer " *org context viewport test*")))
    (unwind-protect
	(with-current-buffer buffer
	  (org-mode)
	  (insert "One\nTwo\nThree\n")
	  (let ((two-pos (progn
			   (goto-char (point-min))
			   (forward-line 1)
			   (point))))
	    (org-context-panel-register-provider
	     (list :name 'test
		   :collect-side-items
		   (lambda (_source-buffer)
		     (list (list :id "two" :source-start two-pos)))))
	    (set-window-buffer (selected-window) buffer)
	    (set-window-start (selected-window) (point-min))
	    (let ((items (org-context-panel-items-for-window (selected-window)
							     buffer)))
	      (should (= (length items) 1))
	      (should (equal (plist-get (car items) :id) "two"))
	      (should (integerp (plist-get (car items) :anchor-line)))
	      (should (>= (plist-get (car items) :anchor-line) 1)))))
      (when (buffer-live-p buffer)
	(kill-buffer buffer)))))

(ert-deftest org-context-panel-items-for-window-skips-folded-org-items ()
  "Items hidden by Org folding are not surfaced as visible side rows."
  (let ((buffer (generate-new-buffer " *org context folded test*")))
    (unwind-protect
	(with-current-buffer buffer
	  (org-mode)
	  (insert "* Visible\nShown body.\n* Folded\nHidden body.\n")
	  (let ((shown-pos (save-excursion
			     (goto-char (point-min))
			     (forward-line 1)
			     (point)))
		(hidden-pos (save-excursion
			      (goto-char (point-min))
			      (search-forward "Hidden body")
			      (match-beginning 0))))
	    (goto-char hidden-pos)
	    (org-back-to-heading t)
	    (if (fboundp 'org-fold-hide-subtree)
		(org-fold-hide-subtree)
	      (outline-hide-subtree))
	    (org-context-panel-register-provider
	     (list :name 'test
		   :collect-side-items
		   (lambda (_source-buffer)
		     (list (list :id "shown" :source-start shown-pos)
			   (list :id "hidden" :source-start hidden-pos)))))
	    (set-window-buffer (selected-window) buffer)
	    (set-window-start (selected-window) (point-min))
	    (let ((items (org-context-panel-items-for-window (selected-window)
							     buffer)))
	      (should (equal (mapcar (lambda (item) (plist-get item :id)) items)
			     '("shown"))))))
      (when (buffer-live-p buffer)
	(kill-buffer buffer)))))

(ert-deftest org-context-panel-renders-composed-provider-items-in-order ()
  "Composable providers render merged side items by source position and priority."
  (let ((source-buffer (generate-new-buffer " *org context composed source*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "One\nTwo\nThree\n")
	  (let ((one (point-min))
		(two (save-excursion (goto-char (point-min)) (forward-line 1) (point)))
		(rendered nil))
	    (org-context-panel-register-provider
	     (list :name 'comments
		   :priority 10
		   :collect-side-items (lambda (_source) (list (list :id "comment" :source-start two)))
		   :render-side-item (lambda (_source item)
				       (push (plist-get item :id) rendered)
				       (insert (plist-get item :id) "\n"))))
	    (org-context-panel-register-provider
	     (list :name 'marginalia
		   :priority 20
		   :collect-side-items (lambda (_source)
					 (list (list :id "note" :source-start one)
					       (list :id "late-note" :source-start two)))
		   :render-side-item (lambda (_source item)
				       (push (plist-get item :id) rendered)
				       (insert (plist-get item :id) "\n"))))
	    (with-temp-buffer
	      (org-context-panel-render-side-panel source-buffer)
	      (should (equal (buffer-string) "note\ncomment\nlate-note\n"))
	      (should (equal (nreverse rendered) '("note" "comment" "late-note")))
	      (should (equal (mapcar (lambda (position)
				       (plist-get (org-context-panel-item-at-position position) :id))
				     (org-context-panel-item-starts))
			     '("note" "comment" "late-note")))
	      (should (org-context-panel-goto-item-key "comment"))
	      (should (looking-at-p "comment")))))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-context-panel-renders-composed-items-at-viewport-rows ()
  "Composable provider rows are padded down to their viewport anchor rows."
  (let ((source-buffer (generate-new-buffer " *org context aligned source*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (org-context-panel-register-provider
	   (list :name 'test
		 :collect-side-items (lambda (_source) nil)
		 :render-side-item (lambda (_source item)
				     (insert (plist-get item :id) "\n"))))
	  (with-temp-buffer
	    (org-context-panel--render-composed-side-panel
	     source-buffer
	     (list (list :id "three" :provider 'test :anchor-line 3 :logical-anchor-line 30)
		   (list :id "six" :provider 'test :anchor-line 6 :logical-anchor-line 60))
	     (with-current-buffer source-buffer
	       (org-context-panel-registered-providers)))
	    (should (equal (split-string (buffer-string) "\n" nil)
			   '("" "" "three" "" "" "six" "")))))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-context-panel-source-navigation-jumps-between-provider-items ()
  "Source navigation moves between registered provider items."
  (let ((source-buffer (generate-new-buffer " *org context source nav*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "One\nTwo\nThree\n")
	  (let ((one (point-min))
		(two (save-excursion
		       (goto-char (point-min))
		       (forward-line 1)
		       (point)))
		(three (save-excursion
			 (goto-char (point-min))
			 (forward-line 2)
			 (point))))
	    (org-context-panel-register-provider
	     (list :name 'test
		   :collect-side-items
		   (lambda (_source)
		     (list (list :id "one" :source-start one)
			   (list :id "two" :source-start two)
			   (list :id "three" :source-start three)))
		   :jump-side-item
		   (lambda (source item)
		     (with-current-buffer source
		       (goto-char (plist-get item :source-start))))))
	    (goto-char one)
	    (org-context-panel-next-item)
	    (should (= (point) two))
	    (org-context-panel-next-item)
	    (should (= (point) three))
	    (org-context-panel-next-item)
	    (should (= (point) one))
	    (org-context-panel-previous-item)
	    (should (= (point) three))))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-context-panel-source-navigation-skips-folded-items ()
  "Source navigation skips context items hidden by Org folding."
  (let ((source-buffer (generate-new-buffer " *org context folded nav*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "* Visible\nShown body.\n* Folded\nHidden body.\n")
	  (let ((shown-pos (save-excursion
			     (goto-char (point-min))
			     (forward-line 1)
			     (point)))
		(hidden-pos (save-excursion
			      (goto-char (point-min))
			      (search-forward "Hidden body")
			      (match-beginning 0))))
	    (goto-char hidden-pos)
	    (org-back-to-heading t)
	    (if (fboundp 'org-fold-hide-subtree)
		(org-fold-hide-subtree)
	      (outline-hide-subtree))
	    (org-context-panel-register-provider
	     (list :name 'test
		   :collect-side-items
		   (lambda (_source)
		     (list (list :id "shown" :source-start shown-pos)
			   (list :id "hidden" :source-start hidden-pos)))
		   :jump-side-item
		   (lambda (source item)
		     (with-current-buffer source
		       (goto-char (plist-get item :source-start))))))
	    (goto-char (point-min))
	    (org-context-panel-next-item)
	    (should (= (point) shown-pos))
	    (org-context-panel-next-item)
	    (should (= (point) shown-pos))))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-context-panel-jump-at-point-dispatches-provider ()
  "Generic row jump dispatches through the item provider."
  (let ((source-buffer (generate-new-buffer " *org context jump source*"))
	(jumped nil))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (org-context-panel-register-provider
	   (list :name 'test
		 :collect-side-items (lambda (_source)
				       (list (list :id "row" :provider 'test)))
		 :render-side-item (lambda (_source item)
				     (insert (plist-get item :id) "\n"))
		 :jump-side-item (lambda (source item)
				   (setq jumped (list source item)))))
	  (with-temp-buffer
	    (setq-local org-context-panel-source-buffer source-buffer)
	    (org-context-panel-render-side-panel source-buffer)
	    (org-context-panel-jump-at-point)
	    (should (eq (car jumped) source-buffer))
	    (should (equal "row" (plist-get (cadr jumped) :id)))) )
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-context-panel-open-renders-provider-side-panel ()
  "Opening the generic side panel dispatches provider rendering."
  (let ((source-buffer (generate-new-buffer " *org context source test*"))
	(org-context-panel-buffer-name "*Org Context Panel Test*"))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "Alpha\n")
	  (org-context-panel-register-provider
	   (list :name 'test
		 :side-panel-buffer-name "*Org Context Provider Panel Test*"
		 :side-panel-width 42
		 :collect-side-items
		 (lambda (_source-buffer)
		   (list (list :id "a" :source-start (point-min))))
		 :render-side-panel
		 (lambda (source-buffer items)
		   (insert (format "Rendered %s for %s"
				   (length items)
				   (buffer-name source-buffer))))))
	  (set-window-buffer (selected-window) source-buffer)
	  (let ((panel-buffer (org-context-panel-open source-buffer)))
	    (should (equal (buffer-name panel-buffer)
			   "*Org Context Provider Panel Test*"))
	    (should (eq org-context-panel-side-panel-buffer panel-buffer))
	    (with-current-buffer panel-buffer
	      (should (derived-mode-p 'org-context-panel-buffer-mode))
	      (should (eq org-context-panel-source-buffer source-buffer))
	      (should (string-match-p "Rendered 1"
				      (buffer-string))))
	    (org-context-panel-close)
	    (should-not (buffer-live-p panel-buffer))
	    (should-not org-context-panel-side-panel-buffer)))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer))
      (when-let* ((panel-buffer (get-buffer org-context-panel-buffer-name)))
	(kill-buffer panel-buffer))
      (when-let* ((panel-buffer (get-buffer "*Org Context Provider Panel Test*")))
	(kill-buffer panel-buffer)))))

(ert-deftest org-context-panel-visible-side-panel-follows-selected-org-buffer ()
  "A visible side panel follows the selected Org buffer with providers."
  (let ((source-a (generate-new-buffer " *org context follow a*"))
	(source-b (generate-new-buffer " *org context follow b*"))
	(org-context-panel-buffer-name "*Org Context Follow Test*"))
    (unwind-protect
	(progn
	  (dolist (source (list source-a source-b))
	    (with-current-buffer source
	      (org-mode)
	      (insert (buffer-name source) "\n")
	      (org-context-panel-mode 1)
	      (org-context-panel-register-provider
	       (list :name 'test
		     :render-side-panel
		     (lambda (source-buffer _items)
		       (insert (format "Rendered %s"
				       (buffer-name source-buffer))))))))
	  (set-window-buffer (selected-window) source-a)
	  (let ((panel-buffer (org-context-panel-open source-a)))
	    (set-window-buffer (selected-window) source-b)
	    (org-context-panel-reconcile-windows)
	    (with-current-buffer panel-buffer
	      (should (eq org-context-panel-source-buffer source-b))
	      (should (string-match-p "Rendered  \\*org context follow b\\*"
				      (buffer-string))))))
      (dolist (source (list source-a source-b))
	(when (buffer-live-p source)
	  (kill-buffer source)))
      (when-let* ((panel-buffer (get-buffer org-context-panel-buffer-name)))
	(kill-buffer panel-buffer)))))

(ert-deftest org-context-panel-visible-side-panel-closes-outside-org ()
  "A visible side panel closes when selection leaves followable Org buffers."
  (let ((source (generate-new-buffer " *org context follow source*"))
	(other (generate-new-buffer " *org context follow other*"))
	(org-context-panel-buffer-name "*Org Context Follow Close Test*"))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (org-context-panel-mode 1)
	  (org-context-panel-register-provider
	   (list :name 'test
		 :render-side-panel (lambda (_source-buffer _items)
				      (insert "Rendered"))))
	  (set-window-buffer (selected-window) source)
	  (let ((panel-buffer (org-context-panel-open source)))
	    (set-window-buffer (selected-window) other)
	    (org-context-panel-reconcile-windows)
	    (should-not (buffer-live-p panel-buffer))))
      (when (buffer-live-p source)
	(kill-buffer source))
      (when (buffer-live-p other)
	(kill-buffer other))
      (when-let* ((panel-buffer (get-buffer org-context-panel-buffer-name)))
	(kill-buffer panel-buffer)))))

(ert-deftest org-context-panel-side-panel-lifecycle-hooks-have-context ()
  "Side-panel lifecycle hooks receive dynamic source and panel context."
  (let ((source-buffer (generate-new-buffer " *org context hook source*"))
	(org-context-panel-buffer-name "*Org Context Hook Test*")
	opened closed)
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (org-context-panel-mode 1)
	  (org-context-panel-register-provider
	   (list :name 'test
		 :render-side-panel (lambda (_source-buffer _items)
				      (insert "Rendered"))))
	  (set-window-buffer (selected-window) source-buffer)
	  (let ((org-context-panel-after-side-panel-open-hook
		 (list (lambda ()
			 (setq opened
			       (list org-context-panel-current-source-buffer
				     org-context-panel-current-panel-buffer
				     org-context-panel-current-source-window
				     org-context-panel-current-panel-window)))))
		(org-context-panel-before-side-panel-close-hook
		 (list (lambda ()
			 (setq closed
			       (list org-context-panel-current-source-buffer
				     org-context-panel-current-panel-buffer
				     org-context-panel-current-source-window
				     org-context-panel-current-panel-window))))))
	    (let ((panel-buffer (org-context-panel-open source-buffer)))
	      (should (eq (nth 0 opened) source-buffer))
	      (should (eq (nth 1 opened) panel-buffer))
	      (should (window-live-p (nth 2 opened)))
	      (should (window-live-p (nth 3 opened)))
	      (org-context-panel-close)
	      (should (eq (nth 0 closed) source-buffer))
	      (should (eq (nth 1 closed) panel-buffer))
	      (should (window-live-p (nth 2 closed))))))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer))
      (when-let* ((panel-buffer (get-buffer org-context-panel-buffer-name)))
	(kill-buffer panel-buffer)))))

(ert-deftest org-context-panel-open-bottom-view-renders-provider-view ()
  "Bottom views are opened and rendered from provider descriptors."
  (let ((source-buffer (generate-new-buffer " *org context bottom source*"))
	(org-context-panel-bottom-buffer-name "*Org Context Bottom Test*"))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (org-context-panel-register-provider
	   (list :name 'test
		 :collect-bottom-views
		 (lambda (_source-buffer)
		   (list (list :id 'details
			       :buffer-name org-context-panel-bottom-buffer-name
			       :render (lambda (source-buffer view)
					 (insert (format "View %s for %s"
							 (plist-get view :id)
							 (buffer-name source-buffer)))))))))
	  (let ((panel-buffer (org-context-panel-open-bottom-view 'details source-buffer)))
	    (should (eq org-context-panel-bottom-panel-buffer panel-buffer))
	    (with-current-buffer panel-buffer
	      (should (derived-mode-p 'org-context-panel-buffer-mode))
	      (should (eq org-context-panel-source-buffer source-buffer))
	      (should (eq org-context-panel-view-id 'details))
	      (should (string-match-p "View details"
				      (buffer-string))))
	    (org-context-panel-close-bottom-view)
	    (should-not (buffer-live-p panel-buffer))
	    (should-not org-context-panel-bottom-panel-buffer)))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer))
      (when-let* ((panel-buffer (get-buffer org-context-panel-bottom-buffer-name)))
	(kill-buffer panel-buffer)))))

(ert-deftest org-context-panel-window-projection-keeps-unanchored-items ()
  "Viewport projection keeps global items that have no source anchor."
  (let ((source-buffer (generate-new-buffer " *org context source*")))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "Alpha\nBeta\n")
	  (let ((window (selected-window)))
	    (set-window-buffer window source-buffer)
	    (org-context-panel-register-provider
	     (list :name 'test
		   :collect-side-items
		   (lambda (_source)
		     (list (list :id "anchored" :source-start (point-min))
			   (list :id "global")))))
	    (let ((items (org-context-panel-items-for-window
			  window source-buffer)))
	      (should (= (length items) 2))
	      (should (cl-find "anchored" items
			       :key (lambda (item) (plist-get item :id))
			       :test #'equal))
	      (should (cl-find "global" items
			       :key (lambda (item) (plist-get item :id))
			       :test #'equal)))))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer)))))

(ert-deftest org-context-panel-open-renders-default-side-panel ()
  "Opening the generic side panel has a fallback renderer."
  (let ((source-buffer (generate-new-buffer " *org context default source*"))
	(org-context-panel-buffer-name "*Org Context Default Panel Test*"))
    (unwind-protect
	(with-current-buffer source-buffer
	  (org-mode)
	  (insert "Alpha\n")
	  (set-window-buffer (selected-window) source-buffer)
	  (let ((panel-buffer (org-context-panel-open source-buffer)))
	    (with-current-buffer panel-buffer
	      (should (string-match-p "No visible context items"
				      (buffer-string))))
	    (org-context-panel-close)))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer))
      (when-let* ((panel-buffer (get-buffer org-context-panel-buffer-name)))
	(kill-buffer panel-buffer)))))

(ert-deftest org-context-panel-mode-owns-refresh-hook-and-cleanup ()
  "The generic mode installs refresh hooks and dispatches cleanup."
  (with-temp-buffer
    (let (cleaned)
      (org-context-panel-register-provider
       (list :name 'test
	     :refresh-source-overlays #'ignore
	     :cleanup-source-overlays (lambda () (setq cleaned t))))
      (org-context-panel-mode 1)
      (should org-context-panel-mode)
      (should (memq #'org-context-panel--refresh-after-save after-save-hook))
      (org-context-panel-mode -1)
      (should-not org-context-panel-mode)
      (should-not (memq #'org-context-panel--refresh-after-save after-save-hook))
      (should cleaned))))

(ert-deftest org-context-panel-refresh-source-overlays-dispatches-providers ()
  "Refreshing source overlays dispatches to registered providers."
  (with-temp-buffer
    (let (calls)
      (org-context-panel-register-provider
       (list :name 'first
	     :refresh-source-overlays (lambda () (push 'first calls))))
      (org-context-panel-register-provider
       (list :name 'second
	     :refresh-source-overlays (lambda () (push 'second calls))))
      (org-context-panel-refresh-source-overlays)
      (should (equal (nreverse calls) '(first second))))))

(ert-deftest org-context-panel-metadata-end-position-skips-leading-keywords ()
  "Metadata end position follows leading Org keyword lines."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Example\n#+FILETAGS: :demo:\n\nBody\n")
    (should (= (org-context-panel-metadata-end-position)
	       (progn
		 (goto-char (point-min))
		 (forward-line 2)
		 (point))))))

(ert-deftest org-context-panel-range-overlay-keeps-provider-properties ()
  "Range overlays preserve provider-owned properties."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha beta\n")
    (let ((overlay (org-context-panel-make-range-overlay
		    1 6
		    :face 'highlight
		    :properties '(:provider comments :id "c1"))))
      (should (overlayp overlay))
      (should (eq (overlay-get overlay 'face) 'highlight))
      (should (eq (overlay-get overlay :provider) 'comments))
      (should (equal (overlay-get overlay :id) "c1")))))

(ert-deftest org-context-panel-refresh-top-markers-renders-provider-descriptors ()
  "Top markers are rendered from registered provider descriptors."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Example\n\nBody\n")
    (org-context-panel-register-provider
     (list :name 'test
	   :collect-top-markers
	   (lambda (_source-buffer)
	     (list (list :id 'notice
			 :label "[Notice]"
			 :help "Open notice"
			 :overlay-variable
			 'org-context-panel-test--marker-overlay)))))
    (org-context-panel-refresh-top-markers)
    (should (overlayp org-context-panel-test--marker-overlay))
    (should (eq (overlay-get org-context-panel-test--marker-overlay
			     'org-context-panel-provider)
		'test))
    (should (string-match-p "\\[Notice\\]"
			    (overlay-get org-context-panel-test--marker-overlay
					 'after-string)))
    (org-context-panel-delete-top-markers)
    (should-not (overlayp org-context-panel-test--marker-overlay))))

(ert-deftest org-context-panel-top-marker-is-actionable-overlay ()
  "Top markers render labels, help, and keymap on overlay text."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Example\n\nBody\n")
    (let* ((map (make-sparse-keymap))
	   (overlay (org-context-panel-make-top-marker
		     :label "[Marker]"
		     :keymap map
		     :help-echo "Open marker")))
      (should (overlayp overlay))
      (should (= (overlay-start overlay)
		 (progn
		   (goto-char (point-min))
		   (forward-line 1)
		   (point))))
      (should (eq (overlay-get overlay 'keymap) map))
      (should (string-match-p "\\[Marker\\]"
			      (overlay-get overlay 'after-string))))))

(provide 'org-context-panel-test)
;;; org-context-panel-test.el ends here

(defun org-context-panel-test--reset-workspace-state ()
  "Reset generic context-panel workspace state for tests."
  (setq org-context-panel--desired-side-panel-p nil)
  (setq org-context-panel--desired-bottom-view-id nil)
  (setq org-context-panel--last-source-buffer nil)
  (setq org-context-panel--temporarily-hidden-p nil)
  (when (timerp org-context-panel--reconcile-timer)
    (cancel-timer org-context-panel--reconcile-timer))
  (setq org-context-panel--reconcile-timer nil))

(defun org-context-panel-test--source-buffer (name label)
  "Return Org source buffer NAME with test provider rendering LABEL."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (org-mode)
      (insert "* Heading\nBody.\n")
      (org-context-panel-register-provider
       (list :name 'test
	     :render-side-panel (lambda (_source _items)
				  (insert "Side " label "\n"))
	     :collect-bottom-views
	     (lambda (_source)
	       (list (list :id 'test-bottom
			   :buffer-name "*Org Context Test Bottom*"
			   :render (lambda (_source _view)
				     (insert "Bottom " label "\n"))))))))
    buffer))

(ert-deftest org-context-panel-reconciler-hides-and-restores-panels ()
  "Panels hide without a visible source and restore for the next Org source."
  (org-context-panel-test--reset-workspace-state)
  (let ((source-a (org-context-panel-test--source-buffer
		   " *org context source a*" "A"))
	(source-b (org-context-panel-test--source-buffer
		   " *org context source b*" "B"))
	(other (generate-new-buffer " *org context other*")))
    (unwind-protect
	(progn
	  (set-window-buffer (selected-window) source-a)
	  (org-context-panel-open source-a)
	  (org-context-panel-open-bottom-view 'test-bottom source-a)
	  (should (org-context-panel--visible-side-panel-window))
	  (should (org-context-panel--visible-bottom-panel-window))
	  (set-window-buffer (selected-window) other)
	  (org-context-panel-reconcile-windows)
	  (should-not (org-context-panel--visible-side-panel-window))
	  (should-not (org-context-panel--visible-bottom-panel-window))
	  (should org-context-panel--desired-side-panel-p)
	  (should (eq org-context-panel--desired-bottom-view-id 'test-bottom))
	  (set-window-buffer (selected-window) source-b)
	  (org-context-panel-reconcile-windows)
	  (should (org-context-panel--visible-side-panel-window))
	  (should (org-context-panel--visible-bottom-panel-window))
	  (with-current-buffer (window-buffer
				(org-context-panel--visible-side-panel-window))
	    (should (eq org-context-panel-source-buffer source-b))
	    (should (string-match-p "Side B" (buffer-string))))
	  (with-current-buffer (window-buffer
				(org-context-panel--visible-bottom-panel-window))
	    (should (eq org-context-panel-source-buffer source-b))
	    (should (string-match-p "Bottom B" (buffer-string)))))
      (org-context-panel-test--reset-workspace-state)
      (dolist (buffer (list source-a source-b other
			    (get-buffer "*Org Context Test Bottom*")
			    (get-buffer org-context-panel-buffer-name)))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(ert-deftest org-context-panel-explicit-close-clears-desired-layout ()
  "Explicit side-panel close prevents automatic restore."
  (org-context-panel-test--reset-workspace-state)
  (let ((source (org-context-panel-test--source-buffer
		 " *org context close source*" "A"))
	(other (generate-new-buffer " *org context close other*")))
    (unwind-protect
	(progn
	  (set-window-buffer (selected-window) source)
	  (org-context-panel-open source)
	  (with-current-buffer source
	    (org-context-panel-close))
	  (should-not org-context-panel--desired-side-panel-p)
	  (set-window-buffer (selected-window) other)
	  (org-context-panel-reconcile-windows)
	  (set-window-buffer (selected-window) source)
	  (org-context-panel-reconcile-windows)
	  (should-not (org-context-panel--visible-side-panel-window)))
      (org-context-panel-test--reset-workspace-state)
      (dolist (buffer (list source other (get-buffer org-context-panel-buffer-name)))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(ert-deftest org-context-panel-reconciler-preserves-bottom-buffer-point ()
  "Reconciliation does not rerender an already-correct bottom panel."
  (org-context-panel-test--reset-workspace-state)
  (let ((source (org-context-panel-test--source-buffer
		 " *org context point source*" "A")))
    (unwind-protect
	(progn
	  (set-window-buffer (selected-window) source)
	  (org-context-panel-open-bottom-view 'test-bottom source)
	  (let* ((bottom-window (org-context-panel--visible-bottom-panel-window))
		 (bottom-buffer (window-buffer bottom-window)))
	    (select-window bottom-window)
	    (with-current-buffer bottom-buffer
	      (goto-char (point-max)))
	    (org-context-panel-reconcile-windows)
	    (should (eq (selected-window) bottom-window))
	    (with-current-buffer bottom-buffer
	      (should (= (point) (point-max))))))
      (org-context-panel-test--reset-workspace-state)
      (dolist (buffer (list source
			    (get-buffer "*Org Context Test Bottom*")))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))
