;;; org-context-panel-test.el --- Context panel primitive tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for reusable Org context panel primitives.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-context-panel)

(defvar-local org-context-panel-test--marker-overlay nil
  "Top marker overlay used by context-panel tests.")

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
	    (org-context-panel--follow-selected-buffer)
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
	    (org-context-panel--follow-selected-buffer)
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
