;;; cp-org-marginalia-context-panel-test.el --- Tests for marginalia provider -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org marginalia context-panel integration.

;;; Code:

(require 'ert)
(require 'org)
(require 'context-panels)
(require 'cp-org-marginalia-context-panel)

(ert-deftest cp-org-marginalia-context-panel-mode-registers-provider ()
  "Marginalia mode registers its context-panel provider."
  (with-temp-buffer
    (org-mode)
    (cp-org-marginalia-context-panel-mode 1)
    (should (context-panels-registered-provider 'marginalia))
    (should (equal (plist-get (context-panels-registered-provider 'marginalia)
			      :icon)
		   "📝"))
    (should context-panels-mode)
    (let ((provider (context-panels-registered-provider 'marginalia)))
      (should (functionp (plist-get provider :present-side-item)))
      (should-not (plist-get provider :render-side-item))
      (should-not (plist-get provider :jump-side-item)))
    (cp-org-marginalia-context-panel-mode -1)
    (should-not (context-panels-registered-provider 'marginalia))))

(ert-deftest cp-org-marginalia-context-panel-renders-footnote-row ()
  "Marginalia provider renders Org-fontified footnote rows."
  (let ((source (generate-new-buffer " *org marginalia source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Text[fn:one]\n\n[fn:one] *Bold* note.\n")
	  (cp-org-marginalia-context-panel-mode 1)
	  (with-temp-buffer
	    (setq-local context-panels-source-buffer source)
	    (context-panels-render-side-panel source)
	    (should (search-forward "✣" nil t))
	    (goto-char (point-min))
	    (should (search-forward "Bold" nil t))
	    (should (get-text-property (point) 'context-panels-item))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest cp-org-marginalia-context-panel-generic-jump-dispatches-to-reference ()
  "Generic context-panel row jump moves to the source footnote reference."
  (let ((source (generate-new-buffer " *org marginalia generic jump source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Text[fn:one]\n\n[fn:one] Note.\n")
	  (cp-org-marginalia-context-panel-mode 1)
	  (let ((panel (generate-new-buffer " *org marginalia generic panel*")))
	    (unwind-protect
		(with-current-buffer panel
		  (context-panels-buffer-mode)
		  (setq-local context-panels-source-buffer source)
		  (context-panels-render-side-panel source)
		  (goto-char (point-min))
		  (search-forward "Note")
		  (context-panels-jump-at-point)
		  (should (eq (current-buffer) source))
		  (should (looking-at-p "\\[fn:one\\]")))
	      (when (buffer-live-p panel)
		(kill-buffer panel)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest cp-org-marginalia-context-panel-jumps-to-reference ()
  "Marginalia-specific row jump moves to its source footnote reference."
  (let ((source (generate-new-buffer " *org marginalia jump source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Text[fn:one]\n\n[fn:one] Note.\n")
	  (cp-org-marginalia-context-panel-mode 1)
	  (let ((panel (generate-new-buffer " *org marginalia panel*")))
	    (unwind-protect
		(with-current-buffer panel
		  (context-panels-buffer-mode)
		  (setq-local context-panels-source-buffer source)
		  (context-panels-render-side-panel source)
		  (goto-char (point-min))
		  (search-forward "Note")
		  (cp-org-marginalia-context-panel-jump-at-point)
		  (should (eq (current-buffer) source))
		  (should (looking-at-p "\\[fn:one\\]")))
	      (when (buffer-live-p panel)
		(kill-buffer panel)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest cp-org-marginalia-context-panel-keys-are-row-local ()
  "Note-row keys preserve other providers' major-mode bindings."
  (with-temp-buffer
    (special-mode)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'ignore)
      (use-local-map map)
      (let ((inhibit-read-only t))
	(insert "Other provider row\n")
	(cp-org-marginalia-context-panel-render-side-item
	 nil '(:id "local-note" :kind sidenote :body "Note body.")))
      (goto-char (point-min))
      (should (eq (key-binding (kbd "RET")) #'ignore))
      (search-forward "local-note")
      (should (eq (key-binding (kbd "RET")) #'context-panels-jump-at-point))
      (should (eq (key-binding (kbd "<return>")) #'context-panels-jump-at-point))
      (goto-char (point-min))
      (should (eq (key-binding (kbd "RET")) #'ignore)))))

(ert-deftest cp-org-marginalia-context-panel-collects-semantics-without-placement ()
  "Provider collection preserves note semantics and leaves placement to panels."
  (with-temp-buffer
    (org-mode)
    (insert "Text[fn:one]\n\n[fn:one] Body.\n")
    (let ((item (car (cp-org-marginalia-context-panel-collect-side-items
		      (current-buffer)))))
      (should (equal (plist-get item :id) "one"))
      (should-not (plist-member item :icon))
      (cp-org-marginalia-context-panel-mode 1)
      (should (equal (plist-get (car (context-panels-collect-side-items)) :icon) "✣"))
      (cp-org-marginalia-context-panel-mode -1)
      (should (equal (plist-get item :body) "Body."))
      (should (integerp (plist-get item :reference-pos)))
      (should-not (plist-member item :display-line))
      (should-not (plist-member item :displaced)))))

(provide 'cp-org-marginalia-context-panel-test)
;;; cp-org-marginalia-context-panel-test.el ends here
