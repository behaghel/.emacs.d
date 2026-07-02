;;; org-marginalia-context-panel-test.el --- Tests for marginalia provider -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org marginalia context-panel integration.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-context-panel)
(require 'org-marginalia-context-panel)

(ert-deftest org-marginalia-context-panel-mode-registers-provider ()
  "Marginalia mode registers its context-panel provider."
  (with-temp-buffer
    (org-mode)
    (org-marginalia-context-panel-mode 1)
    (should (org-context-panel-registered-provider 'marginalia))
    (should org-context-panel-mode)
    (org-marginalia-context-panel-mode -1)
    (should-not (org-context-panel-registered-provider 'marginalia))))

(ert-deftest org-marginalia-context-panel-renders-footnote-row ()
  "Marginalia provider renders Org-fontified footnote rows."
  (let ((source (generate-new-buffer " *org marginalia source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Text[fn:one]\n\n[fn:one] *Bold* note.\n")
	  (org-marginalia-context-panel-mode 1)
	  (with-temp-buffer
	    (setq-local org-context-panel-source-buffer source)
	    (org-context-panel-render-side-panel source)
	    (should (search-forward "✣" nil t))
	    (goto-char (point-min))
	    (should (search-forward "Bold" nil t))
	    (should (get-text-property (point) 'org-marginalia-item))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-marginalia-context-panel-generic-jump-dispatches-to-reference ()
  "Generic context-panel row jump moves to the source footnote reference."
  (let ((source (generate-new-buffer " *org marginalia generic jump source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Text[fn:one]\n\n[fn:one] Note.\n")
	  (org-marginalia-context-panel-mode 1)
	  (let ((panel (generate-new-buffer " *org marginalia generic panel*")))
	    (unwind-protect
		(with-current-buffer panel
		  (org-context-panel-buffer-mode)
		  (setq-local org-context-panel-source-buffer source)
		  (org-context-panel-render-side-panel source)
		  (goto-char (point-min))
		  (search-forward "Note")
		  (org-context-panel-jump-at-point)
		  (should (eq (current-buffer) source))
		  (should (looking-at-p "\\[fn:one\\]")))
	      (when (buffer-live-p panel)
		(kill-buffer panel)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-marginalia-context-panel-jumps-to-reference ()
  "Marginalia-specific row jump moves to the source footnote reference."
  (let ((source (generate-new-buffer " *org marginalia jump source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Text[fn:one]\n\n[fn:one] Note.\n")
	  (org-marginalia-context-panel-mode 1)
	  (let ((panel (generate-new-buffer " *org marginalia panel*")))
	    (unwind-protect
		(with-current-buffer panel
		  (org-context-panel-buffer-mode)
		  (setq-local org-context-panel-source-buffer source)
		  (org-context-panel-render-side-panel source)
		  (goto-char (point-min))
		  (search-forward "Note")
		  (org-marginalia-context-panel-jump-at-point)
		  (should (eq (current-buffer) source))
		  (should (looking-at-p "\\[fn:one\\]")))
	      (when (buffer-live-p panel)
		(kill-buffer panel)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(provide 'org-marginalia-context-panel-test)
;;; org-marginalia-context-panel-test.el ends here
