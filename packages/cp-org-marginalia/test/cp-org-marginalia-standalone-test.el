;;; cp-org-marginalia-standalone-test.el --- Standalone marginalia tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise package metadata and native note display/navigation with only
;; Emacs, Org, and context-panels, without personal configuration or comments.

;;; Code:

(require 'ert)
(require 'org)
(require 'package)

(ert-deftest cp-org-marginalia-standalone-declares-panel-dependency ()
  "The renamed entrypoints declare their context-panels dependency."
  (let ((library (locate-library "cp-org-marginalia")))
    (should library)
    (dolist (name '("cp-org-marginalia.el" "cp-org-marginalia-context-panel.el"))
      (with-temp-buffer
	(insert-file-contents (expand-file-name name (file-name-directory library)))
	(let ((dependency (assq 'context-panels
				(package-desc-reqs (package-buffer-info)))))
	  (should dependency)
	  (should (version-list-<= '(0 1 0) (cadr dependency))))))))

(ert-deftest cp-org-marginalia-standalone-displays-notes-and-jumps-to-references ()
  "Both note kinds render without metadata and jump to their prose reference."
  (require 'cp-org-marginalia-context-panel)
  (let* ((example (expand-file-name
		   "examples/standalone.org"
		   (file-name-directory (locate-library "cp-org-marginalia"))))
	 (directory (make-temp-file "cp-org-marginalia-standalone" t))
	 (source-file (expand-file-name "draft.org" directory))
	 source panel)
    (unwind-protect
	(save-window-excursion
	  (copy-file example source-file)
	  (setq source (find-file-noselect source-file))
	  (switch-to-buffer source)
	  (let ((content (buffer-string))
		(notes (cp-org-marginalia-collect)))
	    (should (= (length notes) 2))
	    (should (equal (mapcar (lambda (note) (plist-get note :kind)) notes)
			   '(sidenote footnote)))
	    (should (equal (plist-get (cadr notes) :status) "open"))
	    (should (equal (plist-get (cadr notes) :source) "local"))
	    (should (equal (plist-get (cadr notes) :remote-id) "demo-foot"))
	    (cp-org-marginalia-context-panel-mode 1)
	    (context-panels-open source)
	    (setq panel context-panels-side-panel-buffer)
	    (should (buffer-live-p panel))
	    (with-current-buffer panel
	      (should-not (string-match-p "HUB_NOTE_" (buffer-string))))
	    (dolist (note notes)
	      (with-current-buffer panel
		(select-window (get-buffer-window panel))
		(goto-char (point-min))
		(search-forward (if (eq (plist-get note :kind) 'sidenote)
				    "Sidenote body." "Traditional footnote body."))
		(execute-kbd-macro
		 (kbd (if (eq (plist-get note :kind) 'sidenote)
			  "RET" "<return>"))))
	      (should (eq (window-buffer (selected-window)) source))
	      (with-current-buffer source
		(should (= (point) (plist-get note :reference-pos)))
		(should (< (point) (plist-get note :definition-pos)))))
	    (with-current-buffer source
	      (cp-org-marginalia-context-panel-mode -1)
	      (should-not (context-panels-registered-provider 'marginalia))
	      (should (equal (buffer-string) content))
	      (should-not (buffer-modified-p)))
	    (should (equal (directory-files directory nil "\\.org\\'")
			   '("draft.org")))))
      (when (buffer-live-p source)
	(with-current-buffer source
	  (when (bound-and-true-p cp-org-marginalia-context-panel-mode)
	    (cp-org-marginalia-context-panel-mode -1)))
	(kill-buffer source))
      (when (buffer-live-p panel) (kill-buffer panel))
      (delete-directory directory t))))

(provide 'cp-org-marginalia-standalone-test)
;;; cp-org-marginalia-standalone-test.el ends here
