;;; context-panel-providers-test.el --- Mixed provider integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real Marginalia and comments providers without sidecars or accounts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-comments)
(require 'cp-org-marginalia-context-panel)

(ert-deftest hub/context-panel-marginalia-and-comments-preserve-row-actions ()
  "Standard Org note rows coexist with rich comments and their custom actions."
  (save-window-excursion
    (with-temp-buffer
      (org-mode)
      (insert "Alpha[fn:one] beta.\n\n[fn:one] *Native note*.\n")
      (set-buffer-modified-p nil)
      (let* ((source (current-buffer))
	     (content (buffer-string))
	     (reference (plist-get (car (cp-org-marginalia-collect)) :reference-pos))
	     (comment '(:type comment :id "c1" :status "OPEN" :author "Alice"
			      :target-start 1 :target-end 6 :target-text "Alpha"
			      :body "Comment body.")))
	(cl-letf (((symbol-function 'org-comments-collect)
		   (lambda (&rest _) (list comment))))
	  (context-panels-register-provider (org-comments-context-panel-provider))
	  (cp-org-marginalia-context-panel-mode 1)
	  (with-temp-buffer
	    (org-comments-panel-mode)
	    (setq-local context-panels-source-buffer source)
	    (let ((panel (current-buffer))
		  (reply-binding (key-binding (kbd "r")))
		  (edit-binding (key-binding (kbd "e"))))
	      (context-panels-render-side-panel source)
	      (search-forward "Native note")
	      (should (eq (plist-get (context-panels-item-at-point) :provider)
			  'marginalia))
	      (should (eq (key-binding (kbd "<return>"))
			  #'context-panels-jump-at-point))
	      (context-panels-jump-at-point)
	      (should (eq (current-buffer) source))
	      (should (= (point) reference))
	      (with-current-buffer panel
		(goto-char (point-min))
		(search-forward "Comment body.")
		(should (get-text-property (1- (point)) 'org-comments-comment))
		(should (eq (lookup-key (current-local-map) (kbd "r"))
			    #'org-comments-reply))
		(should (eq (lookup-key (current-local-map) (kbd "e"))
			    #'org-comments-edit))
		(should (eq (key-binding (kbd "r")) reply-binding))
		(should (eq (key-binding (kbd "e")) edit-binding))
		(should (string-match-p "r: reply"
					(get-text-property (point) 'help-echo)))
		(context-panels-jump-at-point)
		(should (eq (current-buffer) source))
		(should (= (point) 1)))))
	  (cp-org-marginalia-context-panel-mode -1)
	  (should (context-panels-registered-provider 'comments))
	  (should context-panels-mode)
	  (context-panels-unregister-provider 'comments)
	  (context-panels-mode -1)
	  (should (equal (buffer-string) content))
	  (should-not (buffer-modified-p)))))))

(provide 'context-panel-providers-test)
;;; context-panel-providers-test.el ends here
