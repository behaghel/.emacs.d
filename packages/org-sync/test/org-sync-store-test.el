;;; org-sync-store-test.el --- Org sync tracking sidecar tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for provider-neutral Org sync tracking sidecars.

;;; Code:

(require 'ert)
(require 'org-sync)

(ert-deftest org-sync-store-roundtrips-tracking-data ()
  "Tracking data is stored in an Org-readable sync sidecar."
  (let* ((directory (make-temp-file "org-sync-store" t))
	 (source-file (expand-file-name "source.org" directory))
	 (tracking '(:provider (:kind fake :remote-id "doc-1")
			       :domains ((content :fetched-remote-ref (:version "1"))))))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "Body\n"))
	  (org-sync-store-write source-file tracking)
	  (let ((sidecar (org-sync-store-path source-file)))
	    (should (file-exists-p sidecar))
	    (with-temp-buffer
	      (insert-file-contents sidecar)
	      (should (search-forward "#+TITLE: Org Sync Tracking" nil t))
	      (should (search-forward "#+BEGIN_SRC emacs-lisp" nil t)))
	    (should (equal (org-sync-store-read source-file) tracking))))
      (delete-directory directory t))))

(provide 'org-sync-store-test)
;;; org-sync-store-test.el ends here
