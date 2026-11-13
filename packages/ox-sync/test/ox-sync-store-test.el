;;; ox-sync-store-test.el --- Org sync tracking sidecar tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for provider-neutral Org sync tracking sidecars.

;;; Code:

(require 'ert)
(require 'ox-sync)

(ert-deftest ox-sync-store-roundtrips-tracking-data ()
  "Tracking data is stored in an Org-readable sync sidecar."
  (let* ((directory (make-temp-file "ox-sync-store" t))
	 (source-file (expand-file-name "source.org" directory))
	 (tracking '(:provider (:kind fake :remote-id "doc-1")
			       :domains ((content :fetched-remote-ref (:version "1"))))))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "Body\n"))
	  (ox-sync-store-write source-file tracking)
	  (let ((sidecar (ox-sync-store-path source-file)))
	    (should (file-exists-p sidecar))
	    (with-temp-buffer
	      (insert-file-contents sidecar)
	      (should (search-forward "#+TITLE: Org Sync Tracking" nil t))
	      (should (search-forward "#+BEGIN_SRC emacs-lisp" nil t)))
	    (should (equal (ox-sync-store-read source-file) tracking))))
      (delete-directory directory t))))

(provide 'ox-sync-store-test)
;;; ox-sync-store-test.el ends here
