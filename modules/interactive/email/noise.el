;;; noise.el --- Email: shared noise predicates -*- lexical-binding: t; -*-

;;; Commentary:
;; Central definition and helpers for noise predicates used by mu4e bookmarks
;; and interactive actions.

;;; Code:

(require 'hub-utils)

(defvar hub/noise-predicates nil
  "Plist list of (:name :query [:category]) entries considered noisy.")

(defun hub/build-noise-query ()
  "Build a disjunction query from `hub/noise-predicates'."
  (when (and (listp hub/noise-predicates) hub/noise-predicates)
    (let* ((getq (lambda (entry) (concat "(" (plist-get entry :query) ")")))
	   (acc (funcall getq (car hub/noise-predicates))))
      (dolist (entry (cdr hub/noise-predicates) acc)
	(setq acc (concat (funcall getq entry) " OR " acc))))))

(provide 'email/noise)
;;; noise.el ends here
