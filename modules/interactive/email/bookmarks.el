;;; bookmarks.el --- Email: mu4e bookmarks and noise -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom bookmarks and a helper to build a unified "noise" query.

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

(with-eval-after-load 'mu4e
  (setq mu4e-bookmarks
	'((:name "Inbox"        :query "maildir:/inbox/ AND NOT flag:trashed" :key ?i)
	  (:name "behaghel.org" :query "maildir:/behaghel.org/inbox AND NOT flag:trashed" :key ?t)
	  (:name "GMail"        :query "maildir:/gmail/inbox AND NOT flag:trashed" :key ?g)
	  (:name "behaghel.fr"  :query "maildir:/behaghel.fr/inbox AND NOT flag:trashed" :key ?t)
	  (:name "Important"    :query "flag:flagged AND NOT flag:trashed" :key ?f)
	  (:name "Drafts"       :query "maildir:/drafts/ AND NOT flag:trashed" :key ?d)
	  (:name "Today"        :query "date:today..now AND NOT maildir:/gmail/inbox AND NOT flag:trashed" :key ?h)
	  (:name "Attachments"  :query "flag:attach" :key ?a)
	  (:name "Invites"      :query "mime:text/calendar" :key ?c)))

  (let ((noise (hub/build-noise-query)))
    (when noise
      (add-to-list 'mu4e-bookmarks
		   `(:name "Noise" :key ?n :query ,(concat "maildir:/inbox/ AND (" noise ")"))))))

(provide 'email/bookmarks)
;;; bookmarks.el ends here
