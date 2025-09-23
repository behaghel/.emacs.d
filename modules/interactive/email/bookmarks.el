;;; bookmarks.el --- Email: mu4e bookmarks and noise -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom bookmarks and a helper to build a unified "noise" query.

;;; Code:

(require 'hub-utils)
(require 'email/noise)

(with-eval-after-load 'mu4e
  (setq mu4e-bookmarks
	'(
	  (:name "Work"         :query "maildir:/work/inbox AND NOT flag:trashed" :key ?w)
	  (:name "GMail"        :query "maildir:/gmail/inbox AND NOT flag:trashed" :key ?g)
	  (:name "behaghel.org" :query "maildir:/behaghel.org/inbox AND NOT flag:trashed" :key ?o)
	  (:name "behaghel.fr"  :query "maildir:/behaghel.fr/inbox AND NOT flag:trashed" :key ?f)
	  (:name "All inboxes"  :query "maildir:/inbox/ AND NOT flag:trashed" :key ?i)
	  (:name "Work Archive" :query "maildir:/work/archive AND NOT maildir:/work/inbox AND NOT flag:trashed" :key ?W)
	  (:name "GMail Archive":query "maildir:/gmail/archive AND NOT maildir:/gmail/inbox AND NOT flag:trashed" :key ?G)
	  (:name "behaghel.org Archive" :query "maildir:/behaghel.org/archive AND NOT maildir:/behaghel.org/inbox AND NOT flag:trashed" :key ?O)
	  (:name "behaghel.fr Archive"  :query "maildir:/behaghel.fr/archive AND NOT maildir:/behaghel.fr/inbox AND NOT flag:trashed" :key ?F)
	  (:name "Important"    :query "flag:flagged AND NOT flag:trashed" :key ?!)
	  (:name "Drafts"       :query "maildir:/drafts/ AND NOT flag:trashed" :key ?d)
	  (:name "Today"        :query "date:today..now AND NOT maildir:/gmail/inbox AND NOT flag:trashed" :key ?h)
	  (:name "Attachments"  :query "flag:attach" :key ?a)
	  (:name "Invites"      :query "mime:text/calendar" :key ?c))
	)

  (let ((noise (hub/build-noise-query)))
    (when noise
      (add-to-list 'mu4e-bookmarks
		   `(:name "Noise" :key ?n :query ,(concat "maildir:/inbox/ AND (" noise ")"))))))

(provide 'email/bookmarks)
;;; bookmarks.el ends here
