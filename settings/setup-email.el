;;; setup-email.el --- Config to manage my emails    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mu4e is my email client in Emacs

;;; Code:
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; FIXME: only activate when on chromebook
(defun make-tmp-file-browsable ()
"Allow temporary files to be accessed by the browser.
On crostini (chromebook) /tmp isn't visible to Chrome breaking
most org export / preview in the browser."
  (interactive)
  (setq-local temporary-file-directory "~/tmp"))

;; https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html#Installation
;; mu4e is part of the mu project, a UNIX CLI therefore not on MELPA
;; I built it from git repo
;; I used $ ./configure.sh --datadir=/path/to/emacs/build/dir
;; that way mu build put all the mu4e elisp files in my load-path on
;; `make install' step
(use-package mu4e
  :ensure nil
  :pin manual
  :config
  ;; a key for
  ;; Search: ê (requête)
  ;; Thread: é
  ;; Refile: à (agenda-prime as alternative to reach agenda)
  ;; In All Views
  ;; "J" mu4e~headers-jump-to-maildir
  ;; "C" mu4e-compose-new
  ;; ";" mu4e-context-switch
  ;; "b" mu4e-headers-search-bookmark
  ;; "B" mu4e-headers-search-bookmark-edit
  ;; "k" mu4e-headers-search
  ;; "ê" mu4e-headers-search

  ;; Main view
  ;; "u" mu4e-update-mail-and-index
  ;; "gl" revert-buffer
  ;; "N" mu4e-news
  ;; ",hh" mu4e-display-manual
  ;; "x" mu4e-kill-update-mail
  ;; "A" mu4e-about
  ;; "f" smtpmail-send-queued-mail
  ;; "m" mu4e~main-toggle-mail-sending-mode

  ;; header and reader view
  ;; "E" mu4e-compose-edit
  ;; "F" mu4e-compose-forward
  ;; "R" mu4e-compose-reply
  ;; "o" mu4e-headers-change-sorting
  ;; "gl" mu4e-headers-rerun-search
  ;; "/" mu4e-headers-search-narrow
  ;; "\" to undo / widen the narrowing
  ;; "K" mu4e-headers-search-edit
  ;; "Ê" mu4e-headers-search-edit
  ;; "x" mu4e-mark-execute-all
  ;; "a" mu4e-headers-action
  ;; "*" mu4e-headers-mark-for-something ; TODO: Don't override evil-seach-word-forward?
  ;; "&" mu4e-headers-mark-custom
  ;; "A" mu4e-headers-mark-for-action
  ;; "m" mu4e-headers-mark-for-move
  ;; "à" mu4e-headers-mark-for-refile
  ;; "D" mu4e-headers-mark-for-delete
  ;; "d" mu4e-headers-mark-for-trash
  ;; "=" mu4e-headers-mark-for-untrash
  ;; "u" mu4e-headers-mark-for-unmark
  ;; "U" mu4e-mark-unmark-all
  ;; "?" mu4e-headers-mark-for-unread
  ;; "!" mu4e-headers-mark-for-read
  ;; "%" mu4e-headers-mark-pattern
  ;; "+" mu4e-headers-mark-for-flag
  ;; "-" mu4e-headers-mark-for-unflag
  ;; "[[" mu4e-headers-prev-unread
  ;; "]]" mu4e-headers-next-unread
  ;; "gs" mu4e-headers-prev-unread
  ;; "gt" mu4e-headers-next-unread
  ;; "\C-t" mu4e-headers-next
  ;; "\C-s" mu4e-headers-prev
  ;; "zj" mu4e-headers-toggle-include-related
  ;; "zÉ" mu4e-headers-toggle-include-related
  ;; "zh" mu4e-headers-toggle-threading
  ;; "zé" mu4e-headers-toggle-threading
  ;; "zd" mu4e-headers-toggle-skip-duplicates
  ;; "gl" mu4e-show-log
  ;; "gL" mu4e-show-log
  ;; "gv" mu4e-select-other-view
  ;; "é!" mark all thread as read
  ;; "éD" mark all thread for Deletion
  (evil-collection-define-key 'normal 'mu4e-main-mode-map
    "ê" 'mu4e-headers-search
    ",hh" 'mu4e-display-manual
    ",C" 'message-mail                  ; to compose with org-msg
                                        ; (sent as HTML)
    )

  (evil-collection-define-key 'normal 'mu4e-headers-mode-map
    ",C" 'message-mail                  ; to compose with org-msg
                                        ; (sent as HTML)
    "O" 'mu4e-org-store-and-capture
    "ê" 'mu4e-headers-search
    "Ê" 'mu4e-headers-search-edit
    "à" 'mu4e-headers-mark-for-refile
    "À" 'mu4e-headers-mark-for-archive
    "gs" 'mu4e-headers-prev-unread
    "gt" 'mu4e-headers-next-unread
    "\C-t" 'mu4e-headers-next
    "\C-s" 'mu4e-headers-prev
    "zÉ" 'mu4e-headers-toggle-include-related
    "zé" 'mu4e-headers-toggle-threading
    "gL" 'mu4e-show-log
    "%" 'mu4e-headers-mark-pattern
    ",é" 'mu4e-headers-mark-pattern
    "É"  'mu4e-headers-mark-thread
    "é!" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(read)))
    "éD" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(delete)))
    "éà" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(refile)))
    )
  (evil-collection-define-key 'normal 'mu4e-view-mode-map
    "O" 'mu4e-org-store-and-capture
    "ê" 'mu4e-headers-search
    ",hh" 'mu4e-display-manual
    "à" 'mu4e-view-mark-for-refile
    "À" 'mu4e-headers-mark-for-archive
    "zh" 'mu4e-view-toggle-html
    "gs" 'mu4e-headers-prev-unread
    "gt" 'mu4e-headers-next-unread
    "\C-t" 'mu4e-view-headers-next
    "\C-s" 'mu4e-view-headers-prev
    "zÉ" 'mu4e-headers-toggle-include-related
    "zé" 'mu4e-headers-toggle-threading
    "zq" 'mu4e-view-fill-long-lines
    "gL" 'mu4e-show-log
    "%" 'mu4e-view-mark-pattern
    ",é" 'mu4e-view-mark-pattern
    "É"  'mu4e-headers-mark-thread
    "é!" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(read)))
    "éD" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(delete)))
    "éà" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(refile)))
    )
  (evil-collection-define-key 'normal 'mu4e-compose-mode-map
    ",hh" 'mu4e-display-manual
    "gs" 'message-goto-subject
    "\C-c\C-s" 'message-goto-subject      ; align with org-msg
    "gb" 'message-goto-body
    )
  (evil-collection-define-key 'normal 'org-msg-edit-mode-map
    ",hh" 'mu4e-display-manual
    "gs" 'message-goto-subject
    "gb" 'message-goto-body
    )

  ;;; Setup
  ;; Contexts / multiple accounts
  (setq mu4e-contexts
        `(
          ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda () (mu4e-message ">> GMail context"))
            :leave-func (lambda () (mu4e-message "<< GMail context"))
            ;; we match based on the contact-fields of the message
            :match-func
            (lambda (msg)
              (when msg
                (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
            ;; (when msg
            ;;   (mu4e-message-contact-field-matches msg
            ;;     :to "behaghel@gmail.com")))
            :vars '((user-mail-address	    . "behaghel@gmail.com")
                    (mu4e-compose-signature  . "Hubert\nhttps://blog.behaghel.org")
                    ))
          ,(make-mu4e-context
            :name "M&S"
            :enter-func (lambda () (mu4e-message ">> M&S context"))
            ;; no leave-func
            ;; we match based on the maildir of the message
            ;; this matches maildir /Arkham and its sub-directories
            :match-func
            (lambda (msg)
              (when msg
                (string-match-p "^/mns" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address	   . "hubert.behaghel@marks-and-spencer.com")
                    (smtpmail-smtp-service  . 1025) ; davmail SMTP
                    (mu4e-compose-signature .
                                            (concat
                                             "Hubert Behaghel\n"
                                             "Head of Software Engineering\n"))))

          ,(make-mu4e-context
            :name "fbehaghel.fr"
            :enter-func (lambda () (mu4e-message ">> behaghel.fr context"))
            ;; :leave-func (lambda () (mu4e-message "<< behaghel.fr context"))
            ;; we match based on the contact-fields of the message
            :match-func
            (lambda (msg)
              ;; (when msg
              ;;   (string-match-p "^/behaghel.fr" (mu4e-message-field msg :maildir))))
              (when msg
                (mu4e-message-contact-field-matches msg
                                                    '(:cc :from :to)
                                                    "hubert@behaghel.fr")
                ))
            :vars '((user-mail-address	   . "hubert@behaghel.fr")
                    (mu4e-compose-signature . "Hubert\nhttps://blog.behaghel.org")
                    ))
          ,(make-mu4e-context
            :name "obehaghel.org"
            :enter-func (lambda () (mu4e-message ">> behaghel.org context"))
            ;; :leave-func (lambda () (mu4e-message "<< behaghel.org context"))
            ;; we match based on the contact-fields of the message
            :match-func
            (lambda (msg)
              ;; (when msg
              ;;   (string-match-p "^/behaghel.org" (mu4e-message-field msg :maildir))))
              (when msg
                (mu4e-message-contact-field-matches msg
                                                    '(:cc :from :to)
                                                    "hubert@behaghel.org")
                ))
            :vars '((user-mail-address	   . "hubert@behaghel.org")
                    (mu4e-compose-signature . "Hubert\nhttps://blog.behaghel.org")
                    ))
          ))

  ;; start with the first (default) context;
  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask-if-none)

  ;; the next are relative to the root maildir
  ;; (see `mu info`).
  ;; instead of strings, they can be functions too, see
  ;; their docstring or the chapter 'Dynamic folders'
  (defun contextual-default-folder (suffix)
    (lambda (msg)
      (concat "/" (mu4e-context-name (mu4e-context-determine msg)) suffix)))
  (setq mu4e-sent-folder   (contextual-default-folder "/sent")
        mu4e-drafts-folder (contextual-default-folder "/drafts")
        mu4e-refile-folder (contextual-default-folder "/archive")
        mu4e-trash-folder  (contextual-default-folder "/trash"))

  ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq   mu4e-maildir-shortcuts
          '((:maildir "/gmail/archive" :key ?a)
            (:maildir "/gmail/sent"    :key ?s)
            (:maildir "/gmail/INBOX"   :key ?g)
            (:maildir "/mns/INBOX"     :key ?m)
            ))


  ;; attempt to reinvent Other view from Outlook which is pretty much
  ;; a spam filter but for emails that are almost spam
  ;; the idea is to automate as much as possible:
  ;; TODO: add a custom action to "add messages like this to noise filter"
  ;; then ask question: based on [s]ubject [l]ist [f]rom
  ;; TODO: add a mark all with custom marker DWIM
  ;; TODO: see mu4e-mark-execute-pre-hook in case it can help automate further
  ;; TODO: also see
  ;; https://emacs.stackexchange.com/questions/55618/rules-for-dealing-with-email-in-mu4e
  ;; and also
  ;; https://emacs.stackexchange.com/questions/51999/muting-threads-in-mu4e
  ;; and also https://www.reddit.com/r/emacs/comments/eu7xxy/mu4e_empty_trash_folder_in_regular_intervals/
  (setq hub/noise-predicates
    '(
      ;; You could try to automatically process cancellations. Outlook
      ;; then starts the subject with "Cancelled"
      ( :name "Calendar Notifications"
              :query "mime:text/calendar")
      ;; M&S
      ;;; Notifications (it's ok if not read)
      ( :name "MS Teams"
              :query "from:noreply@email.teams.microsoft.com")
      ( :name "Yammer"
              :query "from:Yammer")
      ;; GMail
      ;;; Notifications (it's ok if not read)
      ( :name "Strava Notifications"
              :query "from:no-reply@strava.com")
      ( :name "Strava Newsletter"
              :query "list:spc.244190.0.sparkpostmail.com")
      ( :name "Garmin Newsletter"
              :query "from:garmin@mails.garmin.com")
      ( :name "VONCRANK"
              :query "from:info@voncrank.com")
      ( :name "Ebay Confirmations"
              :query "from:ebay@ebay.co.uk")
      ( :name "Amazon Confirmations"
              :query "from:auto-confirm@amazon.co.uk")
      ( :name "Amazon Updates"
              :query "from:no-reply@amazon.co.uk")
      ( :name "Amazon Shipment"
              :query "from:shipment-tracking@amazon.co.uk")
      ( :name "Amazon Order Updates"
              :query "from:order-update@amazon.co.uk")
      ( :name "Amazon Alexa Newsletter"
              :query "from:amazon-offers@amazon.co.uk")
      ( :name "Charles Stanley Direct Contract Notes"
              :query "from:info@charles-stanley-direct.co.uk AND subject:\"Contract Note\"")
      ( :name "Charles Stanley Direct Newsletter"
              :query "from:info@cs-d.co.uk OR from:\"Charles Stanley Direct\"")
      ( :name "Proactive Investor Alerts"
              :query "from:noreply@proactiveinvestors.com")
      ( :name "HP Instant Ink"
              :query "from:HP@email.hpconnected.com")
      ( :name "ParuVendu"
              :query "from:info@paruvendu.fr")
      ( :name "Marks & Spencer Marketing"
              :query "list:1060191.xt.local")
      ( :name "Detox Kitchen"
              :query "from:postman@candymail.co")
      ( :name "Libertie Shopping"
              :query "list:contact.boutiquelibertie.com.ipo3-0v378.mj")
      ( :name "InCorio"
              :query "from:bonjour@incorio.com")
      ( :name "GoHugo Forum"
              :query "from:gohugo@discoursemail.com")
      ( :name "Hozana"
              :query "from:contact@hozana.org OR list:ac6ab4ad6642b7f06d375784a.63591.list-id.mcsv.net")
      ;;; Lists / News (to read when time permits) / Reports
      ;; Hobby
      ( :name "Wet Shaving by Mantic59"
              :query "list:a6f734e009f696324350cdedf.807213.list-id.mcsv.net")
      ( :name "Kudo Coach"
              :query "from:hello@kudo.coach")
      ;; Religious
      ( :name "CAFOD"
              :query "from:news@reply.cafod.net")
      ( :name "ECLJ"
              :query "from:secretariat@eclj.org")
      ( :name "Theodom"
              :query
              "list:dominicains.communaute.theodom.org.j8ko-ik9.mj" )
      ( :name "Franciscan at Home"
              :query "list:a1ee93991069fffd045e3a111.56591.list-id.mcsv.net")
      ( :name "Jean-Baptiste Maillard - Light in the Dark"
              :query
              "list:jean-baptiste.maillard.lightsinthedark.info.x0wws-5vl5g.mj")
      ( :name "AFC France"
              :query "from:info@afc-france.org")
      ( :name "Aid to Church in Need"
              :query "from:enews@acnuk.org")
      ;; General Info
      ( :name "La Selection Du Jour"
              :query "from:lsdjabos@laselectiondujour.com")
      ( :name "Westminster COVID Update"
              :query
              "list:a50c8b0dd980669ef713b4cca.55853.list-id.mcsv.net" )
      ( :name "TfL"
              :query "from:Transport_for_London@email.tfl.gov.uk")
      ( :name "Babylon Health"
              :query "from:no-reply@news.babylonhealth.com")
      ( :name "Dr Willem - Lettre Santé Naturelle"
              :query "list:100017990.xt.local")
      ( :name "Avaaz"
              :query "from:avaaz@avaaz.org")
      ;; Technology
      ( :name "SWLW"
              :query "from:oren@softwareleadweekly.com")
      ( :name "Patrick Kua — Level Up"
              :query "from:level-up@getrevue.co")
      ( :name "Remote HQ"
              :query "list:info.nohq.co.x33xq-2w4.mj")
      ( :name "Grafana"
              :query "list:2aeb5711db2aececc990be536.793549.list-id.mcsv.net")
      ( :name "Real Python"
              :query "from:info@realpython.com")
      ( :name "Mu"
              :query "list:mu-discuss.googlegroups.com")
      ( :name "Hillel Wayne's Newsletter"
              :query "list:hillelwayne.buttondown.email")
      ;; Finance
      ( :name "Hargreaves Lansdown"
              :query "from:hl@email.hl.co.uk")
      ( :name "Boursorama"
              :query "from:noreply@boursorama.fr")
      ( :name "L&C Mortgage"
              :query "List: 500008880.xt.local")
      ( :name "Rightmove"
              :query "from:autoresponder@rightmove.com")
      ))
  (defun hub/build-noise-query ()
    (let* (
           (lplist hub/noise-predicates)
           (get-query (lambda (entry) (concat "(" (plist-get entry :query) ")")))
           (f (lambda (acc entry) (concat (funcall get-query entry) " OR " acc))))
      (message "%s"
               (seq-reduce f (cdr lplist) (funcall get-query (car lplist))))
      ))
  ;; (length hub/noise-predicates)

  (add-to-list 'mu4e-bookmarks
               ;; add bookmark for recent messages on the Mu mailing list.
               `( :name "Noise"
                        :key  ?N
                        :query ,(concat "maildir:/INBOX/" " AND ("
                                        (hub/build-noise-query) ")")))

  ;; To delete all meeting notifications or updates
  ;; 1. call M-x mu4e-headers-toggle-full-search to not limit search to
  ;; first 500 but to really capture them all in one go. It seems to
  ;; be bound to Q.
  ;; 2. type 'k' for initiating a query
  ;; FIXED: find a better binding. It's supposed to be 's' but my hjkl
  ;; rotation moves it to k. Also try to have the main view to advertise
  ;; this binding (currently advertising s still when it's k)
  ;; Done: ê
  ;; 3. use query string 'mime:text/calendar'
  ;; 4. invoke M-x mu4e-headers-mark-pattern
  ;; FIXED: it is supposed to be bound to '%' but this is shadowed. Find
  ;; a suitable binding.
  ;; Done: unshadowed
  ;; 5. apply pattern on 'S'ubject and use pattern '.*'
  ;; (not mark all function, so that does it)
  ;; 6. use D for 'D'elete
  ;; Now: Qêmime:text/calendar[RET]C-xhD
  ;; Better: smart refile: https://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html#Smart-refiling

  ;; Fetching
  ;; program to get mail; alternatives are 'fetchmail', 'getmail'
  ;; isync or your own shellscript. called when 'U' is pressed in
  ;; main view.
  ;; If you get your mail without an explicit command,
  ;; use "true" for the command (this is the default)
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 450)
  ;; rename files when moving
  ;; needed for mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Reading
  ;; don't keep message buffers around
  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '( (:date           . 15)    ;; alternatively, use :human-date
           (:maildir        . 15)
           (:flags          . 6)
           (:from           . 22)
           (:thread-subject . nil)))
  (setq
   mu4e-headers-date-format "%F"     ; ISO format yyyy-mm-dd
   )
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-attachment-dir "~/Downloads"
        mu4e-headers-skip-duplicates t
        mu4e-view-show-images t
        ;; messes up with alignment, not that useful anyway
        ;; mu4e-use-fancy-chars t
        ;; gmail style conversations: not by default activate with zé
        mu4e-headers-include-related nil
        )
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  ;; to really display images inline (and not at the end of the message)
  ;; see https://github.com/djcb/mu/issues/868#issuecomment-543315407
  (setq mu4e-view-use-gnus t
        ;; adapt for dark theme
        shr-color-visible-luminance-min 80)
  ;; prefer the plain text version when available in gnus view
  (with-eval-after-load "mm-decode"
   (add-to-list 'mm-discouraged-alternatives "text/html")
   (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  ;; TODO: need to figure how to switch / toggle to HTML part
  ;; mu4e-view-toggle-html isn't supported in gnus view
  ;; for now I am adding this action
  (add-to-list 'mu4e-view-actions
               '("bview in browser" . mu4e-action-view-in-browser) t)

  ;; FIXME: make-tmp-file-browsable isn't run automatically in spite
  ;; of this:
  (add-hook 'mu4e-view-mode-hook 'make-tmp-file-browsable)
  ;; Call EWW to display HTML messages, not useful for now
  ;; stolen from https://irreal.org/blog/?p=6122
  ;; (defun jcs-view-in-eww (msg)
  ;;   (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
  ;; (add-to-list 'mu4e-view-actions
  ;;              '("eww view" . jcs-view-in-eww) t)

  ;; so that you can reply to calendar invites
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (setq mu4e-icalendar-trash-after-reply nil) ; nil until I trust it
  ;; org integration
  (require 'org-agenda)
  (setq gnus-icalendar-org-capture-file org-default-notes-file)
  (setq gnus-icalendar-org-capture-headline '("Calendar"))
  (gnus-icalendar-org-setup)
  ;; FIXME: doesn't work for Outlook Calendar invites but it does for
  ;; GMail assuming you're not inviting yourself
  ;; see: https://github.com/djcb/mu/pull/1403#issuecomment-626689596

  ;; Processing
  ;; M-x org-store-link should link to the message not the query in
  ;; header view
  (setq org-mu4e-link-query-in-headers-mode nil)
  ;; mark as read and refile (archive) in one go
  (add-to-list 'mu4e-marks
               '(archive
                 :char       "À"
                 :prompt     "Archive"
                 :dyn-target  (lambda (target msg) (mu4e-get-refile-folder msg))
                 :show-target (lambda (target) target)
                 :action      (lambda (docid msg target)
		                ;; must come before proc-move since retag runs
		                ;; 'sed' on the file
		                (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))))
  (mu4e~headers-defun-mark-for archive)

  ;; Contacts
  ;; stolen from https://martinralbrecht.wordpress.com/2016/05/30/handling-email-with-emacs/
  (defun malb/canonicalise-contact-name (name)
    "Normalise NAME before recording it in contact DB."
    (let ((case-fold-search nil))
      (setq name (or name ""))
      (if (string-match-p "^[^ ]+@[^ ]+\.[^ ]" name)
          ""
        (progn
          ;; drop email address
          (setq name (replace-regexp-in-string "^\\(.*\\) [^ ]+@[^ ]+\.[^ ]" "\\1" name))
          ;; strip quotes
          (setq name (replace-regexp-in-string "^\"\\(.*\\)\"" "\\1" name))
          ;; deal with YELL’d last names
          (setq name (replace-regexp-in-string "^\\(\\<[[:upper:]]+\\>\\) \\(.*\\)" "\\2 \\1" name))
          ;; Foo, Bar becomes Bar Foo
          (setq name (replace-regexp-in-string "^\\(.*\\), \\([^ ]+\\).*" "\\2 \\1" name))
          ;; look up names and replace from static table, TODO look this up by email
          (setq name (or (cdr (assoc name malb/mu4e-name-replacements)) name))
          ))))

  (defun malb/mu4e-contact-rewrite-function (contact)
    "Adapt normalisation function for CONTACT in mu4e workflow."
    (let* ((name (or (plist-get contact :name) ""))
           (mail (plist-get contact :mail))
           (case-fold-search nil))
      (plist-put contact :name (malb/canonicalise-contact-name name))
      contact))

  (setq mu4e-contact-rewrite-function #'malb/mu4e-contact-rewrite-function)

  ;; Composing
  (setq mu4e-completing-read-function 'completing-read
        ;; I'd rather go with 'traditional but I guess the world isn't
        ;; traditional enough
        message-cite-reply-position 'above
        message-citation-line-format "On %A, %d %B %Y at %R %Z, %N wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-apply-format_003dflowed-to-my-outgoing-messages_003f
        mu4e-compose-format-flowed t
        )
  (add-hook 'message-mode-hook #'flyspell-mode)
  (add-hook 'message-mode-hook #'footnote-mode)

  ;; TODO https://github.com/jorgenschaefer/typoel
  ;; (add-hook 'message-mode-hook #'typo-mode)

  ;; to select the right language in spell check
  ;; TODO https://github.com/nschum/auto-dictionary-mode
  ;; (add-hook 'message-mode-hook #'adict-guess-dictionary)

  ;; Also see use-package org-msg

  ;; Sending
  (setq send-mail-function 'sendmail-send-it)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq smtpmail-smtp-server "localhost")
  ;; (setq
  ;;    ;; if you need offline mode, set these -- and create the queue dir
  ;;    ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
  ;;    smtpmail-queue-mail  nil
  ;;    smtpmail-queue-dir  "/home/user/Maildir/queue/cur")
  )

(use-package org-mu4e
  :ensure nil
  :pin manual
  :config (setq org-mu4e-link-query-in-headers-mode nil))

(use-package org-msg
  :pin melpa

  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi %s,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-text-plain-alternative t
	org-msg-signature "

Regards,

#+begin_signature
--
Hubert
#+end_signature")
  (add-hook 'org-msg-mode-hook 'make-tmp-file-browsable)
  ;; to avoid an error from diff-hl on deleted buffer right after
  ;; message is sent
  (add-hook 'org-msg-mode-hook (lambda () (diff-hl-mode -1)))

  (org-msg-mode))


(provide 'setup-email)
;;; setup-email.el ends here
