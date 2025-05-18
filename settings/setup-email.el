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

(defun make-tmp-file-browsable ()
"Allow temporary files to be accessed by the browser.
On crostini (chromebook) /tmp isn't visible to Chrome breaking
most org export / preview in the browser."
  (interactive)
  (setq-local temporary-file-directory "~/tmp"))

;; --------------------------------------------------------------------------
;; Mu4e setup helpers
;; --------------------------------------------------------------------------
(require 'cl-lib)

(defun hub/add-mu4e-load-path ()
  "Locate Mu4e and add its lisp directory to `load-path'.
Return the path to the `mu' binary when found.  This makes the
configuration resilient across macOS, Linux and NixOS installs
where Mu4e may live in different locations."
  (let* ((mu-bin (executable-find "mu"))
         (prefix (when mu-bin (expand-file-name ".." (file-name-directory mu-bin))))
         (candidates (list
                      (when prefix (expand-file-name "share/emacs/site-lisp/mu/mu4e" prefix))
                      (when prefix (expand-file-name "share/emacs/site-lisp/mu4e" prefix))
                      "/usr/local/share/emacs/site-lisp/mu/mu4e"
                      "/usr/local/share/emacs/site-lisp/mu4e"
                      "/usr/share/emacs/site-lisp/mu/mu4e"
                      "/usr/share/emacs/site-lisp/mu4e")))
    (cl-loop for dir in candidates do
             (when (and dir (file-directory-p dir))
               (add-to-list 'load-path dir)
               (cl-return)))
    mu-bin))

(defvar hub/mu-binary (hub/add-mu4e-load-path)
  "Location of the `mu' binary if available.")

;; https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html#Installation
;; mu4e is part of the mu project, a UNIX CLI therefore not on MELPA
;; I built it from git repo
;; I used $ ./configure.sh --datadir=/path/to/emacs/build/dir
;; that way mu build put all the mu4e elisp files in my load-path on
;; `make install' step
(use-package mu4e
  ;;; :straight (:host github
  ;;;            :repo "djcb/mu"
  ;;;            :branch "master"
  ;;;            :files ("mu4e/*")
  ;;;            :pre-build (("./autogen.sh" "-Dguile=disabled") ("make")))
  ;;; :custom   (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  :init (unless hub/mu-binary
          (message "mu binary not found; mu4e may not be available"))
  :custom   (mu4e-mu-binary (or hub/mu-binary "mu"))
  ;; :disabled t
  :ensure nil
  ;; :pin manual
  :config
  (evil-collection-define-key 'normal 'mu4e-main-mode-map
    "ê" 'mu4e-headers-search
    ",hh" 'mu4e-display-manual
    "zO" 'org-msg-mode
    "zê" 'mu4e-headers-toggle-full-search
    )

  (evil-collection-define-key 'normal 'mu4e-headers-mode-map
    "F" 'mu4e-compose-forward
    "O" 'mu4e-org-store-and-capture
    "zO" 'org-msg-mode
    ",à" 'mu4e-org-store-and-capture
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
    "zê" 'mu4e-headers-toggle-full-search
    "gL" 'mu4e-show-log
    "%" 'mu4e-headers-mark-pattern
    ",é" 'mu4e-headers-mark-pattern
    "É"  'mu4e-headers-mark-thread
    "SPC" nil
    "z!" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(read)))
    "zD" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(delete)))
    "zà" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(refile)))
    )

  (defun hub/copy-url-at-point-dwim (url)
    "Copy the URL under point to the kill ring.
If in an shr context (html) then it takes it from there
otherwise it looks for a plain text url. With a prefix argument,
or if there is no link under point, but there is an image under
point then copy the URL of the image under point instead."
    (interactive (list (shr-url-at-point current-prefix-arg)))
    (let ((target (or url (ffap-url-at-point))))
      (if (not target)
          (message "No URL under point")
        (setq target (url-encode-url target))
        (kill-new target)
        (message "Copied %s" target))))

  (evil-collection-define-key 'normal 'mu4e-view-mode-map
    (kbd "<tab>") (lambda()
                    (interactive)
                    (if (equal system-type 'darwin)
                        (forward-button 1)
                      (widget-forward)))
    "zO" 'org-msg-mode
    "O" 'mu4e-org-store-and-capture
    ",à" 'mu4e-org-store-and-capture
    "F" 'mu4e-compose-forward
    "ê" 'mu4e-headers-search
    ",hh" 'mu4e-display-manual
    "à" 'mu4e-view-mark-for-refile
    "À" 'mu4e-headers-mark-for-archive
    "Y" 'hub/copy-url-at-point-dwim                  ; when on a link in html
    "zh" 'mu4e-view-toggle-html
    "gs" 'mu4e-headers-prev-unread
    "gt" 'mu4e-headers-next-unread
    "gb" 'message-goto-body
    "\C-t" 'mu4e-view-headers-next
    "\C-s" 'mu4e-view-headers-prev
    "zÉ" 'mu4e-headers-toggle-include-related
    "zé" 'mu4e-headers-toggle-threading
    "zq" (lambda()
           (interactive)
           (if mu4e-view-use-gnus
               (article-fill-long-lines)
             (mu4e-view-fill-long-lines)))
    "gL" 'mu4e-show-log
    "%" 'mu4e-view-mark-pattern
    ",é" 'mu4e-view-mark-pattern
    "É"  'mu4e-headers-mark-thread
    "SPC" nil
    "z!" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(read)))
    "zD" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(delete)))
    "zà" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(refile)))
    )
  (evil-collection-define-key 'normal 'mu4e-compose-mode-map
    ",hh" 'mu4e-display-manual
    "gs" 'message-goto-subject
    "\C-c\C-s" 'message-goto-subject      ; align with org-msg
    "gb" 'message-goto-body
    (kbd "zn") 'use-hard-newlines       ; reintroduce hard nl
    )
  (evil-collection-define-key 'normal 'org-msg-edit-mode-map
    ",hh" 'mu4e-display-manual
    "gs" 'message-goto-subject
    "gb" 'org-msg-goto-body
    )
  (evil-collection-define-key 'insert 'mu4e-compose-mode-map
    (kbd "M-.") 'message-goto-body
    (kbd "M-,") 'message-goto-subject
    )
  (evil-collection-define-key 'insert 'org-msg-edit-mode-map
    (kbd "M-.") 'org-msg-goto-body
    (kbd "M-,") 'message-goto-subject
    )

  (evil-collection-define-key 'normal 'gnus-mime-button-map
    "t" 'evil-next-line
    "T" 'gnus-mime-view-part-as-type
    "\C-s" 'gnus-mime-save-part
    "p" 'evil-paste-after
    "P" 'gnus-mime-print-part
    "r" 'evil-forward-char
    "R" 'gnus-mime-replace-part
    "c" 'evil-backward-char
    "y" 'gnus-mime-copy-part
    "\C-v" 'gnus-mime-view-part-externally
    "\C-o" 'gnus-mime-view-part-internally
    )
  ;; (defvar gnus-mime-button-commands
  ;;   '((gnus-article-press-button "\r" "Toggle Display")
  ;;     (gnus-mime-view-part "v" "View Interactively...")
  ;;     (gnus-mime-view-part-as-type "t" "View As Type...")
  ;;     (gnus-mime-view-part-as-charset "C" "View As charset...")
  ;;     (gnus-mime-save-part "o" "Save...")
  ;;     (gnus-mime-save-part-and-strip "\C-o" "Save and Strip")
  ;;     (gnus-mime-replace-part "r" "Replace part")
  ;;     (gnus-mime-delete-part "d" "Delete part")
  ;;     (gnus-mime-copy-part "c" "View As Text, In Other Buffer")
  ;;     (gnus-mime-inline-part "i" "View As Text, In This Buffer")
  ;;     (gnus-mime-view-part-internally "E" "View Internally") ;; Why `E'?
  ;;     (gnus-mime-view-part-externally "e" "View Externally")
  ;;     (gnus-mime-print-part "p" "Print")
  ;;     (gnus-mime-pipe-part "|" "Pipe To Command...")
  ;;     (gnus-mime-action-on-part "." "Take action on the part...")))

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
            :vars '((user-mail-address      . "behaghel@gmail.com")
                    (smtpmail-smtp-user     . "behaghel@gmail.com")
                    (org-msg-signature      . "#+begin_signature
--\n\nHubert
#+end_signature")))
          ,(make-mu4e-context
            :name "fbehaghel.fr"
            :enter-func (lambda () (mu4e-message ">> behaghel.fr context"))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches msg
                                                    '(:cc :from :to)
                                                    "hubert@behaghel.fr")
                ))
            :vars '((user-mail-address     . "hubert@behaghel.fr")
                    (smtpmail-smtp-user    . "hubert@behaghel.fr")
                    ))
          ,(make-mu4e-context
            :name "obehaghel.org"
            :enter-func (lambda () (mu4e-message ">> behaghel.org context"))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches msg
                                                    '(:cc :from :to)
                                                    "hubert@behaghel.org")
                ))
            :vars '((user-mail-address     . "hubert@behaghel.org")
                    (smtpmail-smtp-user    . "hubert@behaghel.org")
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
      (let* ((msg-context (mu4e-context-determine msg))
             (context (if msg-context msg-context (mu4e-context-current)))
             (ctx-name (mu4e-context-name context))
             (box-path (if (string-match-p "behaghel." ctx-name) (substring ctx-name 1 nil) ctx-name)))
        (concat "/" box-path suffix))))
  (setq mu4e-sent-folder   (contextual-default-folder "/sent")
        mu4e-drafts-folder (contextual-default-folder "/drafts")
        mu4e-refile-folder (contextual-default-folder "/archive")
        mu4e-trash-folder  (contextual-default-folder "/trash"))

  ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq   mu4e-maildir-shortcuts
          '(
            (:maildir "/gmail/inbox"   :key ?g)
            (:maildir "/behaghel.fr/inbox"     :key ?f)
            (:maildir "/behaghel.org/inbox"     :key ?o)
            (:maildir "/gmail/archive" :key ?G)
            (:maildir "/behaghel.fr/archive"     :key ?F)
            (:maildir "/behaghel.org/archive"     :key ?O)
            (:maildir "/gmail/sent"    :key ?s)
            (:maildir "/behaghel.fr/sent"     :key ?h)
            (:maildir "/behaghel.org/sent"     :key ?S)
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

      ;; GMail
      ;;; Notifications (it's ok if not read)
      ( :name "Qustodio Notifications"
              :query "from:no-reply@qustodio.com")
      ( :name "Strava Notifications"
              :query "from:no-reply@strava.com"
              :category "cycling")
      ( :name "Ebay Confirmations"
              :query "from:ebay@ebay.co.uk OR from:ebay@ebay.com"
              :category "shopping")
      ( :name "Amazon Confirmations"
              :query "from:auto-confirm@amazon.co.uk"
              :category "shopping")
      ( :name "Amazon Updates"
              :query "from:no-reply@amazon.co.uk"
              :category "shopping")
      ( :name "Amazon Shipment"
              :query "from:shipment-tracking@amazon.co.uk"
              :category "shopping")
      ( :name "Amazon Order Updates"
              :query "from:order-update@amazon.co.uk"
              :category "shopping")
      ( :name "Enterprise Rent-a-car"
              :query "list:10780075.xt.local")
      ( :name "Charles Stanley Direct Contract Notes"
              :query "from:info@charles-stanley-direct.co.uk AND subject:\"Contract Note\"")
      ( :name "Proactive Investor Alerts"
              :query "from:noreply@proactiveinvestors.com")
      ( :name "HP Instant Ink"
              :query "from:HP@email.hpconnected.com")
      ( :name "ParuVendu"
              :query "from:info@paruvendu.fr")
      ( :name "Dropbox"
              :query "from:no-reply@dropbox.com")
      ( :name "Ocado Confirmation"
              :query "from:customerservices@ocado.com AND subject:\"Confirmation of your order\"")
      ( :name "Netflix"
              :query "from:info@mailer.netflix.com")
      ( :name "Analyzati code"
              :query "from:hello@analyzati.com AND subject:\"Security code - Analyzati\"")
      ( :name "idealista"
        :query "from:noresponder@idealista.com OR from:noresponder@avisos.idealista.com")
      ( :name "CosmoCaixa"
        :query "from:info@news.cosmocaixa.org")

      ( :name "Amazon Orders"
        :query "from:confirmar-envio@amazon.es")
      ( :name "Amazon Kindle Orders"
        :query "from:digital-no-reply@amazon.es")

      ( :name "FreeNow"
        :query "from:no-reply@free-now.com")
      ( :name "NAGA"
        :query "from:noreply@nagamarkets.com")
      ( :name "Otta"
        :query "from:hello@otta.com")
      ( :name "OCU"
             :query "from:noreply@notify.ocu.org")
      ( :name "OCU Mailing Lists"
             :query "from:maillist@emailing.ocu.org")

      (:name "BBVA"
             :query "from:bbva@comunica.bbva.com")
      (:name "mediolanum"
             :query "from:bancomediolanum@bancomediolanum.es")
      ( :name "Analyzati"
             :query "from:hello@analyzati.com")
      ( :name "Property Search"
             :query "subject:/^Your Property Search/")
      ( :name "LinkedIn Job Alerts"
        :query "from:jobalerts-noreply@linkedin.com")
      ( :name "LinkedIn Newsletters"
        :query "from:newsletters-noreply@linkedin.com")



      ;; Newsletter
      ( :name "Mu"
              :query "list:mu-discuss.googlegroups.com"
              :category "tech")
      ;; Finance
      ( :name "Money Saving Expert - Cheap Energy Club"
              :query "list:1081285.xt.local"
              :category "finance")
      ( :name "Bulb"
              :query "from:hello@bulb.co.uk"
              :category "finance")
      ( :name "HSBC"
              :query "from:statements@email1.hsbc.co.uk"
              :category "finance")
      ( :name "Hargreaves Lansdown"
              :query "from:hl@email.hl.co.uk"
              :category "finance")
      ( :name "Boursorama"
              :query "from:noreply@boursorama.fr OR from:noreply@client.boursorama.fr"
              :category "finance")
      ( :name "L&C Mortgage"
              :query "list:500008880.xt.local"
              :category "finance")
      ( :name "Charles Stanley Direct Newsletter"
              :query "from:info@cs-d.co.uk OR from:\"Charles Stanley Direct\""
              :category "finance")
      ( :name "Rightmove"
              :query "from:autoresponder@rightmove.com"
              :category "finance")
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

  ;; default mu4e-bookmarks value
  (setq mu4e-bookmarks '(
                         (:name "Inbox" :query "maildir:/inbox/ AND NOT flag:trashed" :key ?i)
                         (:name "behaghel.org" :query "maildir:/behaghel.org/inbox AND NOT flag:trashed" :key ?t)
                         (:name "GMail" :query "maildir:/gmail/inbox AND NOT flag:trashed" :key ?g)
                         (:name "behaghel.fr" :query "maildir:/behaghel.fr/inbox AND NOT flag:trashed" :key ?t)
                         (:name "Important" :query "flag:flagged AND NOT flag:trashed" :key ?f)
                         (:name "Drafts" :query "maildir:/drafts/ AND NOT flag:trashed" :key ?d)
                         (:name "Today" :query "date:today..now AND NOT maildir:/gmail/inbox AND NOT flag:trashed" :key ?h)
                         (:name "Attachments" :query "flag:attach" :key ?a)
                         (:name "Invites" :query "mime:text/calendar" :key ?c)
                         ))

  (add-to-list 'mu4e-bookmarks
               ;; add bookmark for recent messages on the Mu mailing list.
               `( :name "Noise"
                        :key  ?n
                        :query ,(concat "maildir:/inbox/" " AND ("
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
  ;; FIXME: make lazy check work, currently just doesn't surface any
  ;; new mail anymore
  ;; (setq mu4e-get-mail-command "mbsync -a && touch ~/Maildir/*"
  ;;       mu4e-update-interval 450
  ;;       mu4e-index-lazy-check t)
  ;; (setq mu4e-index-cleanup t)
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
        '(
           (:thread-subject . 42)
           (:from-or-to     . 22)
           (:date           . 15)    ;; alternatively, use :human-date
           (:flags          . 6)
           (:maildir        . 15)
           ))
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
        gnus-inhibit-mime-unbuttonizing nil
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
  ;; to get gpg signature verified and buttonised
  (setq
   gnus-buttonized-mime-types '("multipart/signed")
   mm-verify-option 'known)

  ;; FIXME: only activate when on chromebook
  ;; FIXME: make-tmp-file-browsable isn't run automatically in spite
  ;; of this:
  (when (eq system-type 'gnu/linux)
    (add-hook 'mu4e-main-mode-hook 'make-tmp-file-browsable))
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
  ;; (setq mu4e-icalendar-diary-file "~/.emacs.d/diary-ical")
  ;; org integration
  (require 'org-agenda)
  (setq gnus-icalendar-org-capture-file org-default-notes-file)
  (setq gnus-icalendar-org-capture-headline '("Calendar"))
  (gnus-icalendar-org-setup)

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
                                (mu4e-action-retag-message msg "-\\Inbox")
		                (mu4e--server-move docid target "+S-u-N"))))
  (mu4e~headers-defun-mark-for archive)

  ;; attempt to remove GMail Inbox label on refile
  (add-hook 'mu4e-mark-execute-pre-hook
            (lambda (mark msg)
              (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
                    ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
                    ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))
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
        message-cite-reply-position 'above ;FIXME: maybe just on pro emails?
        ;; TODO: think of dropping the last colon: https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
        message-citation-line-format "On %A, %d %B %Y at %R %Z, %N wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        ;; org-msg doesn't work well with mu4e sig
        ;; https://github.com/jeremy-compostella/org-msg/issues/57
        ;; mu4e-compose-signature "Hubert" ;\nhttps://blog.behaghel.org"
        )
  ;; Do not auto-wrap lines in favor of format=flowed, but still
  ;; display them nicely wrapped in Emacs.
  (setq
   ;; enable format=flowed
   ;; - mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
   ;; - each paragraph is a single long line; at sending, emacs will add the
   ;;   special line continuation characters.
   ;; - also see visual-line-fringe-indicators setting below
   ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-apply-format_003dflowed-to-my-outgoing-messages_003f
   mu4e-compose-format-flowed t
   ;; because it looks like email clients are basically ignoring format=flowed,
   ;; let's complicate their lives too. send format=flowed with looong lines. :)
   ;; https://www.ietf.org/rfc/rfc2822.txt
   fill-flowed-encode-column 998
   ;; in mu4e with format=flowed, this gives me feedback where the
   ;; soft-wraps are
   visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
   )
  (defun visual-clean ()
    "Clean up messy buffers (i.e. web wikis or elfeed-show)"
    (interactive)
    (turn-off-auto-fill)
    (setq-local fill-column 82)
    (when (require 'visual-fill-column nil t)
      (visual-fill-column-mode))
    (visual-line-mode t))
  (add-hook 'mu4e-compose-mode-hook 'visual-clean)

  (setq ispell-program-name "aspell")
  (add-hook 'message-mode-hook #'flyspell-mode)
  (add-hook 'message-mode-hook #'footnote-mode)
  ;; I hope this help format=flowed look better
  (add-hook 'message-mode-hook (lambda () (use-hard-newlines -1)))
  ;; if this doesn't help, look here:
  ;; https://emacs.stackexchange.com/questions/3061/how-to-stop-mu4e-from-inserting-line-breaks

  ;; TODO: https://github.com/jorgenschaefer/typoel
  ;; (add-hook 'message-mode-hook #'typo-mode)

  ;; to select the right language in spell check
  ;; TODO: https://github.com/nschum/auto-dictionary-mode
  ;; (add-hook 'message-mode-hook #'adict-guess-dictionary)

  ;; if you want to request ACK
  ;; Kindly shared by @Chris00
  ;; TODO: make it on-demand through a function bound to zK
  ;; (add-hook
  ;;  'mu4e-compose-mode-hook
  ;;  (defun my/add-headers ()
  ;;    "Add some headers when composing mails."
  ;;    (save-excursion
  ;;      (message-add-header
  ;;       "Return-Receipt-To: Hubert.Behaghel@marks-and-spencer.com\n"
  ;;       "Disposition-Notification-To: Hubert.Behaghel@marks-and-spencer.com\n")
  ;;      )))


  ;; Also see use-package org-msg

  ;; Sending
  (setq send-mail-function 'sendmail-send-it
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header)
  ;; sync / blocking
  ;; (setq send-mail-function 'smtpmail-send-it)
  ;; (setq message-send-mail-function 'message-smtpmail-send-it)
  ;; (setq smtpmail-smtp-server "localhost"
  ;;       ;; smtpmail-auth-supported '(login)
  ;;       smtpmail-debug-info t)
  ;; this uses pass localhost.gpg to retrieve password (now moved
  ;; into init.el as it's not email specific)
  ;; (require 'auth-source-pass)
  ;; (auth-source-pass-enable)
  ;; (setq auth-source-debug t)
  ;; (setq auth-source-do-cache nil)

  ;; async
  ;; (setq send-mail-function 'sendmail-send-it)
  ;; (setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;; (setq
  ;;    ;; if you need offline mode, set these -- and create the queue dir
  ;;    ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
  ;;    smtpmail-queue-mail  nil
  ;;    smtpmail-queue-dir  "/home/user/Maildir/queue/cur")

  ;; both outlook and GMail take care of filing sent messages under
  ;; Sent dir
  (setq mu4e-sent-messages-behavior 'delete)
  )

(use-package org-msg
  ;; :pin melpa
  ;; :disabled t
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi%s,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-text-plain-alternative t
        ;; 'top-posting is the default but is it what you want?
        org-msg-posting-style 'top-posting
        ;; once upon a time, I was trying to avoid html format as much
        ;; as possible but my need for it depends on the content
        ;; I produce not the one I receive. I shall not use org-msg if
        ;; I want plain text. E.g. rendering of tables in text doesn't
        ;; look great.
        org-msg-default-alternatives '((new           . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text html)))
        org-msg-convert-citation t
        org-msg-signature "

#+begin_signature
--\n
Hubert
#+end_signature")
  (add-hook 'org-msg-mode-hook 'make-tmp-file-browsable)
  ;; FIXME: don't auto-fill but only in org-msg composition buffer
  (add-hook 'org-msg-mode-hook 'turn-off-auto-fill)
  ;; TODO: function that disables org-msg, initiate the composition of
  ;; a new message (plain text), add a hook to reinstate org-msg-mode
  ;; on successful sending
  ;; see inspiration here: https://github.com/jeremy-compostella/org-msg/pull/149/files

  (defun my-org-msg-composition-parameters (orig-fun &rest args)
    "Tweak my greeting message and my signature when replying as
   plain/text."
    (let ((res (apply orig-fun args)))
      ;; not perfect as when replying with both text and html, text
      ;; has no signature
      (when (equal (cadr args) '(text))
        (setf (alist-get 'signature res)
              (replace-regexp-in-string "\\.html.*" ""
                                        org-msg-signature)))
      res))
  (advice-add 'org-msg-composition-parameters
              :around #'my-org-msg-composition-parameters)
  (org-msg-mode))

(use-package mu4e-alert
  ; it's now something I see on my desktop
  :disabled t

  :after (mu4e)
  :init (mu4e-alert-enable-mode-line-display)
  )

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(use-package mu4e-dashboard
  :straight (mu4e-dashboard :type git :host github :repo "rougier/mu4e-dashboard")
  :custom (mu4e-dashboard-file "~/.emacs.d/settings/mail-sidebar.org")
  :config
  ;; (defun open-mail-sidebar (&optional args) (find-file "~/.emacs.d/settings/mail-sidebar.org"))
  (defun mu4e-dashboard-hook ()
    (when (string= (file-name-nondirectory buffer-file-name) "mail-sidebar.org")
      (mu4e-dashboard-mode)
      (when (functionp org-modern-mode)
        (org-modern-mode 0))
      ))
  (add-hook 'find-file-hook 'mu4e-dashboard-hook)
  ;; (advice-add 'mu4e :after #'open-mail-sidebar)
  )


(provide 'setup-email)
;;; setup-email.el ends here
