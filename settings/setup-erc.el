;;; ERC
;;; start with M-x erc-select
;;; switch channel by switching emacs buffer
;;; switch to last active channel C-c C-SPC
(use-package erc
  :commands erc-select
  :config
  (load "~/.emacs.d/.ercpass")
  (setq erc-echo-notices-in-minibuffer-flag t)
  (require 'tls)
  (setq erc-modules '(autojoin button completion fill
                               irccontrols list match menu
                               move-to-prompt netsplit networks
                               noncommands
                               readonly ring scrolltobottom
                               services stamp track))
  (setq erc-prompt-for-password 1)  ; use authinfo instead
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-fill-function 'erc-fill-static)
  (setq erc-fill-static-center 18)    ; margin for ts + nicks
  (setq erc-timestamp-format "[%H:%M] "
        erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-scroll-to-bottom)
  (setq erc-input-line-position -1)
  (setq erc-nickserv-passwords
        `((freenode (("behaghel" . ,freenode-passwd-behaghel)
                     ("larch" . ,freenode-passwd-behaghel)))
          (skyglobal (("hubert" . ,slack-global-passwd-hubert)))))
  (require 'erc-match)
  (setq erc-keywords '("\\bhub\\b" "behaghel" "hubert" "hubertb"))
  (setq erc-pals '("aloiscochard"))
  (setq erc-autojoin-mode t)
  (setq erc-autojoin-channels-alist
        '((".*\\.freenode.net" "#scala" "#scalaz" "#shapeless")
          ("irc.amazon.com" "#ingestion" "#reconciliation")))
  (setq erc-auto-query 'bury)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-lurker-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
  ;; (setq erc-echo-notices-in-minibuffer-flag t)
  ;; (erc-minibuffer-notice t)
  (setq erc-lurker-threshold-time 3600))

(provide 'setup-erc)
