;; Twitter
(use-package twittering-mode
  :commands (twit)
  :config
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t)
  (setq twittering-use-icon-storage t)
  (setq twittering-timer-interval 90)
  (setq twittering-relative-retrieval-interval-alist
	'(("\\`:direct.*\\'" 4)    ; 360 seconds
	  (":home" ":mentions" 1)  ; 90 seconds
	  (t 30)))                 ; anything else in 15 minutes)
  (setq twittering-status-format
	"%i %s,  %@:  %FACE[shadow]{%R}
%FILL[  ]{%T %FACE[shadow]{// from %f%L%r}}
 ")
  (setq twittering-initial-timeline-spec-string
	'(":home"
	  ":mentions"
	  ":direct_messages"
	  ":favorites"
	  "behaghel/scala"
	  "behaghel/catholic"
	  "behaghel/cycling"
	  ))
  (evil-define-key 'normal twittering-mode-map ",," 'twittering-enter)
  (evil-define-key 'normal twittering-mode-map (kbd "C-m") 'twittering-enter)
  (evil-define-key 'normal twittering-mode-map (kbd  "C-i") 'twittering-goto-next-thing)
  (evil-define-key 'normal twittering-mode-map (kbd "M-C-i") 'twittering-goto-previous-thing)
  (evil-define-key 'normal twittering-mode-map ",gd" 'twittering-direct-messages-timeline)
  (evil-define-key 'normal twittering-mode-map ",gr" 'twittering-replies-timeline)
  (evil-define-key 'normal twittering-mode-map ",R" 'twittering-retweet)
  (evil-define-key 'normal twittering-mode-map ",r" 'twittering-native-retweet)
  (evil-define-key 'normal twittering-mode-map ",d" 'twittering-direct-message)
  (evil-define-key 'normal twittering-mode-map ",n" 'twittering-update-status-interactive)
  (evil-define-key 'normal twittering-mode-map ")" 'twittering-switch-to-next-timeline)
  (evil-define-key 'normal twittering-mode-map ",N" 'twittering-switch-to-next-timeline)
  (evil-define-key 'normal twittering-mode-map "(" 'twittering-switch-to-next-timeline)
  (evil-define-key 'normal twittering-mode-map "t" 'twittering-goto-next-status)
  (evil-define-key 'normal twittering-mode-map "s" 'twittering-goto-previous-status)
  (defun custom-persp/social ()
    (interactive)
    (custom-persp "social"
		  (twit)
		  (switch-to-buffer ":home")))
  (define-key evil-normal-state-map (kbd ",ot") 'custom-persp/social))

(provide 'setup-twitter)
