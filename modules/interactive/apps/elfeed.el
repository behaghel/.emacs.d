;;; elfeed.el --- Feeds: Elfeed configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; RSS reader with comfortable reading defaults and media helpers.

;;; Code:

(use-package elfeed
  :straight (:depth full)
  :commands elfeed
  :config
  (setq shr-width 80
	elfeed-show-entry-switch #'elfeed-display-buffer
	elfeed-search-filter "@6-months-ago +unread -podcast")
  (defun elfeed-display-buffer (buf &optional _act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))
  (defun elfeed-show-eww-open (&optional use-generic-p)
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)))
  (defun elfeed-search-eww-open (&optional use-generic-p)
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-search-browse-url use-generic-p)))
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
			      (kbd "TAB") 'shr-next-link
			      (kbd "C-t") 'elfeed-show-next
			      (kbd "C-s") 'elfeed-show-prev
			      "B" 'elfeed-show-eww-open
			      "q" 'delete-window)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
			      "U" 'elfeed-update
			      "à" 'elfeed-search-untag-all-unread
			      "b" 'elfeed-search-browse-url
			      "B" 'elfeed-search-eww-open))

(use-package elfeed-org
  :after elfeed
  :straight (:host github :repo "remyhonig/elfeed-org" :protocol https)
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "modules/interactive/apps/elfeed.org" user-emacs-directory)))
  (elfeed-org))

;; Optional web interface
;; (use-package elfeed-web
;;   :commands (elfeed-web-start)
;;   :config
;;   (setq httpd-host "0.0.0.0" httpd-port 9000)
;;   (elfeed-web-start))

(use-package emms
  :commands (emms)
  :bind (:map evil-normal-state-map
	      ("'mp" . emms-pause)
	      ("'mm" . emms))
  :config
  (emms-all)
  (emms-default-players)
  (setq emms-player-list '(emms-player-vlc-playlist emms-player-vlc emms-player-mplayer emms-player-mpg321 emms-player-ogg123))
  (defun browse-with-emms (url &optional _new-window) (emms-play-url url))
  (setq browse-url-browser-function '(("mp3$" . browse-with-emms)
				      ("." . browse-url-default-browser))))

(provide 'apps/elfeed)
;;; elfeed.el ends here
