;;; setup-elfeed.el --- RSS Reader right here        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: news

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

;; My attempt to connect better to content that matters to me.

;;; Code:

(use-package elfeed
  :commands elfeed
  :config
  ;; to limit width of entries for reading comfort
  (setq shr-width 80)
  ;; stolen from https://karthinks.com/blog/lazy-elfeed/
  (setq elfeed-show-entry-switch #'elfeed-display-buffer)
  (setq-default elfeed-search-filter "@6-months-ago +unread -podcast")
  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height))))
                   ;; '(
                   ;;   (display-buffer-at-bottom)
                   ;;   (inhibit-same-window . nil)
                   ;;       (window-height . 0.8)
                   ;;       ;; (reusable-frames . nil)
                   ;;       )
                   ;; )
    )
  (defun elfeed-show-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)))
  (defun elfeed-search-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-search-browse-url use-generic-p)))

  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    (kbd "TAB") 'shr-next-link
    (kbd "C-t") 'elfeed-show-next
    (kbd "C-s") 'elfeed-show-prev
    "B" 'elfeed-show-eww-open
    "q" 'delete-window
  )
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "U" 'elfeed-update
    "à" 'elfeed-search-untag-all-unread
    "b" 'elfeed-search-browse-url
    "B" 'elfeed-search-eww-open
  )
  )
(use-package elfeed-web
  :commands (elfeed-web-start)
  :config
  ;; on chromebook you also need a forward rule e.g. using this add-on
  ;; https://chrome.google.com/webstore/detail/connection-forwarder/ahaijnonphgkgnkbklchdhclailflinn/related
  (setq httpd-host "0.0.0.0")
  (setq httpd-port 9000)
  (elfeed-web-start))
(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org))

(use-package emms
  :bind (:map evil-normal-state-map
              ("'mp" . emms-pause)
              ("'mm" . emms)
              )
  :config
  (progn
    (emms-all)
    (emms-default-players)
    (setq emms-player-list '(emms-player-vlc-playlist
                             emms-player-vlc
                             emms-player-mplayer
                             emms-player-mpg321
                             emms-player-ogg123))
    (defun browse-with-emms (url &optional new-window)
      (emms-play-url url))

    (setq browse-url-browser-function
          '(
            ("mp3$" . browse-with-emms)
            ("." . browse-url-default-browser)
            ))

    ))

(provide 'setup-elfeed)
;;; setup-elfeed.el ends here