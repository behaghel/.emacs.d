;;; core.el --- Personal apps entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Coarse entrypoint for standalone personal productivity applications.

;;; Code:

(require 'apps/elfeed)
(require 'apps/restclient)
(require 'apps/edit-server)
(require 'apps/herbert)

(use-package websocket :defer t)

;; Entry keys stay available before any player, library or palette is loaded.
(require 'apps/music)

(autoload 'hub/spotify-browser-connect "apps/spotify-browser" nil t)
(autoload 'hub/spotify-browser-disconnect "apps/spotify-browser" nil t)
(autoload 'hub/spotify-sdk-probe "apps/spotify-sdk-probe"
  "Check one track through the official SDK in Chrome, independently of EMMS." t)
(autoload 'hub/spotify-sdk-probe-stop "apps/spotify-sdk-probe"
  "Expire the isolated browser SDK session." t)

(autoload 'hub/spotify-headless-connect "apps/spotify-headless"
  "Connect the private headless Spotify receiver." t)
(autoload 'hub/spotify-headless-disconnect "apps/spotify-headless"
  "Disconnect the private headless Spotify receiver." t)

(autoload 'hub/spotify-liked-songs "apps/spotify"
  "Browse Spotify Liked Songs in the shared bottom slot." t)
(autoload 'hub/spotify-connect "apps/spotify-auth"
  "Authorize read-only Spotify library access." t)

(provide 'apps/core)
;;; core.el ends here
