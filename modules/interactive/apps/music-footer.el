;;; music-footer.el --- Provider data for shared panel footers -*- lexical-binding: t; -*-

;;; Commentary:
;; Music supplies actions and retained metadata.  Context-panels owns footer
;; geometry, wrapping, input dispatch and lifetime.  Nothing here fetches data.

;;; Code:
(require 'hub-utils)
(require 'apps/music)
(defvar hub/spotify--tracks)
(defvar hub/spotify--total)
(declare-function hub/emms-player--snapshot "apps/emms-player" ())
(declare-function hub/music-console--total "apps/music-console" (tracks))

(defun hub/music-footer--navigation (view)
  "Return shared navigation actions, omitting VIEW's own destination."
  (append
   (unless (eq view 'player)
     '((:key "m" :label "Player" :command hub/emms-player-open :select t)))
   (unless (eq view 'liked)
     '((:key "l" :label "Liked Songs" :command hub/spotify-liked-songs :select t)))
   '((:key "+" :label "Queue" :command hub/music-queue :select t)
     (:key "/" :label "Search" :command hub/music-search :select t)
     (:key "?" :label "Actions" :command hub/music-palette :select t))))

(defun hub/music-player-footer (_source _view)
  "Return passive player footer actions and the honest whole-queue total."
  (let* ((snapshot (hub/emms-player--snapshot))
	 (tracks (plist-get snapshot :tracks))
	 (state (plist-get snapshot :state))
	 (label (cond ((equal state "Waiting for audio") "Cancel request")
		      ((member state '("Playing" "Paused" "Seeking"))
		       (if (plist-get snapshot :paused) "Resume" "Pause"))
		      (t "Play"))))
    (list :actions
	  (append (list (list :key "SPC" :label label :command 'hub/emms-player-toggle :primary t))
		  '((:key "c" :label "-10s" :command hub/emms-seek-backward-10)
		    (:key "r" :label "+30s" :command hub/emms-seek-forward-30)
		    (:key "s" :label "Previous" :command hub/emms-player-previous)
		    (:key "t" :label "Next" :command hub/emms-player-next)
		    (:key "v" :label "Spectrum" :command hub/emms-spectrum-toggle))
		  (hub/music-footer--navigation 'player)
		  '((:key "q" :label "Close" :command hub/emms-player-close :select t)))
	  :metadata (format "%d tracks · %s" (length tracks) (hub/music-console--total tracks)))))

(defun hub/music-library-footer (_source _view)
  "Return passive Liked Songs actions and loaded-library coverage."
  (list :actions
	(append '((:key "RET" :label "Play" :command hub/spotify-play-selected :primary t)
		  (:key "a" :label "Enqueue" :command hub/spotify-add-selected)
		  (:key "g+" :label "More" :command hub/spotify-load-more)
		  (:key "gr" :label "Refresh" :command hub/spotify-refresh))
		(hub/music-footer--navigation 'liked)
		'((:key "q" :label "Close" :command hub/spotify-close :select t)))
	:metadata (if (numberp hub/spotify--total)
		      (format "%d of %d loaded" (length hub/spotify--tracks) hub/spotify--total)
		    "Library not loaded")))

(provide 'apps/music-footer)
;;; music-footer.el ends here
