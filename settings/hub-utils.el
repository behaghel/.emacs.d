;;; hub-utils.el --- helpers and interactive functions I find useful  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: lisp

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

;; A bag of useful functions

;;; Code:

(defun hub/save-all ()
  "To be used to automatically save when I leave Emacs."
  (interactive)
  (save-some-buffers t))

;; stolen from @magnars
(defun hub/transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]."
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

;; stolen: http://stackoverflow.com/a/6541072/249234
(defun func-region (start end func)
  "Run a function FUNC over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun url-encode-region (start end)
  "Urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun url-decode-region (start end)
  "De-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun teleprompter ()
  "Scroll the display at a given interval"
  (interactive)
  (while 1
    (let ((sleep-time 4))
      (scroll-up-line)
      (sit-for sleep-time))))
;; (global-set-key (kbd "’") 'teleprompter)


(defun hub/dwim-other-window (f)
  "Run F in a new window if only one window is visible.
Otherwise switch to other window before."
  (if (one-window-p t 'visible)
      (split-window-right))
  (other-window 1)
  (funcall f))

(defun hub/switch-to-other-buffer ()
  "Switch to topmost non-visible buffer. On default bindings, same as
C-x b RET. The buffer selected is the one returned by (other-buffer)."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun hub/switch-dwim ()
  "Switch to the previously visited windows if multiple windows
  are visible else switch to other buffer."
  (interactive)
  (if (one-window-p t 'visible) (evil-buffer (other-buffer))
    (evil-window-mru)))

(defun hub/copy-buffer-file-name ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun hub/comment-dwim-line (&optional arg)
  "Replacement for the 'comment-dwim' command.
If no region is selected and current line is not blank
and we are not at the end of the line, then comment
current line. Replaces default behaviour of 'comment-dwim',
when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;;; stolen here: http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
(defun hub/font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<XXX\\>" 0 'font-lock-warning-face t)
         ("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 'font-lock-warning-face t))))
(provide 'hub-utils)
;;; hub-utils.el ends here
