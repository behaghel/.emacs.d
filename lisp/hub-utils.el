;;; hub-utils.el --- helpers and interactive functions I find useful  -*- lexical-binding: t; -*-

;;; Commentary:
;; A bag of useful functions.

;;; Code:

(defun hub/save-all ()
  "Automatically save when leaving Emacs."
  (interactive) (save-some-buffers t))

(defun hub/transpose-params ()
  "Transpose params like (a, b) or {a, b} or [a, b]."
  (interactive)
  (let* ((end-of-first (cond
			((looking-at ", ") (point))
			((and (looking-back ",") (looking-at " ")) (- (point) 1))
			((looking-back ", ") (- (point) 2))
			(t (error "Place point between params to transpose."))))
	 (start-of-first (save-excursion (goto-char end-of-first) (move-backward-out-of-param) (point)))
	 (start-of-last (+ end-of-first 2))
	 (end-of-last (save-excursion (goto-char start-of-last) (move-forward-out-of-param) (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond ((point-is-in-string-p) (move-point-forward-out-of-string))
	  ((looking-at "(\\|{\\|\\[") (forward-list))
	  (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond ((point-is-in-string-p) (move-point-backward-out-of-string))
	  ((looking-back ")\\|}\\|\\]") (backward-list))
	  (t (backward-char)))))

(defun current-quotes-char () (nth 3 (syntax-ppss)))
(defalias 'point-is-in-string-p 'current-quotes-char)
(defun move-point-forward-out-of-string () (while (point-is-in-string-p) (forward-char)))
(defun move-point-backward-out-of-string () (while (point-is-in-string-p) (backward-char)))

(defun func-region (start end func)
  "Run function FUNC over region START..END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun url-encode-region (start end) (interactive "r") (func-region start end #'url-hexify-string))
(defun url-decode-region (start end) (interactive "r") (func-region start end #'url-unhex-string))

(defun teleprompter ()
  "Scroll the display at a fixed interval."
  (interactive)
  (while 1 (let ((sleep-time 4)) (scroll-up-line) (sit-for sleep-time))))

(defun hub/dwim-other-window (f)
  "Run F in a new window if only one visible; otherwise switch."
  (if (one-window-p t 'visible) (split-window-right))
  (other-window 1) (funcall f))

(defun hub/switch-to-other-buffer ()
  "Switch to topmost non-visible buffer."
  (interactive) (switch-to-buffer (other-buffer)))

(defun hub/switch-dwim ()
  "Switch to previous window layout or other buffer."
  (interactive)
  (if (one-window-p t 'visible) (evil-buffer (other-buffer)) (evil-window-mru)))

(defun hub/window--center-x (window)
  "Return WINDOW horizontal center in frame coordinates."
  (let ((edges (window-edges window)))
    (/ (+ (nth 0 edges) (nth 2 edges)) 2.0)))

(defun hub/window--center-y (window)
  "Return WINDOW vertical center in frame coordinates."
  (let ((edges (window-edges window)))
    (/ (+ (nth 1 edges) (nth 3 edges)) 2.0)))

(defun hub/window--candidate-windows (target-side)
  "Return visible window candidates for moving toward TARGET-SIDE.
If the current window is itself a side window, moving in any other direction
prefers non-side windows so that directional focus can escape sidebars."
  (let* ((windows (window-list nil 'no-minibuf))
	 (current-side (window-parameter (selected-window) 'window-side))
	 (prefer-main (and current-side (not (eq current-side target-side))))
	 main-windows)
    (dolist (window windows)
      (unless (window-parameter window 'window-side)
	(push window main-windows)))
    (if (and prefer-main main-windows)
	(nreverse main-windows)
      windows)))

(defun hub/window--select-extreme (edge-index direction secondary-fn target-side)
  "Select the extreme visible window along EDGE-INDEX.
DIRECTION is the symbol `<` or `>` to minimize or maximize the edge value.
SECONDARY-FN computes the tie-break distance axis from a window.
TARGET-SIDE is the semantic direction symbol: left, top, bottom, or right."
  (let* ((current (selected-window))
	 (current-secondary (funcall secondary-fn current))
	 (windows (hub/window--candidate-windows target-side))
	 best-window
	 best-primary
	 best-distance)
    (dolist (window windows)
      (let* ((edges (window-edges window))
	     (primary (nth edge-index edges))
	     (distance (abs (- (funcall secondary-fn window) current-secondary))))
	(when (or (null best-window)
		  (and (eq direction '<) (< primary best-primary))
		  (and (eq direction '>) (> primary best-primary))
		  (and (= primary best-primary)
		       (< distance best-distance)))
	  (setq best-window window
		best-primary primary
		best-distance distance))))
    (when (window-live-p best-window)
      (select-window best-window))))

(defun hub/window-focus-far-left ()
  "Focus the leftmost visible window."
  (interactive)
  (hub/window--select-extreme 0 '< #'hub/window--center-y 'left))

(defun hub/window-focus-far-up ()
  "Focus the topmost visible window."
  (interactive)
  (hub/window--select-extreme 1 '< #'hub/window--center-x 'top))

(defun hub/window-focus-far-down ()
  "Focus the bottommost visible window."
  (interactive)
  (hub/window--select-extreme 3 '> #'hub/window--center-x 'bottom))

(defun hub/window-focus-far-right ()
  "Focus the rightmost visible window."
  (interactive)
  (hub/window--select-extreme 2 '> #'hub/window--center-y 'right))

(defun hub/copy-buffer-file-name ()
  "Copy the current file name to clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (with-temp-buffer (insert filename) (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun hub/sluggify (s)
  "Return a URL slug for string S.
Lowercase, trim, replace non-alphanumerics with '-', and collapse repeats."
  (let* ((down (downcase (string-trim s)))
	 (norm (replace-regexp-in-string "[^a-z0-9]+" "-" down))
	 (collapse (replace-regexp-in-string "-+" "-" norm))
	 (trimmed (string-trim collapse "^-+" "-+$")))
    trimmed))

(defun hub/comment-dwim-line (&optional arg)
  "Comment DWIM on current line if no region; otherwise delegate."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun hub/font-lock-comment-annotations ()
  "Highlight comment annotations like TODO, FIXME."
  (font-lock-add-keywords
   nil '(("\\<XXX\\>" 0 'font-lock-warning-face t)
	 ("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
	  1 'font-lock-warning-face t))))

(provide 'hub-utils)
;;; hub-utils.el ends here
