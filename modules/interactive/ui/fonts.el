;;; fonts.el --- UI font and icon configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Graphical font, icon, emoji, and symbol fallback configuration.

;;; Code:

(use-package all-the-icons :if (display-graphic-p))

;; Emoji and symbol font fallback so headers/subjects render properly.
(when (display-graphic-p)
  (setq use-default-font-for-symbols nil)
  (let ((emoji-font
	 (or (and (find-font (font-spec :family "Noto Color Emoji"))
		  (font-spec :family "Noto Color Emoji"))
	     (and (find-font (font-spec :family "Apple Color Emoji"))
		  (font-spec :family "Apple Color Emoji"))
	     (and (find-font (font-spec :family "Segoe UI Emoji"))
		  (font-spec :family "Segoe UI Emoji")))))
    (when emoji-font
      ;; Prefer emoji font for emoji and general symbol ranges.
      (set-fontset-font t 'emoji emoji-font nil 'prepend)
      (set-fontset-font t 'symbol emoji-font nil 'prepend)
      ;; Ensure common emoji blocks have a fallback.
      (dolist (range '((#x1F300 . #x1FAFF) ; Misc Symbols and Pictographs .. Symbols and Pictographs Extended-A
		       (#x2600 . #x27BF))) ; Misc symbols
	(set-fontset-font t range emoji-font nil 'prepend)))))

;; Fonts (adjust per system).
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 180)
(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.0)

(provide 'ui/fonts)
;;; fonts.el ends here
