;;; setup-ui.el --- Where I put all the tweaks for how emacs looks like  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: convenience, faces

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

;; TBD

;;; Code:

;; (set-face-attribute 'default nil :font "Droid Sans Mono-12")
;; (when (member "Source Code Pro-12" (font-family-list))
;; (when (member "mononoki-12" (font-family-list))
;;   (set-face-attribute 'default nil :font "mononoki-12"))
;; https://github.com/be5invis/Iosevka
(set-face-attribute 'default nil :font "Iosevka-14")
;; stolen from: https://gist.github.com/mrkgnao/49c7480e1df42405a36b7ab09fe87f3d
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun setup-iosevka-ligatures ()
  (setq prettify-symbols-alist
        (append prettify-symbols-alist '(

                                         ;; Double-ended hyphen arrows ----------------
                                         ("<->" . #Xe100)
                                         ("<-->" . #Xe101)
                                         ("<--->" . #Xe102)
                                         ("<---->" . #Xe103)
                                         ("<----->" . #Xe104)

                                         ;; Double-ended equals arrows ----------------
                                         ("<=>" . #Xe105)
                                         ("<==>" . #Xe106)
                                         ("<===>" . #Xe107)
                                         ("<====>" . #Xe108)
                                         ("<=====>" . #Xe109)

                                         ;; Double-ended asterisk operators ----------------
                                         ("<**>" . #Xe10a)
                                         ("<***>" . #Xe10b)
                                         ("<****>" . #Xe10c)
                                         ("<*****>" . #Xe10d)

                                         ;; HTML comments ----------------
                                         ("<!--" . #Xe10e)
                                         ("<!---" . #Xe10f)

                                         ;; Three-char ops with discards ----------------
                                         ("<$" . #Xe110)
                                         ("<$>" . #Xe111)
                                         ("$>" . #Xe112)
                                         ("<." . #Xe113)
                                         ("<.>" . #Xe114)
                                         (".>" . #Xe115)
                                         ("<*" . #Xe116)
                                         ("<*>" . #Xe117)
                                         ("*>" . #Xe118)
                                         ("<\\" . #Xe119)
                                         ("<\\>" . #Xe11a)
                                         ("\\>" . #Xe11b)
                                         ("</" . #Xe11c)
                                         ("</>" . #Xe11d)
                                         ;; ("/>" . #Xe11e) ruins XML doc
                                         ("<\"" . #Xe11f)
                                         ("<\">" . #Xe120)
                                         ("\">" . #Xe121)
                                         ("<'" . #Xe122)
                                         ("<'>" . #Xe123)
                                         ("'>" . #Xe124)
                                         ("<^" . #Xe125)
                                         ("<^>" . #Xe126)
                                         ("^>" . #Xe127)
                                         ("<&" . #Xe128)
                                         ("<&>" . #Xe129)
                                         ("&>" . #Xe12a)
                                         ("<%" . #Xe12b)
                                         ("<%>" . #Xe12c)
                                         ("%>" . #Xe12d)
                                         ("<@" . #Xe12e)
                                         ("<@>" . #Xe12f)
                                         ("@>" . #Xe130)
                                         ("<#" . #Xe131)
                                         ("<#>" . #Xe132)
                                         ("#>" . #Xe133)
                                         ("<+" . #Xe134)
                                         ("<+>" . #Xe135)
                                         ("+>" . #Xe136)
                                         ("<-" . #Xe137)
                                         ("<->" . #Xe138)
                                         ("->" . #Xe139)
                                         ("<!" . #Xe13a)
                                         ("<!>" . #Xe13b)
                                         ("!>" . #Xe13c)
                                         ("<?" . #Xe13d)
                                         ("<?>" . #Xe13e)
                                         ("?>" . #Xe13f)
                                         ("<|" . #Xe140)
                                         ("<|>" . #Xe141)
                                         ("|>" . #Xe142)
                                         ("<:" . #Xe143)
                                         ("<:>" . #Xe144)
                                         (":>" . #Xe145)

                                         ;; Colons ----------------
                                         ("::" . #Xe146)
                                         (":::" . #Xe147)
                                         ("::::" . #Xe148)

                                         ;; Arrow-like operators ----------------
                                         ("->" . #Xe149)
                                         ("->-" . #Xe14a)
                                         ("->--" . #Xe14b)
                                         ("->>" . #Xe14c)
                                         ("->>-" . #Xe14d)
                                         ("->>--" . #Xe14e)
                                         ("->>>" . #Xe14f)
                                         ("->>>-" . #Xe150)
                                         ("->>>--" . #Xe151)
                                         ("-->" . #Xe152)
                                         ("-->-" . #Xe153)
                                         ("-->--" . #Xe154)
                                         ("-->>" . #Xe155)
                                         ("-->>-" . #Xe156)
                                         ("-->>--" . #Xe157)
                                         ("-->>>" . #Xe158)
                                         ("-->>>-" . #Xe159)
                                         ("-->>>--" . #Xe15a)
                                         (">-" . #Xe15b)
                                         (">--" . #Xe15c)
                                         (">>-" . #Xe15d)
                                         (">>--" . #Xe15e)
                                         (">>>-" . #Xe15f)
                                         (">>>--" . #Xe160)
                                         ("=>" . #Xe161)
                                         ("=>=" . #Xe162)
                                         ("=>==" . #Xe163)
                                         ("=>>" . #Xe164)
                                         ("=>>=" . #Xe165)
                                         ("=>>==" . #Xe166)
                                         ("=>>>" . #Xe167)
                                         ("=>>>=" . #Xe168)
                                         ("=>>>==" . #Xe169)
                                         ("==>" . #Xe16a)
                                         ("==>=" . #Xe16b)
                                         ("==>==" . #Xe16c)
                                         ("==>>" . #Xe16d)
                                         ("==>>=" . #Xe16e)
                                         ("==>>==" . #Xe16f)
                                         ("==>>>" . #Xe170)
                                         ("==>>>=" . #Xe171)
                                         ("==>>>==" . #Xe172)
                                         (">=" . #Xe173)
                                         (">==" . #Xe174)
                                         (">>=" . #Xe175)
                                         (">>==" . #Xe176)
                                         (">>>=" . #Xe177)
                                         (">>>==" . #Xe178)
                                         ("<-" . #Xe179)
                                         ("-<-" . #Xe17a)
                                         ("--<-" . #Xe17b)
                                         ("<<-" . #Xe17c)
                                         ("-<<-" . #Xe17d)
                                         ("--<<-" . #Xe17e)
                                         ("<<<-" . #Xe17f)
                                         ("-<<<-" . #Xe180)
                                         ("--<<<-" . #Xe181)
                                         ("<--" . #Xe182)
                                         ("-<--" . #Xe183)
                                         ("--<--" . #Xe184)
                                         ("<<--" . #Xe185)
                                         ("-<<--" . #Xe186)
                                         ("--<<--" . #Xe187)
                                         ("<<<--" . #Xe188)
                                         ("-<<<--" . #Xe189)
                                         ("--<<<--" . #Xe18a)
                                         ("-<" . #Xe18b)
                                         ("--<" . #Xe18c)
                                         ("-<<" . #Xe18d)
                                         ("--<<" . #Xe18e)
                                         ("-<<<" . #Xe18f)
                                         ("--<<<" . #Xe190)
                                         ("<=" . #Xe191)
                                         ("=<=" . #Xe192)
                                         ("==<=" . #Xe193)
                                         ("<<=" . #Xe194)
                                         ("=<<=" . #Xe195)
                                         ("==<<=" . #Xe196)
                                         ("<<<=" . #Xe197)
                                         ("=<<<=" . #Xe198)
                                         ("==<<<=" . #Xe199)
                                         ("<==" . #Xe19a)
                                         ("=<==" . #Xe19b)
                                         ("==<==" . #Xe19c)
                                         ("<<==" . #Xe19d)
                                         ("=<<==" . #Xe19e)
                                         ("==<<==" . #Xe19f)
                                         ("<<<==" . #Xe1a0)
                                         ("=<<<==" . #Xe1a1)
                                         ("==<<<==" . #Xe1a2)
                                         ("=<" . #Xe1a3)
                                         ("==<" . #Xe1a4)
                                         ("=<<" . #Xe1a5)
                                         ("==<<" . #Xe1a6)
                                         ("=<<<" . #Xe1a7)
                                         ("==<<<" . #Xe1a8)

                                         ;; Monadic operators ----------------
                                         (">=>" . #Xe1a9)
                                         (">->" . #Xe1aa)
                                         (">-->" . #Xe1ab)
                                         (">==>" . #Xe1ac)
                                         ("<=<" . #Xe1ad)
                                         ("<-<" . #Xe1ae)
                                         ("<--<" . #Xe1af)
                                         ("<==<" . #Xe1b0)

                                         ;; Composition operators ----------------
                                         (">>" . #Xe1b1)
                                         (">>>" . #Xe1b2)
                                         ("<<" . #Xe1b3)
                                         ("<<<" . #Xe1b4)

                                         ;; Lens operators ----------------
                                         (":+" . #Xe1b5)
                                         (":-" . #Xe1b6)
                                         (":=" . #Xe1b7)
                                         ("+:" . #Xe1b8)
                                         ("-:" . #Xe1b9)
                                         ("=:" . #Xe1ba)
                                         ("=^" . #Xe1bb)
                                         ("=+" . #Xe1bc)
                                         ("=-" . #Xe1bd)
                                         ("=*" . #Xe1be)
                                         ("=/" . #Xe1bf)
                                         ("=%" . #Xe1c0)
                                         ("^=" . #Xe1c1)
                                         ("+=" . #Xe1c2)
                                         ("-=" . #Xe1c3)
                                         ("*=" . #Xe1c4)
                                         ("/=" . #Xe1c5)
                                         ("%=" . #Xe1c6)

                                         ;; Logical ----------------
                                         ("/\\" . #Xe1c7)
                                         ("\\/" . #Xe1c8)

                                         ;; Semigroup/monoid operators ----------------
                                         ("<>" . #Xe1c9)
                                         ("<+" . #Xe1ca)
                                         ("<+>" . #Xe1cb)
                                         ("+>" . #Xe1cc)
                                         ))))

(defun refresh-pretty ()
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))

;; Hooks for modes in which to install the Iosevka ligatures
(mapc (lambda (hook)
        (add-hook hook (lambda () (setup-iosevka-ligatures) (refresh-pretty))))
      '(text-mode-hook
        prog-mode-hook))
(global-prettify-symbols-mode +1)

; try to stabilize windows and buffers positions
(setq switch-to-buffer-preserve-window-point 'already-displayed)

;; No annoying buffer for completion, compilation, help...
(use-package popwin
  :config
  (popwin-mode 1))

(save-place-mode 1)

;; Look / Theme
;; http://pawelbx.github.io/emacs-theme-gallery/
;;
;; (load-theme 'zenburn t)
;; ;; zenburn region face is invisible...
;; (set-face-attribute 'region nil :background "#666")
;; (use-package moe-theme
;;   :config
;;   (moe-dark))
;; (use-package dakrone-theme
;;   :config
;;   (load-theme 'dakrone t))
;; http://chriskempson.github.io/base16/#eighties
;; (load-theme 'base16-eighties-dark t)
(use-package sublime-themes
  :config
  (load-theme 'odersky t))


(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

;;; Smart Mode Line
(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(column-number-mode 1)               ; show column number in mode line

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'setup-ui)
;;; setup-ui.el ends here
