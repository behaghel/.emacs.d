;;; keys.el --- Global keymaps and discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; MVP global keys using leader/localleader semantics and DWIM helpers.
;; - Requires hub-keys for definers and DWIM
;; - Labels categories for which-key
;; - Adds a simple cheatsheet hydra under ",?"

;;; Code:

(require 'hub-utils)
(require 'hub-keys)

;; Ensure which-key labels are friendly if which-key is present
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
   ",f" "Files"
   ",b" "Buffers"
   ",w" "Windows"
   ",p" "Project"
   ",g" "VCS/Git"
   ",s" "Search"
   ",e" "Eval/Exec"
   ",c" "Code"
   ",t" "Test/Toggle"
   ",d" "Debug"
   ",r" "REPL/Run"
   ",h" "Help/Docs"
   ",o" "Open/Apps"
   ",x" "Text/Transform"
   ",y" "Yank/Copy"
   ",q" "Quit/Session"
   ",?" "Cheat sheet"))

;; Minimal cheatsheet hydra
(with-eval-after-load 'hydra
  (require 'hub-struct)
  (defhydra hub/hydra-cheatsheet (:hint nil :color blue)
	    "\nKeybinding categories (leader ,)
  f:Files  b:Buffers  w:Windows  p:Project  g:Git  s:Search  c:Code
  t:Test   d:Debug    r:REPL     e:Eval     h:Help o:Apps   q:Quit\n"
	    ("f" (message "See ,f prefix") :exit t)
	    ("b" (message "See ,b prefix") :exit t)
	    ("w" (message "See ,w prefix") :exit t)
	    ("p" (message "See ,p prefix") :exit t)
	    ("g" (message "See ,g prefix") :exit t)
	    ("s" (message "See ,s prefix") :exit t)
	    ("c" (message "See ,c prefix") :exit t)
	    ("t" (message "See ,t prefix") :exit t)
	    ("d" (message "See ,d prefix") :exit t)
	    ("r" (message "See ,r prefix") :exit t)
	    ("e" (message "See ,e prefix") :exit t)
	    ("h" (message "See ,h prefix") :exit t)
	    ("o" (message "See ,o prefix") :exit t)
	    ("q" (message "See ,q prefix") :exit t)
	    ("?" nil "exit"))
  (with-eval-after-load 'general
    (hub/define-leaders)
    (hub/leader "?" #'hub/hydra-cheatsheet/body)))

;; MVP global bindings
(with-eval-after-load 'general
  (hub/define-leaders)
  (hub/leader
   "f f" #'hub/dwim-find-file
   "b b" #'hub/dwim-switch-buffer
   "c f" #'hub/dwim-format-buffer
   "t t" #'hub/dwim-test-current
   "r r" #'hub/dwim-repl
   "g /" #'hub/dwim-vcs-grep
   "g s" #'magit-status
   "c s" #'hub/hydra-struct/body)
  (hub/localleader
   "s" #'hub/hydra-struct/body))

(provide 'editing/keys)
;;; keys.el ends here
