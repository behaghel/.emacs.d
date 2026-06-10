;;; elisp.el --- Emacs Lisp development conveniences -*- lexical-binding: t; -*-

;;; Commentary:
;; Language-agnostic Emacs Lisp helpers and bindings shared by Lisp-derived
;; modes.

;;; Code:

(require 'jka-compr)

(evil-define-key 'normal lisp-mode-shared-map
		 ",." #'find-function
		 ",hf" #'describe-function
		 ",hv" #'describe-variable
		 ",hc" #'describe-char
		 ",el" #'eval-last-sexp
		 ",il" #'eval-print-last-sexp)
(eval-after-load 'eldoc '(diminish 'eldoc-mode))

(provide 'dev/elisp)
;;; elisp.el ends here
