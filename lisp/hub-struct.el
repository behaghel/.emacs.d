;;; hub-struct.el --- Structural motions and hydra -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal structural motion helpers with Tree-sitter when available,
;; Smartparens or built-ins as fallback. Includes a hydra entrypoint.

;;; Code:

(require 'hub-utils)
(defun hub/treesit-available-p ()
  (and (fboundp 'treesit-node-at) (treesit-node-at (point))))

(defun hub/struct-parent ()
  "Go to parent node; fallback to backward-up-list."
  (interactive)
  (cond
   ((hub/treesit-available-p)
    (let* ((node (treesit-node-at (point)))
	   (parent (and node (treesit-node-parent node))))
      (when parent (goto-char (treesit-node-start parent)))))
   ((fboundp 'sp-backward-up-sexp) (sp-backward-up-sexp))
   (t (backward-up-list))))

(defun hub/struct-first-child ()
  "Go to first child node; fallback to down-list."
  (interactive)
  (cond
   ((hub/treesit-available-p)
    (let* ((node (treesit-node-at (point)))
	   (child (and node (treesit-node-child node 0))))
      (when child (goto-char (treesit-node-start child)))))
   ((fboundp 'sp-down-sexp) (sp-down-sexp))
   (t (down-list 1))))

(defun hub/struct-next-sibling ()
  (interactive)
  (when (hub/treesit-available-p)
    (let* ((node (treesit-node-at (point)))
	   (sib (and node (treesit-node-next-sibling node))))
      (when sib (goto-char (treesit-node-start sib))))))

(defun hub/struct-prev-sibling ()
  (interactive)
  (when (hub/treesit-available-p)
    (let* ((node (treesit-node-at (point)))
	   (sib (and node (treesit-node-prev-sibling node))))
      (when sib (goto-char (treesit-node-start sib))))))

(defun hub/struct-next-defun ()
  "Next function/class via Tree-sitter if available; else end-of-defun."
  (interactive)
  (cond
   ((fboundp 'treesit-end-of-defun) (treesit-end-of-defun 1))
   (t (end-of-defun 1))))

(defun hub/struct-prev-defun ()
  (interactive)
  (cond
   ((fboundp 'treesit-beginning-of-defun) (treesit-beginning-of-defun 1))
   (t (beginning-of-defun 1))))

(defun hub/struct-mark-defun ()
  (interactive)
  (cond
   ((fboundp 'mark-defun) (mark-defun))
   (t (message "No mark-defun available"))))

;; Smartparens operations (if available)
(defun hub/struct-sp-slurp-forward () (interactive) (when (fboundp 'sp-forward-slurp-sexp) (sp-forward-slurp-sexp)))
(defun hub/struct-sp-slurp-backward () (interactive) (when (fboundp 'sp-backward-slurp-sexp) (sp-backward-slurp-sexp)))
(defun hub/struct-sp-barf-forward () (interactive) (when (fboundp 'sp-forward-barf-sexp) (sp-forward-barf-sexp)))
(defun hub/struct-sp-barf-backward () (interactive) (when (fboundp 'sp-backward-barf-sexp) (sp-backward-barf-sexp)))
(defun hub/struct-sp-unwrap () (interactive) (when (fboundp 'sp-unwrap-sexp) (sp-unwrap-sexp)))
(defun hub/struct-sp-splice () (interactive) (when (fboundp 'sp-splice-sexp) (sp-splice-sexp)))
(defun hub/struct-sp-transpose () (interactive) (when (fboundp 'sp-transpose-sexp) (sp-transpose-sexp)))
(defun hub/struct-sp-wrap-round () (interactive) (when (fboundp 'sp-wrap-round) (sp-wrap-round 1)))

(with-eval-after-load 'hydra
  (defhydra hub/hydra-struct (:hint nil)
	    "\nStructural motions
  u:up  d:first-child  n:next-sib  p:prev-sib   f/F:next/prev defun   m:mark defun
  (: slurp←  ) slurp→   < barf←    > barf→      U:unwrap
"
	    ("u" hub/struct-parent)
	    ("d" hub/struct-first-child)
	    ("n" hub/struct-next-sibling)
	    ("p" hub/struct-prev-sibling)
	    ("f" hub/struct-next-defun)
	    ("F" hub/struct-prev-defun)
	    ("m" hub/struct-mark-defun)
	    (":" hub/struct-sp-slurp-backward)
	    (")" hub/struct-sp-slurp-forward)
	    ("<" hub/struct-sp-barf-backward)
	    (">" hub/struct-sp-barf-forward)
	    ("U" hub/struct-sp-unwrap)
	    ("q" nil "quit" :color blue)))

;; Insert-mode structural editing (SLIME/paredit-style) without leaving insert
(with-eval-after-load 'evil
  ;; Restrict structural editing bindings to programming buffers so text modes
  ;; (Org, Markdown, etc.) keep their native Meta-based motions.
  (require 'prog-mode)
  (evil-define-key 'insert prog-mode-map (kbd "C-<right>") #'hub/struct-sp-slurp-forward)
  (evil-define-key 'insert prog-mode-map (kbd "C-<left>")  #'hub/struct-sp-barf-forward)
  (evil-define-key 'insert prog-mode-map (kbd "M-<left>")  #'hub/struct-sp-slurp-backward)
  (evil-define-key 'insert prog-mode-map (kbd "M-<right>") #'hub/struct-sp-barf-backward)
  (evil-define-key 'insert prog-mode-map (kbd "C-M-u")     #'hub/struct-sp-unwrap)
  (evil-define-key 'insert prog-mode-map (kbd "C-M-s")     #'hub/struct-sp-splice)
  (evil-define-key 'insert prog-mode-map (kbd "C-M-t")     #'hub/struct-sp-transpose)
  (evil-define-key 'insert prog-mode-map (kbd "C-M-w")     #'hub/struct-sp-wrap-round))

;; ------------------------------
;; Evil text objects (MVP)
;; ------------------------------

(defcustom hub/struct-function-node-types
  '("function" "function_definition" "function_declaration" "method_definition"
    ;; Scala family
    "def_definition")
  "Tree-sitter node types that should be treated as a function/method definition."
  :type '(repeat string))

(defcustom hub/struct-class-node-types
  '("class" "class_definition" "type_definition"
    ;; Scala family
    "object_definition" "trait_definition" "enum_definition")
  "Tree-sitter node types that should be treated as a class/type/container."
  :type '(repeat string))

(defcustom hub/struct-block-node-types
  '("block" "compound_statement" "do_block" "braced_block" "suite")
  "Tree-sitter node types that should be treated as a code block."
  :type '(repeat string))

(defun hub/--treesit-node-of-type (types)
  "Return nearest ancestor TS node whose type is in TYPES."
  (when (hub/treesit-available-p)
    (let ((node (treesit-node-at (point))))
      (while (and node (not (member (treesit-node-type node) types)))
	(setq node (treesit-node-parent node)))
      node)))

(defun hub/--region-for-node (node inner)
  "Return cons (BEG . END) for NODE. If INNER, try to trim outer delimiters."
  (when node
    (let ((beg (treesit-node-start node))
	  (end (treesit-node-end node)))
      (when inner
	(let* ((first (treesit-node-child node 0))
	       (last  (treesit-node-child node (1- (treesit-node-child-count node)))))
	  (when (and first last)
	    (setq beg (treesit-node-start first))
	    (setq end (treesit-node-end last)))))
      (cons beg end))))

(defun hub/--select-region (beg end)
  (when (and beg end (< beg end))
    (goto-char beg)
    (set-mark (point))
    (goto-char end)))

;; Function text object
(defun hub/textobj-func (inner)
  (let* ((node (hub/--treesit-node-of-type hub/struct-function-node-types))
	 (reg (and node (hub/--region-for-node node inner))))
    (unless (and reg (hub/--select-region (car reg) (cdr reg)))
      (message "No function node here"))))

(defun hub/textobj-func-inner () (interactive) (hub/textobj-func t))
(defun hub/textobj-func-outer () (interactive) (hub/textobj-func nil))

;; Class text object
(defun hub/textobj-class (inner)
  (let* ((node (hub/--treesit-node-of-type hub/struct-class-node-types))
	 (reg (and node (hub/--region-for-node node inner))))
    (unless (and reg (hub/--select-region (car reg) (cdr reg)))
      (message "No class node here"))))

(defun hub/textobj-class-inner () (interactive) (hub/textobj-class t))
(defun hub/textobj-class-outer () (interactive) (hub/textobj-class nil))

;; Block text object
(defun hub/textobj-block (inner)
  (let* ((node (hub/--treesit-node-of-type hub/struct-block-node-types))
	 (reg (and node (hub/--region-for-node node inner))))
    (unless (and reg (hub/--select-region (car reg) (cdr reg)))
      (message "No block node here"))))

(defun hub/textobj-block-inner () (interactive) (hub/textobj-block t))
(defun hub/textobj-block-outer () (interactive) (hub/textobj-block nil))

(with-eval-after-load 'evil
  ;; Map text objects: if Tree-sitter isn’t available, these will just message.
  (define-key evil-inner-text-objects-map "f" #'hub/textobj-func-inner)
  (define-key evil-outer-text-objects-map "f" #'hub/textobj-func-outer)
  (define-key evil-inner-text-objects-map "c" #'hub/textobj-class-inner)
  (define-key evil-outer-text-objects-map "c" #'hub/textobj-class-outer)
  (define-key evil-inner-text-objects-map "b" #'hub/textobj-block-inner)
  (define-key evil-outer-text-objects-map "b" #'hub/textobj-block-outer))

;; Debug helper to understand node types at point and ancestors
(defun hub/struct-debug-node ()
  "Print the Tree-sitter node type at point and its ancestors."
  (interactive)
  (if (not (hub/treesit-available-p))
      (message "Tree-sitter node not available at point")
    (let ((node (treesit-node-at (point)))
	  (types '()))
      (while node
	(push (treesit-node-type node) types)
	(setq node (treesit-node-parent node)))
      (message "TS node chain: %s" (mapconcat #'identity types " -> ")))))

(provide 'hub-struct)
;;; hub-struct.el ends here
