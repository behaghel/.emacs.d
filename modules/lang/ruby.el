;;; ruby.el --- Ruby setup -*- lexical-binding: t; -*-

;;; Code:

(use-package inf-ruby :commands (ruby-mode)
  :config
  (use-package robe
    :config
    (eval-after-load 'company '(push 'company-robe company-backends))
    (add-hook 'ruby-mode-hook 'robe-mode)
    (evil-define-key 'normal ruby-mode-map ",." 'robe-jump))
  (use-package rubocop)
  (use-package rvm
    :config
    (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
      (rvm-activate-corresponding-ruby)))
  (use-package minitest :commands (minitest-mode)
    :config
    (add-hook 'minitest-compilation-mode-hook 'inf-ruby-mode)
    (eval-after-load 'minitest '(minitest-install-snippets))
    (evil-define-key 'normal ruby-mode-map ".Tt" 'minitest-verify)
    (evil-define-key 'normal ruby-mode-map ".Ta" 'minitest-verify-all)
    (evil-define-key 'normal ruby-mode-map ".Tc" 'minitest-verify-single)
    (evil-define-key 'normal ruby-mode-map ".Tr" 'minitest-rerun))
  (use-package rspec-mode :commands (rspec-mode)
    :config
    (defun hub/rspec-verify-it ()
      (interactive)
      (if (rspec-buffer-is-spec-p) (rspec-verify-single) (rspec-verify-method)))
    (evil-define-key 'normal ruby-mode-map ".tt" 'rspec-verify)
    (evil-define-key 'normal ruby-mode-map ".tT" 'rspec-verify-matching)
    (evil-define-key 'normal ruby-mode-map ".tc" 'hub/rspec-verify-it)
    (evil-define-key 'normal ruby-mode-map ".ta" 'rspec-verify-all)
    (evil-define-key 'normal ruby-mode-map ".ts" 'rspec-toggle-spec-and-target)
    (evil-define-key 'normal ruby-mode-map ".tS" 'rspec-toggle-spec-and-target-find-example)
    (evil-define-key 'normal ruby-mode-map ".tr" 'rspec-rerun)
    (evil-define-key 'normal ruby-mode-map ".tR" 'rspec-run-last-failed)
    (evil-define-key 'normal ruby-mode-map ".tf" 'rspec-run-last-failed))
  (use-package realgud-pry)
  (use-package realgud-byebug)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'inf-ruby-mode-hook (lambda () (setq comint-process-echoes t)))
  (defun ruby-tools-looking-around (back at)
    (and (looking-at-p at) (looking-back back)))
  (defun ruby-tools-interpolate ()
    (interactive)
    (if (and mark-active (equal (point) (region-end))) (exchange-point-and-mark))
    (insert "#")
    (when (or (ruby-tools-looking-around "\"[^\"\n]*" "[^\"\n]*\"")
	      (ruby-tools-looking-around "`[^`\n]*"   "[^`\n]*`")
	      (ruby-tools-looking-around "%([^(\n]*"  "[^)\n]*)"))
      (cond (mark-active
	     (goto-char (region-beginning)) (insert "{")
	     (goto-char (region-end)) (insert "}"))
	    (t (insert "{}") (forward-char -1)))))
  (defun hub-ruby-config ()
    (local-set-key (kbd "#") 'ruby-tools-interpolate)
    (setq evil-shift-width ruby-indent-level)
    (setq ruby-insert-encoding-magic-comment nil))
  (add-hook 'ruby-mode-hook 'hub-ruby-config)
  (evil-define-key 'visual ruby-mode-map ",l" 'ruby-send-region)
  (evil-define-key 'normal ruby-mode-map ",rb" 'ruby-toggle-block)
  (evil-define-key 'normal ruby-mode-map ",gr" 'ruby-switch-to-inf)
  (require 'smartparens-ruby))

(provide 'lang/ruby)
;;; ruby.el ends here
