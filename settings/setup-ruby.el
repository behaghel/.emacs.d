; Ruby
(use-package inf-ruby
  :commands (ruby-mode)
  :config

  (use-package robe
    :config
    (eval-after-load 'company
      '(push 'company-robe company-backends))
    (add-hook 'ruby-mode-hook 'robe-mode)
    (evil-define-key 'normal ruby-mode-map ",." 'robe-jump))

  (use-package rubocop)

  (use-package rvm
    :config
    (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
      (rvm-activate-corresponding-ruby)))

  (use-package minitest
    :commands (minitest-mode)
    :config
    (add-hook 'minitest-compilation-mode-hook 'inf-ruby-mode) ;or you can't debug
    (eval-after-load 'minitest
      '(minitest-install-snippets))
    ;; (add-hook 'ruby-mode-hook 'minitest-mode)
    (evil-define-key 'normal ruby-mode-map ",Tt" 'minitest-verify)
    (evil-define-key 'normal ruby-mode-map ",Ta" 'minitest-verify-all)
    (evil-define-key 'normal ruby-mode-map ",Tc" 'minitest-verify-single)
    (evil-define-key 'normal ruby-mode-map ",Tr" 'minitest-rerun))

  (use-package rspec-mode
    :commands (rspec-mode)
    :config
    (defun hub/rspec-verify-it ()
      "Run example at point or spec related to method at point."
      (interactive)
      (if (rspec-buffer-is-spec-p)
          (rspec-verify-single)
        (rspec-verify-method)))
    (evil-define-key 'normal ruby-mode-map ",et" 'rspec-verify) ; run spec for file
    (evil-define-key 'normal ruby-mode-map ",eT" 'rspec-verify-matching)
    (evil-define-key 'normal ruby-mode-map ",ec" 'hub/rspec-verify-it)
    (evil-define-key 'normal ruby-mode-map ",ea" 'rspec-verify-all)
    (evil-define-key 'normal ruby-mode-map ",es" 'rspec-toggle-spec-and-target)
    (evil-define-key 'normal ruby-mode-map ",eS" 'rspec-toggle-spec-and-target-find-example)
    (evil-define-key 'normal ruby-mode-map ",er" 'rspec-rerun)
    (evil-define-key 'normal ruby-mode-map ",eR" 'rspec-run-last-failed)
    (evil-define-key 'normal ruby-mode-map ",ef" 'rspec-run-last-failed))

  (use-package realgud-pry)
  (use-package realgud-byebug)
  ;; so that I can debug things with `binding.pry'
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby.
  (add-hook 'inf-ruby-mode-hook '(lambda ()
                                   ;; turn off the annoying input echo in irb
                                   (setq comint-process-echoes t)))
  ;; stolen from https://github.com/rejeep/ruby-tools.el/blob/master/ruby-tools.el
  (defun ruby-tools-looking-around (back at)
    "Check if looking backwards at BACK and forward at AT."
    (and (looking-at-p at) (looking-back back)))
  (defun ruby-tools-interpolate ()
    "Interpolate with #{} in some places."
    (interactive)
    (if (and mark-active (equal (point) (region-end)))
        (exchange-point-and-mark))
    (insert "#")
    (when (or
           (ruby-tools-looking-around "\"[^\"\n]*" "[^\"\n]*\"")
           (ruby-tools-looking-around "`[^`\n]*"   "[^`\n]*`")
           (ruby-tools-looking-around "%([^(\n]*"  "[^)\n]*)"))
      (cond (mark-active
             (goto-char (region-beginning))
             (insert "{")
             (goto-char (region-end))
             (insert "}"))
            (t
             (insert "{}")
             (forward-char -1)))))
  (defun hub-ruby-config ()
    "My Ruby config."
    (local-set-key (kbd "#") 'ruby-tools-interpolate)
    ;; (hub/set-newline-and-indent-comment)
    ;; fix indenting with Evil
    (setq evil-shift-width ruby-indent-level)
    ;; stop adding coding: utf8 comment in every header
    (setq ruby-insert-encoding-magic-comment nil))
  (add-hook 'ruby-mode-hook 'hub-ruby-config)
  (evil-define-key 'visual ruby-mode-map ",l" 'ruby-send-region)
  (evil-define-key 'normal ruby-mode-map ",rb" 'ruby-toggle-block) ; do..end to {}
  (evil-define-key 'normal ruby-mode-map ",gr" 'ruby-switch-to-inf)
  (require 'smartparens-ruby))

(provide 'setup-ruby)