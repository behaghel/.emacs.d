;;; ssh.el --- VCS: SSH agent integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Graphical SSH askpass integration used by Git tooling.

;;; Code:

(use-package ssh-agency
  :if window-system
  :config (setenv "SSH_ASKPASS" "git-gui--askpass"))

(provide 'vcs/ssh)
;;; ssh.el ends here
