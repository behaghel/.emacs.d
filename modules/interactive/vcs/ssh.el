;;; ssh.el --- VCS: SSH agent integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Graphical SSH askpass integration used by Git tooling.

;;; Code:

(when window-system
  ;; This is enough for plain Git subprocesses and avoids loading ssh-agency on
  ;; the GUI startup path.  The package remains available for Magit credential
  ;; hooks via its generated autoloads.
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

(use-package ssh-agency
  :if window-system
  :commands (ssh-agency-ensure))

(provide 'vcs/ssh)
;;; ssh.el ends here
