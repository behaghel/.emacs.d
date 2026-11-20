;;; core-auth-test.el --- Platform pinentry policy tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the real auth configuration in isolated Emacs subprocesses.  Simulate
;; platform selection without opening a PIN prompt or accessing any secrets.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst hub/auth-test--root
  (expand-file-name
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository root for authentication configuration tests.")

(defun hub/auth-test--pinentry (platform)
  "Load authentication configuration for PLATFORM and return status and output."
  (with-temp-buffer
    (let* ((form
	    `(progn
	       (require 'epa)
	       (defmacro use-package (&rest _args) nil)
	       (let ((system-type ',platform))
		 (load ,(expand-file-name "core/core-auth.el"
					  hub/auth-test--root) nil t))
	       (princ (format "PINENTRY:%S:%S\n"
			      epg-pinentry-mode epa-pinentry-mode))))
	   (status
	    (call-process (expand-file-name invocation-name invocation-directory)
			  nil t nil "--batch" "-Q" "--eval"
			  (prin1-to-string form))))
      (list status (buffer-string)))))

(ert-deftest hub/auth-macos-uses-native-pinentry ()
  "macOS delegates PIN entry to the configured external pinentry program."
  (pcase-let ((`(,status ,output) (hub/auth-test--pinentry 'darwin)))
    (should (equal status 0))
    (should (string-match-p "^PINENTRY:ask:ask$" output))))

(ert-deftest hub/auth-linux-retains-loopback-pinentry ()
  "Linux preserves PIN entry through Emacs, including terminal sessions."
  (pcase-let ((`(,status ,output) (hub/auth-test--pinentry 'gnu/linux)))
    (should (equal status 0))
    (should (string-match-p "^PINENTRY:loopback:loopback$" output))))

(provide 'core-auth-test)
;;; core-auth-test.el ends here
