;;; smoke.el --- Simple load tests -*- lexical-binding: t; -*-

(require 'ert)

;; Ensure we load the user's init in a controlled HOME
(let ((user-emacs-directory (file-name-as-directory default-directory)))
  (load-file "init.el"))

(ert-deftest hub/init-loads-and-writer-autoload-present ()
  "Config loads and exposes writing command autoload."
  (should (fboundp 'writing/enable-basics)))

;; Local Variables:
;; no-byte-compile: t
;; End:
