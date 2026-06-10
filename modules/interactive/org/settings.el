;;; settings.el --- Org mode: shared customization group -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared Org customization group and path defaults.

;;; Code:

(require 'hub-utils)

(defgroup hub/org nil
  "Customizations for Org paths and behavior."
  :group 'org)

(defcustom hub/org-directory "~/Documents/org/"
  "Default Org directory. Set to nil to avoid overriding."
  :type '(choice (const :tag "Do not override" nil) directory)
  :group 'hub/org)

(when hub/org-directory (setq org-directory hub/org-directory))

(provide 'org/settings)
;;; settings.el ends here
