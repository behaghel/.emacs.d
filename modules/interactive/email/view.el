;;; view.el --- Email: mu4e viewing UX and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Keys and helpers for mu4e main and headers views.

;;; Code:

(require 'hub-utils)

(require 'ffap)

(defun hub/copy-url-at-point-dwim (url)
  "Copy URL under point to kill-ring; consider shr and plain links.
If URL is non-nil (from `shr-url-at-point'), prefer it. Otherwise try
`ffap-url-at-point'."
  (interactive (list (when (fboundp 'shr-url-at-point)
		       (shr-url-at-point current-prefix-arg))))
  (let ((target (or url (ffap-url-at-point))))
    (if (not target)
	(message "No URL under point")
      (setq target (url-encode-url target))
      (kill-new target)
      (message "Copied %s" target))))

(with-eval-after-load 'mu4e
  ;; mu4e main buffer
  (evil-collection-define-key 'normal 'mu4e-main-mode-map
			      "ê" 'mu4e-headers-search
			      ",hh" 'mu4e-display-manual
			      "zO" 'org-msg-mode
			      "zê" 'mu4e-headers-toggle-full-search))

(with-eval-after-load 'mu4e-headers
  ;; headers view (bind only after mu4e-headers is available)
  (evil-collection-define-key 'normal 'mu4e-headers-mode-map
			      "F"  'mu4e-compose-forward
			      "O"  'mu4e-org-store-and-capture
			      "zO" 'org-msg-mode
			      ",à" 'mu4e-org-store-and-capture
			      "ê"  'mu4e-headers-search
			      "Ê"  'mu4e-headers-search-edit
			      "à"  'mu4e-headers-mark-for-refile
			      "À"  'mu4e-headers-mark-for-archive
			      "gs" 'mu4e-headers-prev-unread
			      "gt" 'mu4e-headers-next-unread
			      "\C-t" 'mu4e-headers-next
			      "\C-s" 'mu4e-headers-prev
			      "zÉ" 'mu4e-headers-toggle-include-related
			      "zé" 'mu4e-headers-toggle-threading
			      "zê" 'mu4e-headers-toggle-full-search
			      "gL" 'mu4e-show-log
			      "%"  'mu4e-headers-mark-pattern
			      ",é"  'mu4e-headers-mark-pattern
			      "É"   'mu4e-headers-mark-thread
			      "SPC" nil
			      "z!" (lambda () (interactive) (mu4e-headers-mark-thread nil '(read)))
			      "zD" (lambda () (interactive) (mu4e-headers-mark-thread nil '(delete)))
			      "zà" (lambda () (interactive) (mu4e-headers-mark-thread nil '(refile)))))

(provide 'email/view)
;;; view.el ends here
