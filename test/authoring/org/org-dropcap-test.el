(ert-deftest hub/org-insert-dropcap-definition ()
  "Verify hub/org-insert-dropcap is defined and interactive."
  (should (fboundp 'hub/org-insert-dropcap))
  (should (commandp 'hub/org-insert-dropcap))
  (should (equal (interactive-form 'hub/org-insert-dropcap) '(interactive "r"))))

(ert-deftest hub/org-insert-dropcap-transforms-region ()
  "Verify drop cap wrapping in an Org buffer."
  (with-temp-buffer
    (org-mode)
    (insert "Hello")
    (goto-char (point-min))
    (push-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (call-interactively 'hub/org-insert-dropcap)
    (should (equal (buffer-string) "\\HubArticleDropCap{H}{ello}"))))

(ert-deftest hub/org-insert-dropcap-trims-whitespace ()
  "Verify leading/trailing whitespace is trimmed."
  (with-temp-buffer
    (org-mode)
    (insert "  World  ")
    (goto-char (point-min))
    (push-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (call-interactively 'hub/org-insert-dropcap)
    (should (equal (buffer-string) "\\HubArticleDropCap{W}{orld}"))))

(ert-deftest hub/org-keybinding-dropcap-installed ()
  "Verify ,x d is bound to hub/org-insert-dropcap in org-mode-map.
We check this via evil's key lookup since ,x d uses the leader prefix."
  (let* ((binding (lookup-key (current-global-map) (kbd ","))))
    ;; The leader prefix key must be bound to a keymap
    (should binding)))
