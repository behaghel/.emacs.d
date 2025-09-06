;; Clojure
(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :init
  (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  :config
  (evil-define-key 'normal cider-mode-map ",el" 'cider-load-buffer)
  (evil-define-key 'visual cider-mode-map ",l" 'cider-eval-region)
  (evil-define-key 'normal cider-mode-map ",." 'cider-jump-to-var)
  (evil-define-key 'normal cider-mode-map ",;" 'cider-jump-back)
  (evil-define-key 'normal cider-mode-map ",ii" 'cider-inspect)
  (evil-define-key 'normal cider-mode-map ",gr" 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal cider-mode-map ",hh" 'cider-doc)
  (evil-define-key 'normal cider-mode-map ",hg" 'cider-docview-grimoire)
  (evil-define-key 'normal cider-mode-map ",hG" 'cider-docview-grimoire-web)
  (yas-minor-mode 1) ; for adding require/use/import
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)
  ;; stolen from http://jakemccrary.com/blog/2015/06/30/my-favorite-clj-refactor-features/
  (use-package clj-refactor
    :init
    ;; Add custom magic requires.
    (dolist (mapping '(("maps" . "outpace.util.maps")
		       ("seqs" . "outpace.util.seqs")
		       ("times" . "outpace.util.times")
		       ("repl" . "outpace.util.repl")
		       ("time" . "clj-time.core")
		       ("string" . "clojure.string")))
      (add-to-list 'cljr-magic-require-namespaces mapping t))
    (setq cljr-favor-prefix-notation nil)
    :config
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-r")))


;;; 4clojure
(defadvice 4clojure-open-question (around 4clojure-open-question-around)
  "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
  ad-do-it
  (unless cider-current-clojure-buffer
    (cider-jack-in)))

(defun endless/4clojure-check-and-proceed ()
  "Check the answer and show the next question if it worked."
  (interactive)
  (unless
      (save-excursion
	;; Find last sexp (the answer).
	(goto-char (point-max))
	(forward-sexp -1)
	;; go to the beginning of the line for answer like :a :b :c
	(beginning-of-line)
	;; Check the answer.
	(cl-letf ((answer
		   (buffer-substring (point) (point-max))))
	  (goto-char (point-min))
	  (while (search-forward "__" nil t)
	    (replace-match answer))
	  (string-match "failed." (4clojure-check-answers))))
    (4clojure-next-question)))

(defadvice 4clojure/start-new-problem
    (after endless/4clojure/start-new-problem-advice () activate)
  ;; Prettify the 4clojure buffer.
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 3)
  (fill-paragraph)
  ;; Position point for the answer
  (goto-char (point-max))
  (insert "\n\n\n")
  (forward-char -1)
  (whitespace-mode -1)                  ; deactivate, it's annoying
  ;; Define our key.
  (local-set-key (kbd "M-j") #'endless/4clojure-check-and-proceed))

(defun 4clojure-login (user pwd)
  "Login to 4clojure"
  (interactive "sWhat's your name? \nsAnd your password ")
  (request
   "http://www.4clojure.com/login"
   :type "POST"
   :sync t
   :headers '(
	      ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:28.0) Gecko/20100101  Firefox/28.0")
	      ("Referer" . "http://www.4clojure.com/login")
	      )
					;   :parser 'buffer-string
   :data `(("user" . ,user) ("pwd" . ,pwd))
   :success (function*
	     (lambda (&key data &allow-other-keys)
	       data))
					; when server send 302 header, `request` redirect request with original method POST,
					; So 4clojure will not handle this redirect and given 404
   :status-code '((404 . (lambda (&rest _) (message "login successful!"))))
   )
  )

(provide 'setup-clojure)
