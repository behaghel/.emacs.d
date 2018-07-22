;; stolen: https://gist.github.com/Yevgnen/9067fcc025caa084d2fa7ae1d65315af
(require 'projectile)

(defvar ivy--switch-buffer-name-max-length 40)
(defvar ivy--switch-buffer-mode-max-length 18)
(defvar ivy--switch-buffer-project-max-length 15)
(defvar ivy--switch-buffer-delimiter "")

(defun ivy--switch-buffer-pad (str maxlen)
  (if (>= (length str) maxlen) str
    (concat str (make-string (- maxlen (length str)) ? ))))

(defun ivy--switch-buffer-mode (mode)
  (capitalize
   (replace-regexp-in-string "-" " " (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))))

(defun ivy--switch-buffer-user-buffer-p (name)
  (not (string-match "^\\*" str)))

(defun ivy--switch-buffer-excluded-modes-p (modes)
  (not (memq major-mode modes)))

(defun ivy--switch-buffer-shorten-path (file)
  (let ((regexp "\\/?.+?\\/\\(.+\\)\\/.+?\\/.*"))
    (replace-regexp-in-string regexp "…" file nil nil 1)))

(defun ivy-switch-buffer-rich-transformer (str)
  (let ((buf (get-buffer str)))
    (if buf
        (with-current-buffer buf
          (let* (;; Buffer flags
                 (modified (propertize (if (and (buffer-modified-p)
                                                (ivy--switch-buffer-user-buffer-p str))
                                           "*"
                                         "")
                                       'face 'error))
                 (readonly (propertize (if (and buffer-read-only
                                                ;; (ivy--switch-buffer-excluded-modes-p '(dired-mode))
                                                (ivy--switch-buffer-user-buffer-p str))
                                           "!"
                                         "")
                                       'face 'error))
                 (process (propertize (if (get-buffer-process (current-buffer))
                                          "&"
                                        "")
                                      'face 'error))
                 ;; Buffer name
                 (indicator (ivy--switch-buffer-pad (format "%s%s%s" readonly modified process) 3))
                 (name (ivy--switch-buffer-pad str ivy--switch-buffer-name-max-length))
                 (name (propertize name 'face 'ivy-modified-buffer))
                 ;; Project
                 (root (file-truename (or (and (projectile-project-p)
                                               (projectile-project-root))
                                          default-directory)))
                 ;; Major mode
                 (mode (ivy--switch-buffer-pad (ivy--switch-buffer-mode major-mode) ivy--switch-buffer-mode-max-length))
                 (mode (propertize mode 'face 'warning))
                 ;; Project
                 (project (projectile-project-name))
                 (project (propertize (ivy--switch-buffer-pad
                                       (if (string= project "-")
                                           ""
                                         project)
                                       ivy--switch-buffer-project-max-length)
                                      'face 'success))
                 ;; Path
                 (path-max-length (- (window-width)
                                     ivy--switch-buffer-name-max-length
                                     (length indicator)
                                     ivy--switch-buffer-mode-max-length
                                     ivy--switch-buffer-project-max-length))
                 (path (file-truename (or (buffer-file-name) default-directory)))
                 (path (if (string= root path)
                           path
                         (substring-no-properties path (length root))))
                 (path (if (> (length path) path-max-length)
                           (ivy--switch-buffer-shorten-path path)
                         path))
                 (path (ivy--switch-buffer-pad path path-max-length))
                 (display (format "%s%s%s%s%s%s%s%s"
                                  name indicator ivy--switch-buffer-delimiter
                                  mode ivy--switch-buffer-delimiter
                                  project ivy--switch-buffer-delimiter
                                  path)))
            display))
      str)))

(ivy-set-display-transformer
 'ivy-switch-buffer 'ivy-switch-buffer-rich-transformer)

(provide 'setup-ivy)