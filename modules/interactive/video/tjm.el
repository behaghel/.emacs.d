;;; tjm.el --- Text-driven video editing major mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Major mode for manipulating Textual Join Manifest (TJM) files that describe
;; text-driven video edits. Segments are presented as readable text while the
;; underlying JSON manifest is kept in sync. Inspired by subed / transcript
;; editing workflows.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'hydra)
(require 'image)
(require 'outline)

(declare-function image-file-name-p "image-mode" (filename))

(defgroup video-tjm nil
  "Editing spoken-word video manifests in Emacs."
  :group 'multimedia)

(defcustom video-tjm-play-program "mpv"
  "Executable used for segment playback."
  :type 'string
  :group 'video-tjm)

(defcustom video-tjm-play-args '("--quiet" "--no-terminal" "--really-quiet")
  "Additional arguments passed to `video-tjm-play-program'."
  :type '(repeat string)
  :group 'video-tjm)

(defcustom video-tjm-validation-on-save t
  "Whether TJM buffers should be validated automatically on save."
  :type 'boolean
  :group 'video-tjm)

(defcustom video-tjm-validation-time-tolerance 0.1
  "Permitted slack (in seconds) when comparing segment and word timings.
This accounts for rounding in source manifests so that we only flag
meaningful timing inconsistencies."
  :type 'float
  :group 'video-tjm)

(defcustom video-tjm-show-words-by-default nil
  "If non-nil, show per-word timings when rendering segments."
  :type 'boolean
  :group 'video-tjm)

(defface video-tjm-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for segment headings."
  :group 'video-tjm)

(defface video-tjm-text-face
  '((t :inherit default))
  "Face for segment prose."
  :group 'video-tjm)

(defface video-tjm-broll-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face to highlight segments with b-roll metadata."
  :group 'video-tjm)

(defface video-tjm-meta-face
  '((t :inherit font-lock-comment-face))
  "Face used for metadata lines (tags, notes, etc.)."
  :group 'video-tjm)

(defface video-tjm-marker-face
  '((t :inherit outline-1 :weight bold))
  "Face used for marker segments that act as headings."
  :group 'video-tjm)

(defface video-tjm-current-segment-face
  '((t :background "#2f3844" :extend t))
  "Face used to highlight the segment under point."
  :group 'video-tjm)

(defvar video-tjm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'video-tjm-play-segment)
    (define-key map (kbd "SPC") #'video-tjm-play-segment)
    (define-key map (kbd "C-t") #'video-tjm-next-segment)
    (define-key map (kbd "C-s") #'video-tjm-previous-segment)
    (define-key map (kbd "M-t") #'video-tjm-move-segment-down)
    (define-key map (kbd "M-s") #'video-tjm-move-segment-up)
    (define-key map (kbd "M-j") #'video-tjm-insert-marker)
    (define-key map (kbd "C-c n") #'video-tjm-next-segment)
    (define-key map (kbd "C-c p") #'video-tjm-previous-segment)
    (define-key map (kbd "C-c N") #'video-tjm-move-segment-down)
    (define-key map (kbd "C-c P") #'video-tjm-move-segment-up)
    (define-key map (kbd "C-c d") #'video-tjm-delete-segment)
    (define-key map (kbd "C-c e") #'video-tjm-edit-text)
    (define-key map (kbd "C-c s") #'video-tjm-edit-speaker)
    (define-key map (kbd "C-c C-n") #'video-tjm-edit-notes)
    (define-key map (kbd "C-c r") #'video-tjm-edit-start-end)
    (define-key map (kbd "C-c t") #'video-tjm-toggle-tag)
    (define-key map (kbd "C-c b") #'video-tjm-edit-broll)
    (define-key map (kbd "C-c C-b") #'video-tjm-edit-broll-placeholders)
    (define-key map (kbd "C-c w") #'video-tjm-toggle-words)
    (define-key map (kbd "C-c j") #'video-tjm-toggle-separator)
    (define-key map (kbd "C-c C-m") #'video-tjm-insert-marker)
    (define-key map (kbd "C-c B") #'video-tjm-toggle-broll-continue)
    (define-key map (kbd "C-c C-w") #'video-tjm-delete-word)
    (define-key map (kbd "C-c C-s") #'video-tjm-split-segment)
    (define-key map (kbd "C-c m") #'video-tjm-merge-with-next)
    (define-key map (kbd "C-c C-c") #'video-tjm-compile)
    (define-key map (kbd "C-c v") #'video-tjm-validate)
    (define-key map (kbd "C-c o") #'video-tjm-open-raw-json)
    (define-key map (kbd "C-c k") #'video-tjm-stop-playback)
    (define-key map (kbd "C-c l") #'video-tjm-reload)
    (define-key map (kbd "C-c u") #'video-tjm-undo)
    (define-key map (kbd "C-c R") #'video-tjm-redo)
    (define-key map (kbd "C-c ?") #'video-tjm-hydra/body)
    map)
  "Keymap used in `video-tjm-mode'.")

(with-eval-after-load 'evil
  (evil-define-key 'normal video-tjm-mode-map
		   (kbd "RET") #'video-tjm-play-segment
		   (kbd "SPC") #'video-tjm-play-segment
		   (kbd "C-t") #'video-tjm-next-segment
		   (kbd "C-s") #'video-tjm-previous-segment
		   (kbd "M-t") #'video-tjm-move-segment-down
		   (kbd "M-s") #'video-tjm-move-segment-up
		   (kbd "M-j") #'video-tjm-insert-marker
		   (kbd "t") #'evil-next-visual-line
		   (kbd "s") #'evil-previous-visual-line
		   (kbd "T") #'evil-next-visual-line
		   (kbd "S") #'evil-previous-visual-line
		   (kbd "k") #'video-tjm-delete-segment
		   (kbd "|") #'video-tjm-split-segment
		   (kbd "J") #'video-tjm-merge-with-next
		   (kbd "d") #'video-tjm-delete-word
		   (kbd "i") #'video-tjm-edit-text
		   (kbd "u") #'video-tjm-undo))

(defvar-local video-tjm--data nil
  "In-memory representation of the current TJM file.")

(defvar-local video-tjm--dirty nil
  "Whether `video-tjm--data' has diverged from disk.")

(defvar-local video-tjm--words-visible video-tjm-show-words-by-default
  "Display flag controlling whether word-level timing is shown.")

(defvar-local video-tjm--mpv-process nil
  "Handle to a running mpv playback process, if any.")

(defvar-local video-tjm--visual-separators nil
  "List of segment ids that should be followed by a blank line.")

(defvar-local video-tjm--focus-overlay nil
  "Overlay highlighting the current segment.")

(defvar-local video-tjm--last-echo-id nil
  "Segment id that was last echoed in the minibuffer.")

(defvar-local video-tjm--undo-stack nil
  "Stack of TJM snapshots for undo operations.")

(defvar-local video-tjm--redo-stack nil
  "Stack of TJM snapshots for redo operations.")

(defun video-tjm--auto-save-blocker (&rest _)
  "Predicate that prevents auto-saving visited files for TJM buffers."
  nil)

(defvar-local video-tjm--pending-output nil
  "Absolute path to the compiled video awaiting playback.")

(defvar-local video-tjm--pending-temp nil
  "Temporary TJM manifest generated for section compilation.")

(defvar-local video-tjm--pending-origin nil
  "Origin buffer associated with a compilation run.")

(defun video-tjm--default-output-file ()
  "Compute the default output mp4 path for the current TJM buffer."
  (when buffer-file-name
    (let ((parent (file-name-directory buffer-file-name))
	  (name (file-name-nondirectory
		 (directory-file-name
		  (file-name-directory buffer-file-name)))))
      (expand-file-name (concat name ".mp4") parent))))

(defun video-tjm--compile-command ()
  "Build the video-text-edit command for the current buffer."
  (let* ((file buffer-file-name)
	 (output (video-tjm--default-output-file)))
    (unless file
      (user-error "TJM buffer is not visiting a file"))
    (unless output
      (user-error "Unable to determine output filename"))
    (format "video-text-edit %s --output %s --subtitles --preserve-short-gaps 1.5"
	    (shell-quote-argument file)
	    (shell-quote-argument output))))

(defun video-tjm--slugify (title)
  "Return a filesystem-safe slug for TITLE."
  (let* ((down (downcase (or title "")))
	 (clean (replace-regexp-in-string "[^a-z0-9]+" "-" down))
	 (trim (replace-regexp-in-string "^-+\|+-+$" "" clean)))
    (if (string-empty-p trim)
	"section"
      trim)))

(defun video-tjm--output-path (&optional slug)
  "Compute the default output path, optionally suffixed with SLUG."
  (when buffer-file-name
    (let* ((dir (file-name-directory buffer-file-name))
	   (parent (directory-file-name dir))
	   (base (file-name-nondirectory parent))
	   (name (if (and slug (not (string-empty-p slug)))
		     (format "%s-%s" base slug)
		   base)))
      (expand-file-name (concat name ".mp4") dir))))

(defun video-tjm--write-json-file (data path)
  "Write DATA as JSON to PATH."
  (let ((json-encoding-pretty-print t)
	(json-encoding-lisp-style-closings t))
    (with-temp-file path
      (insert (json-encode data))
      (unless (bolp) (insert "\n")))))

(defun video-tjm--section-segments (marker)
  "Return segments following MARKER until the next marker."
  (let* ((segments (video-tjm--segments))
	 (start (cl-position marker segments :test #'eq)))
    (when start
      (let (result)
	(cl-loop for idx from (1+ start) below (length segments)
		 for seg = (nth idx segments)
		 until (video-tjm--marker-p seg)
		 do (push seg result))
	(nreverse result)))))

(defun video-tjm--raw-buffer-name ()
  "Return the buffer name used for the raw JSON view."
  (format "*video-tjm-raw:%s*"
	  (or (and buffer-file-name
		   (file-name-nondirectory buffer-file-name))
	      (buffer-name))))

(defun video-tjm--reload-associated-buffers (file)
  "Reload any `video-tjm-mode' buffers visiting FILE."
  (let ((target (expand-file-name file)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
	(with-current-buffer buf
	  (when (and (derived-mode-p 'video-tjm-mode)
		     buffer-file-name
		     (string= (expand-file-name buffer-file-name) target))
	    (video-tjm-reload)))))))

(defun video-tjm--raw-after-save ()
  "Refresh structured buffers when saving a raw TJM manifest."
  (when buffer-file-name
    (video-tjm--reload-associated-buffers buffer-file-name)))

(defun video-tjm--populate-raw-buffer (buf file)
  "Populate BUF with the literal contents of FILE."
  (with-current-buffer buf
    (setq-local buffer-file-name file)
    (setq-local buffer-file-truename
		(when (file-exists-p file)
		  (ignore-errors (file-truename file))))
    (setq-local default-directory (file-name-directory file))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents file nil nil nil t)
      (goto-char (point-min)))
    (set-buffer-modified-p nil)
    (setq-local buffer-read-only nil)
    (setq-local revert-buffer-function
		(lambda (&rest _)
		  (video-tjm--populate-raw-buffer buf file)))
    (add-hook 'after-save-hook #'video-tjm--raw-after-save nil t)
    (cond
     ((fboundp 'json-mode)
      (let* ((local-remap (copy-alist major-mode-remap-alist)))
	(setq local-remap (assq-delete-all 'json-mode local-remap))
	(setq local-remap (assq-delete-all 'jsonc-mode local-remap))
	(let ((major-mode-remap-alist local-remap))
	  (json-mode))))
     ((fboundp 'js-json-mode) (js-json-mode))
     ((fboundp 'js-mode) (js-mode))
     (t (fundamental-mode)))))

(defun video-tjm--play-file (file)
  "Play FILE using the configured media player."
  (let ((abs (expand-file-name file)))
    (unless (file-readable-p abs)
      (user-error "Output file not found: %s" abs))
    (video-tjm--play-with-mpv abs nil nil)
    (message "Playing %s" (file-name-nondirectory abs))))

(defun video-tjm--compilation-finished (buffer status)
  "Handle completion of video-text-edit BUFFER with STATUS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((output video-tjm--pending-output)
	    (temp video-tjm--pending-temp)
	    (origin video-tjm--pending-origin))
	(when (and temp (file-exists-p temp))
	  (ignore-errors (delete-file temp)))
	(when (and output
		   (string-match-p "^finished" (string-trim status))
		   (buffer-live-p origin))
	  (with-current-buffer origin
	    (condition-case err
		(video-tjm--play-file output)
	      (error (message "%s" (or (cadr err) err))))))
	(setq video-tjm--pending-output nil
	      video-tjm--pending-temp nil
	      video-tjm--pending-origin nil)))))

(defun video-tjm--run-compile (command output &optional temp)
  "Run COMMAND via `compilation-start`, capturing OUTPUT (and TEMP manifest)."
  (setq-local compile-command command)
  (let* ((origin-buffer (current-buffer))
	 (origin-window (selected-window))
	 (display-buffer-overriding-action
	  '((display-buffer-reuse-mode-window display-buffer-in-side-window)
	    (side . bottom)
	    (slot . 0)
	    (window-height . 12)
	    (inhibit-same-window . t)
	    (select . nil)
	    (window-parameters . ((no-other-window . t)))))
	 (compilation-buffer-name-function (lambda (_mode) "*video-text-edit*")))
    (let ((buffer (compilation-start command nil)))
      (when buffer
	(with-current-buffer buffer
	  (setq-local video-tjm--pending-output (and output (expand-file-name output)))
	  (setq-local video-tjm--pending-temp temp)
	  (setq-local video-tjm--pending-origin origin-buffer)
	  (add-hook 'compilation-finish-functions #'video-tjm--compilation-finished nil t))))
    (when (window-live-p origin-window)
      (when (and (buffer-live-p origin-buffer)
		 (not (eq (window-buffer origin-window) origin-buffer)))
	(set-window-buffer origin-window origin-buffer))
      (select-window origin-window))))

(defun video-tjm-toggle-broll-continue (segment)
  "Inherit b-roll from the previous segment and mark it as a continuation."
  (interactive (list (or (video-tjm--segment-at-point)
			 (user-error "No segment at point"))))
  (when (video-tjm--marker-p segment)
    (user-error "Markers do not carry b-roll"))
  (let* ((segments (video-tjm--segments))
	 (idx (cl-position segment segments :test #'eq)))
    (unless idx
      (error "Unable to determine segment index"))
    (let ((previous nil)
	  (scan (1- idx)))
      (while (and (null previous) (>= scan 0))
	(let ((candidate (nth scan segments)))
	  (when (and (not (video-tjm--marker-p candidate))
		     (alist-get 'broll candidate))
	    (setq previous candidate))
	  (setq scan (1- scan))))
      (unless previous
	(user-error "No previous segment with b-roll metadata found"))
      (let* ((prev-broll (copy-alist (alist-get 'broll previous)))
	     (curr-broll (copy-alist prev-broll))
	     (prev-offset (alist-get 'start_offset prev-broll))
	     (prev-duration (alist-get 'duration prev-broll))
	     (start-prev (video-tjm--coerce-number (alist-get 'start previous)))
	     (end-prev (video-tjm--coerce-number (alist-get 'end previous)))
	     (duration-prev (and start-prev end-prev (- end-prev start-prev)))
	     (offset-seconds (video-tjm--time->seconds prev-offset))
	     (continuation-offset (+ (or offset-seconds 0.0)
				     (or duration-prev 0.0)))
	     (remaining (when prev-duration
			  (max 0.0 (- (video-tjm--time->seconds prev-duration)
				      (or duration-prev 0.0))))))
	(video-tjm--record-state)
	(when curr-broll
	  (setf (alist-get 'continue curr-broll) t)
	  (setf (alist-get 'start_offset curr-broll)
		(video-tjm--seconds->time-like prev-offset continuation-offset))
	  (when prev-duration
	    (setf (alist-get 'duration curr-broll)
		  (if (> remaining 0.0)
		      (video-tjm--seconds->time-like prev-duration remaining)
		    nil))))
	(setf (alist-get 'broll segment) curr-broll)
	(video-tjm--mark-dirty)
	(video-tjm--render t)
	(video-tjm--goto-segment (alist-get 'id segment))
	(video-tjm--update-focus-overlay)
	(video-tjm--echo-segment-info)
	(message "Copied b-roll from previous segment and set continue")))))

(defun video-tjm-compile ()
  "Compile the current TJM (or section) using `video-text-edit`."
  (interactive)
  (let* ((segment (video-tjm--segment-at-point))
	 (marker? (and segment (video-tjm--marker-p segment))))
    (if (not marker?)
	(video-tjm--run-compile (video-tjm--compile-command)
				(video-tjm--default-output-file))
      (let* ((title (video-tjm--stringify (alist-get 'title segment)))
	     (slug (video-tjm--slugify title))
	     (subset (video-tjm--section-segments segment)))
	(unless subset
	  (user-error "Marker has no following segments to compile"))
	(let* ((temp (make-temp-file "video-tjm-section" nil ".json"))
	       (data (copy-tree video-tjm--data t))
	       (segments-copy (mapcar (lambda (seg) (copy-tree seg t)) subset))
	       (output (video-tjm--output-path slug))
	       (cmd (format "video-text-edit %s --output %s --subtitles --preserve-short-gaps 1.5"
			    (shell-quote-argument temp)
			    (shell-quote-argument output))))
	  (setf (alist-get 'segments data) segments-copy)
	  (video-tjm--write-json-file data temp)
	  (video-tjm--run-compile cmd output temp))))))

(defun video-tjm--play-marker (segment)
  "Play the compiled video associated with marker SEGMENT, compiling if needed."
  (let* ((title (video-tjm--stringify (alist-get 'title segment)))
	 (slug (video-tjm--slugify title))
	 (output (video-tjm--output-path slug)))
    (if (and output (file-exists-p output))
	(video-tjm--play-file output)
      (progn
	(message "No compiled output for '%s'; compiling section..." (or title slug))
	(video-tjm-compile)))))

(defhydra video-tjm-hydra (:hint nil :color teal)
	  "
Navigation         Edit                Metadata           Misc
───────────────   ─────────────────   ─────────────────  ───────────────────
_RET_/_SPC_ play   _C-c e_  text       _C-c t_ toggle tag  _C-c v_ validate
_C-t_       next   _C-c s_  speaker    _C-c b_ edit b-roll _C-c o_ raw JSON
_C-s_       prev   _C-c C-n_ notes     _C-c C-b_ placeholders _C-c k_ stop mpv
_C-c N_ / _M-t_ move↓  _C-c r_ timing    _C-c j_ paragraph    _C-c l_ reload
_C-c P_ / _M-s_ move↑  _C-c d_ segment    _C-c u_ undo         _C-c R_ redo
_M-j_       marker _C-c C-w_ word      _C-c C-s_ split
		  _C-c m_ merge
"
	  ("RET" video-tjm-play-segment)
	  ("SPC" video-tjm-play-segment)
	  ("C-t" video-tjm-next-segment)
	  ("C-s" video-tjm-previous-segment)
	  ("C-c n" video-tjm-next-segment)
	  ("C-c p" video-tjm-previous-segment)
	  ("C-c N" video-tjm-move-segment-down)
	  ("C-c P" video-tjm-move-segment-up)
	  ("M-t" video-tjm-move-segment-down)
	  ("M-s" video-tjm-move-segment-up)
	  ("C-c e" video-tjm-edit-text)
	  ("C-c s" video-tjm-edit-speaker)
	  ("C-c C-n" video-tjm-edit-notes)
	  ("C-c r" video-tjm-edit-start-end)
	  ("C-c d" video-tjm-delete-segment)
	  ("C-c C-w" video-tjm-delete-word)
	  ("C-c C-s" video-tjm-split-segment)
	  ("C-c m" video-tjm-merge-with-next)
	  ("M-j" video-tjm-insert-marker)
	  ("C-c C-m" video-tjm-insert-marker)
	  ("C-c t" video-tjm-toggle-tag)
	  ("C-c b" video-tjm-edit-broll)
	  ("C-c C-b" video-tjm-edit-broll-placeholders)
	  ("C-c w" video-tjm-toggle-words)
	  ("C-c j" video-tjm-toggle-separator)
	  ("C-c v" video-tjm-validate)
	  ("C-c o" video-tjm-open-raw-json)
	  ("C-c k" video-tjm-stop-playback)
	  ("C-c l" video-tjm-reload)
	  ("C-c u" video-tjm-undo)
	  ("C-c R" video-tjm-redo)
	  ("q" nil "quit"))

(defun video-tjm--snapshot ()
  "Create a deep copy of `video-tjm--data'."
  (json-parse-string (json-encode video-tjm--data)
		     :object-type 'alist :array-type 'list
		     :null-object nil :false-object nil))

(defun video-tjm--record-state ()
  "Push current state onto the undo stack prior to mutation."
  (push (video-tjm--snapshot) video-tjm--undo-stack)
  (setq video-tjm--redo-stack nil))

(defun video-tjm--stringify (value)
  "Return VALUE rendered as a string."
  (cond
   ((null value) "")
   ((stringp value) value)
   (t (format "%s" value))))

(defun video-tjm--aget (key alist)
  "Fetch KEY from ALIST supporting both symbol and string lookups."
  (cond
   ((null alist) nil)
   ((symbolp key)
    (or (alist-get key alist)
	(alist-get (symbol-name key) alist nil nil #'equal)))
   ((stringp key)
    (or (alist-get key alist nil nil #'equal)
	(let ((sym (ignore-errors (intern key))))
	  (and (symbolp sym) (alist-get sym alist)))))
   (t (alist-get key alist nil nil #'equal))))

(defun video-tjm--segment-kind (segment)
  (let ((kind (alist-get 'kind segment)))
    (when kind
      (downcase (video-tjm--stringify kind)))))

(defun video-tjm--marker-p (segment)
  (string= (video-tjm--segment-kind segment) "marker"))

(defun video-tjm--display-time (value)
  (cond
   ((null value) nil)
   ((numberp value) (video-tjm--format-time value))
   ((and (stringp value) (string-match-p ":" value)) value)
   ((stringp value)
    (let ((num (video-tjm--coerce-number value)))
      (if num (video-tjm--format-time num) value)))
   (t (video-tjm--format-time value))))

(defun video-tjm--normalize-time-input (input)
  "Normalise user-entered time INPUT to either a float, string, or nil."
  (let ((trim (string-trim (or input ""))))
    (cond
     ((string-empty-p trim) nil)
     ((string-match-p "\\`[+-]?[0-9]*\\.?[0-9]+\\'" trim)
      (string-to-number trim))
     ((string-match-p "\\`[0-9]+:\(?:[0-9]+:\)?[0-9]+\\(?:\\.[0-9]+\\)?\\'" trim)
      trim)
     (t (user-error "Invalid time format: %s" input)))))

(defun video-tjm--time->seconds (value)
  "Convert VALUE (string or number) to seconds as a float."
  (cond
   ((null value) nil)
   ((numberp value) (float value))
   ((stringp value)
    (let ((trim (string-trim value)))
      (cond
       ((string-match-p "\\`[0-9]*\\.?[0-9]+\\'" trim)
	(string-to-number trim))
       ((string-match-p "\\`[0-9]+:[0-9]+\\(?:\\.[0-9]+\\)?\\'" trim)
	(pcase-let* ((`(,minutes ,seconds)
		      (mapcar #'string-to-number (split-string trim ":")))
		     (total (+ (* minutes 60.0) seconds)))
	  total))
       ((string-match-p "\\`[0-9]+:[0-9]+:[0-9]+\\(?:\\.[0-9]+\\)?\\'" trim)
	(pcase-let* ((parts (mapcar #'string-to-number (split-string trim ":")))
		     (`(,hours ,minutes ,seconds) parts)
		     (total (+ (* hours 3600.0) (* minutes 60.0) seconds)))
	  total))
       (t nil))))
   (t nil)))

(defun video-tjm--resolve-relative-path (path)
  "Return PATH expanded relative to the current buffer."
  (when (and path (not (string-empty-p path)))
    (let ((base (file-name-directory (or buffer-file-name default-directory))))
      (expand-file-name path base))))

(defun video-tjm--read-json-file (file)
  "Read FILE into an alist, returning nil when FILE is unreadable."
  (when (and file (file-readable-p file))
    (condition-case err
	(with-temp-buffer
	  (insert-file-contents file)
	  (let ((json-source (buffer-substring-no-properties (point-min) (point-max))))
	    (unless (string-empty-p (string-trim json-source))
	      (json-parse-string json-source
				 :object-type 'alist
				 :array-type 'list
				 :null-object nil
				 :false-object nil))))
      (error
       (message "[video-tjm] Failed to read JSON template %s: %s" file (error-message-string err))
       nil))))

(defun video-tjm--broll-template-data (broll)
  "Return template metadata referenced by BROLL, if any."
  (let* ((file (video-tjm--stringify (alist-get 'file broll)))
	 (resolved (and file
			(string-match-p "\\.json\\'" file)
			(video-tjm--resolve-relative-path file))))
    (when resolved
      (video-tjm--read-json-file resolved))))

(defun video-tjm--placeholder-get (key placeholders)
  "Lookup KEY inside PLACEHOLDERS (string-keyed alist)."
  (alist-get (video-tjm--stringify key) placeholders nil nil #'equal))

(defun video-tjm--placeholder-set (key value placeholders)
  "Associate KEY with VALUE inside PLACEHOLDERS, removing when VALUE is nil.
Returns the updated PLACEHOLDERS alist."
  (let ((key (video-tjm--stringify key)))
    (if value
	(setf (alist-get key placeholders nil nil #'equal) value)
      (setf (alist-get key placeholders nil 'remove #'equal) nil))
    placeholders))

(defun video-tjm--merge-placeholder-maps (&rest maps)
  "Return the merged associative list from MAPS, later maps overriding earlier ones."
  (let ((result nil))
    (dolist (map maps)
      (dolist (entry map)
	(let ((key (video-tjm--stringify (car entry)))
	      (value (cdr entry)))
	  (when key
	    (setf (alist-get key result nil nil #'equal) value)))))
    result))

(defun video-tjm--placeholder-keys (&rest maps)
  "Return all unique placeholder keys mentioned in MAPS."
  (let ((keys nil))
    (dolist (map maps)
      (dolist (entry map)
	(let ((key (video-tjm--stringify (car entry))))
	  (when (and key (not (string-empty-p key)))
	    (cl-pushnew key keys :test #'string=)))))
    (sort keys #'string<)))

(defun video-tjm--quote-value (value)
  "Return VALUE formatted for human readable output."
  (cond
   ((stringp value) (format "%S" value))
   ((numberp value) (format "%.3f" value))
   (t (format "%S" value))))

(defun video-tjm--summarize-placeholders (defaults overrides)
  "Return a human-friendly summary of placeholder DEFAULTS and OVERRIDES."
  (let* ((keys (video-tjm--placeholder-keys defaults overrides))
	 (parts
	  (mapcar
	   (lambda (key)
	     (let* ((default (video-tjm--placeholder-get key defaults))
		    (override (video-tjm--placeholder-get key overrides))
		    (value (or override default))
		    (role (cond
			   (override (if default "override" "custom"))
			   (default "template")
			   (t "unset"))))
	       (format "%s=%s (%s)"
		       key
		       (video-tjm--quote-value value)
		       role)))
	   keys)))
    (when parts
      (string-join parts ", "))))

(defun video-tjm--seconds->time-like (original seconds)
  "Convert SECONDS to match ORIGINAL's format."
  (when seconds
    (if (stringp original)
	(video-tjm--format-time seconds)
      seconds)))

(defun video-tjm--auto-correct ()
  "Attempt to auto-correct common validation issues. Return change summaries."
  (let ((tolerance video-tjm-validation-time-tolerance)
	(changes nil)
	(recorded nil))
    (dolist (segment (video-tjm--segments))
      (unless (video-tjm--marker-p segment)
	(let* ((id (video-tjm--stringify (alist-get 'id segment)))
	       (start (video-tjm--coerce-number (alist-get 'start segment)))
	       (end (video-tjm--coerce-number (alist-get 'end segment)))
	       (words (alist-get 'words segment))
	       (changed nil))
	  (when words
	    (let* ((word-starts (delq nil (mapcar (lambda (word)
						    (video-tjm--coerce-number (alist-get 'start word)))
						  words)))
		   (word-ends (delq nil (mapcar (lambda (word)
						  (video-tjm--coerce-number (alist-get 'end word)))
						words))))
	      (when (and word-starts word-ends)
		(let ((first (apply #'min word-starts))
		      (last (apply #'max word-ends)))
		  (when (or (null start) (and start (> (- start first) tolerance)))
		    (unless recorded
		      (video-tjm--record-state)
		      (setq recorded t))
		    (setf (alist-get 'start segment) first)
		    (setq start first
			  changed t)
		    (push (format "%s: aligned start to %s" id (video-tjm--format-time first))
			  changes))
		  (when (or (null end) (and end (> (- last end) tolerance)))
		    (unless recorded
		      (video-tjm--record-state)
		      (setq recorded t))
		    (setf (alist-get 'end segment) last)
		    (setq end last
			  changed t)
		    (push (format "%s: aligned end to %s" id (video-tjm--format-time last))
			  changes))))))
	  (when (and start end (<= (- end start) tolerance))
	    (unless recorded
	      (video-tjm--record-state)
	      (setq recorded t))
	    (let ((new-end (+ start (max tolerance 0.001))))
	      (setf (alist-get 'end segment) new-end)
	      (setq end new-end
		    changed t)
	      (push (format "%s: extended duration to %s" id (video-tjm--format-time new-end))
		    changes)))
	  (when changed
	    (setf (alist-get 'start segment) start)
	    (setf (alist-get 'end segment) end)))))
    (when changes
      (video-tjm--mark-dirty)
      (video-tjm--render t))
    (nreverse changes)))

(defun video-tjm--generate-marker-id ()
  "Generate a unique marker segment id."
  (let* ((ids (video-tjm--collect-segment-ids))
	 (n 1)
	 (candidate (format "marker-%03d" n)))
    (while (member candidate ids)
      (setq n (1+ n)
	    candidate (format "marker-%03d" n)))
    candidate))

(defun video-tjm-mode--ensure-json-library ()
  (unless (fboundp 'json-parse-buffer)
    (user-error "This mode requires native JSON parsing (Emacs 27+)")))

;;;###autoload
(define-derived-mode video-tjm-mode special-mode "Video-TJM"
  "Major mode for Textual Join Manifest files (.tjm.json)."
  (video-tjm-mode--ensure-json-library)
  (setq-local indent-tabs-mode nil)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local revert-buffer-function #'video-tjm--revert)
  (setq-local write-contents-functions (list #'video-tjm--write-file))
  (setq-local video-tjm--visual-separators nil)
  (setq-local video-tjm--last-echo-id nil)
  (auto-save-mode -1)
  (setq-local buffer-auto-save-file-name nil)
  (setq-local auto-save-default nil)
  (when (boundp 'auto-save-visited-predicate)
    (setq-local auto-save-visited-predicate #'video-tjm--auto-save-blocker))
  (add-hook 'post-command-hook #'video-tjm--post-command nil t)
  (add-hook 'window-configuration-change-hook #'video-tjm--apply-visual-wrap nil t)
  (add-hook 'kill-buffer-hook #'video-tjm--remove-wrap nil t)
  (video-tjm-reload)
  (video-tjm--apply-visual-wrap)
  (when (fboundp 'hub/localleader)
    (hub/localleader
     :states '(normal motion)
     :keymaps 'video-tjm-mode-map
     "n" #'video-tjm-next-segment
     "p" #'video-tjm-previous-segment
     "N" #'video-tjm-move-segment-down
     "P" #'video-tjm-move-segment-up
     "d" #'video-tjm-delete-segment
     "e" #'video-tjm-edit-text
     "s" #'video-tjm-edit-speaker
     "t" #'video-tjm-toggle-tag
     "b" #'video-tjm-edit-broll
     "B" #'video-tjm-toggle-broll-continue
     "w" #'video-tjm-toggle-words
     "W" #'video-tjm-delete-word
     "S" #'video-tjm-split-segment
     "m" #'video-tjm-insert-marker
     "j" #'video-tjm-toggle-separator
     "r" #'video-tjm-edit-start-end
     "C-n" #'video-tjm-edit-notes
     "v" #'video-tjm-validate
     "o" #'video-tjm-open-raw-json
     "k" #'video-tjm-stop-playback
     "u" #'video-tjm-undo
     "R" #'video-tjm-redo
     "l" #'video-tjm-reload
     "?" #'video-tjm-hydra/body)))

(with-eval-after-load 'general
  (when (fboundp 'general-define-key)
    (general-define-key
     :states '(normal motion)
     :keymaps 'video-tjm-mode-map
     :prefix "C-c"
     "n" #'video-tjm-next-segment
     "p" #'video-tjm-previous-segment
     "N" #'video-tjm-move-segment-down
     "P" #'video-tjm-move-segment-up
     "d" #'video-tjm-delete-segment
     "e" #'video-tjm-edit-text
     "s" #'video-tjm-edit-speaker
     "t" #'video-tjm-toggle-tag
     "b" #'video-tjm-edit-broll
     "w" #'video-tjm-toggle-words
     "C-w" #'video-tjm-delete-word
     "C-s" #'video-tjm-split-segment
     "C-m" #'video-tjm-insert-marker
     "j" #'video-tjm-toggle-separator
     "r" #'video-tjm-edit-start-end
     "C-n" #'video-tjm-edit-notes
     "v" #'video-tjm-validate
     "o" #'video-tjm-open-raw-json
     "k" #'video-tjm-stop-playback
     "u" #'video-tjm-undo
     "R" #'video-tjm-redo
     "l" #'video-tjm-reload
     "?" #'video-tjm-hydra/body)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tjm\\.json\\'" . video-tjm-mode))

(defun video-tjm-reload (&optional _ignore-auto _noconfirm)
  "Reload TJM data from disk and re-render the buffer."
  (interactive)
  (video-tjm--load-data)
  (video-tjm--render t)
  (setq video-tjm--dirty nil)
  (setq video-tjm--undo-stack nil
	video-tjm--redo-stack nil)
  (set-buffer-modified-p nil)
  (message "Reloaded TJM manifest"))

(defun video-tjm--revert (&rest _)
  (video-tjm-reload))

(defun video-tjm--write-file ()
  "Custom saver used by `video-tjm-mode'."
  (when video-tjm--dirty
    (let* ((issues (when video-tjm-validation-on-save
		     (video-tjm-validate t)))
	   (auto-fixes (and issues (video-tjm--auto-correct))))
      (when auto-fixes
	(setq issues (when video-tjm-validation-on-save
		       (video-tjm-validate t)))
	(message "Auto-corrected %d issue(s)" (length auto-fixes)))
      (when (and issues
		 (not (yes-or-no-p (format "%d validation issue(s) found; save anyway? "
					   (length issues)))))
	(user-error "Aborted save"))
      (video-tjm--serialize-to-file buffer-file-name)
      (when buffer-file-name
	(set-visited-file-modtime))
      (setq video-tjm--dirty nil)))
  (set-buffer-modified-p nil)
  t)

(defun video-tjm--load-data ()
  "Parse buffer contents (or backing file) into `video-tjm--data'."
  (let* ((file buffer-file-name)
	 (json-source
	  (cond
	   ((and file (file-readable-p file))
	    (with-temp-buffer
	      (insert-file-contents file)
	      (buffer-substring-no-properties (point-min) (point-max))))
	   (t
	    (buffer-substring-no-properties (point-min) (point-max)))))
	 (json-source (string-trim json-source)))
    (setq video-tjm--data
	  (if (string-empty-p json-source)
	      '((version . 1) (sources . nil) (segments . nil))
	    (json-parse-string json-source :object-type 'alist :array-type 'list
			       :null-object nil :false-object nil)))))

(defun video-tjm--segments ()
  (or (alist-get 'segments video-tjm--data)
      (let ((segments '()))
	(setf (alist-get 'segments video-tjm--data) segments))))

(defun video-tjm--source-by-id (source-id)
  (seq-find (lambda (item)
	      (string= (alist-get 'id item) source-id))
	    (alist-get 'sources video-tjm--data)))

(defun video-tjm--format-time (seconds)
  (let* ((tot (or (video-tjm--coerce-number seconds) 0.0))
	 (mins (floor (/ tot 60)))
	 (secs (- tot (* mins 60))))
    (format "%02d:%06.3f" mins secs)))

(defun video-tjm--coerce-number (value)
  "Return VALUE as a float when representable, otherwise nil."
  (cond
   ((numberp value) (float value))
   ((stringp value)
    (let ((parsed (string-to-number value)))
      (if (and (= parsed 0.0)
	       (not (string-match-p "\\`[+-]?[0-9]*\\.?[0-9]+\\'" value)))
	  nil
	parsed)))
   (t nil)))

(defun video-tjm--render (&optional preserve-point)
  "Render `video-tjm--data' into the current buffer."
  (let* ((segments (video-tjm--segments))
	 (current-id (and preserve-point (video-tjm--segment-id-at-point)))
	 (last-id (and segments (alist-get 'id (car (last segments)))))
	 (inhibit-read-only t))
    (setq video-tjm--last-echo-id nil)
    (setq video-tjm--visual-separators
	  (cl-remove-if-not (lambda (id)
			      (seq-some (lambda (segment)
					  (equal id (alist-get 'id segment)))
					segments))
			    video-tjm--visual-separators))
    (erase-buffer)
    (dolist (segment segments)
      (let* ((id (or (alist-get 'id segment) ""))
	     (tags (seq-filter (lambda (tag)
				 (let ((s (video-tjm--stringify tag)))
				   (and s (not (string-empty-p s)))))
			       (alist-get 'tags segment)))
	     (notes (let ((s (video-tjm--stringify (alist-get 'notes segment))))
		      (unless (string-empty-p s) s)))
	     (broll (alist-get 'broll segment))
	     (start-pos (point)))
	(when (and (video-tjm--marker-p segment)
		   (> (point) (point-min)))
	  (let* ((start (max (point-min) (- (point) 2)))
		 (need-extra
		  (not (and (> (point) start)
			    (string= "\n\n"
				     (buffer-substring-no-properties
				      start (point)))))))
	    (when need-extra
	      (unless (eq (char-before) ?\n)
		(insert "\n"))
	      (insert "\n"))))
	(if (video-tjm--marker-p segment)
	    (progn
	      (let* ((title (string-trim (video-tjm--stringify (alist-get 'title segment))))
		     (display (format "# %s" (if (string-empty-p title)
						 "(Untitled marker)"
					       title))))
		(setq start-pos (point))
		(insert display)
		(add-text-properties start-pos (point)
				     (list 'face 'video-tjm-marker-face
					   'font-lock-face 'video-tjm-marker-face
					   'video-tjm-segment segment
					   'video-tjm-segment-id id
					   'video-tjm-tags tags
					   'video-tjm-notes notes
					   'video-tjm-broll broll
					   'video-tjm-marker t))))
	  (let* ((raw-text (video-tjm--stringify (alist-get 'text segment)))
		 (text (string-trim (replace-regexp-in-string "\s-+" " " raw-text))))
	    (setq start-pos (point))
	    (insert text)
	    (add-text-properties start-pos (point)
				 (list 'face (if broll 'video-tjm-broll-face 'video-tjm-text-face)
				       'font-lock-face (if broll 'video-tjm-broll-face 'video-tjm-text-face)
				       'video-tjm-segment segment
				       'video-tjm-segment-id id
				       'video-tjm-tags tags
				       'video-tjm-notes notes
				       'video-tjm-broll broll))
	    (when (and video-tjm--words-visible (alist-get 'words segment))
	      (insert (propertize (format "\n[words] %s"
					  (video-tjm--format-words (alist-get 'words segment)))
				  'face 'video-tjm-meta-face)))))
	(let ((meta-inserted (video-tjm--render-broll-summary segment)))
	  (cond
	   ((equal id last-id)
	    ;; no delimiter after final segment
	    )
	   ((video-tjm--marker-p segment)
	    (insert "\n\n"))
	   ((member id video-tjm--visual-separators)
	    (insert "\n\n"))
	   (meta-inserted
	    (insert "\n"))
	   (t
	    (insert " "))))))
    (goto-char (point-min))
    (when current-id
      (video-tjm--goto-segment current-id))
    (video-tjm--update-focus-overlay)
    (video-tjm--apply-visual-wrap)))

(defun video-tjm--format-words (words)
  (mapconcat (lambda (word)
	       (let ((token (alist-get 'token word))
		     (start (video-tjm--format-time (alist-get 'start word)))
		     (end (video-tjm--format-time (alist-get 'end word))))
		 (format "%s[%s-%s]" token start end)))
	     words " "))

(defun video-tjm--summarize-broll (broll)
  (let* ((file (video-tjm--stringify (alist-get 'file broll)))
	 (mode (video-tjm--aget 'mode broll))
	 (audio (video-tjm--aget 'audio broll))
	 (offset (video-tjm--aget 'start_offset broll))
	 (duration (video-tjm--aget 'duration broll))
	 (still (video-tjm--aget 'still broll))
	 (continue (video-tjm--aget 'continue broll))
	 (overlays (video-tjm--aget 'overlays broll))
	 (placeholders (video-tjm--aget 'placeholders broll))
	 (template (video-tjm--broll-template-data broll))
	 (template-media (and template (video-tjm--stringify (video-tjm--aget 'template template))))
	 (template-placeholders (and template (video-tjm--aget 'placeholders template)))
	 (template-overlays (and template (video-tjm--aget 'overlays template)))
	 (placeholder-info (video-tjm--summarize-placeholders template-placeholders placeholders))
	 (overlay-part
	  (cond
	   ((and overlays (listp overlays)) (format "overlays=%d" (length overlays)))
	   ((and template-overlays (listp template-overlays))
	    (format "overlays=%d (template)" (length template-overlays)))))
	 (parts
	  (cl-remove-if
	   #'null
	   (list
	    (when file (format "file=%s" file))
	    (when template-media (format "template=%s" template-media))
	    (when mode (format "mode=%s" (video-tjm--stringify mode)))
	    (when audio (format "audio=%s" (video-tjm--stringify audio)))
	    (when offset
	      (format "offset=%s" (or (video-tjm--display-time offset) (video-tjm--stringify offset))))
	    (when duration
	      (format "duration=%s" (or (video-tjm--display-time duration) (video-tjm--stringify duration))))
	    overlay-part
	    (when placeholder-info (format "placeholders: %s" placeholder-info))
	    (when continue "continue")
	    (when still "still")))))
    (string-join parts ", ")))

(defun video-tjm--render-broll-summary (segment)
  "Insert a descriptive summary line for SEGMENT's b-roll metadata.
Returns non-nil when a summary was inserted."
  (let ((broll (alist-get 'broll segment)))
    (when broll
      (let* ((summary (or (video-tjm--summarize-broll broll) ""))
	     (line (if (string-empty-p (string-trim summary))
		       "[b-roll] attached"
		     (format "[b-roll] %s" summary))))
	(insert (propertize (concat "\n" line)
			    'face 'video-tjm-meta-face
			    'video-tjm-segment segment
			    'video-tjm-segment-id (alist-get 'id segment)
			    'video-tjm-broll broll))
	t))))

(defun video-tjm--segment-at-point ()
  (or (get-text-property (point) 'video-tjm-segment)
      (get-text-property (max (point-min) (1- (point))) 'video-tjm-segment)))

(defun video-tjm--segment-id-at-point ()
  (or (get-text-property (point) 'video-tjm-segment-id)
      (get-text-property (max (point-min) (1- (point))) 'video-tjm-segment-id)))

(defun video-tjm--goto-segment (segment-id)
  (let ((pos (point-min))
	(found nil))
    (while (and (< pos (point-max)) (not found))
      (if (equal (get-text-property pos 'video-tjm-segment-id) segment-id)
	  (setq found pos)
	(setq pos (or (next-single-property-change pos 'video-tjm-segment-id nil (point-max))
		      (point-max)))))
    (when found (goto-char found))
    found))

(defun video-tjm--segment-word-spans (text)
  "Return a list of (START END WORD) spans parsed from TEXT."
  (let ((len (length text))
	(idx 0)
	(spans nil))
    (while (< idx len)
      (while (and (< idx len)
		  (memq (aref text idx) '(?	 ?\n ?\r ? )))
	(setq idx (1+ idx)))
      (when (< idx len)
	(let ((start idx))
	  (while (and (< idx len)
		      (not (memq (aref text idx) '(?	 ?\n ?\r ? ))))
	    (setq idx (1+ idx)))
	  (push (list start idx (substring text start idx)) spans))))
    (nreverse spans)))

(defun video-tjm--spans->text (spans)
  "Reconstruct segment text from SPANS produced by `video-tjm--segment-word-spans'."
  (string-trim (mapconcat (lambda (span) (nth 2 span)) spans " ")))

(defun video-tjm--word-bounds-in-segment (pos seg-start seg-end)
  "Return the bounds of the word around POS limited to SEG-START..SEG-END."
  (let ((pos (min (max pos seg-start)
		  (if (> seg-end seg-start)
		      (max (1- seg-end) seg-start)
		    seg-start))))
    (save-excursion
      (goto-char pos)
      (when (and (< (point) seg-end)
		 (memq (char-after) '(?	 ?\n ?\r ? )))
	(skip-chars-forward "\s-" seg-end)
	(when (>= (point) seg-end)
	  (goto-char pos)
	  (skip-chars-backward "\s-" seg-start)))
      (let ((mid (point)))
	(let ((word-start (progn
			    (skip-chars-backward "^\s-" seg-start)
			    (point))))
	  (goto-char mid)
	  (let ((word-end (progn
			    (skip-chars-forward "^\s-" seg-end)
			    (point))))
	    (when (< word-start word-end)
	      (cons word-start word-end))))))))

(defun video-tjm--word-info-at-point ()
  "Return a plist describing the word at point in the current segment."
  (let* ((segment (video-tjm--segment-at-point)))
    (unless segment
      (user-error "No segment at point"))
    (when (video-tjm--marker-p segment)
      (user-error "Markers do not contain words"))
    (let* ((segment-id (alist-get 'id segment))
	   (bounds (video-tjm--segment-bounds segment-id)))
      (unless bounds
	(error "Failed to determine segment bounds"))
      (let* ((seg-start (car bounds))
	     (seg-end (cdr bounds))
	     (word-bounds (video-tjm--word-bounds-in-segment (point) seg-start seg-end)))
	(unless word-bounds
	  (user-error "No word at point"))
	(let* ((segment-text (buffer-substring-no-properties seg-start seg-end))
	       (relative-start (- (car word-bounds) seg-start))
	       (relative-end (- (cdr word-bounds) seg-start))
	       (spans (video-tjm--segment-word-spans segment-text))
	       (word-text (substring segment-text relative-start relative-end))
	       (word-index
		(or (cl-loop for span in spans
			     for idx from 0
			     when (and (>= relative-start (nth 0 span))
				       (< relative-start (nth 1 span)))
			     return idx)
		    (cl-position word-text spans
				 :test #'string=
				 :key (lambda (span) (nth 2 span))))))
	  (unless (numberp word-index)
	    (user-error "Unable to locate word index"))
	  (list :segment segment
		:segment-id segment-id
		:segment-text segment-text
		:segment-bounds bounds
		:word-bounds word-bounds
		:word-text word-text
		:word-index word-index
		:word-spans spans))))))

(defun video-tjm--collect-segment-ids ()
  "Return a list of all segment ids in the current buffer."
  (mapcar (lambda (segment)
	    (video-tjm--stringify (alist-get 'id segment)))
	  (video-tjm--segments)))

(defun video-tjm--generate-segment-id (base)
  "Generate a unique segment id derived from BASE."
  (let* ((base (or (and (stringp base) (not (string-empty-p base)) base)
		   "segment"))
	 (ids (video-tjm--collect-segment-ids))
	 (n 1)
	 (candidate (format "%s-split-%d" base n)))
    (while (member candidate ids)
      (setq n (1+ n)
	    candidate (format "%s-split-%d" base n)))
    candidate))

(defun video-tjm--remove-nth (n list)
  "Return LIST without the element at index N (0-based)."
  (if (or (null list) (< n 0))
      list
    (let ((result nil)
	  (idx 0))
      (dolist (item list (nreverse result))
	(unless (= idx n)
	  (push item result))
	(setq idx (1+ idx))))))

(defun video-tjm--find-word-token-index (words token fallback-index)
  "Find TOKEN within WORDS, preferring FALLBACK-INDEX when it matches."
  (when words
    (let ((len (length words)))
      (cond
       ((and (numberp fallback-index)
	     (<= 0 fallback-index)
	     (< fallback-index len)
	     (string=
	      token
	      (video-tjm--stringify (alist-get 'token (nth fallback-index words)))))
	fallback-index)
       (t
	(cl-position token words
		     :test #'string=
		     :key (lambda (word)
			    (video-tjm--stringify (alist-get 'token word)))))))))

(defun video-tjm--goto-word-by-index (segment-id index)
  "Move point to the start of word INDEX within SEGMENT-ID."
  (let ((bounds (video-tjm--segment-bounds segment-id)))
    (when bounds
      (let* ((seg-start (car bounds))
	     (seg-end (cdr bounds))
	     (segment-text (buffer-substring-no-properties seg-start seg-end))
	     (spans (video-tjm--segment-word-spans segment-text)))
	(when (and spans (<= 0 index) (< index (length spans)))
	  (goto-char (+ seg-start (nth 0 (nth index spans))))
	  t)))))

(defun video-tjm--window-total-width (window)
  (let* ((margins (window-margins window))
	 (left (or (car margins) 0))
	 (right (or (cdr margins) 0)))
    (+ (window-width window) left right)))

(defun video-tjm--apply-visual-wrap (&rest _ignore)
  (when (derived-mode-p 'video-tjm-mode)
    (let ((fill (or fill-column (default-value 'fill-column))))
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
	(let* ((margins (window-margins window))
	       (left (or (car margins) 0))
	       (right (or (cdr margins) 0))
	       (total (video-tjm--window-total-width window))
	       (desired (if (> total fill) (- total fill) 0))
	       (current-right right))
	  (unless (= desired current-right)
	    (unless (window-parameter window 'video-tjm--saved-margins)
	      (set-window-parameter window 'video-tjm--saved-margins
				    (cons left right)))
	    (set-window-margins window left desired)
	    (set-window-parameter window 'video-tjm--wrapped t)))
	(video-tjm--apply-fringe-width window 0 0)))))

(defun video-tjm--apply-fringe-width (window left right)
  "Adjust WINDOW fringes, saving the original values when first changed."
  (let ((current (window-fringes window)))
    (unless (window-parameter window 'video-tjm--saved-fringes)
      (set-window-parameter window 'video-tjm--saved-fringes current))
    (set-window-fringes window left right (nth 2 current))
    (set-window-parameter window 'video-tjm--wrapped t)))

(defun video-tjm--remove-wrap ()
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (video-tjm--restore-wrap window)))

(defun video-tjm--restore-wrap (window)
  (when-let* ((saved (window-parameter window 'video-tjm--saved-margins)))
    (set-window-margins window (car saved) (cdr saved)))
  (when-let* ((saved-fringes (window-parameter window 'video-tjm--saved-fringes)))
    (apply #'set-window-fringes (append (list window) saved-fringes)))
  (set-window-parameter window 'video-tjm--saved-margins nil)
  (set-window-parameter window 'video-tjm--saved-fringes nil)
  (set-window-parameter window 'video-tjm--wrapped nil))

(defun video-tjm--cleanup-wrap ()
  (dolist (window (window-list))
    (unless (with-current-buffer (window-buffer window)
	      (derived-mode-p 'video-tjm-mode))
      (when (or (window-parameter window 'video-tjm--saved-margins)
		(window-parameter window 'video-tjm--saved-fringes))
	(video-tjm--restore-wrap window)))))

(add-hook 'window-configuration-change-hook #'video-tjm--cleanup-wrap)

(defun video-tjm--segment-index (segment-id segments)
  "Return the index of SEGMENT-ID within SEGMENTS."
  (when segment-id
    (cl-position segment-id segments
		 :test #'equal
		 :key (lambda (segment)
			(alist-get 'id segment)))))

(defun video-tjm--move-by-segments (delta)
  "Move point DELTA segments forward (or backward when negative)."
  (let* ((segments (video-tjm--segments))
	 (count (length segments)))
    (unless segments
      (user-error "No segments available"))
    (let* ((current-id (video-tjm--segment-id-at-point))
	   (current-index (or (video-tjm--segment-index current-id segments)
			      (if (> delta 0) -1 count)))
	   (target-index (+ current-index delta)))
      (if (or (< target-index 0) (>= target-index count))
	  (message (if (> delta 0)
		       "Already at last segment"
		     "Already at first segment"))
	(video-tjm--goto-segment (alist-get 'id (nth target-index segments)))
	(video-tjm--update-focus-overlay)
	(video-tjm--echo-segment-info)
	t))))

(defun video-tjm-next-segment (&optional count)
  "Move point to the next TJM segment.
Optional COUNT moves forward multiple segments."
  (interactive "p")
  (video-tjm--move-by-segments (or count 1)))

(defun video-tjm-previous-segment (&optional count)
  "Move point to the previous TJM segment.
Optional COUNT moves backward multiple segments."
  (interactive "p")
  (video-tjm--move-by-segments (- (or count 1))))

(defun video-tjm--mark-dirty ()
  (setq video-tjm--dirty t)
  (set-buffer-modified-p t))

(defun video-tjm-undo ()
  "Restore the previous TJM state from the undo stack, preserving point."
  (interactive)
  (if (null video-tjm--undo-stack)
      (message "Nothing to undo")
    (push (video-tjm--snapshot) video-tjm--redo-stack)
    (let ((target-id (video-tjm--segment-id-at-point)))
      (setq video-tjm--data (pop video-tjm--undo-stack))
      (video-tjm--mark-dirty)
      (video-tjm--render t)
      (when target-id
	(video-tjm--goto-segment target-id))
      (message "TJM undo"))))

(defun video-tjm-redo ()
  "Reapply a state from the redo stack."
  (interactive)
  (if (null video-tjm--redo-stack)
      (message "Nothing to redo")
    (push (video-tjm--snapshot) video-tjm--undo-stack)
    (setq video-tjm--data (pop video-tjm--redo-stack))
    (video-tjm--mark-dirty)
    (video-tjm--render t)
    (message "TJM redo")))

(defun video-tjm-delete-segment ()
  "Delete the segment at point."
  (interactive)
  (let* ((segment (video-tjm--segment-at-point))
	 (segments (video-tjm--segments)))
    (unless segment
      (user-error "No segment at point"))
    (when (yes-or-no-p (format "Delete segment %s? " (alist-get 'id segment)))
      (video-tjm--record-state)
      (let* ((segment-index (cl-position segment segments :test #'eq))
	     (remaining (cl-remove segment segments :test #'eq))
	     (next-id (cond
		       ((and segment-index
			     remaining
			     (< segment-index (length remaining)))
			(alist-get 'id (nth segment-index remaining)))
		       ((and remaining segment-index)
			(alist-get 'id (car (last remaining))))
		       (remaining
			(alist-get 'id (car remaining))))))
	(setf (alist-get 'segments video-tjm--data) remaining)
	(video-tjm--mark-dirty)
	(video-tjm--render t)
	(when next-id
	  (video-tjm--goto-segment next-id)
	  (video-tjm--update-focus-overlay)
	  (video-tjm--echo-segment-info))
	(message "Segment deleted")))))

(defun video-tjm-delete-word ()
  "Delete the word at point from the current segment."
  (interactive)
  (let* ((info (video-tjm--word-info-at-point))
	 (segment (plist-get info :segment))
	 (segment-id (plist-get info :segment-id))
	 (segment-start (video-tjm--coerce-number (alist-get 'start segment)))
	 (word-index (plist-get info :word-index))
	 (word-text (plist-get info :word-text))
	 (word-spans (plist-get info :word-spans))
	 (words (alist-get 'words segment)))
    (video-tjm--record-state)
    (let* ((new-spans (video-tjm--remove-nth word-index word-spans))
	   (new-text (video-tjm--spans->text new-spans))
	   (token-index (video-tjm--find-word-token-index words word-text word-index))
	   (fallback-token-index (and words (numberp word-index)
				      (<= 0 word-index)
				      (< word-index (length words))
				      word-index))
	   (effective-token-index (or token-index fallback-token-index))
	   (new-length (length new-spans))
	   (target-index (cond
			  ((> new-length word-index) word-index)
			  ((> word-index 0) (1- word-index))
			  (t nil))))
      (setf (alist-get 'text segment) new-text)
      (when (and words (numberp effective-token-index))
	(let ((updated (video-tjm--remove-nth effective-token-index words)))
	  (setf (alist-get 'words segment) (and updated updated))))
      (let ((words-after (alist-get 'words segment)))
	(if words-after
	    (let* ((first-word (car words-after))
		   (last-word (car (last words-after))))
	      (setf (alist-get 'start segment)
		    (video-tjm--coerce-number (alist-get 'start first-word)))
	      (setf (alist-get 'end segment)
		    (video-tjm--coerce-number (alist-get 'end last-word))))
	  (when (and (string-empty-p new-text)
		     segment-start)
	    ;; Collapse the segment when no words remain so validation highlights it.
	    (setf (alist-get 'end segment) segment-start)
	    (setf (alist-get 'start segment) segment-start))))
      (when (and words (null (alist-get 'words segment)))
	(setf (alist-get 'words segment) nil))
      (video-tjm--mark-dirty)
      (video-tjm--render t)
      (video-tjm--goto-segment segment-id)
      (when (and target-index (>= target-index 0))
	(video-tjm--goto-word-by-index segment-id target-index))
      (video-tjm--update-focus-overlay)
      (video-tjm--echo-segment-info)
      (message "Deleted word '%s'" word-text))))

(defun video-tjm-split-segment ()
  "Split the current segment at point, creating a new segment that starts at the word under point."
  (interactive)
  (let* ((info (video-tjm--word-info-at-point))
	 (segment (plist-get info :segment))
	 (broll-orig (let ((b (alist-get 'broll segment)))
		       (and b (copy-alist b))))
	 (segment-id (plist-get info :segment-id))
	 (word-index (plist-get info :word-index))
	 (word-spans (plist-get info :word-spans))
	 (words (alist-get 'words segment)))
    (unless words
      (user-error "Segment has no word timings to split"))
    (let ((total (length words)))
      (when (or (<= word-index 0)
		(>= word-index total))
	(user-error "Move to a word away from the segment boundary to split")))
    (video-tjm--record-state)
    (let* ((segments (video-tjm--segments))
	   (segment-index (cl-position segment segments :test #'eq)))
      (unless segment-index
	(error "Unable to determine segment index"))
      (let* ((before-words (cl-subseq words 0 word-index))
	     (after-words (cl-subseq words word-index))
	     (before-spans (cl-subseq word-spans 0 word-index))
	     (after-spans (cl-subseq word-spans word-index))
	     (segment-before (copy-tree segment t))
	     (segment-after (copy-tree segment t))
	     (new-id (video-tjm--generate-segment-id segment-id)))
	;; Update text and timings for the first half
	(setf (alist-get 'text segment-before) (video-tjm--spans->text before-spans))
	(setf (alist-get 'words segment-before) before-words)
	(setf (alist-get 'broll segment-before) (and broll-orig (copy-alist broll-orig)))
	(let ((first before-words)
	      (last (last before-words)))
	  (when (and first last)
	    (setf (alist-get 'start segment-before)
		  (video-tjm--coerce-number (alist-get 'start (car first))))
	    (setf (alist-get 'end segment-before)
		  (video-tjm--coerce-number (alist-get 'end (car last))))))
	;; Update text, id, and timings for the second half
	(setf (alist-get 'id segment-after) new-id)
	(setf (alist-get 'text segment-after) (video-tjm--spans->text after-spans))
	(setf (alist-get 'words segment-after) after-words)
	(setf (alist-get 'broll segment-after) (and broll-orig (copy-alist broll-orig)))
	(let ((first after-words)
	      (last (last after-words)))
	  (when (and first last)
	    (setf (alist-get 'start segment-after)
		  (video-tjm--coerce-number (alist-get 'start (car first))))
	    (setf (alist-get 'end segment-after)
		  (video-tjm--coerce-number (alist-get 'end (car last))))))
	;; Splice the new segments into the manifest
	(let* ((head (cl-subseq segments 0 segment-index))
	       (tail (cl-subseq segments (1+ segment-index)))
	       (combined (append head (list segment-before segment-after) tail)))
	  (setf (alist-get 'segments video-tjm--data) combined))
	;; Maintain visual separators, moving the flag to the new tail if necessary
	(when (member segment-id video-tjm--visual-separators)
	  (setq video-tjm--visual-separators
		(cons new-id (cl-remove segment-id video-tjm--visual-separators
					:test #'equal))))
	(video-tjm--mark-dirty)
	(video-tjm--render t)
	(when broll-orig
	  (let* ((broll-before (alist-get 'broll segment-before))
		 (broll-after (alist-get 'broll segment-after))
		 (start-before (video-tjm--coerce-number (alist-get 'start segment-before)))
		 (end-before (video-tjm--coerce-number (alist-get 'end segment-before)))
		 (start-after (video-tjm--coerce-number (alist-get 'start segment-after)))
		 (end-after (video-tjm--coerce-number (alist-get 'end segment-after)))
		 (duration-before (and start-before end-before (- end-before start-before)))
		 (orig-offset (alist-get 'start_offset broll-orig))
		 (orig-duration (alist-get 'duration broll-orig))
		 (offset-seconds (video-tjm--time->seconds orig-offset))
		 (new-offset (+ (or offset-seconds 0.0)
				(or duration-before 0.0)))
		 (remaining (when orig-duration
			      (max 0.0 (- (video-tjm--time->seconds orig-duration)
					  (or duration-before 0.0))))))
	    (when broll-before
	      (setf (alist-get 'continue broll-before) nil)
	      (when orig-duration
		(setf (alist-get 'duration broll-before)
		      (video-tjm--seconds->time-like orig-duration (or duration-before 0.0)))))
	    (when broll-after
	      (setf (alist-get 'continue broll-after) t)
	      (setf (alist-get 'start_offset broll-after)
		    (video-tjm--seconds->time-like orig-offset new-offset))
	      (when orig-duration
		(setf (alist-get 'duration broll-after)
		      (if (> (or remaining 0.0) 0)
			  (video-tjm--seconds->time-like orig-duration remaining)
			nil))))
	    (message "Segment split; marked b-roll to continue into the next segment.")))
	(video-tjm--goto-segment new-id)
	(video-tjm--goto-word-by-index new-id 0)
	(video-tjm--update-focus-overlay)
	(video-tjm--echo-segment-info)
	(message "Split segment %s" segment-id)))))

(defun video-tjm-merge-with-next ()
  "Merge the segment at point with the next segment in the manifest."
  (interactive)
  (let* ((segment (or (video-tjm--segment-at-point)
		      (user-error "No segment at point")))
	 (segments (video-tjm--segments))
	 (idx (cl-position segment segments :test #'eq)))
    (unless idx
      (error "Unable to determine segment index"))
    (when (video-tjm--marker-p segment)
      (user-error "Cannot merge marker segments"))
    (let ((next (nth (1+ idx) segments)))
      (unless next
	(user-error "Already at last segment"))
      (when (video-tjm--marker-p next)
	(user-error "Cannot merge into a marker segment"))
      (let* ((source (alist-get 'source segment))
	     (next-source (alist-get 'source next))
	     (source-str (video-tjm--stringify source))
	     (next-source-str (video-tjm--stringify next-source)))
	(when (and source-str next-source-str
		   (not (string= source-str next-source-str)))
	  (user-error "Segments use different sources (%s vs %s)" source-str next-source-str))
	(when (and (null source) next-source)
	  (setf (alist-get 'source segment) next-source)))
      (video-tjm--record-state)
      (let* ((text-a (video-tjm--stringify (alist-get 'text segment)))
	     (text-b (video-tjm--stringify (alist-get 'text next)))
	     (combined-text
	      (string-trim
	       (string-join
		(seq-filter (lambda (chunk)
			      (and chunk (not (string-empty-p chunk))))
			    (list text-a text-b))
		" ")))
	     (words-a (copy-sequence (alist-get 'words segment)))
	     (words-b (copy-sequence (alist-get 'words next)))
	     (merged-words (append words-a words-b))
	     (start-a (video-tjm--coerce-number (alist-get 'start segment)))
	     (end-a (video-tjm--coerce-number (alist-get 'end segment)))
	     (start-b (video-tjm--coerce-number (alist-get 'start next)))
	     (end-b (video-tjm--coerce-number (alist-get 'end next)))
	     (note-a (video-tjm--stringify (alist-get 'notes segment)))
	     (note-b (video-tjm--stringify (alist-get 'notes next)))
	     (tag-a (copy-sequence (alist-get 'tags segment)))
	     (tag-b (copy-sequence (alist-get 'tags next)))
	     (next-id (alist-get 'id next))
	     (next-had-broll (alist-get 'broll next)))
	(setf (alist-get 'text segment)
	      (and (not (string-empty-p combined-text)) combined-text))
	(setf (alist-get 'words segment) (and merged-words merged-words))
	(let ((notes (seq-filter (lambda (chunk)
				   (and chunk (not (string-empty-p chunk))))
				 (list note-a note-b))))
	  (setf (alist-get 'notes segment)
		(cond
		 ((null notes) nil)
		 ((= (length notes) 1) (car notes))
		 (t (mapconcat #'identity notes "\n\n")))))
	(let* ((combined-tags (append tag-a tag-b))
	       (clean-tags
		(seq-filter (lambda (tag)
			      (let ((as-string (video-tjm--stringify tag)))
				(and as-string (not (string-empty-p as-string)))))
			    combined-tags)))
	  (setf (alist-get 'tags segment)
		(when clean-tags
		  (cl-delete-duplicates clean-tags
					:test #'string=
					:key #'video-tjm--stringify))))
	(if merged-words
	    (let ((first-word (car merged-words))
		  (last-word (car (last merged-words))))
	      (setf (alist-get 'start segment)
		    (video-tjm--coerce-number (alist-get 'start first-word)))
	      (setf (alist-get 'end segment)
		    (video-tjm--coerce-number (alist-get 'end last-word))))
	  (let ((starts (seq-filter #'numberp (list start-a start-b)))
		(ends (seq-filter #'numberp (list end-a end-b))))
	    (when starts
	      (setf (alist-get 'start segment) (apply #'min starts)))
	    (when ends
	      (setf (alist-get 'end segment) (apply #'max ends)))))
	(setq video-tjm--visual-separators
	      (cl-remove (video-tjm--stringify next-id) video-tjm--visual-separators
			 :test #'string= :count 1))
	(setf (alist-get 'segments video-tjm--data)
	      (cl-remove next segments :test #'eq :count 1))
	(video-tjm--mark-dirty)
	(let ((merged-id (alist-get 'id segment)))
	  (video-tjm--render t)
	  (when merged-id
	    (video-tjm--goto-segment merged-id)
	    (video-tjm--update-focus-overlay)
	    (video-tjm--echo-segment-info))
	  (message "Merged segment %s with %s%s"
		   merged-id
		   (video-tjm--stringify next-id)
		   (if next-had-broll
		       " (review b-roll metadata)"
		     "")))))))

(defun video-tjm-move-segment-up ()
  "Move the current segment up by one position."
  (interactive)
  (let* ((segment (video-tjm--segment-at-point))
	 (segments (video-tjm--segments))
	 (idx (cl-position segment segments :test #'eq)))
    (unless segment
      (user-error "No segment at point"))
    (if (or (null idx) (<= idx 0))
	(message "Already at first segment")
      (video-tjm--record-state)
      (cl-rotatef (nth (1- idx) segments) (nth idx segments))
      (video-tjm--mark-dirty)
      (video-tjm--render t))))

(defun video-tjm-move-segment-down ()
  "Move the current segment down by one position."
  (interactive)
  (let* ((segment (video-tjm--segment-at-point))
	 (segments (video-tjm--segments))
	 (idx (cl-position segment segments :test #'eq)))
    (unless segment
      (user-error "No segment at point"))
    (if (or (null idx) (>= idx (1- (length segments))))
	(message "Already at last segment")
      (video-tjm--record-state)
      (cl-rotatef (nth idx segments) (nth (1+ idx) segments))
      (video-tjm--mark-dirty)
      (video-tjm--render t))))

(defun video-tjm-edit-text (segment)
  "Edit the text of SEGMENT (defaults to segment at point)."
  (interactive (list (or (video-tjm--segment-at-point)
			 (user-error "No segment at point"))))
  (video-tjm--record-state)
  (if (video-tjm--marker-p segment)
      (let* ((current (alist-get 'title segment))
	     (new (read-string "Marker title: " (video-tjm--stringify current))))
	(setf (alist-get 'title segment) (unless (string-empty-p new) new))
	(video-tjm--mark-dirty)
	(video-tjm--render t)
	(message "Updated marker title"))
    (let* ((current (alist-get 'text segment))
	   (new (read-string "Segment text: " current)))
      (setf (alist-get 'text segment) new)
      (video-tjm--mark-dirty)
      (video-tjm--render t))))

(defun video-tjm-edit-speaker (segment)
  "Edit the speaker field of SEGMENT."
  (interactive (list (or (video-tjm--segment-at-point)
			 (user-error "No segment at point"))))
  (when (video-tjm--marker-p segment)
    (user-error "Markers do not have speakers"))
  (video-tjm--record-state)
  (let* ((current (alist-get 'speaker segment))
	 (new (read-string "Speaker (blank to clear): " current)))
    (setf (alist-get 'speaker segment) (unless (string-empty-p new) new))
    (video-tjm--mark-dirty)
    (video-tjm--render t)))

(defun video-tjm-edit-notes (segment)
  "Edit the notes field of SEGMENT."
  (interactive (list (or (video-tjm--segment-at-point)
			 (user-error "No segment at point"))))
  (video-tjm--record-state)
  (let* ((current (alist-get 'notes segment))
	 (new (read-string "Notes (blank to clear): " (or current ""))))
    (setf (alist-get 'notes segment) (unless (string-empty-p new) new))
    (video-tjm--mark-dirty)
    (video-tjm--render t)))

(defun video-tjm-edit-start-end (segment)
  "Edit start and end timestamps of SEGMENT."
  (interactive (list (or (video-tjm--segment-at-point)
			 (user-error "No segment at point"))))
  (video-tjm--record-state)
  (if (video-tjm--marker-p segment)
      (let* ((current-source (video-tjm--stringify (alist-get 'source segment)))
	     (source-input (read-string "Marker source (blank to clear): " current-source))
	     (current-start (alist-get 'start segment))
	     (start-default (or (video-tjm--display-time current-start) ""))
	     (start-input (read-string "Marker start (MM:SS or seconds, blank to clear): "
				       start-default))
	     (start (video-tjm--normalize-time-input start-input)))
	(setf (alist-get 'source segment)
	      (unless (string-empty-p source-input) source-input))
	(if start
	    (setf (alist-get 'start segment) start)
	  (setf (alist-get 'start segment) nil))
	(video-tjm--mark-dirty)
	(video-tjm--render t)
	(message "Updated marker timing"))
    (let* ((start (alist-get 'start segment))
	   (end (alist-get 'end segment))
	   (new-start (read-number (format "Start (%.3f): " start) start))
	   (new-end (read-number (format "End (%.3f): " end) end)))
      (when (>= new-start new-end)
	(user-error "Start must be strictly less than end"))
      (setf (alist-get 'start segment) new-start
	    (alist-get 'end segment) new-end)
      (video-tjm--mark-dirty)
      (video-tjm--render t))))

(defun video-tjm-toggle-tag (segment tag)
  "Toggle TAG on SEGMENT. Prompts for both if interactive."
  (interactive
   (let ((seg (or (video-tjm--segment-at-point)
		  (user-error "No segment at point")))
	 (tag (read-string "Tag: ")))
     (list seg tag)))
  (video-tjm--record-state)
  (let* ((tags (copy-sequence (alist-get 'tags segment)))
	 (existing (member tag tags)))
    (if existing
	(setq tags (delete tag tags))
      (setq tags (append tags (list tag))))
    (setf (alist-get 'tags segment) tags)
    (video-tjm--mark-dirty)
    (video-tjm--render t)))

(defun video-tjm-edit-broll (segment)
  "Edit b-roll metadata for SEGMENT."
  (interactive (list (or (video-tjm--segment-at-point)
			 (user-error "No segment at point"))))
  (video-tjm--record-state)
  (let* ((broll (copy-alist (alist-get 'broll segment)))
	 (file (read-string "B-roll file (blank to clear): "
			    (video-tjm--stringify (alist-get 'file broll))))
	 (mode (read-string "Mode (overlay/replace/pip): "
			    (alist-get 'mode broll)))
	 (audio-default (or (video-tjm--stringify (alist-get 'audio broll))
			    "source"))
	 (audio (read-string "Audio (source/mute/broll): " audio-default))
	 (offset-existing (alist-get 'start_offset broll))
	 (offset-default (cond
			  ((stringp offset-existing) offset-existing)
			  ((numberp offset-existing) (video-tjm--format-time offset-existing))
			  (t "")))
	 (offset-input (read-string "Start offset (MM:SS or seconds, blank to clear): "
				    offset-default))
	 (offset (video-tjm--normalize-time-input offset-input))
	 (duration-existing (alist-get 'duration broll))
	 (duration-default (cond
			    ((stringp duration-existing) duration-existing)
			    ((numberp duration-existing) (video-tjm--format-time duration-existing))
			    (t "")))
	 (duration-input (read-string "Duration (MM:SS or seconds, blank to clear): "
				      duration-default))
	 (duration (video-tjm--normalize-time-input duration-input))
	 (still-existing (alist-get 'still broll))
	 (still-default (cond
			 ((booleanp still-existing) still-existing)
			 ((and file (not (string-empty-p file))
			       (image-file-name-p file)))
			 (t nil)))
	 (still-input (if (string-empty-p file)
			  nil
			(string-trim (downcase
				      (read-string
				       (format "Treat as still image? (y/n) [%s]: "
					       (if still-default "y" "n"))
				       (if still-default "y" "n"))))))
	 (still (cond
		 ((null still-input) still-default)
		 ((member still-input '("y" "yes")) t)
		 ((member still-input '("n" "no")) nil)
		 ((string-empty-p still-input) still-default)
		 (t (user-error "Enter y or n for still image"))))
	 (continue-existing (alist-get 'continue broll))
	 (continue-default (and continue-existing t))
	 (continue-input (if (string-empty-p file)
			     nil
			   (string-trim (downcase
					 (read-string
					  (format "Continue from previous segment? (y/n) [%s]: "
						  (if continue-default "y" "n"))
					  (if continue-default "y" "n"))))))
	 (continue (cond
		    ((null continue-input) continue-default)
		    ((member continue-input '("y" "yes")) t)
		    ((member continue-input '("n" "no")) nil)
		    ((string-empty-p continue-input) continue-default)
		    (t (user-error "Enter y or n for continue flag"))))
	 (template (unless (string-empty-p file)
		     (video-tjm--broll-template-data (list (cons 'file file))))))
    (when (and still (string= (downcase audio) "broll"))
      (user-error "Still images cannot use b-roll audio"))
    (if (string-empty-p file)
	(setf (alist-get 'broll segment) nil)
      (progn
	(setf (alist-get 'file broll nil t) file)
	(setf (alist-get 'mode broll nil t) (unless (string-empty-p mode) mode))
	(setf (alist-get 'audio broll nil t) (unless (string-empty-p audio) audio))
	(setf (alist-get 'start_offset broll nil t) offset)
	(setf (alist-get 'duration broll nil t) duration)
	(setf (alist-get 'still broll nil t) (and still t))
	(setf (alist-get 'continue broll nil t) (and continue t))
	(setf (alist-get 'broll segment) broll)
	(when (and template
		   (null (video-tjm--aget 'placeholders broll))
		   (video-tjm--aget 'placeholders template))
	  (message "Template provides placeholders; use C-c C-b to edit them."))))
    (video-tjm--mark-dirty)
    (video-tjm--render t)))

(defun video-tjm-edit-broll-placeholders (segment)
  "Edit placeholder overrides for SEGMENT's b-roll metadata."
  (interactive (list (or (video-tjm--segment-at-point)
			 (user-error "No segment at point"))))
  (let ((broll (alist-get 'broll segment)))
    (unless broll
      (user-error "Segment has no b-roll metadata")))
  (video-tjm--record-state)
  (let* ((broll (copy-alist (alist-get 'broll segment)))
	 (existing (copy-tree (video-tjm--aget 'placeholders broll)))
	 (template (video-tjm--broll-template-data broll))
	 (template-defaults (copy-tree (video-tjm--aget 'placeholders template)))
	 (overrides existing)
	 (keys (video-tjm--placeholder-keys template-defaults existing)))
    (dolist (key keys)
      (let* ((default (video-tjm--placeholder-get key template-defaults))
	     (current (video-tjm--placeholder-get key overrides))
	     (initial (video-tjm--stringify (or current default "")))
	     (prompt (if default
			 (format "Placeholder %s (default %s): "
				 key (video-tjm--quote-value default))
		       (format "Placeholder %s: " key)))
	     (input (string-trim (read-string prompt initial))))
	(if (string-empty-p input)
	    (setq overrides (video-tjm--placeholder-set key nil overrides))
	  (setq overrides (video-tjm--placeholder-set key input overrides)))))
    (let ((continue t))
      (while continue
	(let ((new-key (string-trim (read-string "New placeholder key (blank to finish): "))))
	  (if (string-empty-p new-key)
	      (setq continue nil)
	    (let* ((existing (video-tjm--placeholder-get new-key overrides))
		   (value (string-trim (read-string (format "Value for %s: " new-key)
						    (video-tjm--stringify existing)))))
	      (if (string-empty-p value)
		  (setq overrides (video-tjm--placeholder-set new-key nil overrides))
		(setq overrides (video-tjm--placeholder-set new-key value overrides))))))))
    (if overrides
	(setf (alist-get 'placeholders broll nil t #'equal) overrides)
      (setf (alist-get 'placeholders broll nil t #'equal) nil))
    (setf (alist-get 'broll segment) broll)
    (video-tjm--mark-dirty)
    (video-tjm--render t)
    (message "Updated b-roll placeholders")))

(defun video-tjm-toggle-words ()
  "Toggle display of per-word timings."
  (interactive)
  (setq video-tjm--words-visible (not video-tjm--words-visible))
  (video-tjm--render t)
  (message "Word timings %s" (if video-tjm--words-visible "enabled" "hidden")))

(defun video-tjm-toggle-separator ()
  "Toggle a visual separator after the current segment."
  (interactive)
  (let ((id (video-tjm--segment-id-at-point)))
    (unless id (user-error "No segment at point"))
    (video-tjm--record-state)
    (if (member id video-tjm--visual-separators)
	(setq video-tjm--visual-separators (delete id video-tjm--visual-separators))
      (cl-pushnew id video-tjm--visual-separators :test #'equal))
    (video-tjm--render t)))

(defun video-tjm-insert-marker (&optional title)
  "Insert a marker segment before the current segment, prompting for TITLE only."
  (interactive)
  (let* ((current (video-tjm--segment-at-point))
	 (segments (video-tjm--segments))
	 (idx (if current
		  (cl-position current segments :test #'eq)
		(length segments)))
	 (default-title (or title ""))
	 (title (read-string "Marker title: " default-title))
	 (marker `((id . ,(video-tjm--generate-marker-id))
		   (kind . "marker"))))
    (video-tjm--record-state)
    (unless (string-empty-p title)
      (setf (alist-get 'title marker) title))
    (when current
      (let ((src (alist-get 'source current))
	    (st (alist-get 'start current)))
	(when src (setf (alist-get 'source marker) src))
	(when st (setf (alist-get 'start marker) st))))
    (let* ((head (cl-subseq segments 0 idx))
	   (tail (cl-subseq segments idx))
	   (new-segments (append head (list marker) tail)))
      (setf (alist-get 'segments video-tjm--data) new-segments))
    (video-tjm--mark-dirty)
    (video-tjm--render t)
    (video-tjm--goto-segment (alist-get 'id marker))
    (video-tjm--update-focus-overlay)
    (video-tjm--echo-segment-info)
    (message "Inserted marker")))

(defun video-tjm-open-raw-json ()
  "View the underlying TJM JSON in the current window."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((raw-name (video-tjm--raw-buffer-name))
	 (buf (get-buffer-create raw-name)))
    (video-tjm--populate-raw-buffer buf buffer-file-name)
    (switch-to-buffer buf)))

(defun video-tjm-play-segment (&optional segment)
  "Play SEGMENT using `video-tjm-play-program', or the rendered section when on a marker."
  (interactive)
  (let* ((seg (or segment (video-tjm--segment-at-point)
		  (user-error "No segment at point")))
	 (marker? (video-tjm--marker-p seg)))
    (when marker?
      (video-tjm--play-marker seg)
      (cl-return-from video-tjm-play-segment nil))
    (let* ((source (alist-get 'source seg))
	   (start (alist-get 'start seg))
	   (end (alist-get 'end seg))
	   (source-entry (video-tjm--source-by-id source)))
      (unless source-entry
	(user-error "Unknown source id: %s" source))
      (let* ((file (alist-get 'file source-entry))
	     (abs-file (expand-file-name file (file-name-directory buffer-file-name))))
	(unless (file-exists-p abs-file)
	  (user-error "Source file not found: %s" abs-file))
	(video-tjm--play-with-mpv abs-file start end)
	(message "Playing %s [%s-%s]" file start end)))))

(defun video-tjm-stop-playback ()
  "Stop current playback process if any."
  (interactive)
  (when (process-live-p video-tjm--mpv-process)
    (kill-process video-tjm--mpv-process)
    (setq video-tjm--mpv-process nil)
    (message "Playback stopped")))

(defun video-tjm--play-with-mpv (file start end)
  (unless (executable-find video-tjm-play-program)
    (user-error "Executable '%s' not found" video-tjm-play-program))
  (video-tjm-stop-playback)
  (let* ((start-arg (format "--start=%f" (or start 0.0)))
	 (end-arg (and end (format "--end=%f" end)))
	 (args (append video-tjm-play-args (list start-arg)
		       (when end-arg (list end-arg))
		       (list file))))
    (setq video-tjm--mpv-process
	  (apply #'start-process "video-tjm-mpv" "*video-tjm-mpv*"
		 video-tjm-play-program args))
    (set-process-sentinel video-tjm--mpv-process
			  (lambda (_proc _event)
			    (setq video-tjm--mpv-process nil))))
  (message "Playing %s" (file-name-nondirectory file)))

(defun video-tjm-validate (&optional silent)
  "Validate manifest contents, returning a list of issue strings.
If SILENT is non-nil, only produce messages when failures occur."
  (interactive)
  (let ((tolerance video-tjm-validation-time-tolerance)
	issues)
    (dolist (segment (video-tjm--segments))
      (unless (video-tjm--marker-p segment)
	(let ((start (video-tjm--coerce-number (alist-get 'start segment)))
	      (end (video-tjm--coerce-number (alist-get 'end segment)))
	      (words (alist-get 'words segment))
	      (id (alist-get 'id segment)))
	  (when (or (null start) (null end))
	    (push (format "%s: missing start/end" (or id "<no-id>")) issues))
	  (when (and start end (<= (- end start) (- tolerance)))
	    (push (format "%s: start >= end (Δ%.2fs)" (or id "<no-id>") (- end start)) issues))
	  (dolist (word words)
	    (let ((w-start (video-tjm--coerce-number (alist-get 'start word)))
		  (w-end (video-tjm--coerce-number (alist-get 'end word)))
		  (token (alist-get 'token word)))
	      (when (or (null w-start) (null w-end))
		(push (format "%s: word '%s' missing timing"
			      (or id "<no-id>")
			      (or token ""))
		      issues))
	      (when (and w-start w-end (< (- w-end w-start) (- tolerance)))
		(push (format "%s: word '%s' start >= end (Δ%.2fs)"
			      (or id "<no-id>")
			      (or token "")
			      (- w-end w-start))
		      issues))
	      (when (and start w-start (> (- start w-start) tolerance))
		(push (format "%s: word '%s' before segment (Δ%.2fs)"
			      (or id "<no-id>")
			      (or token "")
			      (- start w-start))
		      issues))
	      (when (and end w-end (> (- w-end end) tolerance))
		(push (format "%s: word '%s' after segment (Δ%.2fs)"
			      (or id "<no-id>")
			      (or token "")
			      (- w-end end))
		      issues)))))))
    (setq issues (cl-remove-duplicates issues :test #'equal))
    (if issues
	(progn
	  (video-tjm--display-validation issues)
	  (unless silent
	    (message "TJM validation reported %d issue(s)" (length issues))))
      (unless silent
	(message "TJM validation passed")))
    issues))

(defun video-tjm--serialize-to-file (file)
  (let ((json-encoding-pretty-print t)
	(json-encoding-lisp-style-closings t))
    (let ((payload (json-encode video-tjm--data)))
      (when (and file (file-exists-p file))
	(let ((backup (concat file "~")))
	  (condition-case _err
	      (copy-file file backup t t t)
	    (error nil))))
      (with-temp-buffer
	(insert payload)
	(unless (bolp) (insert "\n"))
	(write-region (point-min) (point-max) file nil 'silent)))))

(defun video-tjm--segment-bounds (segment-id)
  (when segment-id
    (save-excursion
      (let ((start (video-tjm--goto-segment segment-id)))
	(when start
	  (let ((end (or (next-single-property-change start 'video-tjm-segment-id nil (point-max))
			 (point-max))))
	    (cons start end)))))))

(defun video-tjm--update-focus-overlay ()
  (let* ((id (video-tjm--segment-id-at-point))
	 (bounds (video-tjm--segment-bounds id)))
    (when (overlayp video-tjm--focus-overlay)
      (delete-overlay video-tjm--focus-overlay)
      (setq video-tjm--focus-overlay nil))
    (when bounds
      (setq video-tjm--focus-overlay (make-overlay (car bounds) (cdr bounds)))
      (overlay-put video-tjm--focus-overlay 'face 'video-tjm-current-segment-face)
      (overlay-put video-tjm--focus-overlay 'priority 100)
      (overlay-put video-tjm--focus-overlay 'evaporate t))))

(defun video-tjm--segment-summary (segment)
  (when segment
    (if (video-tjm--marker-p segment)
	(let* ((id (video-tjm--stringify (alist-get 'id segment)))
	       (title (string-trim (video-tjm--stringify (alist-get 'title segment))))
	       (display (format "# %s"
				(if (string-empty-p title) "(Untitled marker)" title))))
	  (format "[%s] %s" id display))
      (let* ((id (video-tjm--stringify (alist-get 'id segment)))
	     (speaker (video-tjm--stringify (alist-get 'speaker segment)))
	     (source (video-tjm--stringify (alist-get 'source segment)))
	     (start (or (video-tjm--display-time (alist-get 'start segment)) ""))
	     (end (or (video-tjm--display-time (alist-get 'end segment)) ""))
	     (tags (alist-get 'tags segment))
	     (notes (video-tjm--stringify (alist-get 'notes segment)))
	     (broll (alist-get 'broll segment))
	     (parts (delq nil (list (format "[%s]" id)
				    speaker
				    (format "(%s %s-%s)" source start end)
				    (when tags (format "tags:%s"
						       (mapconcat #'video-tjm--stringify tags ",")))
				    (when (and broll (alist-get 'file broll))
				      (format "broll:%s" (alist-get 'file broll)))
				    notes))))
	(string-join (cl-remove-if #'string-empty-p (mapcar #'string-trim parts)) " ")))))

(defun video-tjm--echo-segment-info ()
  (let* ((segment (video-tjm--segment-at-point))
	 (id (and segment (alist-get 'id segment))))
    (when (and id (not (equal id video-tjm--last-echo-id)))
      (setq video-tjm--last-echo-id id)
      (message "%s" (video-tjm--segment-summary segment)))))

(defun video-tjm--post-command ()
  (video-tjm--update-focus-overlay)
  (video-tjm--echo-segment-info))

(defun video-tjm--display-validation (issues)
  (let ((buf (get-buffer-create "*TJM Validation*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Validation issues:\n\n")
      (dolist (issue issues)
	(insert " - " issue "\n"))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer buf))
  issues)

(provide 'video/tjm)

;;; tjm.el ends here
