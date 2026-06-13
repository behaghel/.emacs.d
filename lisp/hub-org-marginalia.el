;;; hub-org-marginalia.el --- Org marginalia model helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Parse native Org footnotes into a small marginalia model and compute a
;; deterministic right-margin layout.  This library is side-effect free so it
;; can be reused by interactive authoring commands and document exporters.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defgroup hub/org-marginalia nil
  "Org marginalia helpers."
  :group 'org)

(defcustom hub/org-marginalia-default-kind 'sidenote
  "Default marginalia kind for ordinary Org footnotes."
  :type '(choice (const :tag "Sidenote" sidenote)
		 (const :tag "Footnote" footnote))
  :group 'hub/org-marginalia)

(defcustom hub/org-marginalia-layout-gap 1
  "Blank-line gap preserved between rendered marginalia boxes."
  :type 'natnum
  :group 'hub/org-marginalia)

(defun hub/org-marginalia--line-number-at (position)
  "Return one-based line number at POSITION in the current buffer."
  (line-number-at-pos position t))

(defun hub/org-marginalia--reference-at-definition-p (position)
  "Return non-nil when footnote reference at POSITION starts a definition."
  (save-excursion
    (goto-char position)
    (let ((reference-position (point)))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (= (point) reference-position)
	   (looking-at-p "\\[fn:[^]\n]+\\]")))))

(defun hub/org-marginalia--collect-references ()
  "Return footnote references in source order for the current Org buffer."
  (let (references)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[fn:\\([^]:\n]+\\)\\]" nil t)
	(let ((start (match-beginning 0))
	      (label (match-string-no-properties 1)))
	  (unless (hub/org-marginalia--reference-at-definition-p start)
	    (push (list :id label
			:reference-pos start
			:anchor-line (hub/org-marginalia--line-number-at start)
			:reference-marker (match-string-no-properties 0))
		  references)))))
    (nreverse references)))

(defun hub/org-marginalia--definition-bounds ()
  "Return bounds for the footnote definition at point.
Point must be at the beginning of a native Org footnote definition."
  (let ((start (point))
	(content-start (match-end 0))
	end)
    (save-excursion
      (goto-char content-start)
      (setq end (or (and (re-search-forward "^[ \t]*\\(?:\\[fn:[^]\n]+\\]\\|\\*+\\s-+\\)" nil t)
			 (match-beginning 0))
		    (point-max))))
    (list start content-start end)))

(defun hub/org-marginalia--collect-definitions ()
  "Return a hash table mapping footnote labels to definition plists."
  (let ((definitions (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\[fn:\\([^]:\n]+\\)\\]" nil t)
	(let* ((label (match-string-no-properties 1))
	       (bounds (hub/org-marginalia--definition-bounds))
	       (content-start (nth 1 bounds))
	       (end (nth 2 bounds))
	       (parsed (hub/org-marginalia--parse-definition-content content-start end)))
	  (puthash label
		   (append (list :definition-pos (car bounds)) parsed)
		   definitions))))
    definitions))

(defun hub/org-marginalia--property-name (key)
  "Return the HUB_NOTE property name for KEY."
  (pcase key
    (:kind "HUB_NOTE_KIND")
    (:status "HUB_NOTE_STATUS")
    (:source "HUB_NOTE_SOURCE")
    (:remote-id "HUB_NOTE_REMOTE_ID")))

(defun hub/org-marginalia--parse-definition-content (start end)
  "Parse footnote content between START and END into a note plist."
  (let ((properties nil)
	(body-start start)
	(body nil))
    (save-excursion
      (goto-char start)
      (skip-chars-forward " \t")
      (when (looking-at "\n")
	(forward-line 1)
	(when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
	  (let ((drawer-start (point)))
	    (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
	      (let ((drawer-end (point)))
		(setq properties (hub/org-marginalia--parse-properties drawer-start drawer-end))
		(forward-line 1)
		(setq body-start (point)))))))
      (setq body (string-trim (buffer-substring-no-properties body-start end))))
    (list :kind (hub/org-marginalia--kind-from-string
		 (alist-get (hub/org-marginalia--property-name :kind) properties nil nil #'equal))
	  :status (alist-get (hub/org-marginalia--property-name :status) properties nil nil #'equal)
	  :source (alist-get (hub/org-marginalia--property-name :source) properties nil nil #'equal)
	  :remote-id (alist-get (hub/org-marginalia--property-name :remote-id) properties nil nil #'equal)
	  :body body
	  :height (max 1 (length (split-string body "\n"))))))

(defun hub/org-marginalia--parse-properties (start end)
  "Return an alist of Org drawer properties between START and END."
  (let (properties)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^[ \t]*:\\([[:alnum:]_]+\\):[ \t]*\\(.*?\\)[ \t]*$" end t)
	(let ((key (match-string-no-properties 1))
	      (value (match-string-no-properties 2)))
	  (unless (member key '("PROPERTIES" "END"))
	    (push (cons key value) properties)))))
    properties))

(defun hub/org-marginalia--kind-from-string (value)
  "Return marginalia kind symbol for VALUE."
  (pcase (downcase (or value ""))
    ("footnote" 'footnote)
    ("comment" 'comment)
    ("remote-comment" 'remote-comment)
    ("sidenote" 'sidenote)
    (_ hub/org-marginalia-default-kind)))

;;;###autoload
(defun hub/org-marginalia-collect ()
  "Collect marginalia records from native Org footnotes in the current buffer."
  (let ((definitions (hub/org-marginalia--collect-definitions)))
    (cl-loop for reference in (hub/org-marginalia--collect-references)
	     for definition = (gethash (plist-get reference :id) definitions)
	     when definition
	     collect (append reference definition))))

(defun hub/org-marginalia-layout (notes &optional gap)
  "Return NOTES with display-line and displaced layout properties.
GAP defaults to `hub/org-marginalia-layout-gap'."
  (let ((next-free-line 1)
	(layout-gap (or gap hub/org-marginalia-layout-gap)))
    (mapcar
     (lambda (note)
       (let* ((anchor-line (or (plist-get note :anchor-line) 1))
	      (height (or (plist-get note :height) 1))
	      (display-line (max anchor-line next-free-line)))
	 (setq next-free-line (+ display-line height layout-gap))
	 (append note (list :display-line display-line
			    :displaced (> display-line anchor-line)))))
     notes)))

(provide 'hub-org-marginalia)
;;; hub-org-marginalia.el ends here
