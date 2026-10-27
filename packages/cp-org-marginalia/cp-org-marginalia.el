;;; cp-org-marginalia.el --- Org marginalia model helpers -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (context-panels "0.1.0"))
;; Keywords: outlines, convenience
;; URL: https://github.com/behaghel/.emacs.d/tree/master/packages/cp-org-marginalia

;;; Commentary:
;; Parse native Org footnotes into a small marginalia model and compute a
;; deterministic right-margin layout.  The collector loads without the panel
;; library; context-panels supplies the package's optional interactive surface.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defgroup cp-org-marginalia nil
  "Org marginalia helpers."
  :group 'org)

(defcustom cp-org-marginalia-default-kind 'sidenote
  "Default marginalia kind for ordinary Org footnotes."
  :type '(choice (const :tag "Sidenote" sidenote)
		 (const :tag "Footnote" footnote))
  :group 'cp-org-marginalia)

(defcustom cp-org-marginalia-layout-gap 1
  "Blank-line gap preserved between rendered marginalia boxes."
  :type 'natnum
  :group 'cp-org-marginalia)

(defun cp-org-marginalia--line-number-at (position)
  "Return one-based line number at POSITION in the current buffer."
  (line-number-at-pos position t))

(defun cp-org-marginalia--reference-at-definition-p (position)
  "Return non-nil when footnote reference at POSITION starts a definition."
  (save-excursion
    (goto-char position)
    (let ((reference-position (point)))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (= (point) reference-position)
	   (looking-at-p "\\[fn:[^]\n]+\\]")))))

(defun cp-org-marginalia--collect-references ()
  "Return footnote references in source order for the current Org buffer."
  (let (references)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[fn:\\([^]:\n]+\\)\\]" nil t)
	(let ((start (match-beginning 0))
	      (label (match-string-no-properties 1)))
	  (unless (cp-org-marginalia--reference-at-definition-p start)
	    (push (list :id label
			:reference-pos start
			:anchor-line (cp-org-marginalia--line-number-at start)
			:reference-marker (match-string-no-properties 0))
		  references)))))
    (nreverse references)))

(defun cp-org-marginalia--definition-bounds ()
  "Return bounds for the footnote definition at point."
  (let ((start (point))
	(content-start (match-end 0))
	end)
    (save-excursion
      (goto-char content-start)
      (setq end (or (and (re-search-forward "^[ \t]*\\(?:\\[fn:[^]\n]+\\]\\|\\*+\\s-+\\)" nil t)
			 (match-beginning 0))
		    (point-max))))
    (list start content-start end)))

(defun cp-org-marginalia--collect-definitions ()
  "Return a hash table mapping footnote labels to definition plists."
  (let ((definitions (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\[fn:\\([^]:\n]+\\)\\]" nil t)
	(let* ((label (match-string-no-properties 1))
	       (bounds (cp-org-marginalia--definition-bounds))
	       (content-start (nth 1 bounds))
	       (end (nth 2 bounds))
	       (parsed (cp-org-marginalia--parse-definition-content content-start end)))
	  (puthash label
		   (append (list :definition-pos (car bounds)) parsed)
		   definitions))))
    definitions))

(defun cp-org-marginalia--property-name (key)
  "Return the HUB_NOTE property name for KEY."
  (pcase key
    (:kind "HUB_NOTE_KIND")
    (:status "HUB_NOTE_STATUS")
    (:source "HUB_NOTE_SOURCE")
    (:remote-id "HUB_NOTE_REMOTE_ID")))

(defun cp-org-marginalia--parse-definition-content (start end)
  "Parse footnote content between START and END into a note plist."
  (let ((properties nil)
	(body-start start)
	(body nil))
    (save-excursion
      (goto-char start)
      (skip-chars-forward " \t")
      (when (looking-at ":")
	(forward-char 1)
	(skip-chars-forward " \t"))
      (when (looking-at "\n")
	(forward-line 1))
      (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
	(let ((drawer-start (point)))
	  (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
	    (let ((drawer-end (point)))
	      (setq properties (cp-org-marginalia--parse-properties drawer-start drawer-end))
	      (forward-line 1)
	      (setq body-start (point))))))
      (unless properties
	(setq body-start (point)))
      (setq body (string-trim (buffer-substring-no-properties body-start end))))
    (list :kind (cp-org-marginalia--kind-from-string
		 (alist-get (cp-org-marginalia--property-name :kind) properties nil nil #'equal))
	  :status (alist-get (cp-org-marginalia--property-name :status) properties nil nil #'equal)
	  :source (alist-get (cp-org-marginalia--property-name :source) properties nil nil #'equal)
	  :remote-id (alist-get (cp-org-marginalia--property-name :remote-id) properties nil nil #'equal)
	  :body body
	  :height (max 1 (length (split-string body "\n"))))))

(defun cp-org-marginalia--parse-properties (start end)
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

(defun cp-org-marginalia--kind-from-string (value)
  "Return marginalia kind symbol for VALUE."
  (pcase (downcase (or value ""))
    ("footnote" 'footnote)
    ("sidenote" 'sidenote)
    (_ cp-org-marginalia-default-kind)))

;;;###autoload
(defun cp-org-marginalia-collect ()
  "Collect marginalia records from native Org footnotes in the current buffer."
  (let ((definitions (cp-org-marginalia--collect-definitions)))
    (cl-loop for reference in (cp-org-marginalia--collect-references)
	     for definition = (gethash (plist-get reference :id) definitions)
	     when definition
	     collect (append reference definition))))

(defun cp-org-marginalia-layout (notes &optional gap)
  "Return NOTES with display-line and displaced layout properties.
GAP defaults to `cp-org-marginalia-layout-gap'."
  (let ((next-free-line 1)
	(layout-gap (or gap cp-org-marginalia-layout-gap)))
    (mapcar
     (lambda (note)
       (let* ((anchor-line (or (plist-get note :anchor-line) 1))
	      (height (or (plist-get note :height) 1))
	      (display-line (max anchor-line next-free-line)))
	 (setq next-free-line (+ display-line height layout-gap))
	 (append note (list :display-line display-line
			    :displaced (> display-line anchor-line)))))
     notes)))

(provide 'cp-org-marginalia)
;;; cp-org-marginalia.el ends here
