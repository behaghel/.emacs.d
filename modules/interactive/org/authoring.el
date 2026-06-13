;;; authoring.el --- Org mode: authoring shortcuts -*- lexical-binding: t; -*-

;;; Commentary:
;; Authoring helpers, inline template completion, and semantic writing shortcuts.

;;; Code:

(require 'hub-utils)

(require 'hub-org-callout)
(require 'subr-x)

(defconst hub/org-confluence-status-colours
  '("Grey" "Red" "Yellow" "Green" "Blue" "Purple")
  "Confluence status colours offered by Org authoring shortcuts.")

(defface hub/org-confluence-status-grey
  '((t :inherit default :foreground "#172B4D" :background "#DFE1E6" :box (:line-width (1 . -1) :color "#DFE1E6")))
  "Face for grey Confluence status links."
  :group 'hub/org)

(defface hub/org-confluence-status-red
  '((t :inherit default :foreground "#FFFFFF" :background "#DE350B" :box (:line-width (1 . -1) :color "#DE350B")))
  "Face for red Confluence status links."
  :group 'hub/org)

(defface hub/org-confluence-status-yellow
  '((t :inherit default :foreground "#172B4D" :background "#FFAB00" :box (:line-width (1 . -1) :color "#FFAB00")))
  "Face for yellow Confluence status links."
  :group 'hub/org)

(defface hub/org-confluence-status-green
  '((t :inherit default :foreground "#FFFFFF" :background "#00875A" :box (:line-width (1 . -1) :color "#00875A")))
  "Face for green Confluence status links."
  :group 'hub/org)

(defface hub/org-confluence-status-blue
  '((t :inherit default :foreground "#FFFFFF" :background "#0052CC" :box (:line-width (1 . -1) :color "#0052CC")))
  "Face for blue Confluence status links."
  :group 'hub/org)

(defface hub/org-confluence-status-purple
  '((t :inherit default :foreground "#FFFFFF" :background "#6554C0" :box (:line-width (1 . -1) :color "#6554C0")))
  "Face for purple Confluence status links."
  :group 'hub/org)

(defun hub/org-confluence-status-face (colour)
  "Return a status chip face for COLOUR."
  (pcase (downcase (or colour ""))
    ("red" 'hub/org-confluence-status-red)
    ("yellow" 'hub/org-confluence-status-yellow)
    ("green" 'hub/org-confluence-status-green)
    ("blue" 'hub/org-confluence-status-blue)
    ("purple" 'hub/org-confluence-status-purple)
    (_ 'hub/org-confluence-status-grey)))

(defun hub/org-confluence-status-activate (start end _path bracketp)
  "Add chip-like display properties to a Confluence status link.

START and END delimit the link.  BRACKETP is non-nil for bracket links with an
optional description."
  (when bracketp
    (save-excursion
      (goto-char start)
      (when (re-search-forward "\\[\\[confluence-status:[^]]+\\]\\[\\([^]]+\\)\\]\\]" end t)
	(let* ((description-start (match-beginning 1))
	       (description-end (match-end 1))
	       (display-title (upcase (match-string-no-properties 1))))
	  (add-text-properties description-start description-end
			       `(display ,display-title)))))))

(with-eval-after-load 'ol
  (org-link-set-parameters
   "confluence-status"
   :face #'hub/org-confluence-status-face
   :activate-func #'hub/org-confluence-status-activate))

(defun hub/org-yas-ready-p ()
  "Return non-nil when Yasnippet can expand snippets in this buffer."
  (and (fboundp 'yas-expand-snippet)
       (bound-and-true-p yas-minor-mode)))

(defun hub/org-callout-template-snippet ()
  "Return a Yasnippet-compatible semantic callout template."
  (format "#+ATTR_CALLOUT: :type ${1|%s|} :title \"${2:Title}\"\n#+begin_callout\n$0\n#+end_callout"
	  (string-join hub/org-callout-types ",")))

(defun hub/org-image-template-snippet ()
  "Return a Yasnippet-compatible Org image template."
  "#+CAPTION: ${1:}\n[[${2:./img/image.png}]]")

(defun hub/org-remove-empty-caption-at (marker)
  "Remove an empty Org #+CAPTION line at MARKER."
  (when (marker-buffer marker)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
	(goto-char marker)
	(when (looking-at "^[ \t]*#\\+CAPTION:[ \t]*$")
	  (delete-region (line-beginning-position)
			 (min (point-max) (1+ (line-end-position)))))))))

(defun hub/org-expand-image-snippet (snippet caption-marker)
  "Expand image SNIPPET and cleanup empty caption at CAPTION-MARKER."
  (letrec ((cleanup (lambda ()
		      (remove-hook 'yas-after-exit-snippet-hook cleanup t)
		      (hub/org-remove-empty-caption-at caption-marker))))
    (add-hook 'yas-after-exit-snippet-hook cleanup nil t)
    (yas-expand-snippet snippet)))

(defun hub/org-image-template-path (path)
  "Return image PATH for insertion in the current Org buffer."
  (let ((expanded (expand-file-name path)))
    (if buffer-file-name
	(let ((relative (file-relative-name expanded (file-name-directory buffer-file-name))))
	  (if (or (string-prefix-p "../" relative)
		  (string-prefix-p "/" relative))
	      expanded
	    (concat "./" relative)))
      expanded)))

(defun hub/org-insert-image-template ()
  "Insert an Org image template at point.

When Yasnippet is available, an empty caption field is removed after the
snippet exits.  Skipping the caption with TAB or clearing it therefore leaves
only the image link."
  (interactive)
  (if (hub/org-yas-ready-p)
      (hub/org-expand-image-snippet (hub/org-image-template-snippet) (point-marker))
    (let ((caption (read-string "Image caption: "))
	  (path (hub/org-image-template-path (read-file-name "Image file: "))))
      (unless (string-empty-p caption)
	(insert (format "#+CAPTION: %s\n" caption)))
      (insert (format "[[%s]]" path)))))

(defun hub/org-tempo-complete-image ()
  "Expand the `<im' Org Tempo shortcut as an image template."
  (when (looking-back "^ *\\(<im\\)" (line-beginning-position))
    (replace-match "" t t nil 1)
    (hub/org-insert-image-template)
    t))

(defun hub/org-insert-callout-template ()
  "Insert a semantic Org callout template at point."
  (interactive)
  (if (hub/org-yas-ready-p)
      (yas-expand-snippet (hub/org-callout-template-snippet))
    (let* ((type (completing-read "Callout type: " hub/org-callout-types nil t nil nil "info"))
	   (title (read-string "Callout title: ")))
      (insert (format "#+ATTR_CALLOUT: :type %s" type))
      (unless (string-empty-p title)
	(insert (format " :title %S" title)))
      (insert "\n#+begin_callout\n\n#+end_callout")
      (forward-line -1))))

(defun hub/org-tempo-complete-callout ()
  "Expand the `<co' Org Tempo shortcut as a semantic callout."
  (when (looking-back "^ *\\(<co\\)" (line-beginning-position))
    (replace-match "" t t nil 1)
    (hub/org-insert-callout-template)
    t))

(defun hub/org-footnote-definition-point (label)
  "Create footnote definition for LABEL and return its insertion point."
  (require 'org-footnote)
  (let ((definition-start (org-footnote-create-definition label)))
    (save-excursion
      (goto-char definition-start)
      (search-forward (format "[fn:%s] " label) nil t)
      (point))))

(defun hub/org-sort-footnotes ()
  "Sort Org footnote definitions to match reference order."
  (require 'org-footnote)
  (org-footnote-sort))

(defun hub/org-expand-footnote-snippet (label return-marker)
  "Expand a footnote body snippet for LABEL and return to RETURN-MARKER."
  (ignore label)
  (letrec ((return-to-reference
	    (lambda ()
	      (remove-hook 'yas-after-exit-snippet-hook return-to-reference t)
	      (hub/org-sort-footnotes)
	      (when (marker-buffer return-marker)
		(goto-char return-marker)))))
    (add-hook 'yas-after-exit-snippet-hook return-to-reference nil t)
    (yas-expand-snippet "${1:Footnote text}")))

(defun hub/org-footnote-insert-properties (properties)
  "Insert footnote metadata PROPERTIES at point.
PROPERTIES is an alist of Org property names to values."
  (when properties
    (insert "\n:PROPERTIES:\n")
    (dolist (property properties)
      (insert (format ":%s: %s\n" (car property) (cdr property))))
    (insert ":END:\n")))

(defun hub/org-insert-footnote-template-with-properties (&optional properties prompt)
  "Insert a footnote reference and edit its definition with PROPERTIES.
PROMPT is used when Yasnippet is unavailable.  With Yasnippet, point returns to
original text after leaving the footnote body field."
  (require 'org-footnote)
  (let* ((label (org-footnote-unique-label))
	 (reference-end nil)
	 (definition-point nil))
    (insert (format "[fn:%s]" label))
    (setq reference-end (point-marker))
    (setq definition-point (hub/org-footnote-definition-point label))
    (goto-char definition-point)
    (hub/org-footnote-insert-properties properties)
    (if (hub/org-yas-ready-p)
	(hub/org-expand-footnote-snippet label reference-end)
      (insert (read-string (or prompt "Footnote: ")))
      (hub/org-sort-footnotes)
      (goto-char reference-end))))

(defun hub/org-insert-footnote-template ()
  "Insert a footnote reference and edit its bottom definition.

With Yasnippet, point returns to the original text after leaving the footnote
body field.  Without Yasnippet, prompt for the body immediately and return."
  (interactive)
  (hub/org-insert-footnote-template-with-properties nil "Footnote: "))

(defun hub/org-insert-traditional-footnote-template ()
  "Insert a footnote forced to remain a traditional bottom footnote."
  (interactive)
  (hub/org-insert-footnote-template-with-properties
   '(("HUB_NOTE_KIND" . "footnote"))
   "Footnote: "))

(defun hub/org-insert-comment-footnote-template ()
  "Insert an editorial comment footnote with open status metadata."
  (interactive)
  (hub/org-insert-footnote-template-with-properties
   '(("HUB_NOTE_KIND" . "comment")
     ("HUB_NOTE_STATUS" . "open"))
   "Comment: "))

(defun hub/org-tempo-complete-footnote-kind (shortcut insert-function)
  "Expand SHORTCUT and call INSERT-FUNCTION for an inline footnote."
  (when (looking-back (format "\\(<%s\\)" (regexp-quote shortcut)) (line-beginning-position))
    (replace-match "" t t nil 1)
    (funcall insert-function)
    t))

(defun hub/org-tempo-complete-footnote ()
  "Expand the `<fn' Org Tempo shortcut as a default footnote or sidenote.

Unlike block-oriented Org Tempo shortcuts, footnotes are inline text objects, so
this shortcut is intentionally accepted anywhere on the current line."
  (hub/org-tempo-complete-footnote-kind "fn" #'hub/org-insert-footnote-template))

(defun hub/org-tempo-complete-traditional-footnote ()
  "Expand the `<ft' Org Tempo shortcut as a traditional bottom footnote."
  (hub/org-tempo-complete-footnote-kind "ft" #'hub/org-insert-traditional-footnote-template))

(defun hub/org-tempo-complete-comment-footnote ()
  "Expand the `<fc' Org Tempo shortcut as an editorial comment footnote."
  (hub/org-tempo-complete-footnote-kind "fc" #'hub/org-insert-comment-footnote-template))

(defun hub/org-insert-confluence-status ()
  "Insert an Org link representing a Confluence status macro."
  (interactive)
  (let* ((colour (completing-read "Status colour: " hub/org-confluence-status-colours nil t nil nil "Grey"))
	 (title (upcase (read-string "Status text: " colour))))
    (insert (format "[[confluence-status:%s][%s]]" colour title))))

(defun hub/org-tempo-complete-status ()
  "Expand the `<st' Org Tempo shortcut as a Confluence status link."
  (when (looking-back "\\(<st\\)" (line-beginning-position))
    (replace-match "" t t nil 1)
    (hub/org-insert-confluence-status)
    t))

(defun hub/org-setup-authoring-templates ()
  "Configure Org Tempo structure templates and custom TAB completions."
  (hub/org-set-structure-template "c" "comment")
  (hub/org-set-structure-template "C" "center")
  (hub/org-set-structure-template "sf" "standfirst")
  (hub/org-set-structure-template "ep" "epigraph")
  (hub/org-set-structure-template "pq" "pullquote")
  (hub/org-set-structure-template "ci" "info")
  (hub/org-set-structure-template "cw" "warning")
  (hub/org-set-structure-template "ca" "authorsnote")
  (hub/org-set-structure-template "me" "metrics")
  (hub/org-set-structure-template "mi" "metric")
  (hub/org-set-structure-template "pi" "pillars")
  (hub/org-set-structure-template "pa" "pillar")
  (hub/org-set-structure-template "gr" "graph")
  (require 'org-tempo)
  (add-hook 'org-tab-before-tab-emulation-hook #'hub/org-tempo-complete-callout -90)
  (add-hook 'org-tab-before-tab-emulation-hook #'hub/org-tempo-complete-image -90)
  (add-hook 'org-tab-before-tab-emulation-hook #'hub/org-tempo-complete-footnote -90)
  (add-hook 'org-tab-before-tab-emulation-hook #'hub/org-tempo-complete-traditional-footnote -90)
  (add-hook 'org-tab-before-tab-emulation-hook #'hub/org-tempo-complete-comment-footnote -90)
  (add-hook 'org-tab-before-tab-emulation-hook #'hub/org-tempo-complete-status -90))

(provide 'org/authoring)
;;; authoring.el ends here
