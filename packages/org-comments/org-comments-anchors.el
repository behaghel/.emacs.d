;;; org-comments-anchors.el --- Anchor matching for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Anchor matching, fuzzy matching, and imported inline comment anchoring for
;; Org comments.  Names remain in the legacy `org-comments-*' namespace until
;; the public API migration slice.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-comments-core)
(require 'org-comments-model)
(require 'org-comments-sidecar)
(require 'org-comments-store)
(require 'org-comments-target)
(require 'seq)
(require 'subr-x)

;;;###autoload
(defun org-comments--inline-anchor-target-text (properties)
  "Return matching target text from inline comment PROPERTIES."
  (alist-get "ORG_COMMENTS_TARGET_TEXT" properties nil nil #'equal))

(defun org-comments--normalized-buffer-with-map ()
  "Return normalized buffer text with source position map for current buffer.
The result is a cons cell (TEXT . MAP).  MAP entries are cons cells of original
buffer start and end positions for each character in TEXT."
  (let ((position (point-min))
	(text nil)
	(map nil)
	(pending-space nil))
    (while (< position (point-max))
      (let ((char (char-after position)))
	(if (and char (string-match-p "[[:space:]]" (char-to-string char)))
	    (let ((start position))
	      (while (and (< position (point-max))
			  (string-match-p "[[:space:]]" (char-to-string (char-after position))))
		(setq position (1+ position)))
	      (setq pending-space (cons start position)))
	  (when pending-space
	    (unless (null text)
	      (push ?\s text)
	      (push pending-space map))
	    (setq pending-space nil))
	  (push char text)
	  (push (cons position (1+ position)) map)
	  (setq position (1+ position)))))
    (cons (apply #'string (nreverse text)) (nreverse map))))

(defun org-comments--source-match-from-normalized (map start end)
  "Return source buffer cons from normalized MAP START END positions."
  (cons (car (nth start map))
	(cdr (nth (1- end) map))))

(defun org-comments--anchor-matches-for-text (source-buffer target-text)
  "Return source-buffer matches for normalized TARGET-TEXT."
  (let ((target (org-comments-normalize-target-text (or target-text ""))))
    (unless (string-empty-p target)
      (with-current-buffer source-buffer
	(let* ((normalized (org-comments--normalized-buffer-with-map))
	       (source (car normalized))
	       (map (cdr normalized))
	       (start 0)
	       matches)
	  (while (string-match (regexp-quote target) source start)
	    (let* ((match-start (match-beginning 0))
		   (match-end (match-end 0)))
	      (push (org-comments--source-match-from-normalized map match-start match-end) matches)
	      (setq start (max (1+ match-start) match-end))))
	  (nreverse matches))))))

(defun org-comments-anchor-matches-for-text (source-buffer target-text)
  "Return source-buffer matches for normalized TARGET-TEXT."
  (org-comments--anchor-matches-for-text source-buffer target-text))

(defun org-comments--property-number (properties key)
  "Return numeric value for KEY in PROPERTIES, or nil."
  (let ((value (alist-get key properties nil nil #'equal)))
    (cond
     ((numberp value) value)
     ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
      (string-to-number value)))))

(defun org-comments--remote-indexed-anchor-match (properties matches)
  "Return MATCHES element selected by remote match metadata in PROPERTIES."
  (let* ((expected-count (or (org-comments--property-number
			      properties "ORG_COMMENTS_TARGET_MATCH_COUNT")
			     (org-comments--property-number
			      properties "ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_COUNT")))
	 (index (or (org-comments--property-number
		     properties "ORG_COMMENTS_TARGET_MATCH_INDEX")
		    (org-comments--property-number
		     properties "ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_INDEX")))
	 (count (length matches)))
    (when (and expected-count index
	       (= expected-count count)
	       (< -1 index)
	       (< index count))
      (nth index matches))))

(defun org-comments--similarity (left right)
  "Return approximate similarity ratio between LEFT and RIGHT."
  (let* ((a (org-comments-normalize-target-text (or left "")))
	 (b (org-comments-normalize-target-text (or right "")))
	 (max-length (max (length a) (length b))))
    (if (zerop max-length)
	0.0
      (max 0.0 (- 1.0 (/ (float (string-distance a b)) max-length))))))

(defun org-comments--target-seeds (target)
  "Return useful word seed plists for normalized TARGET."
  (let* ((text (org-comments-normalize-target-text (or target "")))
	 (words nil)
	 (index 0))
    (while (string-match "[[:alnum:]][[:alnum:]'’_-]*" text index)
      (let ((word (match-string 0 text)))
	(when (>= (length word) 3)
	  (push (list :word word :index (match-beginning 0)) words)))
      (setq index (match-end 0)))
    (setq words (nreverse words))
    (delete-dups
     (append (delq nil (list (car words)
			     (car (last words))
			     (cadr words)
			     (cadr (reverse words))))
	     (sort (copy-sequence words)
		   (lambda (left right)
		     (> (length (plist-get left :word))
			(length (plist-get right :word)))))))))

(defun org-comments--target-window-eligible-p (window)
  "Return non-nil when shrink WINDOW is safe enough for exact anchoring."
  (let ((text (org-comments-normalize-target-text (or window "")))
	(count 0)
	(index 0))
    (while (string-match "[[:alnum:]][[:alnum:]'’_-]*" text index)
      (when (>= (length (match-string 0 text)) 3)
	(setq count (1+ count)))
      (setq index (match-end 0)))
    (or (>= count 3) (>= (length text) 12))))

(defun org-comments--camel-boundary-spaced (text)
  "Return TEXT with likely lowercase-to-uppercase word boundaries spaced."
  (let ((case-fold-search nil))
    (replace-regexp-in-string "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2" text)))

(defun org-comments--shrink-windows (target-text)
  "Return contiguous shrink windows for TARGET-TEXT, longest first."
  (let* ((target (org-comments-normalize-target-text
		  (org-comments--camel-boundary-spaced (or target-text ""))))
	 (words (split-string target "[[:space:]]+" t))
	 windows)
    (cl-loop for size from (length words) downto 1
	     do (cl-loop for start from 0 to (- (length words) size)
			 for window = (string-join (seq-subseq words start (+ start size)) " ")
			 when (org-comments--target-window-eligible-p window)
			 do (push window windows)))
    (nreverse (delete-dups windows))))

(defun org-comments--shrink-anchor-candidates (source-buffer target-text)
  "Return exact shrink candidates for TARGET-TEXT in SOURCE-BUFFER."
  (let (candidates best-ambiguous-count unique)
    (catch 'found
      (dolist (window (org-comments--shrink-windows target-text))
	(let* ((matches (org-comments--anchor-matches-for-text source-buffer window))
	       (count (length matches)))
	  (cond
	   ((= count 1)
	    (setq unique (list :kind 'exact
			       :start (caar matches)
			       :end (cdar matches)
			       :target-text window))
	    (throw 'found nil))
	   ((> count 1)
	    (setq best-ambiguous-count (if best-ambiguous-count
					   (min best-ambiguous-count count)
					 count))
	    (dolist (match matches)
	      (push (list :kind 'exact
			  :match-count count
			  :window-length (length window)
			  :start (car match)
			  :end (cdr match)
			  :target-text window)
		    candidates)))))))
    (list :unique unique
	  :candidates
	  (seq-take
	   (sort (cl-remove-duplicates
		  candidates
		  :test (lambda (left right)
			  (and (= (plist-get left :start) (plist-get right :start))
			       (= (plist-get left :end) (plist-get right :end))
			       (equal (plist-get left :target-text)
				      (plist-get right :target-text)))))
		 (lambda (left right)
		   (or (> (plist-get left :window-length) (plist-get right :window-length))
		       (and (= (plist-get left :window-length) (plist-get right :window-length))
			    (< (plist-get left :match-count) (plist-get right :match-count)))
		       (and (= (plist-get left :window-length) (plist-get right :window-length))
			    (= (plist-get left :match-count) (plist-get right :match-count))
			    (< (plist-get left :start) (plist-get right :start))))))
	   5)
	  :best-count best-ambiguous-count)))

(defun org-comments--fuzzy-anchor-candidates (source-buffer target-text)
  "Return fuzzy candidate plists for TARGET-TEXT in SOURCE-BUFFER."
  (let ((target (org-comments-normalize-target-text (or target-text ""))))
    (unless (string-empty-p target)
      (with-current-buffer source-buffer
	(let* ((normalized (org-comments--normalized-buffer-with-map))
	       (source (car normalized))
	       (map (cdr normalized))
	       (target-length (length target))
	       (margin (max 12 (/ target-length 5)))
	       candidates)
	  (dolist (seed (org-comments--target-seeds target))
	    (let ((start 0)
		  (word (regexp-quote (plist-get seed :word)))
		  (seed-index (plist-get seed :index)))
	      (while (string-match word source start)
		(let* ((match-start (match-beginning 0))
		       (window-start (max 0 (- match-start seed-index margin)))
		       (window-end (min (length source)
					(+ window-start target-length (* 2 margin))))
		       (window (substring source window-start window-end))
		       (score (org-comments--similarity target window)))
		  (when (>= score 0.72)
		    (let ((source-match (org-comments--source-match-from-normalized
					 map window-start window-end)))
		      (push (list :kind 'fuzzy
				  :score score
				  :start (car source-match)
				  :end (cdr source-match)
				  :text window)
			    candidates)))
		  (setq start (max (1+ match-start) (match-end 0)))))))
	  (setq candidates
		(sort (cl-remove-duplicates
		       candidates
		       :test (lambda (left right)
			       (and (= (plist-get left :start) (plist-get right :start))
				    (= (plist-get left :end) (plist-get right :end)))))
		      (lambda (left right)
			(> (plist-get left :score) (plist-get right :score)))))
	  (let ((strong (cl-remove-if-not (lambda (candidate)
					    (>= (plist-get candidate :score) 0.9))
					  candidates)))
	    (if strong
		(seq-take strong 3)
	      (seq-take candidates 3))))))))

(defun org-comments--put-anchor-properties (source-buffer start end target-text)
  "Put anchor properties for SOURCE-BUFFER START END and TARGET-TEXT at point."
  (let ((start-line-column nil)
	(end-line-column nil)
	(normalized (org-comments-normalize-target-text target-text)))
    (with-current-buffer source-buffer
      (setq start-line-column (org-comments--line-column-at start))
      (setq end-line-column (org-comments--line-column-at end)))
    (org-entry-put nil "ORG_COMMENTS_TARGET" (format "%s %s" start end))
    (org-entry-put nil "ORG_COMMENTS_TARGET_LINES"
		   (format "%s:%s %s:%s"
			   (car start-line-column) (cdr start-line-column)
			   (car end-line-column) (cdr end-line-column)))
    (org-entry-put nil "ORG_COMMENTS_TARGET_TEXT" normalized)
    (org-entry-delete nil "ORG_COMMENTS_TARGET_HASH")
    (org-entry-delete nil "ORG_COMMENTS_ANCHOR_STATE")
    (org-entry-delete nil "ORG_COMMENTS_ANCHOR_MATCH_COUNT")))

(defun org-comments--source-context-label (source-buffer candidate &optional exact)
  "Return completion label for CANDIDATE in SOURCE-BUFFER.
When EXACT is non-nil, use an exact-match prefix instead of a fuzzy score."
  (with-current-buffer source-buffer
    (let* ((start (plist-get candidate :start))
	   (end (plist-get candidate :end))
	   (line (line-number-at-pos start t))
	   (column (save-excursion (goto-char start) (current-column)))
	   (before-start (max (point-min) (- start 36)))
	   (after-end (min (point-max) (+ end 36)))
	   (before (org-comments-normalize-target-text
		    (buffer-substring-no-properties before-start start)))
	   (match (org-comments-normalize-target-text
		   (buffer-substring-no-properties start end)))
	   (after (org-comments-normalize-target-text
		   (buffer-substring-no-properties end after-end)))
	   (prefix (if exact
		       "exact"
		     (format "%.2f" (plist-get candidate :score)))))
      (format "%s · L%s:C%s · …%s [%s] %s…"
	      prefix line column before match after))))

(defun org-comments--prompt-preview (text length)
  "Return a short decoded prompt preview for TEXT up to LENGTH."
  (or (org-comments--preview-text text length) ""))

(defun org-comments--triage-prompt (properties target-text body)
  "Return prompt for anchoring PROPERTIES with TARGET-TEXT and BODY."
  (let ((author (or (alist-get "ORG_COMMENTS_AUTHOR" properties nil nil #'equal)
		    (alist-get "ORG_COMMENTS_REMOTE_AUTHOR_ID" properties nil nil #'equal))))
    (format "Anchor “%s”%s: "
	    (org-comments--prompt-preview target-text 36)
	    (let ((body-preview (org-comments--prompt-preview body 48)))
	      (if (string-empty-p body-preview)
		  ""
		(format " — %s%s" (if author (concat author ": ") "") body-preview))))))

(defun org-comments--candidate-choice (source-buffer properties target-text body exact-matches)
  "Read an anchor candidate for PROPERTIES from exact and fuzzy matches."
  (let* ((exact-candidates
	  (mapcar (lambda (match)
		    (list :kind 'exact :start (car match) :end (cdr match)
			  :target-text target-text))
		  exact-matches))
	 (fuzzy-candidates (org-comments--fuzzy-anchor-candidates source-buffer target-text))
	 (shrink (unless fuzzy-candidates
		   (org-comments--shrink-anchor-candidates source-buffer target-text)))
	 (shrink-candidates (plist-get shrink :candidates))
	 (candidates (append exact-candidates fuzzy-candidates shrink-candidates))
	 (skip-label "SKIP · leave this comment unanchored")
	 (entries (list (cons skip-label nil))))
    (dolist (candidate candidates)
      (push (cons (org-comments--source-context-label
		   source-buffer candidate (eq (plist-get candidate :kind) 'exact))
		  candidate)
	    entries))
    (setq entries (nreverse entries))
    (alist-get
     (completing-read (org-comments--triage-prompt properties target-text body)
		      (mapcar #'car entries) nil t nil nil skip-label)
     entries nil nil #'equal)))

(defun org-comments--triage-imported-inline-comments (source-buffer sidecar-file)
  "Interactively triage unresolved imported inline comments in SIDECAR-FILE."
  (let ((selected 0)
	(skipped 0)
	(last-start nil)
	(last-end nil))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(goto-char (match-beginning 0))
	(let* ((properties (org-comments--parse-properties-at-heading))
	       (source (alist-get "ORG_COMMENTS_SOURCE" properties nil nil #'equal))
	       (sync-kind (alist-get "ORG_COMMENTS_SYNC_KIND" properties nil nil #'equal)))
	  (when (and (equal source "confluence")
		     (equal sync-kind "inline")
		     (not (org-comments--valid-anchor-p source-buffer properties)))
	    (let* ((entry-end (save-excursion (org-end-of-subtree t t)))
		   (body (org-comments--entry-body entry-end))
		   (target-text (org-comments--inline-anchor-target-text properties))
		   (exact-matches (org-comments--anchor-matches-for-text source-buffer target-text))
		   (choice (org-comments--candidate-choice
			    source-buffer properties target-text body
			    (when (> (length exact-matches) 1) exact-matches))))
	      (if choice
		  (progn
		    (org-comments--put-anchor-properties
		     source-buffer (plist-get choice :start) (plist-get choice :end)
		     (or (plist-get choice :target-text) target-text))
		    (setq selected (1+ selected)
			  last-start (plist-get choice :start)
			  last-end (plist-get choice :end)))
		(setq skipped (1+ skipped))))))
	(forward-line 1))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))
    (when last-start
      (with-current-buffer source-buffer
	(goto-char last-start)
	(pulse-momentary-highlight-region last-start last-end)))
    (list :selected selected :skipped skipped)))

(defun org-comments--anchor-imported-inline-comments (&optional source-buffer sidecar-file)
  "Anchor imported Confluence inline comments for SOURCE-BUFFER in SIDECAR-FILE.
Return a plist summary with `:anchored', `:missing', and `:ambiguous' counts."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (target-sidecar (or sidecar-file (and source-file (org-comments-sidecar-path source-file))))
	 (anchored 0)
	 (missing 0)
	 (ambiguous 0))
    (unless (and target-sidecar (file-exists-p target-sidecar))
      (user-error "No sidecar comments file: %s" target-sidecar))
    (with-temp-buffer
      (insert-file-contents target-sidecar)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(goto-char (match-beginning 0))
	(let* ((properties (org-comments--parse-properties-at-heading))
	       (source (alist-get "ORG_COMMENTS_SOURCE" properties nil nil #'equal))
	       (sync-kind (alist-get "ORG_COMMENTS_SYNC_KIND" properties nil nil #'equal)))
	  (when (and (equal source "confluence")
		     (equal sync-kind "inline")
		     (not (org-comments--valid-anchor-p buffer properties)))
	    (let* ((target-text (org-comments--inline-anchor-target-text properties))
		   (matches (org-comments--anchor-matches-for-text buffer target-text))
		   (count (length matches)))
	      (pcase count
		(0
		 (let* ((fuzzy-candidates (org-comments--fuzzy-anchor-candidates buffer target-text))
			(shrink (unless fuzzy-candidates
				  (org-comments--shrink-anchor-candidates buffer target-text)))
			(unique (plist-get shrink :unique))
			(best-count (plist-get shrink :best-count)))
		   (cond
		    (unique
		     (org-comments--put-anchor-properties
		      buffer (plist-get unique :start) (plist-get unique :end)
		      (plist-get unique :target-text))
		     (setq anchored (1+ anchored)))
		    (best-count
		     (org-entry-put nil "ORG_COMMENTS_ANCHOR_STATE" "ambiguous")
		     (org-entry-put nil "ORG_COMMENTS_ANCHOR_MATCH_COUNT" (number-to-string best-count))
		     (setq ambiguous (1+ ambiguous)))
		    (t
		     (org-entry-put nil "ORG_COMMENTS_ANCHOR_STATE" "missing")
		     (org-entry-put nil "ORG_COMMENTS_ANCHOR_MATCH_COUNT" "0")
		     (setq missing (1+ missing))))))
		(1
		 (org-comments--put-anchor-properties
		  buffer (caar matches) (cdar matches) target-text)
		 (setq anchored (1+ anchored)))
		(_
		 (if-let* ((remote-match (org-comments--remote-indexed-anchor-match
					  properties matches)))
		     (progn
		       (org-comments--put-anchor-properties
			buffer (car remote-match) (cdr remote-match) target-text)
		       (setq anchored (1+ anchored)))
		   (org-entry-put nil "ORG_COMMENTS_ANCHOR_STATE" "ambiguous")
		   (org-entry-put nil "ORG_COMMENTS_ANCHOR_MATCH_COUNT" (number-to-string count))
		   (setq ambiguous (1+ ambiguous))))))))
	(forward-line 1))
      (write-region (point-min) (point-max) target-sidecar nil 'silent))
    (list :anchored anchored :missing missing :ambiguous ambiguous)))

(defun org-comments-anchor-imported-inline-comments (&optional source-buffer sidecar-file)
  "Anchor imported inline comments for SOURCE-BUFFER in SIDECAR-FILE.
Return a plist summary with `:anchored', `:missing', and `:ambiguous' counts."
  (org-comments--anchor-imported-inline-comments source-buffer sidecar-file))

(provide 'org-comments-anchors)
;;; org-comments-anchors.el ends here
