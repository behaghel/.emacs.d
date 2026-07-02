;;; org-confluence-inline-comments.el --- Confluence inline comment markers -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for reading, relocating, and preserving Confluence inline comment
;; markers in Storage Format XHTML.

;;; Code:

(require 'org-comments-anchors)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-api)
(require 'org-confluence-comments-remote)
(require 'org-confluence-response)

(defvar org-confluence-publish-preserve-inline-comments)
(defvar org-confluence-publish--pages-needing-inline-marker-preservation)

(defconst org-confluence-inline-comments-inline-comment-marker-regexp
  "[<]ac:inline-comment-marker[^>]*ac:ref=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)[<]/ac:inline-comment-marker>"
  "Regexp matching a simple Confluence storage inline comment marker.")

(defconst org-confluence-inline-comments-inline-comment-context-window 100
  "Number of storage bytes used around inline comment markers for relocation.")

(defun org-confluence-inline-comments-count-normalized-occurrences (needle haystack)
  "Return count of normalized NEEDLE occurrences in normalized HAYSTACK."
  (let ((target (org-comments-normalize-target-text (or needle "")))
	(source (org-comments-normalize-target-text (or haystack "")))
	(start 0)
	(count 0))
    (unless (string-empty-p target)
      (while (string-match (regexp-quote target) source start)
	(setq count (1+ count)
	      start (max (1+ (match-beginning 0)) (match-end 0)))))
    count))

(defun org-confluence-inline-comments-storage-marker-occurrence-info (storage comment)
  "Return marker occurrence info for COMMENT in STORAGE, or nil."
  (let ((ref (org-confluence-comments-remote-inline-marker-ref comment))
	(selection (org-confluence-comments-remote-inline-target-text comment))
	chosen)
    (when (and (org-confluence-api--present-string-p ref)
	       (org-confluence-api--present-string-p selection))
      (dolist (marker (org-confluence-inline-comments-inline-marker-ranges storage))
	(when (and (not chosen) (equal ref (plist-get marker :ref)))
	  (setq chosen marker)))
      (when chosen
	(let ((before (org-confluence-inline-comments-storage-visible-text
		       (substring storage 0 (plist-get chosen :body-start))))
	      (all (org-confluence-inline-comments-storage-visible-text storage)))
	  (list :count (org-confluence-inline-comments-count-normalized-occurrences selection all)
		:index (org-confluence-inline-comments-count-normalized-occurrences selection before)))))))

(defun org-confluence-inline-comments-annotate-inline-comment-with-marker-occurrence (storage comment)
  "Return COMMENT annotated with marker occurrence metadata from STORAGE."
  (if-let* ((info (org-confluence-inline-comments-storage-marker-occurrence-info storage comment)))
      (append comment `((org-confluence-marker-occurrence-count . ,(plist-get info :count))
			(org-confluence-marker-occurrence-index . ,(plist-get info :index))))
    comment))

(defun org-confluence-inline-comments-html-escape-text (text)
  "Escape TEXT for Confluence storage text-node matching."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string "&" "&amp;" (or text "") t t)
    t t)
   t t))

(defun org-confluence-inline-comments-context-before (string position)
  "Return storage context before POSITION in STRING."
  (let ((start (max 0 (- position org-confluence-inline-comments-inline-comment-context-window))))
    (substring string start position)))

(defun org-confluence-inline-comments-context-after (string position)
  "Return storage context after POSITION in STRING."
  (substring string position
	     (min (length string)
		  (+ position org-confluence-inline-comments-inline-comment-context-window))))

(defun org-confluence-inline-comments-common-prefix-length (a b)
  "Return length of common prefix between A and B."
  (let ((limit (min (length a) (length b)))
	(index 0))
    (while (and (< index limit)
		(= (aref a index) (aref b index)))
      (setq index (1+ index)))
    index))

(defun org-confluence-inline-comments-common-suffix-length (a b)
  "Return length of common suffix between A and B."
  (let ((limit (min (length a) (length b)))
	(index 0))
    (while (and (< index limit)
		(= (aref a (- (length a) index 1))
		   (aref b (- (length b) index 1))))
      (setq index (1+ index)))
    index))

(defun org-confluence-inline-comments-inline-marker-ranges (storage)
  "Return inline comment marker ranges parsed from STORAGE."
  (let ((stack nil)
	(markers nil))
    (with-temp-buffer
      (insert storage)
      (goto-char (point-min))
      (while (re-search-forward "<\\(/?\\)ac:inline-comment-marker\\b\\([^>]*\\)>" nil t)
	(let* ((tag-start (1- (match-beginning 0)))
	       (tag-end (1- (match-end 0)))
	       (closing (equal (match-string 1) "/"))
	       (attrs (match-string 2))
	       (ref (save-match-data
		      (and (string-match "ac:ref=\"\\([^\"]+\\)\"" attrs)
			   (match-string 1 attrs))))
	       (self-closing (and (not closing)
				  (string-match-p "/[[:space:]]*\\='" attrs))))
	  (cond
	   (self-closing
	    (push (list :ref ref
			:start tag-start
			:start-tag-end tag-end
			:body-start tag-end
			:body-end tag-end
			:end tag-end
			:self-closing t)
		  markers))
	   (closing
	    (when-let* ((opening (pop stack)))
	      (push (append opening
			    (list :body-end tag-start
				  :end tag-end))
		    markers)))
	   (t
	    (push (list :ref ref
			:start tag-start
			:start-tag-end tag-end
			:body-start tag-end
			:self-closing nil)
		  stack))))))
    (sort markers (lambda (a b) (< (plist-get a :start) (plist-get b :start))))))

(defun org-confluence-inline-comments-complex-inline-marker-ref-reasons (storage)
  "Return an alist of complex inline marker refs to reasons in STORAGE."
  (let ((markers (org-confluence-inline-comments-inline-marker-ranges storage))
	(counts nil)
	reasons)
    (dolist (marker markers)
      (when-let* ((ref (plist-get marker :ref)))
	(push ref (alist-get ref counts nil nil #'equal))))
    (dolist (entry counts)
      (when (> (length (cdr entry)) 1)
	(push (cons (car entry) "split inline marker topology") reasons)))
    (dolist (outer markers)
      (dolist (inner markers)
	(when (and (not (eq outer inner))
		   (plist-get outer :ref)
		   (plist-get inner :ref)
		   (not (equal (plist-get outer :ref) (plist-get inner :ref)))
		   (< (plist-get outer :start) (plist-get inner :start))
		   (< (plist-get inner :end) (plist-get outer :end)))
	  (unless (assoc-string (plist-get outer :ref) reasons t)
	    (push (cons (plist-get outer :ref) "nested inline marker topology") reasons))
	  (unless (assoc-string (plist-get inner :ref) reasons t)
	    (push (cons (plist-get inner :ref) "nested inline marker topology") reasons)))))
    reasons))

(defun org-confluence-inline-comments-inline-marker-contexts (old-storage)
  "Return an alist of marker ref to context plist from OLD-STORAGE."
  (let (contexts)
    (with-temp-buffer
      (insert old-storage)
      (goto-char (point-min))
      (while (re-search-forward org-confluence-inline-comments-inline-comment-marker-regexp nil t)
	(let ((ref (match-string-no-properties 1))
	      (start (match-beginning 0))
	      (end (match-end 0)))
	  (push (cons ref (list :before (string-remove-suffix
					 "<" (org-confluence-inline-comments-context-before old-storage start))
				:after (org-confluence-inline-comments-context-after old-storage end)))
		contexts))))
    contexts))

(defun org-confluence-inline-comments-inline-comment-exact-candidates (storage selection)
  "Return exact candidate bounds in STORAGE for COMMENT SELECTION."
  (let* ((escaped (org-confluence-inline-comments-html-escape-text selection))
	 (forms (delete-dups (delq nil (list escaped selection))))
	 candidates)
    (dolist (form forms)
      (when (and (stringp form) (not (string-empty-p form)))
	(let ((start 0))
	  (while (string-match (regexp-quote form) storage start)
	    (push (list :start (match-beginning 0)
			:end (match-end 0))
		  candidates)
	    (setq start (1+ (match-beginning 0)))))))
    (nreverse candidates)))

(defun org-confluence-inline-comments-decode-storage-entity (entity)
  "Return visible text for storage ENTITY, or nil when unknown."
  (cond
   ((equal entity "amp") "&")
   ((equal entity "lt") "<")
   ((equal entity "gt") ">")
   ((equal entity "quot") "\"")
   ((equal entity "apos") "'")
   ((equal entity "nbsp") " ")
   ((string-match-p "\\`#x[[:xdigit:]]+\\'" entity)
    (char-to-string (string-to-number (substring entity 2) 16)))
   ((string-match-p "\\`#[0-9]+\\'" entity)
    (char-to-string (string-to-number (substring entity 1) 10)))))

(defun org-confluence-inline-comments-push-visible-storage-span (text start end text-parts starts ends)
  "Push TEXT spanning START to END into TEXT-PARTS, STARTS, and ENDS.
Return a list `(TEXT-PARTS STARTS ENDS)' with one storage span per visible
character."
  (dolist (char (string-to-list text))
    (push (char-to-string char) text-parts)
    (push start starts)
    (push end ends))
  (list text-parts starts ends))

(defun org-confluence-inline-comments-storage-text-index (storage)
  "Return a plist mapping visible text positions to STORAGE positions.
Tags are skipped, so text split by inline storage tags can still be matched as
one contiguous visible selection.  Common XML/HTML entities are decoded while
retaining their original storage span."
  (let ((index 0)
	(text-parts nil)
	(starts nil)
	(ends nil))
    (while (< index (length storage))
      (cond
       ((= (aref storage index) ?<)
	(let ((tag-end (string-match ">" storage index)))
	  (setq index (if tag-end (1+ tag-end) (length storage)))))
       ((= (aref storage index) ?&)
	(let* ((entity-end (string-match ";" storage index))
	       (entity (and entity-end (substring storage (1+ index) entity-end)))
	       (decoded (and entity (org-confluence-inline-comments-decode-storage-entity entity))))
	  (if decoded
	      (pcase-let ((`(,new-parts ,new-starts ,new-ends)
			   (org-confluence-inline-comments-push-visible-storage-span
			    decoded index (1+ entity-end) text-parts starts ends)))
		(setq text-parts new-parts
		      starts new-starts
		      ends new-ends
		      index (1+ entity-end)))
	    (let ((start index))
	      (push (char-to-string (aref storage index)) text-parts)
	      (push start starts)
	      (setq index (1+ index))
	      (push index ends)))))
       (t
	(let ((start index))
	  (push (char-to-string (aref storage index)) text-parts)
	  (push start starts)
	  (setq index (1+ index))
	  (push index ends)))))
    (list :text (apply #'concat (nreverse text-parts))
	  :starts (vconcat (nreverse starts))
	  :ends (vconcat (nreverse ends)))))

(defun org-confluence-inline-comments-inline-comment-visible-candidates (storage selection)
  "Return tag-insensitive candidate bounds in STORAGE for COMMENT SELECTION."
  (let* ((index (org-confluence-inline-comments-storage-text-index storage))
	 (text (plist-get index :text))
	 (starts (plist-get index :starts))
	 (ends (plist-get index :ends))
	 candidates)
    (when (and (org-confluence-api--present-string-p selection)
	       (> (length text) 0))
      (let ((start 0))
	(while (string-match (regexp-quote selection) text start)
	  (let* ((text-start (match-beginning 0))
		 (text-end (match-end 0))
		 (storage-start (aref starts text-start))
		 (storage-end (aref ends (1- text-end))))
	    (push (list :start storage-start
			:end storage-end)
		  candidates))
	  (setq start (1+ (match-beginning 0))))))
    (nreverse candidates)))

(defun org-confluence-inline-comments-dedupe-inline-comment-candidates (candidates)
  "Return CANDIDATES without duplicate start/end bounds."
  (let (seen result)
    (dolist (candidate candidates (nreverse result))
      (let ((key (cons (plist-get candidate :start) (plist-get candidate :end))))
	(unless (member key seen)
	  (push key seen)
	  (push candidate result))))))

(defun org-confluence-inline-comments-storage-tag-ranges (storage tag)
  "Return ranges for matching XML TAG in STORAGE."
  (let (ranges)
    (with-temp-buffer
      (insert storage)
      (goto-char (point-min))
      (while (re-search-forward
	      (format "<%s\\(?:[[:space:]][^>]*\\)?>\\(?:.\\|\n\\)*?</%s>"
		      (regexp-quote tag) (regexp-quote tag))
	      nil t)
	(push (cons (match-beginning 0) (match-end 0)) ranges)))
    (nreverse ranges)))

(defun org-confluence-inline-comments-storage-cdata-ranges (storage)
  "Return CDATA ranges in STORAGE."
  (let ((start 0)
	ranges)
    (while (string-match "<!\\[CDATA\\[" storage start)
      (let* ((range-start (match-beginning 0))
	     (body-start (match-end 0))
	     (range-end (if (string-match "\\]\\]>" storage body-start)
			    (match-end 0)
			  (length storage))))
	(push (cons range-start range-end) ranges)
	(setq start range-end)))
    (nreverse ranges)))

(defun org-confluence-inline-comments-storage-ri-tag-ranges (storage)
  "Return resource identifier tag ranges in STORAGE."
  (let (ranges)
    (with-temp-buffer
      (insert storage)
      (goto-char (point-min))
      (while (re-search-forward "<ri:[^>]+/?>" nil t)
	(push (cons (match-beginning 0) (match-end 0)) ranges)))
    (nreverse ranges)))

(defun org-confluence-inline-comments-range-overlaps-p (start end range)
  "Return non-nil when START..END overlaps RANGE."
  (and (< start (cdr range))
       (> end (car range))))

(defun org-confluence-inline-comments-range-contained-in-ranges-p (start end ranges)
  "Return non-nil when START..END is contained in one of RANGES."
  (seq-some (lambda (range)
	      (and (<= (car range) start)
		   (<= end (cdr range))))
	    ranges))

(defun org-confluence-inline-comments-storage-text-container-ranges (storage)
  "Return safe text container ranges in STORAGE."
  (let (ranges)
    (dolist (tag '("p" "li" "td" "th" "h1" "h2" "h3" "h4" "h5" "h6"
		   "ac:link-body" "ac:rich-text-body"))
      (setq ranges (append ranges
			   (org-confluence-inline-comments-storage-tag-ranges storage tag))))
    ranges))

(defun org-confluence-inline-comments-candidate-crosses-text-container-p (storage start end)
  "Return non-nil when START..END crosses text container boundaries in STORAGE."
  (let* ((ranges (org-confluence-inline-comments-storage-text-container-ranges storage))
	 (overlapping (seq-filter (lambda (range)
				    (org-confluence-inline-comments-range-overlaps-p start end range))
				  ranges)))
    (and overlapping
	 (not (org-confluence-inline-comments-range-contained-in-ranges-p
	       start end overlapping)))))

(defun org-confluence-inline-comments-unsafe-inline-comment-candidate-p (storage candidate)
  "Return non-nil when CANDIDATE is in unsafe Confluence storage."
  (let* ((start (plist-get candidate :start))
	 (end (plist-get candidate :end))
	 (blocked-ranges (append
			  (org-confluence-inline-comments-storage-tag-ranges storage "ac:parameter")
			  (org-confluence-inline-comments-storage-tag-ranges storage "ac:plain-text-body")
			  (org-confluence-inline-comments-storage-cdata-ranges storage)
			  (org-confluence-inline-comments-storage-ri-tag-ranges storage)))
	 (macro-ranges (org-confluence-inline-comments-storage-tag-ranges storage "ac:structured-macro"))
	 (rich-text-ranges (org-confluence-inline-comments-storage-tag-ranges storage "ac:rich-text-body")))
    (or (org-confluence-inline-comments-candidate-crosses-text-container-p storage start end)
	(seq-some (lambda (range)
		    (org-confluence-inline-comments-range-overlaps-p start end range))
		  blocked-ranges)
	(and (org-confluence-inline-comments-range-contained-in-ranges-p start end macro-ranges)
	     (not (org-confluence-inline-comments-range-contained-in-ranges-p
		   start end rich-text-ranges))))))

(defun org-confluence-inline-comments-safe-inline-comment-candidates (storage candidates)
  "Return CANDIDATES that are safe marker insertion ranges in STORAGE."
  (seq-remove (lambda (candidate)
		(org-confluence-inline-comments-unsafe-inline-comment-candidate-p storage candidate))
	      candidates))

(defun org-confluence-inline-comments-inline-comment-candidates (storage selection)
  "Return candidate bounds in STORAGE for COMMENT SELECTION."
  (org-confluence-inline-comments-safe-inline-comment-candidates
   storage
   (org-confluence-inline-comments-dedupe-inline-comment-candidates
    (append (org-confluence-inline-comments-inline-comment-exact-candidates storage selection)
	    (org-confluence-inline-comments-inline-comment-visible-candidates storage selection)))))

(defun org-confluence-inline-comments-score-inline-comment-candidate (storage candidate context)
  "Return context score for CANDIDATE in STORAGE using old marker CONTEXT."
  (let ((before (plist-get context :before))
	(after (plist-get context :after)))
    (+ (if before
	   (org-confluence-inline-comments-common-suffix-length
	    before (org-confluence-inline-comments-context-before storage (plist-get candidate :start)))
	 0)
       (if after
	   (org-confluence-inline-comments-common-prefix-length
	    after (org-confluence-inline-comments-context-after storage (plist-get candidate :end)))
	 0))))

(defun org-confluence-inline-comments-best-inline-comment-candidate (storage selection context)
  "Return best STORAGE candidate for SELECTION using optional CONTEXT."
  (let ((best nil)
	(best-score -1))
    (dolist (candidate (org-confluence-inline-comments-inline-comment-candidates storage selection))
      (let ((score (if context
		       (org-confluence-inline-comments-score-inline-comment-candidate storage candidate context)
		     0)))
	(when (> score best-score)
	  (setq best candidate
		best-score score))))
    best))

(defun org-confluence-inline-comments-storage-visible-text (storage)
  "Return visible text represented by STORAGE."
  (plist-get (org-confluence-inline-comments-storage-text-index storage) :text))

(defun org-confluence-inline-comments-active-inline-comment-refs (comments)
  "Return active inline marker refs from COMMENTS."
  (let (refs)
    (dolist (comment comments (nreverse refs))
      (let ((status (org-confluence-comments-remote-resolution-status comment))
	    (ref (org-confluence-comments-remote-inline-marker-ref comment)))
	(when (and (not (member status '("resolved" "dangling")))
		   (org-confluence-api--present-string-p ref)
		   (not (member ref refs)))
	  (push ref refs))))))

(defun org-confluence-inline-comments-inline-marker-segment-replacements (new-storage old-storage)
  "Return marker segment replacements preserving OLD-STORAGE topology."
  (let (replacements)
    (dolist (marker (org-confluence-inline-comments-inline-marker-ranges old-storage))
      (let* ((ref (plist-get marker :ref))
	     (body (substring old-storage
			      (plist-get marker :body-start)
			      (plist-get marker :body-end)))
	     (selection (org-confluence-inline-comments-storage-visible-text body))
	     (context (list :before (string-remove-suffix
				     "<" (org-confluence-inline-comments-context-before
					  old-storage (plist-get marker :start)))
			    :after (org-confluence-inline-comments-context-after
				    old-storage (plist-get marker :end)))))
	(when (and (org-confluence-api--present-string-p ref)
		   (org-confluence-api--present-string-p selection))
	  (when-let* ((candidate (org-confluence-inline-comments-best-inline-comment-candidate
				  new-storage selection context)))
	    (push (append candidate (list :ref ref)) replacements)))))
    replacements))

(defun org-confluence-inline-comments-inline-marker-required-segment-counts (old-storage active-refs)
  "Return required non-empty marker segment counts for ACTIVE-REFS in OLD-STORAGE."
  (let (counts)
    (dolist (marker (org-confluence-inline-comments-inline-marker-ranges old-storage) counts)
      (let* ((ref (plist-get marker :ref))
	     (body (substring old-storage
			      (plist-get marker :body-start)
			      (plist-get marker :body-end)))
	     (selection (org-confluence-inline-comments-storage-visible-text body)))
	(when (and (member ref active-refs)
		   (org-confluence-api--present-string-p selection))
	  (setf (alist-get ref counts nil nil #'equal)
		(1+ (or (alist-get ref counts nil nil #'equal) 0))))))))

(defun org-confluence-inline-comments-inline-marker-replacement-counts (replacements)
  "Return marker segment counts by ref for REPLACEMENTS."
  (let (counts)
    (dolist (replacement replacements counts)
      (let ((ref (plist-get replacement :ref)))
	(when ref
	  (setf (alist-get ref counts nil nil #'equal)
		(1+ (or (alist-get ref counts nil nil #'equal) 0))))))))

(defun org-confluence-inline-comments-partial-overlap-inline-replacements-p (replacements)
  "Return non-nil when REPLACEMENTS contain crossing marker ranges."
  (seq-some
   (lambda (a)
     (seq-some
      (lambda (b)
	(and (not (eq a b))
	     (< (plist-get a :start) (plist-get b :start))
	     (< (plist-get b :start) (plist-get a :end))
	     (< (plist-get a :end) (plist-get b :end))))
      replacements))
   replacements))

(defun org-confluence-inline-comments-apply-inline-comment-replacements (storage replacements)
  "Return STORAGE with inline marker REPLACEMENTS inserted."
  (let ((opens nil)
	(closes nil)
	(output nil))
    (dolist (replacement replacements)
      (let ((start (plist-get replacement :start))
	    (end (plist-get replacement :end))
	    (ref (plist-get replacement :ref)))
	(when (and (integerp start) (integerp end) (< start end) ref)
	  (push replacement (alist-get start opens nil nil #'equal))
	  (push replacement (alist-get end closes nil nil #'equal)))))
    (dotimes (index (1+ (length storage)))
      (dolist (_replacement (sort (copy-sequence (alist-get index closes nil nil #'equal))
				  (lambda (a b) (> (plist-get a :start)
						   (plist-get b :start)))))
	(push (format "</ac:inline-comment-marker>") output))
      (dolist (replacement (sort (copy-sequence (alist-get index opens nil nil #'equal))
				 (lambda (a b) (> (plist-get a :end)
						  (plist-get b :end)))))
	(push (format "<ac:inline-comment-marker ac:ref=\"%s\">"
		      (xml-escape-string (plist-get replacement :ref)))
	      output))
      (when (< index (length storage))
	(push (substring storage index (1+ index)) output)))
    (apply #'concat (nreverse output))))

(defun org-confluence-inline-comments-inline-comment-replacements (new-storage old-storage comments)
  "Return marker replacements for COMMENTS from OLD-STORAGE into NEW-STORAGE."
  (let ((contexts (org-confluence-inline-comments-inline-marker-contexts old-storage))
	(seen nil)
	replacements)
    (dolist (comment comments)
      (let ((status (org-confluence-comments-remote-resolution-status comment))
	    (ref (org-confluence-comments-remote-inline-marker-ref comment))
	    (selection (org-confluence-comments-remote-inline-target-text comment)))
	(when (and (not (member status '("resolved" "dangling")))
		   (org-confluence-api--present-string-p ref)
		   (org-confluence-api--present-string-p selection)
		   (not (member ref seen)))
	  (push ref seen)
	  (when-let* ((candidate (org-confluence-inline-comments-best-inline-comment-candidate
				  new-storage selection (cdr (assoc-string ref contexts t)))))
	    (push (append candidate (list :ref ref)) replacements)))))
    (sort replacements (lambda (a b) (> (plist-get a :start) (plist-get b :start))))))

(defun org-confluence-inline-comments-insert-inline-comment-markers (new-storage old-storage comments)
  "Return NEW-STORAGE with Confluence inline markers restored from COMMENTS.
OLD-STORAGE supplies surrounding context and existing marker topology for
duplicate target disambiguation."
  (let* ((active-refs (org-confluence-inline-comments-active-inline-comment-refs comments))
	 (segment-replacements
	  (org-confluence-inline-comments-inline-marker-segment-replacements
	   new-storage old-storage))
	 (required-counts
	  (org-confluence-inline-comments-inline-marker-required-segment-counts
	   old-storage active-refs))
	 (replacement-counts
	  (org-confluence-inline-comments-inline-marker-replacement-counts segment-replacements)))
    (cond
     ((and required-counts
	   (or (org-confluence-inline-comments-partial-overlap-inline-replacements-p
		segment-replacements)
	       (seq-some (lambda (entry)
			   (< (or (alist-get (car entry) replacement-counts nil nil #'equal) 0)
			      (cdr entry)))
			 required-counts)))
      (user-error "Refusing to publish: complex inline comment topology could not be preserved"))
     (segment-replacements
      (org-confluence-inline-comments-apply-inline-comment-replacements
       new-storage segment-replacements))
     (t
      (org-confluence-inline-comments-apply-inline-comment-replacements
       new-storage
       (org-confluence-inline-comments-inline-comment-replacements
	new-storage old-storage comments))))))

(defun org-confluence-inline-comments-preserve-inline-comments (page-id new-storage)
  "Return NEW-STORAGE with active inline comment markers preserved for PAGE-ID."
  (if (or (not org-confluence-publish-preserve-inline-comments)
	  (not (member (format "%s" page-id)
		       org-confluence-publish--pages-needing-inline-marker-preservation)))
      new-storage
    (let* ((page-response (org-confluence-api--get-page page-id "storage"))
	   (old-storage (org-confluence-response-page-body-storage-value page-response))
	   (comments-response (org-confluence-api--list-page-comments
			       page-id "inline-comments" "storage"))
	   (comments (org-confluence-response-comment-results comments-response)))
      (org-confluence-inline-comments-insert-inline-comment-markers
       new-storage old-storage comments))))

(provide 'org-confluence-inline-comments)
;;; org-confluence-inline-comments.el ends here
