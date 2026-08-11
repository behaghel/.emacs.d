;;; ob-copilot.el --- Org Babel support for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Org Babel language backend for AI-maintained Org regions.  The `copilot'
;; language returns raw Org by default so `:exports results' can publish the
;; generated result while keeping the prompt reviewable in source.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ob)
(require 'org)
(require 'org-copilot-debug)
(require 'org-copilot-gptel)
(require 'subr-x)

(defgroup org-copilot-babel nil
  "Org Babel integration for Org Copilot."
  :group 'org-copilot)

(defcustom org-copilot-babel-generate-function #'org-copilot-babel-gptel-generate
  "Function used to generate Copilot Babel output.
The function receives one request plist and must return a JSON string with an
`output' field.  Adapter packages may install a backend-specific function."
  :type '(choice (const :tag "No generator" nil)
		 function)
  :group 'org-copilot-babel)

(defcustom org-copilot-babel-gptel-timeout 120
  "Seconds to wait for synchronous Copilot Babel gptel generation."
  :type 'natnum
  :group 'org-copilot-babel)

(defconst org-copilot-babel-schema-version "ob-copilot-v1"
  "Schema version used for Copilot Babel request/result contracts.")

(defvar org-babel-default-header-args:copilot
  '((:results . "value raw replace")
    (:exports . "results")
    (:eval . "never-export")
    (:context . "document")
    (:output . "org"))
  "Default header arguments for Org Babel `copilot' blocks.")

(defun org-copilot-babel--param (params key fallback)
  "Return PARAMS value at KEY, or FALLBACK when absent/blank."
  (let ((value (cdr (assq key params))))
    (if (and (stringp value) (not (string-empty-p (string-trim value))))
	value
      fallback)))

(defun org-copilot-babel--src-block-head ()
  "Return beginning position of the current Copilot source block."
  (or (org-babel-where-is-src-block-head)
      (save-excursion
	(beginning-of-line)
	(when (looking-at-p "^[ \t]*#\\+begin_src[ \t]+copilot\\_>")
	  (point)))
      (save-excursion
	(when (re-search-backward "^[ \t]*#\\+begin_src[ \t]+copilot\\_>" nil t)
	  (point)))
      (save-excursion
	(when (re-search-forward "^[ \t]*#\\+begin_src[ \t]+copilot\\_>" nil t)
	  (line-beginning-position)))))

(defun org-copilot-babel--src-block-end ()
  "Return end position of the current source block."
  (save-excursion
    (goto-char (or (org-copilot-babel--src-block-head)
		   (user-error "Not in a Copilot source block")))
    (unless (re-search-forward "^[ \t]*#\\+end_src" nil t)
      (user-error "Copilot source block has no #+end_src"))
    (forward-line 1)
    (point)))

(defun org-copilot-babel--block-and-result-bounds ()
  "Return cons bounds covering current source block and following result."
  (save-excursion
    (let* ((start (progn
		    (goto-char (or (org-copilot-babel--src-block-head)
				   (user-error "Not in a Copilot source block")))
		    (line-beginning-position)))
	   (end (org-copilot-babel--src-block-end)))
      (goto-char end)
      (skip-chars-forward " \t\n")
      (when (looking-at-p "^[ \t]*#\\+RESULTS:")
	(forward-line 1)
	(while (and (not (eobp))
		    (not (looking-at-p "[ \t]*$")))
	  (forward-line 1))
	(setq end (point)))
      (cons start end))))

(defun org-copilot-babel--context-bounds (context-kind)
  "Return source context bounds for CONTEXT-KIND."
  (pcase context-kind
    ("subtree"
     (save-excursion
       (if (org-before-first-heading-p)
	   (cons (point-min) (point-max))
	 (org-back-to-heading t)
	 (let ((start (point)))
	   (org-end-of-subtree t t)
	   (cons start (point))))))
    (_ (cons (point-min) (point-max)))))

(defun org-copilot-babel--context (context-kind)
  "Return CONTEXT-KIND text excluding current block and its previous result."
  (let* ((context-bounds (org-copilot-babel--context-bounds context-kind))
	 (excluded (org-copilot-babel--block-and-result-bounds))
	 (context-start (car context-bounds))
	 (context-end (cdr context-bounds))
	 (exclude-start (max context-start (car excluded)))
	 (exclude-end (min context-end (cdr excluded))))
    (concat
     (buffer-substring-no-properties context-start exclude-start)
     (when (< exclude-end context-end)
       (buffer-substring-no-properties exclude-end context-end)))))

(defun org-copilot-babel--request (body params)
  "Return a Copilot Babel request plist for BODY and PARAMS."
  (let ((context-kind (org-copilot-babel--param params :context "document"))
	(output (org-copilot-babel--param params :output "org")))
    (list :request-kind 'babel
	  :schema-version org-copilot-babel-schema-version
	  :prompt body
	  :context-kind context-kind
	  :context (org-copilot-babel--context context-kind)
	  :output output
	  :file (cdr (assq :file params))
	  :params params)))

(defun org-copilot-babel--fingerprint (body params)
  "Return freshness fingerprint for BODY and relevant PARAMS."
  (secure-hash
   'sha256
   (prin1-to-string
    (list :schema-version org-copilot-babel-schema-version
	  :prompt body
	  :context (org-copilot-babel--param params :context "document")
	  :output (org-copilot-babel--param params :output "org")
	  :file (cdr (assq :file params))))))

(defun org-copilot-babel--source-output-language (output-kind)
  "Return nested source language when OUTPUT-KIND is src:LANG, or nil."
  (when (and (stringp output-kind)
	     (string-prefix-p "src:" output-kind))
    (substring output-kind 4)))

(defun org-copilot-babel--language-available-p (language)
  "Return non-nil when Babel LANGUAGE appears available."
  (let ((symbol (intern language)))
    (or (fboundp (intern (format "org-babel-execute:%s" language)))
	(cdr (assq symbol org-babel-load-languages)))))

(defun org-copilot-babel--validate-output-kind (output-kind)
  "Validate OUTPUT-KIND before model generation."
  (when-let* ((language (org-copilot-babel--source-output-language output-kind)))
    (unless (org-copilot-babel--language-available-p language)
      (user-error "Org Babel language is unavailable: %s" language))))

(defun org-copilot-babel--nested-source-headers (params)
  "Return nested source block headers derived from PARAMS."
  (string-join
   (delq nil
	 (list (when-let* ((file (cdr (assq :file params))))
		 (format ":file %s" file))
	       ":exports results"))
   " "))

(defun org-copilot-babel--wrap-output (output output-kind params)
  "Return OUTPUT wrapped according to OUTPUT-KIND and PARAMS."
  (if-let* ((language (org-copilot-babel--source-output-language output-kind)))
      (format "#+begin_src %s %s\n%s\n#+end_src"
	      language
	      (org-copilot-babel--nested-source-headers params)
	      output)
    output))

(defun org-copilot-babel--format-result (output fingerprint output-kind)
  "Return OUTPUT prefixed with FINGERPRINT and OUTPUT-KIND metadata."
  (string-join
   (list (format "# copilot-fingerprint: sha256:%s" fingerprint)
	 (format "# copilot-output: %s" output-kind)
	 output)
   "\n"))

(defun org-copilot-babel--result-region ()
  "Return cons bounds for the current Copilot result, or nil."
  (save-excursion
    (goto-char (org-copilot-babel--src-block-end))
    (skip-chars-forward " \t\n")
    (when (looking-at-p "^[ \t]*#\\+RESULTS:")
      (let ((start (line-beginning-position)))
	(forward-line 1)
	(while (and (not (eobp))
		    (not (looking-at-p "[ \t]*$")))
	  (forward-line 1))
	(cons start (point))))))

(defun org-copilot-babel--delete-existing-result ()
  "Delete existing result for the current Copilot block, if present."
  (when-let* ((region (org-copilot-babel--result-region)))
    (delete-region (car region) (cdr region))
    (when (looking-at-p "[ \t]*$")
      (delete-region (point) (min (point-max) (1+ (point)))))))

(defun org-copilot-babel--result-metadata ()
  "Return Copilot result metadata for the current source block, or nil."
  (save-excursion
    (when-let* ((region (org-copilot-babel--result-region)))
      (goto-char (car region))
      (forward-line 1)
      (let ((end (cdr region))
	    fingerprint output)
	(while (and (< (point) end) (not (and fingerprint output)))
	  (cond
	   ((looking-at "[ \t]*# copilot-fingerprint: sha256:\\(.+\\)$")
	    (setq fingerprint (match-string 1)))
	   ((looking-at "[ \t]*# copilot-output: \\(.+\\)$")
	    (setq output (match-string 1))))
	  (forward-line 1))
	(list :fingerprint fingerprint :output output)))))

;;;###autoload
(defun org-copilot-babel-result-state-at-point ()
  "Return `fresh', `stale', or `missing' for Copilot block at point."
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
	 (body (nth 1 info))
	 (params (nth 2 info))
	 (metadata (org-copilot-babel--result-metadata))
	 (expected (org-copilot-babel--fingerprint body params)))
    (cond
     ((not (plist-get metadata :fingerprint)) 'missing)
     ((equal (plist-get metadata :fingerprint) expected) 'fresh)
     (t 'stale))))

(defun org-copilot-babel--parse-response (response)
  "Parse Copilot Babel RESPONSE and return output string."
  (condition-case err
      (let* ((parsed (json-parse-string response
					:object-type 'plist
					:array-type 'list
					:null-object nil
					:false-object nil))
	     (output (plist-get parsed :output)))
	(unless (and (stringp output) (not (string-empty-p output)))
	  (user-error "Copilot Babel response has no output field"))
	output)
    (json-error
     (signal 'user-error
	     (list (format "Malformed Copilot Babel response: %s"
			   (error-message-string err)))))))

(defun org-copilot-babel--state-label (state)
  "Return human label for Copilot result STATE."
  (pcase state
    ('missing "missing")
    ('stale "stale")
    (_ (symbol-name state))))

;;;###autoload
(defun org-copilot-babel-export-preflight (_backend)
  "Check Copilot Babel blocks before Org export.
Fresh blocks are left alone.  Missing or stale blocks ask whether to evaluate;
after any export-time generation, ask whether export should continue with
unreviewed generated content."
  (let ((generated 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+copilot\\_>" nil t)
	(beginning-of-line)
	(let ((block-start (point))
	      (state (org-copilot-babel-result-state-at-point)))
	  (unless (eq state 'fresh)
	    (if (y-or-n-p
		 (format "Evaluate %s Copilot block before export? "
			 (org-copilot-babel--state-label state)))
		(progn
		  (org-babel-execute-src-block)
		  (cl-incf generated))
	      (user-error "Aborted export: %s Copilot block was not evaluated"
			  (org-copilot-babel--state-label state))))
	  (goto-char (save-excursion
		       (goto-char block-start)
		       (org-copilot-babel--src-block-end))))))
    (when (and (> generated 0)
	       (not (y-or-n-p
		     (format "%d Copilot block(s) generated unreviewed content; continue export? "
			     generated))))
      (user-error "Aborted export: Copilot generated unreviewed content"))))

(defun org-copilot-babel--export-as-preflight
    (_backend &optional _subtreep _visible-only _body-only _ext-plist)
  "Run Copilot Babel preflight in the original buffer before export copying."
  (when (derived-mode-p 'org-mode)
    (org-copilot-babel-export-preflight nil)))

(advice-add 'org-export-as :before
	    #'org-copilot-babel--export-as-preflight)

(defun org-copilot-babel-gptel-prompt (request)
  "Return a strict JSON prompt for Copilot Babel REQUEST."
  (format (string-join
	   '("You are maintaining a generated Org Babel result."
	     "Return strict JSON only, with this shape: {\"output\":\"...\"}."
	     "Do not include Markdown fences or prose outside JSON."
	     "The output field must contain the complete replacement result."
	     "For output values like src:LANG, return only the inner LANG code; Emacs wraps the source block."
	     ""
	     "Output kind: %s"
	     "Context kind: %s"
	     ""
	     "Prompt:"
	     "%s"
	     ""
	     "Context:"
	     "%s")
	   "\n")
	  (plist-get request :output)
	  (plist-get request :context-kind)
	  (plist-get request :prompt)
	  (plist-get request :context)))

(defun org-copilot-babel-gptel-generate (request)
  "Generate Copilot Babel output for REQUEST using gptel synchronously."
  (unless (fboundp 'gptel-request)
    (user-error "Org Copilot Babel requires gptel"))
  (let ((done nil)
	(response-text nil)
	(error-message nil)
	(chunks nil)
	(start (float-time))
	(prompt (org-copilot-babel-gptel-prompt request)))
    (let* ((gptel-backend (or org-copilot-gptel-backend
			      (and (boundp 'gptel-backend) gptel-backend)))
	   (gptel-model (or org-copilot-gptel-model
			    (and (boundp 'gptel-model) gptel-model)))
	   (stream (org-copilot-gptel--stream-required-p gptel-backend)))
      (gptel-request
       prompt
       :stream stream
       :callback
       (lambda (response info)
	 (cond
	  ((and stream (stringp response))
	   (push response chunks))
	  ((and stream (eq response t))
	   (setq response-text (apply #'concat (nreverse chunks)))
	   (setq done t))
	  ((stringp response)
	   (setq response-text response)
	   (setq done t))
	  (t
	   (org-copilot-debug-record
	    "Babel gptel generation failed"
	    :request request
	    :prompt prompt
	    :status (plist-get info :status)
	    :response response
	    :info info)
	   (let* ((error (plist-get info :error))
		  (message (plist-get error :message))
		  (code (plist-get error :code))
		  (detail (string-join (delq nil (list message code)) " / ")))
	     (setq error-message
		   (format "gptel status=%S%s"
			   (or (plist-get info :status) (plist-get info :http-status))
			   (if (string-empty-p detail) "" (format " — %s" detail)))))
	   (setq done t))))))
    (while (and (not done)
		(< (- (float-time) start) org-copilot-babel-gptel-timeout))
      (accept-process-output nil 0.05))
    (unless done
      (user-error "Timed out waiting for Copilot Babel response"))
    (when error-message
      (user-error "Copilot Babel generation failed: %s" error-message))
    response-text))

;;;###autoload
(defun org-babel-execute:copilot (body params)
  "Execute a Copilot Org Babel source block with BODY and PARAMS."
  (unless org-copilot-babel-generate-function
    (user-error "No Org Copilot Babel generator configured"))
  (let* ((output-kind (org-copilot-babel--param params :output "org"))
	 (request (progn
		    (org-copilot-babel--validate-output-kind output-kind)
		    (org-copilot-babel--request body params)))
	 (response (funcall org-copilot-babel-generate-function request))
	 (output (org-copilot-babel--parse-response response))
	 (fingerprint (org-copilot-babel--fingerprint body params))
	 (wrapped-output (org-copilot-babel--wrap-output
			  output output-kind params))
	 (result (org-copilot-babel--format-result
		  wrapped-output fingerprint output-kind)))
    (org-copilot-debug-record
     "Babel block evaluated"
     :request request
     :raw-response response
     :output output)
    (org-copilot-babel--delete-existing-result)
    result))

(provide 'ob-copilot)
;;; ob-copilot.el ends here
