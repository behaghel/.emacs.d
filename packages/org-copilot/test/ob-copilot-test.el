;;; ob-copilot-test.el --- Babel tests for Org Copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the `copilot' Org Babel language.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ob-copilot)
(require 'org)
(require 'ob)

(defvar ob-copilot-test--calls 0
  "Call counter for Copilot Babel tests.")

(ert-deftest ob-copilot-evaluates-json-output-as-raw-org-result ()
  "A Copilot block evaluates through the generator into raw Org results."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"Generated summary.\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot\nWrite a summary.\n#+end_src\n")
      (goto-char (point-min))
      (org-babel-execute-src-block)
      (goto-char (point-min))
      (should (search-forward "#+RESULTS:" nil t))
      (should (search-forward "Generated summary." nil t))
      (should-not (search-forward "#+begin_example" nil t)))))

(ert-deftest ob-copilot-request-uses-default-headers-and-document-context ()
  "Copilot Babel requests include defaults and document context."
  (let ((org-confirm-babel-evaluate nil)
	captured-request)
    (let ((org-copilot-babel-generate-function
	   (lambda (request)
	     (setq captured-request request)
	     "{\"output\":\"New result.\"}")))
      (with-temp-buffer
	(org-mode)
	(insert "Intro context.\n\n"
		"#+begin_src copilot\nPrompt body.\n#+end_src\n"
		"#+RESULTS:\nOld result.\n\n"
		"Outro context.\n")
	(goto-char (point-min))
	(search-forward "#+begin_src copilot")
	(org-babel-execute-src-block)
	(should (equal (plist-get captured-request :prompt) "Prompt body."))
	(should (equal (plist-get captured-request :context-kind) "document"))
	(should (equal (plist-get captured-request :output) "org"))
	(should (string-match-p "Intro context" (plist-get captured-request :context)))
	(should (string-match-p "Outro context" (plist-get captured-request :context)))
	(should-not (string-match-p "Prompt body" (plist-get captured-request :context)))
	(should-not (string-match-p "Old result" (plist-get captured-request :context)))))))

(ert-deftest ob-copilot-request-supports-subtree-context ()
  "Copilot Babel requests can limit context to the current subtree."
  (let ((org-confirm-babel-evaluate nil)
	captured-request)
    (let ((org-copilot-babel-generate-function
	   (lambda (request)
	     (setq captured-request request)
	     "{\"output\":\"Subtree summary.\"}")))
      (with-temp-buffer
	(org-mode)
	(insert "* One\nLocal context.\n"
		"#+begin_src copilot :context subtree\nSummarize this subtree.\n#+end_src\n"
		"* Two\nOther context.\n")
	(goto-char (point-min))
	(search-forward "#+begin_src copilot")
	(org-babel-execute-src-block)
	(should (equal (plist-get captured-request :context-kind) "subtree"))
	(should (string-match-p "Local context" (plist-get captured-request :context)))
	(should-not (string-match-p "Other context" (plist-get captured-request :context)))))))

(ert-deftest ob-copilot-regeneration-replaces-previous-result ()
  "Re-evaluating a Copilot block replaces rather than prepends results."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"Second result.\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot\nPrompt.\n#+end_src\n\n"
	      "#+RESULTS:\n"
	      "# copilot-fingerprint: sha256:old\n"
	      "# copilot-output: org\n"
	      "First result.\n")
      (goto-char (point-min))
      (org-babel-execute-src-block)
      (goto-char (point-min))
      (should (search-forward "Second result." nil t))
      (should-not (search-forward "First result." nil t))
      (goto-char (point-min))
      (should (= 1 (how-many "# copilot-fingerprint:"))))))

(ert-deftest ob-copilot-evaluation-inserts-fingerprint-metadata ()
  "Copilot Babel results include hidden freshness metadata."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"Generated summary.\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot :context subtree\nWrite a summary.\n#+end_src\n")
      (goto-char (point-min))
      (org-babel-execute-src-block)
      (goto-char (point-min))
      (should (search-forward "#+RESULTS:" nil t))
      (should (looking-at-p "\n# copilot-fingerprint: sha256:"))
      (should (search-forward "# copilot-output: org" nil t))
      (should (search-forward "Generated summary." nil t)))))

(ert-deftest ob-copilot-fingerprint-detects-fresh-and-stale-results ()
  "Copilot Babel freshness compares result metadata to current prompt headers."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"Generated summary.\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot :output org\nWrite a summary.\n#+end_src\n")
      (goto-char (point-min))
      (org-babel-execute-src-block)
      (goto-char (point-min))
      (should (eq (org-copilot-babel-result-state-at-point) 'fresh))
      (search-forward "Write a summary.")
      (replace-match "Write a better summary.")
      (goto-char (point-min))
      (should (eq (org-copilot-babel-result-state-at-point) 'stale)))))

(ert-deftest ob-copilot-wraps-source-output-with-file-header ()
  "Copilot Babel wraps src:LANG output in a nested source block."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"(+ 1 2)\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot :output src:emacs-lisp :file result.txt\n"
	      "Generate code.\n"
	      "#+end_src\n")
      (goto-char (point-min))
      (org-babel-execute-src-block)
      (goto-char (point-min))
      (should (search-forward "# copilot-output: src:emacs-lisp" nil t))
      (should (search-forward "#+begin_src emacs-lisp :file result.txt :exports results" nil t))
      (should (search-forward "(+ 1 2)" nil t))
      (should (search-forward "#+end_src" nil t)))))

(ert-deftest ob-copilot-src-output-errors-before-model-for-unavailable-language ()
  "Copilot Babel rejects unavailable nested source languages before generation."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request)
	   (cl-incf ob-copilot-test--calls)
	   "{\"output\":\"code\"}")))
    (setq ob-copilot-test--calls 0)
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot :output src:not-a-real-babel-language\n"
	      "Generate code.\n"
	      "#+end_src\n")
      (goto-char (point-min))
      (should-error (org-babel-execute-src-block) :type 'user-error)
      (should (= ob-copilot-test--calls 0)))))

(ert-deftest ob-copilot-export-preflight-skips-fresh-results ()
  "Export preflight does not evaluate fresh Copilot blocks."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request)
	   (cl-incf ob-copilot-test--calls)
	   "{\"output\":\"Generated summary.\"}")))
    (setq ob-copilot-test--calls 0)
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot\nWrite a summary.\n#+end_src\n")
      (goto-char (point-min))
      (org-babel-execute-src-block)
      (setq ob-copilot-test--calls 0)
      (org-copilot-babel-export-preflight nil)
      (should (= ob-copilot-test--calls 0)))))

(ert-deftest ob-copilot-export-preflight-aborts-when-evaluation-declined ()
  "Export preflight aborts when a missing Copilot result is not evaluated."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"Generated summary.\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot\nWrite a summary.\n#+end_src\n")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
	(should-error (org-copilot-babel-export-preflight nil)
		      :type 'user-error)))))

(ert-deftest ob-copilot-export-preflight-evaluates-and-prompts-for-review ()
  "Export preflight evaluates missing results and asks whether to continue."
  (let ((org-confirm-babel-evaluate nil)
	(prompts nil)
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"Generated summary.\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot\nWrite a summary.\n#+end_src\n")
      (cl-letf (((symbol-function 'y-or-n-p)
		 (lambda (prompt)
		   (push prompt prompts)
		   t)))
	(org-copilot-babel-export-preflight nil))
      (goto-char (point-min))
      (should (search-forward "Generated summary." nil t))
      (should (= (length prompts) 2))
      (should (string-match-p "Evaluate missing" (cadr prompts)))
      (should (string-match-p "unreviewed" (car prompts))))))

(ert-deftest ob-copilot-export-preflight-aborts-after-unreviewed-generation ()
  "Export preflight aborts when generated content is not approved for export."
  (let ((org-confirm-babel-evaluate nil)
	(answers '(t nil))
	(org-copilot-babel-generate-function
	 (lambda (_request) "{\"output\":\"Generated summary.\"}")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot\nWrite a summary.\n#+end_src\n")
      (cl-letf (((symbol-function 'y-or-n-p)
		 (lambda (_prompt) (pop answers))))
	(should-error (org-copilot-babel-export-preflight nil)
		      :type 'user-error)))))

(ert-deftest ob-copilot-gptel-generator-builds-json-request ()
  "The gptel generator requests JSON output for Copilot Babel blocks."
  (let ((org-copilot-babel-gptel-timeout 1)
	(org-copilot-gptel-model 'test-model)
	(org-copilot-gptel-backend 'test-backend)
	captured-prompt captured-args)
    (cl-letf (((symbol-function 'gptel-request)
	       (lambda (prompt &rest args)
		 (setq captured-prompt prompt
		       captured-args args)
		 (funcall (plist-get args :callback)
			  "{\"output\":\"Generated.\"}"
			  (list :status 'success)))))
      (should (equal (org-copilot-babel-gptel-generate
		      (list :prompt "Write summary."
			    :context "Document body."
			    :context-kind "document"
			    :output "org"))
		     "{\"output\":\"Generated.\"}"))
      (should (string-match-p "JSON" captured-prompt))
      (should (string-match-p "Write summary" captured-prompt))
      (should (string-match-p "Document body" captured-prompt))
      (should (eq (plist-get captured-args :stream) nil)))))

(ert-deftest ob-copilot-gptel-generator-errors-without-gptel ()
  "The gptel generator fails clearly when gptel is unavailable."
  (let ((old-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
	       (lambda (symbol)
		 (and (not (eq symbol 'gptel-request))
		      (funcall old-fboundp symbol)))))
      (should-error (org-copilot-babel-gptel-generate
		     (list :prompt "Write." :context "" :output "org"))
		    :type 'user-error))))

(ert-deftest ob-copilot-malformed-response-errors ()
  "Malformed Copilot Babel responses fail closed."
  (let ((org-confirm-babel-evaluate nil)
	(org-copilot-babel-generate-function (lambda (_request) "not json")))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src copilot\nWrite.\n#+end_src\n")
      (goto-char (point-min))
      (should-error (org-babel-execute-src-block)))))

(provide 'ob-copilot-test)
;;; ob-copilot-test.el ends here
