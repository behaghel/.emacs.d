;;; org-google-docs-disposable-coverage.el --- Offline mixed canary plans -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;;; Commentary:
;; Batch-only choreography for synthetic mixed-document acceptance.  Reuse the
;; production footnote planner and SDK conversion, never authentication or HTTP.
;; Each phase writes to a fresh empty directory; snapshots/replies are explicit
;; inputs, not hidden state in a running Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'gdocs-convert)
(require 'gdocs-diff)
(require 'gdocs-sync)
(require 'gdocs-disposable-plan)
(require 'org-google-docs)

(defun org-google-docs-disposable-coverage--read-json (file)
  "Read explicit JSON FILE using the SDK's object representation."
  (unless (and file (file-readable-p file))
    (user-error "This phase requires a readable JSON snapshot or reply"))
  (let ((json-object-type 'alist) (json-array-type 'vector)
        (json-key-type 'symbol) (json-false :json-false))
    (json-read-file file)))

(defun org-google-docs-disposable-coverage--write-json (directory name value)
  "Write new artifact NAME containing VALUE under DIRECTORY."
  (let ((file (expand-file-name name directory)))
    (with-temp-buffer
      (insert (json-encode value) "\n")
      (write-region (point-min) (point-max) file nil 'silent nil 'excl))
    (set-file-modes file #o600)))

(defun org-google-docs-disposable-coverage--values (key tree)
  "Collect values of KEY in decoded JSON TREE, including native segments."
  (cond ((vectorp tree)
         (cl-mapcan (lambda (value)
                      (org-google-docs-disposable-coverage--values key value))
                    (append tree nil)))
        ((consp tree)
         (append (when (eq (car tree) key) (list (cdr tree)))
                 (org-google-docs-disposable-coverage--values key (car tree))
                 (org-google-docs-disposable-coverage--values key (cdr tree))))))

(defun org-google-docs-disposable-coverage--native-evidence (document)
  "Check raw native fields in mixed canary DOCUMENT, not only normalized IR."
  (let* ((dates (org-google-docs-disposable-coverage--values 'dateElement document))
         (notes (org-google-docs-disposable-coverage--values 'footnoteReference document))
         (runs (org-google-docs-disposable-coverage--values 'textRun document))
         targets)
    (dolist (run runs)
      (when-let* ((url (alist-get 'url (alist-get 'link (alist-get 'textStyle run)))))
        (when (string-blank-p (or (alist-get 'content run) ""))
          (user-error "Native canary contains linked whitespace"))
        (cl-pushnew url targets :test #'equal)))
    (unless (= (length dates) 2)
      (user-error "Mixed canary requires exactly two native day-date chips"))
    (unless (and (= (length notes) 1)
                 (alist-get (intern (alist-get 'footnoteId (car notes)))
                            (alist-get 'footnotes document)))
      (user-error "Mixed canary requires one reference with a native note segment"))
    (unless (= (length targets) 7)
      (user-error "Mixed canary requires seven distinct native hyperlink targets"))
    (list :native-dates (length dates) :native-footnotes (length notes)
          :native-link-targets (length targets) :linked-whitespace 0)))

(defun org-google-docs-disposable-coverage--ir (image-uri)
  "Return validated current source IR with explicit test IMAGE-URI enrichment."
  (let ((ir (org-google-docs-footnotes-filter-native-footnote-ir
             (gdocs-convert-org-buffer-to-ir))))
    (dolist (element ir)
      (when (eq (plist-get element :type) 'image)
        (unless (and (stringp image-uri) (string-prefix-p "https://" image-uri))
          (user-error "Mixed canary images require explicit public HTTPS enrichment"))
        (plist-put element :uri image-uri)))
    ir))

(defun org-google-docs-disposable-coverage--inspect (document directory image-uri verify)
  "Plan current source against DOCUMENT in DIRECTORY with IMAGE-URI.
When VERIFY is non-nil, require raw native evidence and both repeats to pass."
  (let* ((evidence (when verify
                     (org-google-docs-disposable-coverage--native-evidence document)))
         (gdocs-convert-footnote-reference-text-function
          (lambda (_object) "⟦native-footnote-probe⟧"))
         (local (org-google-docs-disposable-coverage--ir image-uri))
         (native (gdocs-sync--filter-empty-paragraphs
                  (gdocs-disposable-plan--body (gdocs-convert-docs-json-to-ir document))))
         (requests (gdocs-diff-generate native local))
         (returned (gdocs-convert-ir-to-org native))
         (pulled (with-temp-buffer
                   (insert returned)
                   (org-mode)
                   (org-google-docs-disposable-coverage--ir image-uri)))
         (repeat (gdocs-diff-generate native pulled)))
    (org-google-docs-disposable-coverage--write-json
     directory "pulled-repeat.requests.json" (list (cons 'requests (vconcat repeat))))
    (with-temp-buffer
      (insert returned)
      (write-region (point-min) (point-max)
                    (expand-file-name "returned.org" directory) nil 'silent nil 'excl))
    (list :requests requests :pulled-repeat-requests (length repeat) :evidence evidence)))

(defun org-google-docs-disposable-coverage-plan (action source remote directory
                                                        &optional reply image-uri)
  "Plan synthetic coverage ACTION from SOURCE into fresh empty DIRECTORY.
ACTION is create, native, fill, inspect or verify.  REMOTE supplies a fetched
snapshot for native/inspect/verify.  REPLY supplies the native creation response
for fill.  IMAGE-URI explicitly enriches synthetic images without Drive upload.
The mixed source contains exactly one supported named footnote reference.
Verify checks the mixed fixture's raw native fields and two empty repeat plans.
No phase authenticates, fetches, creates a document, or sends requests."
  (unless noninteractive (user-error "Run mixed canary planning in batch Emacs"))
  (unless (member action '("create" "native" "fill" "inspect" "verify"))
    (user-error "Unknown mixed canary phase"))
  (unless (and (file-directory-p directory)
               (null (directory-files directory nil directory-files-no-dot-files-regexp)))
    (user-error "Use a fresh empty artifact directory for each canary phase"))
  (when org-google-docs-footnotes--push-session
    (user-error "An active footnote push session cannot be replaced by a canary"))
  (let* ((org-mode-hook nil)
         (document (when remote (org-google-docs-disposable-coverage--read-json remote)))
         (response (when reply (org-google-docs-disposable-coverage--read-json reply)))
         (revision (alist-get 'revisionId document))
         references requests extra)
    (with-temp-buffer
      (insert-file-contents source)
      (setq-local buffer-file-name (expand-file-name source)
                  default-directory (file-name-directory (expand-file-name source)))
      (org-mode)
      (org-google-docs--validate-source-profile)
      (let ((plan (org-google-docs-footnotes-plan-buffer)))
        (unless (and (plist-get plan :ready-p)
                     (= (length (plist-get plan :references)) 1))
          (user-error "Mixed canary requires exactly one supported named footnote reference"))
        (setq references (plist-get plan :references))
        (pcase action
          ("create"
           (unwind-protect
               (progn
                 (org-google-docs-footnotes--activate-session plan)
                 (setq requests (gdocs-convert-ir-to-docs-requests
                                 (org-google-docs-disposable-coverage--ir image-uri))))
             (org-google-docs-footnotes--deactivate-session))
           (let ((trace (org-google-docs-debug-pipeline)))
             (unwind-protect
                 (with-current-buffer trace
                   (write-region (point-min) (point-max)
                                 (expand-file-name "pipeline.org" directory)
                                 nil 'silent nil 'excl))
               (kill-buffer trace))))
          ("native"
           (unless (and document (stringp revision) (not (string-empty-p revision)))
             (user-error "Native phase requires a sentinel snapshot with revisionId"))
           (setq requests
                 (org-google-docs-footnotes-sentinel-create-requests
                  (org-google-docs-footnotes--sort-references-for-mutation
                   (org-google-docs-footnotes--locate-sentinels references document)))))
          ("fill"
           (unless response (user-error "Fill phase requires native creation replies"))
           (setq revision (or (alist-get 'requiredRevisionId (alist-get 'writeControl response))
                              (alist-get 'targetRevisionId (alist-get 'writeControl response))
                              revision))
           (unless (and (stringp revision) (not (string-empty-p revision)))
             (user-error "Fill phase requires a reply revision or fresh post-native snapshot"))
           (setq requests
                 (org-google-docs-footnotes-body-insert-requests
                  references (org-google-docs-footnotes--create-footnote-response response))))
          ((or "inspect" "verify")
           (unless document (user-error "Inspection requires a native snapshot"))
           (setq extra (org-google-docs-disposable-coverage--inspect
                        document directory image-uri (equal action "verify"))
                 requests (plist-get extra :requests))))))
    (let ((body (list (cons 'requests (vconcat requests))))
          (summary (append (list :phase action :references (length references)
                                 :requests (length requests))
                           (when extra (list :pulled-repeat-requests
                                             (plist-get extra :pulled-repeat-requests)))
                           (plist-get extra :evidence))))
      (when revision
        (setq body (append body (list (cons 'writeControl
                                            (list (cons 'requiredRevisionId revision)))))))
      (org-google-docs-disposable-coverage--write-json
       directory (concat action ".requests.json") body)
      (org-google-docs-disposable-coverage--write-json directory "summary.json" summary)
      (when (and (equal action "verify")
                 (or requests (> (plist-get extra :pulled-repeat-requests) 0)))
        (user-error "Canary verification failed; inspect the saved nonempty plans"))
      summary)))

(provide 'org-google-docs-disposable-coverage)
;;; org-google-docs-disposable-coverage.el ends here
