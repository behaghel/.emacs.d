;;; org-google-docs-disposable-coverage-test.el --- Offline canary lifecycle -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;;; Commentary:
;; Exercise the tracked mixed-document driver with real conversion and native
;; fixtures.  No accounts, authentication, HTTP, or running Emacs are required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(defconst hub/disposable-coverage-test--build
  (expand-file-name (or (getenv "STRAIGHT_BUILD_PATH") "straight/build"))
  "Existing dependencies for isolated canary tests.")
(defconst hub/disposable-coverage-test--sdk
  (expand-file-name (or (getenv "GDOCS_PATH")
                        (expand-file-name "gdocs" hub/disposable-coverage-test--build)))
  "Explicit SDK source or managed build.")

(add-to-list 'load-path
             (expand-file-name (or (getenv "CONTEXT_PANELS_PATH")
                                   (expand-file-name "context-panels" hub/disposable-coverage-test--build))))
(dolist (name '("org" "compat" "cond-let" "llama" "transient" "dash" "s" "plz"
                "magit-section"))
  (add-to-list 'load-path (expand-file-name name hub/disposable-coverage-test--build)))
(dolist (directory (list hub/disposable-coverage-test--sdk
                         (expand-file-name "test" hub/disposable-coverage-test--sdk)
                         (expand-file-name "modules")
                         (expand-file-name "packages/org-google-docs")
                         (expand-file-name "packages/org-google-docs/test")
                         (expand-file-name "packages/org-comments")
                         (expand-file-name "packages/ox-sync")))
  (add-to-list 'load-path directory))

(defun hub/disposable-coverage-test--load ()
  "Load the canary when the explicit SDK is available."
  (unless (locate-library "gdocs-convert")
    (ert-skip "gdocs-convert is unavailable"))
  (require 'gdocs-convert)
  (require 'gdocs-diff)
  (require 'org-google-docs)
  (require 'org/google-docs-styles)
  ;; An absent implementation must fail at the tested call, not skip the test.
  (require 'org-google-docs-disposable-coverage nil t))

(defun hub/disposable-coverage-test--fixture (name)
  "Return SDK fixture NAME beside the authoritative SDK source."
  (expand-file-name
   (concat "test/fixtures/" name)
   (file-name-directory (file-truename (locate-library "gdocs-convert.el")))))

(defmacro hub/disposable-coverage-test--isolated (&rest body)
  "Run BODY with fresh source and artifact paths and network tripwires."
  (declare (indent 0))
  `(let* ((root (make-temp-file "gdocs-canary-test-" t))
          (source (expand-file-name "source.org" root))
          (artifacts (expand-file-name "artifacts" root))
          (org-mode-hook nil))
     (make-directory artifacts)
     (unwind-protect
         (cl-letf (((symbol-function 'gdocs-api--request)
                    (lambda (&rest _) (ert-fail "Unexpected HTTP")))
                   ((symbol-function 'gdocs-auth-get-access-token)
                    (lambda (&rest _) (ert-fail "Unexpected authentication"))))
           ,@body)
       (delete-directory root t))))

(ert-deftest hub/disposable-coverage-plans-native-footnote-phases ()
  "Create, native-reference and fill plans work without session-local helpers."
  (hub/disposable-coverage-test--load)
  (hub/disposable-coverage-test--isolated
   (with-temp-file source
     (insert "Body 😀[fn:one].\n\n* Footnotes\n\n[fn:one] *Rich* note.\n"))
   (let* ((create (org-google-docs-disposable-coverage-plan
                   "create" source nil artifacts))
          (json-object-type 'alist) (json-array-type 'vector)
          (sentinel (expand-file-name "sentinel.json" root))
          (reply (expand-file-name "reply.json" root)))
     (should (= (plist-get create :references) 1))
     (should (> (plist-get create :requests) 0))
     (should (file-exists-p (expand-file-name "create.requests.json" artifacts)))
     (should (file-exists-p (expand-file-name "pipeline.org" artifacts)))
     (with-temp-file sentinel
       (insert "{\"revisionId\":\"synthetic-revision\",\"body\":{\"content\":[{\"paragraph\":{\"elements\":[{\"startIndex\":1,\"textRun\":{\"content\":\"Body 😀⟦GDOCS_FN:1:one⟧.\\n\"}}]}}]}}"))
     (with-temp-file reply
       (insert "{\"replies\":[{}, {\"createFootnote\":{\"footnoteId\":\"synthetic-note\"}}],\"writeControl\":{\"requiredRevisionId\":\"filled-revision\"}}"))
     (let ((native (expand-file-name "native" root))
           (fill (expand-file-name "fill" root)))
       (make-directory native) (make-directory fill)
       (should (= (plist-get (org-google-docs-disposable-coverage-plan
                              "native" source sentinel native) :requests) 3))
       (let* ((body (json-read-file (expand-file-name "native.requests.json" native)))
              (range (alist-get 'range (alist-get 'deleteContentRange
                                                  (aref (alist-get 'requests body) 0)))))
         (should (= (alist-get 'startIndex range) 8))
         (should (alist-get 'createNamedRange (aref (alist-get 'requests body) 2)))
         (should (equal (alist-get 'requiredRevisionId (alist-get 'writeControl body))
                        "synthetic-revision")))
       (should (> (plist-get (org-google-docs-disposable-coverage-plan
                              "fill" source nil fill reply) :requests) 0))
       (let* ((body (json-read-file (expand-file-name "fill.requests.json" fill)))
              (insert (alist-get 'insertText (aref (alist-get 'requests body) 0))))
         (should (equal (alist-get 'segmentId (alist-get 'location insert)) "synthetic-note"))
         (should (equal (alist-get 'requiredRevisionId (alist-get 'writeControl body))
                        "filled-revision")))))))

(ert-deftest hub/disposable-coverage-mixed-native-and-pulled-repeats ()
  "The mixed native fixture passes independent and freshly pulled repeat plans."
  (hub/disposable-coverage-test--load)
  (hub/disposable-coverage-test--isolated
   (copy-file (hub/disposable-coverage-test--fixture "disposable-coverage-polished.org") source)
   (let* ((gdocs-style-definitions
           (append (hub/org-google-docs-style-definitions 'veriff-working) gdocs-style-definitions))
          (summary (org-google-docs-disposable-coverage-plan
                    "verify" source
                    (hub/disposable-coverage-test--fixture "live-coverage-polished.json")
                    artifacts nil "https://www.gstatic.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png")))
     (should (= (plist-get summary :requests) 0))
     (should (= (plist-get summary :pulled-repeat-requests) 0))
     (should (= (plist-get summary :native-dates) 2))
     (should (= (plist-get summary :native-footnotes) 1))
     (should (= (plist-get summary :native-link-targets) 7))
     (should (file-exists-p (expand-file-name "returned.org" artifacts))))))

(ert-deftest hub/disposable-coverage-refuses-artifact-reuse ()
  "An existing artifact directory is never overwritten by another phase."
  (hub/disposable-coverage-test--load)
  (hub/disposable-coverage-test--isolated
   (with-temp-file source (insert "Body.\n"))
   (with-temp-file (expand-file-name "evidence.txt" artifacts) (insert "keep"))
   (should-error (org-google-docs-disposable-coverage-plan "create" source nil artifacts)
                 :type 'user-error)))

(ert-deftest hub/disposable-coverage-rejects-source-before-writing-artifacts ()
  "Unsupported source fails preflight without emitting a mutation plan."
  (hub/disposable-coverage-test--load)
  (hub/disposable-coverage-test--isolated
   (with-temp-file source (insert "#+AUTHOR: Unsupported\nBody.\n"))
   (should-error (org-google-docs-disposable-coverage-plan "create" source nil artifacts)
                 :type 'user-error)
   (should-not (directory-files artifacts nil directory-files-no-dot-files-regexp))))

(ert-deftest hub/disposable-coverage-verify-refuses-a-nonempty-plan ()
  "Verification cannot report success for an independently changed source."
  (hub/disposable-coverage-test--load)
  (hub/disposable-coverage-test--isolated
   (with-temp-file source
     (insert-file-contents (hub/disposable-coverage-test--fixture "disposable-coverage-polished.org"))
     (goto-char (point-min)) (search-forward "updated 🧭") (replace-match "changed 🧭" t t))
   (let ((gdocs-style-definitions
          (append (hub/org-google-docs-style-definitions 'veriff-working) gdocs-style-definitions)))
     (should-error
      (org-google-docs-disposable-coverage-plan
       "verify" source (hub/disposable-coverage-test--fixture "live-coverage-polished.json")
       artifacts nil "https://www.gstatic.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png")
      :type 'user-error))))

(ert-deftest hub/disposable-coverage-requires-revision-for-mutation-plans ()
  "Native and fill plans refuse to emit unguarded remote mutations."
  (hub/disposable-coverage-test--load)
  (dolist (action '("native" "fill"))
    (hub/disposable-coverage-test--isolated
     (with-temp-file source
       (insert "Body[fn:one].\n\n* Footnotes\n\n[fn:one] Note.\n"))
     (let ((remote (expand-file-name "remote.json" root))
           (reply (expand-file-name "reply.json" root)))
       (with-temp-file remote
         (insert "{\"body\":{\"content\":[{\"paragraph\":{\"elements\":[{\"startIndex\":1,\"textRun\":{\"content\":\"Body⟦GDOCS_FN:1:one⟧.\\n\"}}]}}]}}"))
       (with-temp-file reply
         (insert "{\"replies\":[{\"createFootnote\":{\"footnoteId\":\"synthetic-note\"}}]}"))
       (should-error (org-google-docs-disposable-coverage-plan
                      action source remote artifacts reply) :type 'user-error)
       (should-not (directory-files artifacts nil directory-files-no-dot-files-regexp))))))

(ert-deftest hub/disposable-coverage-native-evidence-is-independent-of-normalized-diff ()
  "An empty normalized plan cannot hide a missing native date object."
  (hub/disposable-coverage-test--load)
  (hub/disposable-coverage-test--isolated
   (copy-file (hub/disposable-coverage-test--fixture "disposable-coverage-polished.org") source)
   (let* ((json-object-type 'alist) (json-array-type 'vector)
          (document (json-read-file (hub/disposable-coverage-test--fixture "live-coverage-polished.json")))
          (remote (expand-file-name "broken-native.json" root)))
     (cl-labels ((remove-dates (tree)
                   (cond ((vectorp tree) (vconcat (mapcar #'remove-dates tree)))
                         ((consp tree)
                          (if (eq (car tree) 'dateElement)
                              (cons 'textRun '((content . "")))
                            (cons (remove-dates (car tree)) (remove-dates (cdr tree)))))
                         (t tree))))
       (with-temp-file remote (insert (json-encode (remove-dates document)))))
     (cl-letf (((symbol-function 'gdocs-diff-generate) (lambda (&rest _) nil)))
       (should-error (org-google-docs-disposable-coverage-plan
                      "verify" source remote artifacts) :type 'user-error)))))

(ert-deftest hub/disposable-coverage-cli-needs-no-session-local-code ()
  "The documented batch entry point verifies a native fixture in a fresh Emacs."
  (hub/disposable-coverage-test--load)
  (hub/disposable-coverage-test--isolated
   (let ((process-environment (copy-sequence process-environment)))
     (setenv "GDOCS_PATH" hub/disposable-coverage-test--sdk)
     (setenv "STRAIGHT_BUILD_PATH" hub/disposable-coverage-test--build)
     (setenv "GDOCS_DISPOSABLE_ACTION" "coverage-verify")
     (setenv "GDOCS_DISPOSABLE_SOURCE"
             (hub/disposable-coverage-test--fixture "disposable-coverage-polished.org"))
     (setenv "GDOCS_DISPOSABLE_REMOTE"
             (hub/disposable-coverage-test--fixture "live-coverage-polished.json"))
     (setenv "GDOCS_DISPOSABLE_REPLY" nil)
     (setenv "GDOCS_DISPOSABLE_ARTIFACTS" artifacts)
     (setenv "GDOCS_DISPOSABLE_THEME" "veriff-working")
     (setenv "GDOCS_DISPOSABLE_IMAGE_URI"
             "https://www.gstatic.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png")
     (with-temp-buffer
       (should (= 0 (call-process
                     (expand-file-name invocation-name invocation-directory)
                     nil t nil "--batch" "-Q" "-l"
                     "packages/org-google-docs/test/org-google-docs-disposable-preflight.el")))
       (should (string-match-p ":pulled-repeat-requests 0" (buffer-string)))))))

(provide 'org-google-docs-disposable-coverage-test)
;;; org-google-docs-disposable-coverage-test.el ends here
