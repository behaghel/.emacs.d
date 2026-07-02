;;; org-confluence-response.el --- Confluence REST response helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared helpers for parsing Confluence REST response payloads.

;;; Code:

(require 'json)

(defun org-confluence-response-body (response)
  "Return JSON body string from REST RESPONSE."
  (cond
   ((stringp response) response)
   ((plist-member response :body) (plist-get response :body))
   (t (user-error "Confluence REST response did not include a body"))))

(defun org-confluence-response-json-alist (json)
  "Parse JSON string JSON into alists and lists."
  (json-parse-string json :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun org-confluence-response-comment-results (response)
  "Return comment results from REST RESPONSE."
  (let* ((json (org-confluence-response-json-alist
		(org-confluence-response-body response)))
	 (results (or (alist-get 'results json)
		      (alist-get 'values json))))
    (unless (listp results)
      (user-error "Confluence comment response did not include results"))
    results))

(defun org-confluence-response-page-body-storage-value (response)
  "Return page body storage value from REST RESPONSE."
  (let* ((json (org-confluence-response-json-alist
		(org-confluence-response-body response)))
	 (storage (alist-get 'storage (alist-get 'body json)))
	 (value (and (listp storage) (alist-get 'value storage))))
    (unless (stringp value)
      (user-error "Confluence page response did not include body.storage.value"))
    value))

(defun org-confluence-response-user-results (response)
  "Return user results from REST RESPONSE."
  (let* ((json (org-confluence-response-json-alist
		(org-confluence-response-body response)))
	 (results (alist-get 'results json)))
    (unless (listp results)
      (user-error "Confluence user response did not include results"))
    results))


(provide 'org-confluence-response)
;;; org-confluence-response.el ends here
