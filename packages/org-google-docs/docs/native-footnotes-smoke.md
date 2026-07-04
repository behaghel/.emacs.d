# Google Docs Native Footnotes Smoke Tests

These manual checks cover Org named-footnote push through the local `gdocs` footnote seams and `org-google-docs` adapter.

## Reload after code changes

Evaluate from the linked Org buffer:

```elisp
(progn
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-convert.el")
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-diff.el")
  (load-file "/Users/hubertbehaghel/ws/.emacs.d-org-googledoc/packages/org-google-docs/org-google-docs-footnotes.el")
  (load-file "/Users/hubertbehaghel/ws/.emacs.d-org-googledoc/packages/org-google-docs/org-google-docs.el")
  (setq-local gdocs-sync--push-in-progress nil
              gdocs-sync--push-queued nil)
  (message "Reloaded Google Docs native footnote support"))
```

## Single named footnote

Org source:

```org
Text[fn:one].

* Footnotes

[fn:one] Body.
```

Expected Google Doc:

- body contains `Text¹.`;
- body does not contain a visible `Footnotes` section;
- native footnote area contains `Body.`;
- document JSON contains a `footnoteReference` with a matching top-level `footnotes` entry.

## Re-push after cleanup

Starting from a partial document where the Org footnotes section was removed but the native marker is missing, push the same source again.

Expected Google Doc:

- native marker is recreated;
- native footnote body is recreated;
- body remains free of the Org-only footnotes section.

## Multiple footnotes with visible paragraph edits

Org source:

```org
Text[fn:one] and more text[fn:two].

* Footnotes

[fn:one] First body.
[fn:two] Second body.
```

Expected Google Doc:

- body contains two native footnote markers in the expected positions;
- footnote 1 body is `First body.`;
- footnote 2 body is `Second body.`;
- body does not contain a visible `Footnotes` section.

## Repeated reference degradation

Org source:

```org
Again[fn:one] and later[fn:one].

* Footnotes

[fn:one] Shared body.
```

Expected Google Doc v1 behavior:

- two separate native Google Docs footnotes are created;
- both bodies contain `Shared body.`;
- this is an accepted v1 degradation because Google Docs native footnotes do not model Org's single definition / multiple reference relationship.

## French conventional section

Org source:

```org
Texte[fn:un].

* Notes de bas de page

[fn:un] Corps.
```

Expected Google Doc:

- body contains native marker after `Texte`;
- body does not contain a visible `Notes de bas de page` section;
- native footnote body contains `Corps.`.

## Known UI caveat

Google Docs footnote markers created through the API may not look or behave like clickable links in all UI states. Treat the API JSON as authoritative: a correct native footnote has both a paragraph `footnoteReference` and a matching top-level `footnotes` entry.

Diagnostic snippet:

```elisp
(require 'gdocs-api)

(gdocs-api-get-document
 (org-google-docs-comments-document-id)
 (lambda (json)
   (with-current-buffer (get-buffer-create "*gdocs-json-smoke*")
     (erase-buffer)
     (pp json (current-buffer))
     (goto-char (point-min))
     (pop-to-buffer (current-buffer))))
 (org-google-docs-comments-account))
```
