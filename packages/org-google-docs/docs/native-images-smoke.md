# Google Docs Native Images Smoke Tests

These manual checks cover standalone Org image push through the local `gdocs` image seams and `org-google-docs` adapter.

## Reload after code changes

Evaluate from the linked Org buffer:

```elisp
(progn
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-api.el")
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-convert.el")
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-diff.el")
  (load-file "/Users/hubertbehaghel/ws/.emacs.d-org-googledoc/packages/org-google-docs/org-google-docs-images.el")
  (load-file "/Users/hubertbehaghel/ws/.emacs.d-org-googledoc/packages/org-google-docs/org-google-docs.el")
  (setq-local gdocs-sync--push-in-progress nil
              gdocs-sync--push-queued nil)
  (message "Reloaded Google Docs native image support"))
```

## Standalone image

Org source:

```org
Before.

[[file:~/Pictures/hubert-escobar-banner.png]]

After.
```

Expected Google Doc:

- body contains an inline image between `Before.` and `After.`;
- body does not contain literal `file:~/Pictures/hubert-escobar-banner.png` text;
- uploaded Drive image is readable by link when `org-google-docs-images-make-uploaded-files-public` is non-nil.

Manual result: passed.

## Captioned standalone image

Org source:

```org
Before.

#+CAPTION: Hubert banner
[[file:~/Pictures/hubert-escobar-banner.png]]

After.
```

Expected Google Doc v1 behavior:

- body contains an inline image between `Before.` and `After.`;
- body contains visible `Hubert banner` text immediately after the image;
- the caption is not styled/native figure metadata yet; it is a visible-text semantic-preservation mapping.

Manual result: passed.

## Important Org syntax caveat

Standalone image links must not be indented. Leading spaces make Org treat the line as indented text, so the image link may not be classified as a standalone image.

Use:

```org
[[file:~/Pictures/hubert-escobar-banner.png]]
```

not:

```org
  [[file:~/Pictures/hubert-escobar-banner.png]]
```

## Pull inline image preservation

Starting from a Google Doc that contains inline images, run pull from the linked Org buffer.

Expected Org source after pull:

- each Google Docs inline image with exposed object metadata becomes an ordinary Org link to its `sourceUri` when available;
- if `sourceUri` is absent but `contentUri` is available, the Org link uses `contentUri`;
- the image is not silently dropped and does not degrade to only an opaque object id unless Google exposes no usable URI;
- captions pushed by the adapter are marked with a neutral `org-image-caption` semantic style name;
- marked caption paragraphs are re-associated with the preceding image and rendered as `#+CAPTION:`;
- visual caption styling is not applied by neutral upstream `gdocs`; users/adapters may style paragraphs identified by the marker;
- image links are separated from surrounding paragraphs with blank lines in pulled Org output, including the three-way merge pull path used after a shadow exists.

Manual result: pending.

## Pipeline trace checklist

Before guessing from live Google Docs behavior, run:

```elisp
M-x org-google-docs-debug-pipeline
```

Inspect:

- `:image-plan`: standalone image entries, absolute paths, captions, and diagnostics;
- `:local-ir`: image elements and captured `:caption` values;
- `:local-requests`: raw request generation before upload;
- `:local-requests-with-placeholder-image-uris`: image/caption request generation using fake non-fetching URIs.

Raw `:local-requests` may omit image and caption requests because image insertion only emits requests after upload enriches image IR with a fetchable `:uri`. Use `:local-requests-with-placeholder-image-uris` to verify request shape without network upload.

For linked buffers, run with a prefix argument:

```elisp
C-u M-x org-google-docs-debug-pipeline
```

This appends remote Docs JSON, remote IR, and diff requests. The trace may contain document content but must not contain OAuth tokens or credential payloads.

## Cache pulled remote images locally

After pulling a Google Doc with inline images, run:

```elisp
M-x org-google-docs-images-cache-remote-images
```

Expected Org source:

- standalone remote image links are downloaded below `assets/google-docs/<document-id>/` relative to the Org file;
- image links are rewritten from `[[https://...]]` to `[[file:assets/google-docs/<document-id>/remote-<hash>.<ext>]]`;
- existing `#+CAPTION:` lines remain intact;
- later pushes use the regular local-image upload path.

If a remote URL is no longer fetchable, the command fails with an actionable fetch error and does not use provider credentials or print tokens.

Manual result: pending.

## Failure modes already encountered

- Missing end boundary in multipart upload: fixed by sending Drive multipart uploads as raw binary bodies.
- Upload callback resumed in the wrong buffer: fixed by resuming image push in the original source buffer.
- Caption present in IR but absent from raw local requests: expected before upload; use placeholder-URI request tracing.
- Caption changes ignored on repeated push: fixed by including image captions in upstream diff keys.
- Caption duplicated on repeated push: fixed by extending the pulled image IR document range to include its marked caption paragraph, so replacements delete the old caption before inserting the new one.
