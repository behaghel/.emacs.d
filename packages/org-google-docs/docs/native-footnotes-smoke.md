# Google Docs Native Footnotes Smoke Tests

These manual checks cover Org named-footnote push through the local `gdocs` footnote seams and `org-google-docs` adapter.

## Test setup

Use matching reviewed SDK and adapter versions, with automatic push disabled.
Run offline fixtures first, then use a disposable Org file and a newly created
Google Doc. Do not reset push queues or reload unrelated modules during an active
operation. Inspect unexpected behavior with `M-x org-google-docs-debug-pipeline`;
a prefix argument includes the linked remote JSON, IR and proposed requests.
Never include credentials in diagnostic output.

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

## Rich note create, edit, push and pull

Use a single-paragraph definition:

```org
Text[fn:authored-name].

* Footnotes

[fn:authored-name] 😀 *bold* and /italic/ with [[https://example.com][a link]].
```

- Create a new Doc. Verify a native reference and native footnote, with emphasis
  and a clickable link, not literal Org markup or a visible definitions section.
- Push unchanged. The pipeline must not plan mutations for the unchanged note.
- Change only the footnote text, emphasis or link target and push again. The
  existing footnote segment changes; reference count, segment ID and authored
  label remain stable. Every mutation must target that segment, not body
  indices. The host paragraph and native reference are not recreated.
- Edit the note in Docs and pull. The Org definition retains emphasis, link label
  and URL, and the reference still uses `authored-name`. An immediate push is a
  no-op for that note.
- Keep source point/scroll unchanged unless the command intentionally replaces
  local content. Check the pipeline trace if marker count, ranges or diffs differ.

Multiple paragraphs, native dates, nested footnotes, status labels and embedded
non-text objects are outside this mapping. Unsupported local or remote content
must block before remote mutation or local replacement, not flatten silently.

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

## Pull native footnotes

Starting from a Google Doc that contains native footnotes, run pull from the linked Org buffer.

Expected Org source after pull:

- paragraph text contains ordinary Org references such as `[fn:1]`;
- footnote definitions are collected once under a conventional `* Footnotes` section;
- definitions are ordinary Org footnotes such as `[fn:1] Body.`;
- native Google Docs footnotes are not imported as styled inline notes or comments.

## Known UI caveat

A paragraph `footnoteReference` and matching top-level `footnotes` entry prove
native structure, not browser usability or appearance. Inspect the native marker
and note in the browser as well as the exported PDF; do not infer click behavior
from JSON alone.

Use the linked remote section of `org-google-docs-debug-pipeline` to inspect the
reference, native segment and proposed segment-local requests together. Isolated
source checks use the same metadata-title and structural-empty-paragraph filters
as `gdocs-sync--push-incremental`; otherwise structural placeholders can produce
spurious body differences before the footnote definitions.
