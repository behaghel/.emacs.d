# Disposable list-fidelity smoke checklist

Use a new disposable Org file and Google Doc, with automatic push disabled.
Do not overwrite a real note. Offline fixtures are not live API acceptance.

```org
1. Parent one
   1. Child one
   2. Child two

   Parent continuation with *bold*, [[https://example.org][a link]] and 😀.
2. Parent two

- [X] Task

  Task continuation with src_python{  x + 1  }.

- *Term 😀* :: Definition with [[https://example.org][a link]].
```

- [ ] Publish a new document. Inspect that parent two visibly has number 2,
      children have the expected nested numbering, and the continuation has no
      extra bullet. Compare task glyph, body, wrapped-line and continuation
      columns with native list items at the same level.
- [ ] Pull back. Verify that the parent continuation belongs to parent one,
      not child two, and all emphasis, links, Unicode and literal spaces survive.
- [ ] Edit continuation text in Docs without splitting its paragraph. Pull and
      verify the edit remains in the same item. Change the task's visible glyph
      and verify only its completion changes.
- [ ] Change continuation text, emphasis and inline source in Org; push the
      existing linked document. Inspect visible numbering and paragraph ownership.
- [ ] Push again unchanged, then pull again: no content or annotation churn.
- [ ] Exercise the same rich continuation inside a callout. Verify full-width
      shading through native bullets, task items and continuations, without
      notches or white seams. Check outer margins, visible prose gaps and exactly
      one task glyph per item. List contents remain compact; literal code/example
      blank lines remain exact.
- [ ] Remove a continuation's owner or split its managed annotation across
      paragraphs in Docs. Pull must fail before replacing local content. Restore
      the boundaries or republish trusted Org; do not guess ownership.
- [ ] Confirm unsupported source blocks inside list items fail before mutation.
- [ ] Verify description terms, the reversible `::` delimiter and rich
      definitions remain visible; pull and push again without duplicated terms.
- [ ] Inside a callout, change an open task to partial in Org, push, then pull.
      The visible glyph must become `◩` and return as `[-]`, not open state.
- [ ] Change list nesting and body emphasis in the same Org edit, then push.
      Both native structure and emphasis must update. Inspect `bullet.nestingLevel`
      in a fresh API fetch, not only the imported Org or list-state annotations.
      Repeat with a shallower edit and a preceding Unicode paragraph edit.
- [ ] Replace a top-level list item with a top-level table, then push. Inspect
      the pipeline requests: no bullet cleanup should target the new table.

Inspect `M-x org-google-docs-debug-pipeline` and its request trace when surprising.
Record create, push, edit/push, pull and repeat results separately. Native list
identity/numbering after bullet removal requires this real-API check; a synthetic
returned-document fixture cannot establish it. No native checkbox-state inference
is part of this workflow: tasks use the accepted editable glyph representation.

## Source-isolated checks

`test/org-google-docs-disposable-preflight.el` runs only in a separate batch
Emacs. Set `GDOCS_PATH` to the reviewed SDK checkout, `STRAIGHT_BUILD_PATH` to
existing dependencies, and `GDOCS_DISPOSABLE_ARTIFACTS` to a private temporary
directory. `GDOCS_DISPOSABLE_SOURCE` selects synthetic Org; the SDK's disposable
fixture is the default. The driver loads tracked publishing styles and the
source/default theme; `GDOCS_DISPOSABLE_THEME` selects an explicit registered
theme. Neutral SDK defaults alone do not establish personal visual acceptance.
The driver plans requests and records the local debug pipeline without
authentication or HTTP. Supplying `GDOCS_DISPOSABLE_REMOTE`
selects inspection of an already fetched synthetic JSON snapshot. Explicit
`GDOCS_DISPOSABLE_ACTION` values are `create`, `inspect`, `remote-edit`,
`local-edit`, `repeat-edit`, `style-repair`, and `export`; exports require a new destination in
`GDOCS_DISPOSABLE_EXPORT` and remove document identity and revision.

Keep each sent request plan and fetched snapshot immutable. Use a new fetch
filename and compare two independent fetches, then parse the pulled Org and
verify that it also plans no changes. The separate disposable transport helper
reuses only the installed authenticated API, requires a recorded completed test
operation, fixes the account to `personal`, and disables automatic retries.
The optional PDF export helper reuses the installed account resolver and HTTP
client, keeps authentication opaque, and writes a new private PDF for rendering.
It does not mutate the document. Never print private errors, headers or account
records. `style-repair` is narrowly limited to final callout list chrome on an
identical semantic source with revision control; it never reapplies named
styles, which can clear authored emphasis in Docs.
Never load the test SDK into the running production Emacs or copy credentials.
Document creation and mutation still require explicit live authorization.

Check raw native properties alongside zero-diff results: parents share the
intended native list ID at level zero; children have the requested levels;
continuations have no bullet; structural tabs are absent from paragraph text.
For mixed native presets, include the group's definition indentation: raw
`nestingLevel` zero is relative to its own list and can have a nested base offset.
Visual numbering, indentation and shading require browser inspection. API
mutations do not establish manual browser editing or native click-to-toggle
checkbox behavior.
