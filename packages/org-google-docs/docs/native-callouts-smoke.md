# Disposable callout and managed-task smoke test

Use a new disposable Org file and a disposable Google Doc. Keep automatic push
turned off. Do not use a real note or publish over a trusted document merely to
test migration. Offline fixtures do not establish live API acceptance.

Canonical callouts use `#+ATTR_CALLOUT`, with type `info`, `note`, `warning`,
`tip` or `important` (`info` is the default):

```org
#+ATTR_CALLOUT: :type warning :title "Heads up 😀"
#+BEGIN_CALLOUT
First *paragraph*.

- [ ] Open task
- [X] Done task
- [-] Partial task

Separate paragraph after the list.
#+END_CALLOUT
```

## Create and inspect

1. Create a source containing two adjacent callouts with identical attributes,
   a Unicode title, separate prose paragraphs, a list followed by prose, and
   ordinary tasks outside callouts. Include `[ ]`, `[X]`, `[-]`, nested tasks,
   bold/italic text, links, inline source and supplementary-plane characters.
2. Publish a new document. Inspect the pipeline trace/requests when surprising.
   Tasks should show `☐`, `☑`, `◩` as editable text, not native checkbox controls.
   Ordinary bullet/number lists should still be native lists.
3. Verify callout title, paragraph boundaries, full-width shading and list
   indentation. Check rich-body emphasis, links and inline-source language.
4. Run `M-x org-google-docs-push-restyle-current` after a style tweak. Verify
   callout chrome and task state remain correct. Unsupported types, nested
   blocks and nested callout list shading should fail before remote mutation.

## Edit in both directions

1. In Docs, replace only `☐` with `☑`, then with `◩`. Leave the following space
   intact. Pull each edit and verify `[X]`, then `[-]`, without body changes.
2. Edit the visible callout title. Pull after changing local callout-label
   configuration; the published prefix, not today's configuration, identifies
   the title. Verify that adjacent identical callouts remain separate.
3. In Org, change task state, nesting, body emphasis and inline-source content.
   Push the linked document. Inspect the visible state and pull it back.
4. Push again without edits: no content/style/annotation changes should appear.
   Repeat after changing a task from an ordinary native bullet and back.
5. Delete a glyph but leave its separator in Docs. Pull should fail before
   overwriting Org. Restore the canonical glyph and space, then retry manually.

## Native checkbox boundary

Native click-to-toggle checklist completion cannot be synchronized through the
current documented API. Legacy-marked native checklists should produce an
explicit error rather than stale/open task state. Unmarked native controls are
not reliably distinguishable from ordinary bullets; do not claim automatic
recognition. Migrate only from trusted authoritative Org, or manually establish
the managed visible-glyph format. Strike-through is not completion evidence.

Record create, push, edit/push, pull and repeated-push outcomes independently;
do not label the feature live-verified until this checklist passes.
