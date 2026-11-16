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
```

- [ ] Publish a new document. Inspect that parent two visibly has number 2,
      children have numbers 1 and 2, and the continuation has no extra bullet.
- [ ] Pull back. Verify that the parent continuation belongs to parent one,
      not child two, and all emphasis, links, Unicode and literal spaces survive.
- [ ] Edit continuation text in Docs without splitting its paragraph. Pull and
      verify the edit remains in the same item. Change the task's visible glyph
      and verify only its completion changes.
- [ ] Change continuation text, emphasis and inline source in Org; push the
      existing linked document. Inspect visible numbering and paragraph ownership.
- [ ] Push again unchanged, then pull again: no content or annotation churn.
- [ ] Exercise the same rich continuation inside a callout. Verify full-width
      shading and exactly one task glyph for the item.
- [ ] Remove a continuation's owner or split its managed annotation across
      paragraphs in Docs. Pull must fail before replacing local content. Restore
      the boundaries or republish trusted Org; do not guess ownership.
- [ ] Confirm unsupported source blocks inside list items fail before mutation.

Inspect `M-x org-google-docs-debug-pipeline` and its request trace when surprising.
Record create, push, edit/push, pull and repeat results separately. Native list
identity/numbering after bullet removal requires this real-API check; a synthetic
returned-document fixture cannot establish it. No native checkbox-state inference
is part of this workflow: tasks use the accepted editable glyph representation.
