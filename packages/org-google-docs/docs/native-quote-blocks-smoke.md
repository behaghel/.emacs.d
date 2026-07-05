# Native quote block smoke checklist

## Status

Automated fixture support is implemented; live smoke is pending.

## Expected behavior

Given Org input:

```org
#+begin_quote
First quoted paragraph.

Second quoted paragraph with *emphasis*.
#+end_quote
```

A Google Docs push should:

- preserve the quote as semantic `quote-block` IR;
- render each paragraph as normal Google Docs text with logical quote line-role styles;
- create `gdocs-org-marker:quote-block:...` named ranges;
- apply authoring-owned visual policy from `modules/org/google-docs-styles.el`;
- pull back to one Org `#+BEGIN_QUOTE` block with paragraph boundaries preserved.

## Current visual policy

The local authoring policy defines:

- `gdocs-quote-block-first`
- `gdocs-quote-block-line`
- `gdocs-quote-block-last`
- `gdocs-quote-block-single`

Current style intent:

- light gray paragraph background;
- left indentation via both `indentStart` and `indentFirstLine` set to the same value;
- symmetric spacing around the whole block;
- no literal blank paragraphs for spacing;
- no italic/foreground-color quote styling, because those are text-run styles and would pull back as Org emphasis or other content-level formatting.

## Live checklist

- [ ] Create a new Google Doc from an Org buffer containing single- and multi-paragraph quote blocks.
- [ ] Confirm quote blocks render indented with a light gray paragraph background according to local style policy.
- [ ] Push again without content changes and confirm no duplication.
- [ ] Run `M-x org-google-docs-push-restyle-current` after changing quote style policy and confirm restyling applies.
- [ ] Pull back and confirm Org quote block boundaries and paragraph text survive.

## Notes

Google Docs does not expose a stable arbitrary custom quote-block semantic object through the public Docs API. The v1 approach therefore preserves semantics with named-range markers and renders visual styling through logical paragraph styles.

Web/API notes:

- Paragraph indentation is applied with `updateParagraphStyle` fields `indentStart` and `indentFirstLine`.
- The field mask should include `indentStart,indentFirstLine` (field names only).
- Quote visual styling intentionally avoids text-run `italic` and `foregroundColor`, because pulled Google Docs text styles are interpreted as Org emphasis and can create merge/conflict noise.
