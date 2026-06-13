---
domain: authoring
status: draft
last-reviewed: 2026-06-08
---

# Knowledge and Writing

## Ubiquitous Language

- **Semantic layer**: Author-facing Org contract describing meaning independently from output styling.
- **Class family**: A related set of LaTeX/PDF export classes and variants.
- **Specimen**: Tracked Org input used to verify export behavior.
- **Authoring shortcut**: Interactive helper that inserts or transforms writing markup.
- **Publishing workflow**: Export or synchronization path from Org content to an external medium.

## Invariants

- Author-facing semantics must stay separate from class-specific visual styling.
- Export behavior should be testable from tracked specimens and textual assertions before relying on visual inspection.
- Writing helpers should not make batch loads depend on interactive-only packages unless guarded.
- Machine-specific paths belong in private overrides or defcustoms, not hard-coded shared behavior.
- Generated PDFs, TeX files, screenshots, and visual diff artifacts belong under runtime output locations, not tracked golden files.
- Marginalia authoring uses native Org footnotes as the canonical source; the panel is a read-only projection and must jump back to footnote definitions for edits.
- Marginalia stacking preserves source order and may push later notes downward when anchors are close.

## Marginalia Contract

- Ordinary Org footnotes in article-oriented authoring buffers default to `sidenote` marginalia.
- Optional footnote definition properties use repo-owned `HUB_NOTE_*` keys, including `HUB_NOTE_KIND`, `HUB_NOTE_STATUS`, `HUB_NOTE_SOURCE`, and `HUB_NOTE_REMOTE_ID`.
- Supported local kinds are `sidenote`, `footnote`, `comment`, and `remote-comment`; Confluence sync is reserved for a later publishing slice.
- The interactive marginalia panel is explicitly opened or toggled with a buffer-local mode; it must not auto-open globally.

## Integration Notes

This domain consumes shared prose/key helpers and conforms to the interactive command surface.  Export-specific specs may live beside the export code or in the existing spec tree when they describe broader authoring contracts.
