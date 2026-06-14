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
- Optional footnote definition properties use repo-owned `HUB_NOTE_*` keys; `HUB_NOTE_KIND: footnote` forces a traditional bottom footnote while ordinary footnotes remain sidenotes.
- Review comments are not marginalia footnote kinds; local comments live in colocated sidecar Org files named like `article.comments.org`.
- Region comments require an active region, keep source Org clean, and render in the context panel when their stored offsets still match the selected text.
- Comment overlays are enabled for Org buffers by `hub/org-comment-overlays-mode`, while `]c` and `[c` navigate to next and previous sidecar comments and open the context panel.
- The interactive context panel is explicitly opened or toggled with a buffer-local mode; it must not auto-open globally.
- Opening the context panel docks visually filled prose toward the panel and renders compact icon/status-chip cards.
- Inline authoring shortcuts are `<fn` for the default note/sidenote and `<ft` for a forced traditional footnote.

## Integration Notes

This domain consumes shared prose/key helpers and conforms to the interactive command surface.  Export-specific specs may live beside the export code or in the existing spec tree when they describe broader authoring contracts.
