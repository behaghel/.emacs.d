---
domain: authoring/documents
status: draft
---

# Documents

## Ubiquitous Language

- **Document production**: Exporting or compiling authored source into PDF, LaTeX, or other document artifacts.
- **Class family**: A related set of LaTeX/PDF export classes and variants.
- **Specimen**: Tracked Org input used to verify export behavior.
- **Export contract**: The stable behavior promised by an export class, compiler wrapper, or document workflow.

## Invariants

- Author-facing semantics must stay separate from class-specific visual styling.
- Google Docs publishing uses the selected tracked theme for presentation. Tasks
  align with native list glyph, body and continuation columns at the same level.
- Prose paragraphs inside typographic blocks and separate lists retain visible
  separation. List-item contents remain compact; literal code/example blank
  lines retain their exact content rather than being converted into spacing.
- Section breaks retain semantic identity across publishing and pull. The selected
  theme supplies a visible separator; arbitrary paragraph borders are not breaks,
  and following prose does not inherit the separator border.
- Centered prose retains authored paragraph boundaries and distinct block
  boundaries, including empty blocks. Centering does not flatten rich text or
  leak into following prose. Body typography and paragraph gaps are presentation.
- Verse retains authored hard lines, stanza gaps, indentation and rich text.
  Verse blocks have visible separation from preceding prose. Consecutive verse
  lines use compact paragraph spacing; the selected body
  typography is presentation and does not replace authored emphasis or links.
- Callouts have outer spacing and a continuous themed background across the
  printable text width, including their list and continuation paragraphs.
  Labels and prose have a small theme-defined inner-left inset; the inset does
  not move established list columns or shift the shaded surface outside the
  printable area.
- The backend-neutral typographic semantics contract lives in [`typographic-semantics.md`](typographic-semantics.md).
- The semantic inventory/audit helper lives in `typographic-semantics.el` and provides `hub/org-typographic-semantics-audit-buffer` and `hub/org-typographic-semantics-audit-file`.
- The tracked specimen for the contract lives in [`specimens/typographic-semantics.org`](specimens/typographic-semantics.org).
- Export behavior should be testable from tracked specimens and textual assertions before relying on visual inspection.
- Generated PDFs, TeX files, screenshots, and visual diff artifacts belong under runtime output locations, not tracked golden files.
- Static class assets, including logo PDFs, are tracked under `etc/latex/assets/`.
  Exports stage these assets from the checkout, without relying on files in HOME.
- Document workflows should route runtime outputs through managed paths under `var/`.
- In `hub-article`, ordinary Org footnotes export as margin sidenotes; `HUB_NOTE_KIND: footnote` preserves bottom footnotes. Review comments are not represented as Org footnotes and should live in sidecar `*.comments.org` files.
- EPUB export is specified in [`export-epub.md`](export-epub.md) and implemented by `org/export-epub` via `M-x hub/org-epub-export-to-epub`.
