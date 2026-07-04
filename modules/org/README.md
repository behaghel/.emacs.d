---
domain: authoring
status: draft
last-reviewed: 2026-06-10
---

# Documents

## Ubiquitous Language

- **Document production**: Exporting or compiling authored source into PDF, LaTeX, or other document artifacts.
- **Class family**: A related set of LaTeX/PDF export classes and variants.
- **Specimen**: Tracked Org input used to verify export behavior.
- **Export contract**: The stable behavior promised by an export class, compiler wrapper, or document workflow.

## Invariants

- Author-facing semantics must stay separate from class-specific visual styling.
- The backend-neutral typographic semantics contract lives in [`typographic-semantics.md`](typographic-semantics.md).
- The semantic inventory/audit helper lives in `typographic-semantics.el` and provides `hub/org-typographic-semantics-audit-buffer` and `hub/org-typographic-semantics-audit-file`.
- The tracked specimen for the contract lives in [`specimens/typographic-semantics.org`](specimens/typographic-semantics.org).
- Export behavior should be testable from tracked specimens and textual assertions before relying on visual inspection.
- Generated PDFs, TeX files, screenshots, and visual diff artifacts belong under runtime output locations, not tracked golden files.
- Document workflows should route runtime outputs through managed paths under `var/`.
- In `hub-article`, ordinary Org footnotes export as margin sidenotes; `HUB_NOTE_KIND: footnote` preserves bottom footnotes. Review comments are not represented as Org footnotes and should live in sidecar `*.comments.org` files.
