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
- Export behavior should be testable from tracked specimens and textual assertions before relying on visual inspection.
- Generated PDFs, TeX files, screenshots, and visual diff artifacts belong under runtime output locations, not tracked golden files.
- Document workflows should route runtime outputs through managed paths under `var/`.
