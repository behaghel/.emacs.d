# Iterative Test Plan

## Context

This is the execution plan required by the spec-driven-TDD workflow. Each iteration must be a user-exercisable end-to-end slice from Org input to LaTeX output to PDF verification.

## Vertical-Slice Rule

- Backend-only progress does not count as a completed iteration.
- Fidelity-only progress does not count as a completed iteration.
- A slice is complete only when a human can exercise it through the actual export path and inspect the resulting PDF.

## Planned Future Test Files

- `test/org-latex-pdf-export-test.el`
  - focused export and `.tex` assertions
- `test/org-latex-pdf-fidelity-test.el`
  - higher-cost PDF and snapshot-oriented checks once the first class is stable enough

Tracked specimen inputs are expected under `test/fixtures/org-export/`.
Generated outputs belong under `var/`.

## Iteration Table

| Iteration | Slice goal | User interaction path | Tests to add first | Expected red signal | Minimal green target | Feedback checkpoint |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | Narrow English `veriff` slice | Export minimal Org specimens with `#+LATEX_CLASS: veriff` (both explicit and default `refresh-overdrive` behavior) to `.tex` then PDF | class selection assertion, variant default assertion, locale selection assertion, PDF exists assertion, extracted-text sanity assertion | export does not recognize the class, variant default fails, locale path is missing, or PDF cannot be produced | English flagship specimens export end to end and produce reviewable PDFs in `var/` | confirm the class and variant foundation feels real before adding richer semantics |
| 2 | Full semantic coverage for `veriff` | Export the full English semantic specimen and inspect both `.tex` structure and rendered PDF | assertions for standfirst, eyebrow, links, epigraph, lists, checklists, native footnotes, quote, callout, metrics, tables, figures/images, pillars, code, footer note | one or more semantic constructs disappear, collapse to plain prose, or compile incorrectly | the flagship class supports the full approved semantic set in English | confirm the class can carry real article content, not just a smoke page |
| 3 | Variants within the same professional direction | Export approval specimens and any variant-bearing specimens through the same class family path | assertions that variant selection changes only the intended preamble or style surfaces; snapshot checks for canonical pages; `dark-campaign` surface check | variant wiring leaks into semantics, or visual changes break approved layout cues | additional class variants (`dark-campaign`) are exercised without a semantic-model rewrite | confirm the class family can branch without breaking the authoring contract |
| 4 | Remaining classes and French coverage | Add personal and remaining professional classes plus French specimen runs through the same architecture | class-selection assertions for new classes, French locale assertions, text extraction checks for accented content, class-specific smoke PDFs | new classes require semantic rewrites, or French cannot be added through the existing locale layer | the architecture expands to remaining classes and French without backtracking on the first-class design | confirm the architecture generalizes beyond the flagship slice |

## Per-Iteration Outputs

Each iteration should leave behind:

- tracked specimen inputs in `test/fixtures/org-export/`
- targeted tests under `test/*-test.el`
- generated `.tex`, `.pdf`, extracted text, and snapshot artifacts under `var/`
- updated spec wording if implementation evidence changes the contract

## Test Value Review Guidance

- Keep cheap structural assertions close to the exporter contract.
- Promote expensive PDF or visual checks only where they catch real regressions that `.tex` checks miss.
- If a test repeatedly changes due to layout churn without catching meaningful regressions, refactor or prune it.

## Cross-Links

- Family inventory: [`../class-family-matrix.md`](../class-family-matrix.md)
- Shared semantics: [`../semantic-layer.md`](../semantic-layer.md)
- Locale separation: [`../locale-layer.md`](../locale-layer.md)
- First real class: [`../classes/veriff.md`](../classes/veriff.md)
- Personal article boundary: [`../classes/article.md`](../classes/article.md)
- Specimen corpus: [`../validation/specimens.md`](../validation/specimens.md)
- Fidelity ladder: [`../validation/pdf-fidelity.md`](../validation/pdf-fidelity.md)

## Non-Goals

- This file does not commit to exact implementation commands before the code exists.
- This file does not require French to be green in iteration 1.
- This file does not let work skip from the narrow slice directly to all-class rollout.

## Acceptance Signals

- The rollout order matches the approved handoff sequence exactly.
- Every iteration is end to end and user-exercisable.
- The plan identifies what should fail first and what counts as the smallest acceptable green state.
