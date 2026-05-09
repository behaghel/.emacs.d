# Specifications

This tree holds behavior-first specifications for work that should be implemented through vertical slices and verified with tests.

## Scope

The active subsystem in this tree is the Org -> LaTeX -> PDF export-class system described in the handoff context for the Veriff document work.

This spec set exists to answer four questions before implementation starts:

1. What semantic contract authors write against in Org.
2. How class styling, locale behavior, and validation are separated.
3. What the first shipped slice is.
4. How the system expands from that slice without a structural rewrite.

## Repo-Grounded Constraints

- There was no existing `spec/` tree before this work.
- `modules/interactive/org/core.el` currently enables the LaTeX backend but does not define a custom class system.
- Future reusable export logic should likely live under `lisp/`, with thin integration under `modules/interactive/org/`.
- Tests in this repo are flat under `test/*-test.el`, with shared helpers in `test/test-helpers.el`.
- Generated `.tex`, `.pdf`, extracted text, snapshots, and visual diff artifacts belong under `var/`, not as tracked golden files.
- This spec set is intentionally under `spec/`, not `docs/spec/`.

## Authoritative Decisions Captured Here

- Professional family: `veriff` (variants: `refresh-overdrive`, `dark-campaign`, `gallery-white`), `pro-whitepaper`, `pro-announcement`.
- Personal family: `essay`, `journal-entry`, `article`.
- The personal `article` class should stay close to the professional article direction without inventing a separate `article-pro` implementation contract in v1.
- English and French are the only v1 locales.
- The first real implementation target is [`veriff`](org-latex-pdf/classes/veriff.md).
- The first shipped locale is English.
- Locale separation still exists from day one so French can be added without refactoring the architecture.
- Rollout order is fixed: one narrow vertical slice on one class, then full semantic coverage for that class, then variants for that class, then the remaining classes and locales.
- The chosen visual authority for the first real class lives in the external prototype workspace at `/Users/hubertbehaghel/tmp/veriff-article-prototypes/`.

## Tree Map

- [`org-latex-pdf/class-family-matrix.md`](org-latex-pdf/class-family-matrix.md)
  - Canonical class inventory, family relationships, spec coverage status, and rollout order.
- [`org-latex-pdf/semantic-layer.md`](org-latex-pdf/semantic-layer.md)
  - Shared authoring contract between Org and export output.
- [`org-latex-pdf/locale-layer.md`](org-latex-pdf/locale-layer.md)
  - Locale separation, keyword handling, and localized output surfaces.
- [`org-latex-pdf/classes/veriff.md`](org-latex-pdf/classes/veriff.md)
  - First fully specified class and first implementation target.
- [`org-latex-pdf/classes/article.md`](org-latex-pdf/classes/article.md)
  - Deferred but constrained personal-class spec aligned with the professional article direction.
- [`org-latex-pdf/validation/specimens.md`](org-latex-pdf/validation/specimens.md)
  - Canonical tracked Org specimens and their semantic coverage.
- [`org-latex-pdf/validation/pdf-fidelity.md`](org-latex-pdf/validation/pdf-fidelity.md)
  - Validation ladder from `.tex` assertions to visual diffing.
- [`org-latex-pdf/plans/iterative-test-plan.md`](org-latex-pdf/plans/iterative-test-plan.md)
  - Approved vertical-slice execution order.

## Writing Order

1. Class family matrix
2. Semantic layer
3. Locale layer
4. `veriff`
5. `article`
6. Specimens
7. PDF fidelity
8. Iterative test plan

The order matters because the later files should consume decisions from the earlier ones instead of redefining them.

## Cross-Link Rules

- The matrix is the single source of truth for class inventory and rollout status.
- The semantic layer defines author-facing meaning, not class styling.
- The locale layer defines language behavior, not family-specific visuals.
- Class specs may narrow or stage requirements, but may not contradict the semantic or locale layers.
- Validation files must reference the semantic and class specs they claim to verify.
- The iterative plan must reference all prior files and convert them into end-to-end slices.

## Non-Goals

- This tree does not implement the export system.
- This tree does not freeze detailed visual rules for every deferred class in advance.
- This tree does not require a derived Org backend unless the class-hook approach later proves insufficient.
- This tree does not track generated golden PDFs in git.

## Acceptance Signals

- A reader can identify the first implementation slice without guessing.
- A later implementation can distinguish semantic, class, locale, and validation responsibilities cleanly.
- Deferred classes are marked as deferred instead of being filled with invented requirements.
- The spec set can drive test-first work without needing a separate planning rewrite.
