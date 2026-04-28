# PDF Fidelity

## Context

PDF fidelity is a ladder, not a single check. The goal is to catch different kinds of regressions at the cheapest stage that can detect them.

The first fidelity target is English `pro-refresh-overdrive`.

## Authoritative References

- class contract: [`../classes/pro-refresh-overdrive.md`](../classes/pro-refresh-overdrive.md)
- specimen corpus: [`specimens.md`](specimens.md)
- external visual oracle: `/Users/hubertbehaghel/tmp/veriff-article-prototypes/04-refresh-overdrive.html`, `prototypes.css`, and `render-pdfs.js`

## Fidelity Ladder

### 1. Generated `.tex` Structure Assertions

Purpose: catch semantic or class-wiring regressions before PDF compilation.

Required checks:

- expected `\documentclass` or class-owned preamble markers are present
- expected locale wiring is present for the chosen profile
- expected custom environments or macros appear when the specimen uses the matching semantic construct
- expected metadata surfaces are emitted from Org inputs

Pass condition: the generated `.tex` proves the semantic and class path was selected correctly.

### 2. PDF Structural Checks

Purpose: confirm the compiled PDF is structurally healthy.

Required checks:

- PDF exists
- page count is plausible for the specimen
- fonts are embedded as required by the chosen pipeline
- document metadata exists where the implementation claims to emit it

Pass condition: the PDF is not merely compilable, but structurally sane.

### 3. Text Extraction Regression

Purpose: confirm readable text survives export.

Required checks:

- authored text expected from the specimen is extractable
- locale-generated labels expected for the current profile are extractable where applicable
- accented French text survives without encoding loss when French coverage is introduced

This layer should compare against an approved text contract, not raw Org source order. Generated labels, running chrome, and metadata may legitimately introduce extra text.

Pass condition: meaningful content and generated locale surfaces survive extraction without corruption.

### 4. Rendered Page Snapshots

Purpose: capture page-level render output for quick review.

Required pages for `pro-refresh-overdrive`:

- first page or hero page
- one interior content page once semantic coverage expands

Pass condition: snapshots are produced into `var/` and are stable enough for review.

### 5. Visual Diffs on Canonical Pages

Purpose: catch layout, spacing, and color regressions that structural checks will miss.

Required focus areas:

- page tone and chrome
- title hierarchy
- callout and quote treatment
- metrics or pillars once those semantics are in scope

Pass condition: differences outside approved tolerance are visible and attributable, not silent.

## Artifact Placement

All fidelity artifacts belong under `var/`, for example:

- `var/org-latex-pdf/<specimen>/output.tex`
- `var/org-latex-pdf/<specimen>/output.pdf`
- `var/org-latex-pdf/<specimen>/text.txt`
- `var/org-latex-pdf/<specimen>/snapshots/page-1.png`
- `var/org-latex-pdf/<specimen>/diffs/page-1.png`

## Escalation Rule

The default implementation path is still class wiring plus preamble control. A derived backend should only be considered if repeated fidelity failures show that class-level control cannot express the required output reliably.

## Manual Review Checklist

- Does the page feel like the approved refresh-overdrive direction rather than a generic article?
- Is the reading hierarchy clear on page one?
- Do branded cues support trust rather than overwhelm the content?
- Do interior content structures remain evidence-friendly and readable?

## Cross-Links

- Specimen corpus: [`specimens.md`](specimens.md)
- First real class: [`../classes/pro-refresh-overdrive.md`](../classes/pro-refresh-overdrive.md)
- Rollout plan: [`../plans/iterative-test-plan.md`](../plans/iterative-test-plan.md)

## Non-Goals

- This file does not freeze pixel-perfect tolerance numbers before tooling exists.
- This file does not require visual diffing in the first red test if cheaper structural checks already fail.
- This file does not replace human review for approval-quality pages.

## Acceptance Signals

- The ladder distinguishes structural correctness from visual fidelity.
- Text regression checks do not rely on the false assumption that PDF text equals raw Org source order.
- Artifact placement in `var/` is explicit at every fidelity stage.
