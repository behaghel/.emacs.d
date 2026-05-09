# Specimens

## Context

Specimens are tracked Org files that exercise the export contract. They are authored inputs, not generated outputs.

The recommended tracked home for these future files is `test/fixtures/org-export/` so they live with test inputs rather than runtime artifacts.

Generated `.tex`, `.pdf`, extracted text, snapshots, and visual diff results belong under `var/`.

## Core Specimen Inventory

### Tier 1: Narrow Slice Specimen

- `slice-en-veriff-refresh-overdrive.org`
  - purpose: first end-to-end English flagship slice using `veriff` class and explicit `refresh-overdrive` variant
  - minimum content: `#+LATEX_CLASS: veriff`, `#+LATEX_VARIANT: refresh-overdrive`, title, author, date, one heading, one paragraph

- `slice-en-veriff-default-variant.org`
  - purpose: verify that omitted variant defaults to `refresh-overdrive`
  - minimum content: `#+LATEX_CLASS: veriff`, title, author, date, one heading, one paragraph

- `slice-en-veriff-dark-campaign.org`
  - purpose: verify `dark-campaign` variant selection and visual surface
  - minimum content: `#+LATEX_CLASS: veriff`, `#+LATEX_VARIANT: dark-campaign`, title, author, date, one heading, one paragraph

- `slice-en-veriff-gallery-white.org`
  - purpose: verify `gallery-white` variant selection and white-paper visual surface
  - minimum content: `#+LATEX_CLASS: veriff`, `#+LATEX_VARIANT: gallery-white`, title, author, date, standfirst, one heading, two paragraphs, one pullquote, and at least one width-controlled media/table specimen

### Tier 2: Full Semantic Coverage Specimens

- `semantic-full-en.org`
  - purpose: full semantic coverage for the `veriff` class in English
- `semantic-full-fr.org`
  - purpose: full semantic coverage for the shared semantic and locale layers in French once the rollout reaches that stage

### Tier 3: Optional Approval Specimens

- `approval-refresh-overdrive-page1.org`
  - purpose: short approval-focused page-one review specimen for visual sign-off using the `veriff` class and `refresh-overdrive` variant

Future dark-campaign approval specimens may be added later under a separate tracked filename, but they are not part of the current inventory.

## Coverage Matrix

| Semantic role | Slice EN | Full EN | Full FR | Approval specimens |
| --- | --- | --- | --- | --- |
| class selection | yes | yes | yes | yes |
| locale selection | yes | yes | yes | yes |
| title or author or date | yes | yes | yes | yes |
| headings and paragraphs | yes | yes | yes | yes |
| standfirst | no | yes | yes | optional |
| eyebrow | no | yes | yes | optional |
| links | no | yes | yes | optional |
| epigraph | no | yes | yes | optional |
| quote | no | yes | yes | optional |
| pullquote | no | yes | yes | no |
| callout | no | yes | yes | optional |
| metrics | no | yes | yes | optional |
| ordered/unordered lists | no | yes | yes | optional |
| checklist items | no | yes | yes | optional |
| tables | no | yes | yes | optional |
| figures/images | no | yes | yes | optional |
| pillars | no | yes | yes | optional |
| graph/chart | no | yes | yes | optional |
| code sample | no | yes | yes | optional |
| native footnotes | no | yes | yes | optional |
| footer note | no | yes | yes | optional |

## Artifact Policy

Generated artifacts should be written under a stable subtree in `var/`, for example:

- `var/org-latex-pdf/<specimen-name>/output.tex`
- `var/org-latex-pdf/<specimen-name>/output.pdf`
- `var/org-latex-pdf/<specimen-name>/text.txt`
- `var/org-latex-pdf/<specimen-name>/page-1.png`
- `var/org-latex-pdf/<specimen-name>/diff-page-1.png`

The exact filenames may evolve, but the class of artifact and its placement in `var/` are part of the contract.

Approval roots should be class+variant aware, for example `var/org-latex-pdf/veriff-refresh-overdrive-page1/` for the page-one approval slice.

## Manual Review Expectations

- Approval specimens exist to accelerate design review without requiring the full semantic corpus every time.
- The narrow slice specimen exists to prove the first end-to-end path before broader semantic ambition is added.
- The full semantic specimens exist to stop semantic regressions from hiding behind visually attractive first pages.

## Cross-Links

- Shared semantics: [`../semantic-layer.md`](../semantic-layer.md)
- Locale separation: [`../locale-layer.md`](../locale-layer.md)
- First real class: [`../classes/veriff.md`](../classes/veriff.md)
- Personal article relationship: [`../classes/article.md`](../classes/article.md)
- Fidelity ladder: [`pdf-fidelity.md`](pdf-fidelity.md)
- Rollout plan: [`../plans/iterative-test-plan.md`](../plans/iterative-test-plan.md)

## Non-Goals

- This file does not require tracked golden PDFs in git.
- This file does not require every deferred class to have specimens before the first slice begins.
- This file does not force French approval specimens into the English-first slice.

## Acceptance Signals

- The specimen set distinguishes authored inputs from generated artifacts.
- Every semantic role in the shared layer has an eventual specimen owner.
- The English-first slice remains intentionally small while still living in the same specimen system as later coverage.
