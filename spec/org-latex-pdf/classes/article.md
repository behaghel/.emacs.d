# Article

## Context

`article` is the personal-family article concept. The implementation-ready export class name is `hub-article`, and authors will use `#+LATEX_CLASS: hub-article` for that concrete target.

## Class Intent

`hub-article` should preserve the same broad semantic capacity as the professional article path while becoming calmer, less branded, and more personal.

It should feel related to the professional article shape, not like an unrelated academic template.

The design direction is public long-form technical essays, essayistic technical authority, a contemporary oldstyle serif body, restrained sans furniture, beautiful mono, a single-column layout with a sparse margin zone, muted deep green and teal accents, a quiet editorial opener, quiet beautiful code, and European literary-technical French-aware typography. The anti-goal is generic LaTeX or Pandoc anonymity.

## Relationship to the Professional Direction

- It shares the semantic layer with `veriff`.
- It shares the locale layer with `veriff`.
- It should preserve article-like information flow: title, optional dek, lead paragraph, quotations, evidence blocks, tables, and code samples where needed.
- It should reduce brand intensity, page chrome, and visual aggression relative to `veriff`.
- It is not a `veriff` variant.
- It should not require a separate `article-pro` class contract in this v1 spec tree.

## Shared Semantic Expectations

At minimum, `hub-article` is expected to consume:

- native headings and paragraphs
- native lists and tables
- source blocks
- native quotes
- optional standfirst
- optional callouts where the content meaning requires them

The concrete export class should be implementation-ready now, even if the final class file lands later in the code slice sequence.

## Deliberate Divergences from `veriff`

- calmer accent and chrome treatment
- less dependence on page-one branded theatre
- greater tolerance for long-form reading without heavy hero composition
- reduced emphasis on campaign-like pattern use
- a quieter opening page that still reads as an editorial essay, not a generic report
- source blocks stay at the same visual level as the prose, with no dark-mode treatment and no background surface
- code presentation should stay high-elegance and supportive, with restrained quasi-monochrome syntax highlighting for legibility, not IDE-like saturation

## Locale Interaction

- English and French remain the only v1 locales.
- The class must consume locale profiles from the shared locale layer rather than embedding its own language handling rules.
- The French-ready typography path is part of the contract from day one.

## Current Status

This is an implementation-ready spec for the concrete `hub-article` export class, with the personal `article` family concept preserved above it.

That means:

- it stays family-level, not variant-level
- it should not become a `veriff` branch by accident
- it already defines enough boundary conditions to keep the later implementation aligned with the approved personal direction

## Cross-Links

- Family inventory: [`../class-family-matrix.md`](../class-family-matrix.md)
- Shared semantics: [`../semantic-layer.md`](../semantic-layer.md)
- Shared locale behavior: [`../locale-layer.md`](../locale-layer.md)
- First flagship class: [`veriff.md`](veriff.md)
- Personal concrete class: `hub-article`
- Coverage and rollout: [`../validation/specimens.md`](../validation/specimens.md), [`../plans/iterative-test-plan.md`](../plans/iterative-test-plan.md)

## Non-Goals

- This file does not specify academic defaults such as fixed one-inch margins or double-spacing.
- This file does not fully define `essay` or `journal-entry`.
- This file does not require the personal class to copy the flagship visual system directly.
- This file does not introduce a `#+LATEX_VARIANT:` setting for `hub-article`.

## Acceptance Signals

- The relationship to the professional article direction is explicit.
- `hub-article` is the concrete export class name and `article` stays the family concept.
- The design direction is specific enough to guide implementation without genericizing the class.
