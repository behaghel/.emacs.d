# Article

## Context

`article` is the personal-family article class. It is not the first implementation target, but it is important enough to constrain now because the user explicitly said it should stay close to the professional article direction.

## Class Intent

`article` should preserve the same broad semantic capacity as the professional article path while becoming calmer, less branded, and more personal.

It should feel related to the professional article shape, not like an unrelated academic template.

## Relationship to the Professional Direction

- It shares the semantic layer with `pro-refresh-overdrive`.
- It shares the locale layer with `pro-refresh-overdrive`.
- It should preserve article-like information flow: title, optional dek, lead paragraph, quotations, evidence blocks, tables, and code samples where needed.
- It should reduce brand intensity, page chrome, and visual aggression relative to `pro-refresh-overdrive`.
- It should not require a separate `article-pro` class contract in this v1 spec tree.

## Shared Semantic Expectations

At minimum, `article` is expected to consume:

- native headings and paragraphs
- native lists and tables
- source blocks
- native quotes
- optional standfirst
- optional callouts where the content meaning requires them

The exact visual treatment is intentionally deferred until after the first flagship class reaches full semantic coverage.

## Deliberate Divergences from `pro-refresh-overdrive`

- calmer accent and chrome treatment
- less dependence on page-one branded theatre
- greater tolerance for long-form reading without heavy hero composition
- reduced emphasis on campaign-like pattern use

## Locale Interaction

- English and French remain the only v1 locales.
- The class must consume locale profiles from the shared locale layer rather than embedding its own language handling rules.

## Current Status

This is a constrained deferred spec, not a full implementation-ready class contract.

That means:

- it is intentionally less detailed than `pro-refresh-overdrive`
- it should not block the first flagship slice
- it already defines enough boundary conditions to prevent a later rewrite from drifting away from the approved family relationship

## Cross-Links

- Family inventory: [`../class-family-matrix.md`](../class-family-matrix.md)
- Shared semantics: [`../semantic-layer.md`](../semantic-layer.md)
- Shared locale behavior: [`../locale-layer.md`](../locale-layer.md)
- First flagship class: [`pro-refresh-overdrive.md`](pro-refresh-overdrive.md)
- Coverage and rollout: [`../validation/specimens.md`](../validation/specimens.md), [`../plans/iterative-test-plan.md`](../plans/iterative-test-plan.md)

## Non-Goals

- This file does not specify academic defaults such as fixed one-inch margins or double-spacing.
- This file does not fully define `essay` or `journal-entry`.
- This file does not require the personal class to copy the flagship visual system directly.

## Acceptance Signals

- The relationship to the professional article direction is explicit.
- The class is constrained enough to avoid later invention of an unrelated personal article model.
- The file stays deferred where authority is still intentionally unresolved.
