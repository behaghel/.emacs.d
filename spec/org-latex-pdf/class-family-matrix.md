# Class Family Matrix

## Context

The export system is organized around class families, not ad hoc templates. This file is the canonical inventory of classes, rollout status, and family relationships.

## Family Inventory

### Professional Family

| Class | Intent | v1 Status | Dedicated Spec |
| --- | --- | --- | --- |
| `pro-refresh-overdrive` | Chosen flagship branded article/report direction for the first real implementation | active first target | [`classes/pro-refresh-overdrive.md`](classes/pro-refresh-overdrive.md) |
| `pro-whitepaper` | Later calmer professional variant for white-paper and scientific-document territory | deferred | none yet |
| `pro-announcement` | Later punchier campaign or official-announcement variant | deferred | none yet |

### Personal Family

| Class | Intent | v1 Status | Dedicated Spec |
| --- | --- | --- | --- |
| `essay` | Later reflective long-form personal writing class | deferred | none yet |
| `journal-entry` | Later diary or notebook-style personal class | deferred | none yet |
| `article` | Personal article class that should stay close to the professional article direction while becoming calmer and less branded | constrained deferred target | [`classes/article.md`](classes/article.md) |

## Locale Matrix

| Locale | Code | v1 Support | First Ship Order |
| --- | --- | --- | --- |
| English | `en` | yes | first |
| French | `fr` | yes | after English-first slice |

No additional locales are in scope for v1.

## Relationship Notes

- The semantic layer is shared across both families.
- Locale profiles are orthogonal to class families.
- The personal `article` class should remain close to the professional article direction in information architecture and semantic affordances.
- That closeness is a design-nearness rule, not a requirement to invent or ship a separate `article-pro` contract in this spec set.
- Deferred classes should remain explicitly deferred until the first flagship class reaches full semantic coverage.

## Rollout Order

1. Narrow vertical slice for `pro-refresh-overdrive` in English.
2. Full semantic coverage for `pro-refresh-overdrive`.
3. Additional variants within the same professional class family direction.
4. Remaining classes and French coverage.

This order is normative. Later files in this tree should not contradict it.

## Cross-Links

- Shared authoring contract: [`semantic-layer.md`](semantic-layer.md)
- Locale separation: [`locale-layer.md`](locale-layer.md)
- First real class: [`classes/pro-refresh-overdrive.md`](classes/pro-refresh-overdrive.md)
- Personal article constraint: [`classes/article.md`](classes/article.md)
- Execution order: [`plans/iterative-test-plan.md`](plans/iterative-test-plan.md)

## Non-Goals

- This file does not define detailed visual rules for deferred classes.
- This file does not decide the final implementation module layout beyond what is already recorded in the root spec README.
- This file does not imply that every listed class must receive a dedicated spec before the first slice begins.

## Acceptance Signals

- All six approved class names appear exactly once in the inventory.
- English and French are the only v1 locales listed.
- `pro-refresh-overdrive` is clearly marked as the first real implementation target.
- Deferred classes are labeled as deferred instead of receiving speculative invented feature lists.
