# Semantic Layer

## Context

The semantic layer defines what authors mean in Org before any class decides how that meaning is rendered. It exists to prevent each class from inventing a separate authoring dialect.

## Governing Rules

1. Prefer native Org whenever Org already expresses the meaning cleanly.
2. Introduce a custom block or custom keyword only when the meaning is materially different from native Org structures.
3. Keep semantic meaning stable across classes even when the visual treatment changes.
4. Treat a derived export backend as a fallback path only if class hooks, filters, or class-level preamble control later prove insufficient.

## Document-Level Contract

The shared authoring surface starts with standard Org metadata and class selection.

| Semantic role | Preferred Org form | Notes |
| --- | --- | --- |
| export class selection | `#+LATEX_CLASS:` | Class-owned behavior starts here. |
| variant selection | `#+LATEX_VARIANT:` | Variant selection within a class family (e.g. `veriff`). |
| locale selection | `#+LANGUAGE:` | Locale remains orthogonal to class. |
| title | `#+TITLE:` | Native Org. |
| subtitle or hero dek | `#+SUBTITLE:` | Native Org, reused by classes that surface a dek. |
| author | `#+AUTHOR:` | Native Org. |
| date | `#+DATE:` | Native Org. |
| optional eyebrow | `#+EXPORT_EYEBROW:` | Custom keyword because native Org has no equivalent semantic role. |
| optional footer note | `#+EXPORT_FOOTER_NOTE:` | Custom keyword because the note is chrome-like metadata, not ordinary body prose. |

## Shared Body Semantics

| Semantic role | Preferred Org form | Why |
| --- | --- | --- |
| headings | Org headings | Native section structure is already correct. |
| paragraphs | Org paragraphs | Native body prose. |
| lists | Org ordered or unordered lists | Native list semantics. |
| checklist items | Org checkbox list items | Native task-status markers inside list semantics. |
| links | Org links | Native references to external or internal targets. |
| footnotes | Org footnotes | Native supporting-note semantics distinct from class chrome/footer notes. |
| tables | Org tables | Native tabular data. |
| figures and images | Org file image link with `#+CAPTION:`, `#+NAME:`, and optional `#+ATTR_LATEX:` | Native figure semantics; classes may own frame and caption treatment. |
| code sample | source block | Native code-block semantics. Exported via minted for high-fidelity syntax highlighting. |
| quotation | quote block | Native quotation semantics. |
| quote attribution | `#+ATTR_QUOTE: :author "Name"` before a quote block | Native quote metadata. Classes may style the attribution line, but the author value is semantic. |
| emphasis | Org emphasis markers | Native inline semantics. |

## Explicit Custom Exceptions

These custom constructs are allowed because they carry meaning that the native structures do not express well enough for the target classes.

| Semantic role | Proposed Org form | Rationale |
| --- | --- | --- |
| epigraph | `#+begin_epigraph` | Opening quotation with attribution is not just a normal quote. |
| pullquote | `#+begin_pullquote` | Large in-flow highlighted quotation is distinct from a standard quote block. |
| callout | `#+ATTR_CALLOUT: :type ... :title ...` + `#+begin_callout` | Boxed explanatory or warning panel with exporter-independent semantics. |
| standfirst | `#+begin_standfirst` | Distinct introductory summary paragraph, especially useful for article classes. |
| section break | Org horizontal rule, e.g. `-----` | Untitled transition or return to a wider section perspective where a heading would be too heavy. |
| metrics cluster | `#+begin_metrics` | A grid or grouped KPI presentation is not an ordinary list or table. |
| pillars cluster | `#+begin_pillars` | Side-by-side cards need an explicit grouped semantic wrapper. |
| graph/chart | `#+begin_graph` | A branded data visualization or chart, distinct from a generic image or table. |

## Class-Owned Composition Rules

Some visible structures are compositions of multiple semantic inputs rather than their own authoring primitives.

- A hero section is class-owned composition, not a custom block in the shared layer.
- `veriff` builds its hero from title, subtitle, author, date, optional eyebrow, locale-aware chrome, and class-owned visual treatment.
- A calmer class may reuse the same inputs without reusing the same hero composition.

## Unsupported or Deferred Shared Semantics

- No separate semantic primitive is defined yet for sidebars, appendices, or multi-page cover sheets.
- No separate custom semantic primitive is defined for figures beyond native Org image links, captions, names, and LaTeX attributes.
- Variant-specific toggles are not part of the shared semantic layer.

## Coverage Hooks for Validation

Every semantic role listed above must later be exercised by at least one tracked specimen in [`validation/specimens.md`](validation/specimens.md).

## Cross-Links

- Class inventory: [`class-family-matrix.md`](class-family-matrix.md)
- Locale separation: [`locale-layer.md`](locale-layer.md)
- First real class mapping: [`classes/veriff.md`](classes/veriff.md)
- Personal article relationship: [`classes/article.md`](classes/article.md)
- Coverage corpus: [`validation/specimens.md`](validation/specimens.md)
- Rollout plan: [`plans/iterative-test-plan.md`](plans/iterative-test-plan.md)

## Non-Goals

- This file does not freeze exact LaTeX package names or macros.
- This file does not define class-specific colors, page furniture, or logo behavior.
- This file does not claim that every custom semantic exception belongs in slice 1.

## Acceptance Signals

- A future implementation can tell which authoring forms are native Org and which are sanctioned exceptions.
- Visible structures such as hero or chrome are not confused with author-authored semantics.
- The semantic vocabulary is rich enough to describe the approved prototype content without forcing every class to invent its own dialect.
