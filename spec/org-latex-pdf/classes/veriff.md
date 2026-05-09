# Veriff

## Context

`veriff` is the flagship professional class family. It replaces the legacy `pro-refresh-overdrive` class with a variant-based system.

This file is the authoritative contract for the `veriff` class and its variants. If another file disagrees with it on flagship scope, this file wins.

## Class and Variant Identity

The `veriff` class supports multiple visual directions through the `#+LATEX_VARIANT:` keyword.

| Variant | Intent | Status |
| --- | --- | --- |
| `refresh-overdrive` | Flagship branded article direction; warm paper, orange accents. | active (default) |
| `dark-campaign` | Derived dark variant; shared typography and semantics; high-contrast dark surface. | active |

### Variant Selection Rules

- Selected via `#+LATEX_VARIANT:`.
- Values are exact lowercase (e.g., `refresh-overdrive`, `dark-campaign`).
- Surrounding whitespace is trimmed.
- Mixed-case values (e.g., `Refresh-Overdrive`) must cause an export error.
- If `#+LATEX_VARIANT:` is omitted, it defaults to `refresh-overdrive`.
- Duplicate `#+LATEX_VARIANT:` keywords must cause an export error.
- Unknown variant values must cause an export error.
- The error message for an unknown variant must explicitly list the valid variants: `refresh-overdrive`, `dark-campaign`.
- Usage of `#+LATEX_VARIANT:` with a non-`veriff` class must cause an export error.

### Legacy Migration

- `#+LATEX_CLASS: pro-refresh-overdrive` is no longer a valid class.
- It is NOT a successful alias for `veriff`.
- It must fail deterministically during export with migration guidance.
- The guidance must explicitly instruct the author to use `#+LATEX_CLASS: veriff` and, if they want the original look, `#+LATEX_VARIANT: refresh-overdrive`.

## Visual Authority

### Official Brand Palette

The `veriff` class should prefer the official Veriff palette below for new color tokens. Prototype-derived or transitional colors may exist during fidelity work, but they should be documented and mapped back to the nearest official token when reviewed.

| Family | Token | Hex |
| --- | --- | --- |
| Teal | Teal 100 | `#E0FCF8` |
| Teal | Teal 300 | `#9DF5EA` |
| Teal | Teal 500 | `#14E5C5` |
| Teal | Teal 700 | `#085559` |
| Teal | Teal 900 | `#0C3035` |
| Orange | Orange 100 | `#FBEDE8` |
| Orange | Orange 300 | `#FFAF97` |
| Orange | Orange 500 | `#FF550F` |
| Orange | Orange 700 | `#7D1F1F` |
| Orange | Orange 900 | `#42081D` |
| Neutral | Neutral 100 | `#F6FDFC` |
| Neutral | Neutral 300 | `#CBD7D4` |
| Neutral | Neutral 500 | `#97A19E` |
| Neutral | Neutral 700 | `#4B5F5A` |
| Neutral | Neutral 900 | `#03140F` |
| Tan | Tan 100 | `#EBECDC` |
| Tan | Tan 300 | `#C8B59A` |
| Tan | Tan 500 | `#845D33` |
| Tan | Tan 700 | `#543F21` |
| Tan | Tan 900 | `#241F0D` |
| Magenta | Magenta 100 | `#F9E9F9` |
| Magenta | Magenta 300 | `#F0A2EC` |
| Magenta | Magenta 500 | `#F451EE` |
| Magenta | Magenta 700 | `#90238A` |
| Magenta | Magenta 900 | `#2F062D` |
| Yellow | Yellow 100 | `#FAFEEA` |
| Yellow | Yellow 300 | `#F7FF9E` |
| Yellow | Yellow 500 | `#EEF51F` |
| Yellow | Yellow 700 | `#73761E` |
| Yellow | Yellow 900 | `#262B06` |

### refresh-overdrive
The approved visual references live at:
- `/Users/hubertbehaghel/tmp/veriff-article-prototypes/04-refresh-overdrive.html`
- `/Users/hubertbehaghel/tmp/veriff-article-prototypes/prototypes.css`
- `/Users/hubertbehaghel/tmp/veriff-article-prototypes/render-pdfs.js`

Non-negotiable signals:
- warm paper tone rather than stark white or black
- strong orange-led accent system with grounded brown support
- visible but restrained pattern use
- clear title hierarchy
- a single shared hero-copy column for eyebrow, title, and dek content, with the dek sitting directly beneath the title rather than drifting beside it
- materially heavier display treatment for the title than for the dek during the current page-one fidelity phase
- branded but still readable PDF page chrome when that chrome is present; the current font-fidelity phase may temporarily suppress running header/footer furniture across `veriff` exports while typography catches up
- evidence-friendly content structures rather than a decorative marketing shell

### dark-campaign
The approved visual reference lives at:
- `/Users/hubertbehaghel/tmp/veriff-article-prototypes/out/01-dark-campaign.pdf`

Non-negotiable signals:
- Derived from `refresh-overdrive`; shares typography and semantic layout.
- Visual differences are limited to palette, chrome, surface, and contrast.
- **CRITICAL:** The white-background webpage-print artefact/region sometimes found in prototypes is non-representative and must be ignored; the dark surface is the goal.

## Required Semantic Affordances

The class must consume the shared semantic layer from [`../semantic-layer.md`](../semantic-layer.md) and must not invent a separate authoring dialect.

| Visible structure | Expected Org source | Notes |
| --- | --- | --- |
| hero composition | title, subtitle, author, date, optional eyebrow | Class-owned composition; not a standalone shared block. |
| eyebrow | `#+EXPORT_EYEBROW:` | Optional but supported. |
| title | `#+TITLE:` | Required. |
| dek | `#+SUBTITLE:` | Optional in general, but expected for flagship review specimens. |
| body standfirst | `#+begin_standfirst` | Preferred for explicit lead paragraph styling. |
| links | native Org links | Required later in full semantic coverage; class owns link colors and PDF link treatment. |
| quote | native quote block or `pullquote` where larger treatment is intended | Native first, custom only when emphasis requires it. |
| callout | `#+begin_callout` | Required later in full semantic coverage. |
| metrics | `#+begin_metrics` | Required later in full semantic coverage. |
| plain lists and checklists | native ordered/unordered Org lists, including checkbox items | Required later in full semantic coverage; class owns list spacing and marker treatment. |
| table | native Org table | Required later in full semantic coverage. |
| figure/image | native Org image link with `#+CAPTION:`, `#+NAME:`, optional `#+ATTR_LATEX:` | Required later in full semantic coverage; class owns image frame and caption treatment. |
| pillars or cards | `#+begin_pillars` | Required later in full semantic coverage. |
| graph/chart | `#+begin_graph` | Required later in full semantic coverage. |
| code sample | source block | High-fidelity syntax highlighting via minted and tcolorbox. Supports line numbers via `-n`. |
| code theme | `#+EXPORT_CODE_THEME:` | Optional. Accepts `light` or `dark`. Defaults to `light`. |
| footer note | `#+EXPORT_FOOTER_NOTE:` | Optional but supported. |

For the current page-one approval slice, the class-owned hero composition must keep eyebrow, title, and dek in one shared left-column text block. The dek must align under the title in that same block, while the title keeps a visibly stronger display treatment than the dek. Hero metadata should read as part of that same column, separated by an internal divider in the hero-copy block rather than by full-width rules framing the entire hero. The hero must also include a distinct branded container background, the left-side pattern treatment, and the top-right logo treatment to establish the card feel from the prototype.

## Phase Expectations

### Slice 1: Veriff Class and Variant Foundation

Slice 1 is intentionally small.

Required in slice 1:
- class selection via `#+LATEX_CLASS: veriff`
- variant selection via `#+LATEX_VARIANT:` with default behavior (omitted variant defaults to `refresh-overdrive`)
- migration failure for `pro-refresh-overdrive` with guidance
- English locale selection path
- title, author, date
- one heading and one paragraph
- class-owned page tone and typography sufficient to prove the class exists
- subtle footer/page accent via class-owned page style with a full horizontal rule, document title recall, and logo
- brown/orange dominant source-code token colors under existing theme contract, with a restrained touch of mint
- running header/footer chrome may be temporarily suppressed during the current fidelity phase if that yields a closer match to the approved reference

Not required yet in slice 1:
- metrics cluster
- pillars cluster
- callout panels
- full hero choreography from the prototype
- French rendering

### Full Semantic Coverage for This Class

Before moving to other classes, this class must cover:
- eyebrow
- dek
- standfirst
- native links
- quote and pullquote behavior
- callout behavior
- metrics cluster
- native ordered/unordered lists and checklist items
- native tables
- native figures/images with captions, labels, and attribute-controlled sizing
- pillars cluster
- graph/chart
- code sample styling
- native Org footnotes
- footer note

## Preamble and Export Expectations

- The default implementation path should use Org LaTeX class machinery, preamble hooks, and locale-layer configuration.
- The class should be expressible through `org-latex-classes`, LaTeX preamble configuration, and related exporter hooks before any derived-backend escalation is considered.
- `veriff` now assumes the high-fidelity XeLaTeX path as its only supported compiler path. If XeLaTeX or the required class assets are unavailable, export should fail loudly instead of degrading to a lower-fidelity fallback.
- The `.cls` file should own as much stable visual rendering as possible, including the page-level font setup and hero/title formatting primitives. Export-time Elisp should stay focused on metadata wiring, asset staging, and variant orchestration.
- Page header and footer behavior should remain consistent with the approved prototype cues once that running chrome is reintroduced.
- During the current font-fidelity phase, the implementation may temporarily suppress running header/footer furniture across `veriff` outputs to keep comparisons focused on typography, hierarchy, and hero composition.

## Locale Interaction

- English is the first shipped locale for this class.
- The class must consume locale-owned labels and package behavior through the locale layer instead of embedding English strings directly in class logic where generated text is involved.
- A later French run for the same class should not require redesigning the class contract.

## Validation Hooks

This class is the first target for:
- slice specimen validation in [`../validation/specimens.md`](../validation/specimens.md)
- PDF fidelity review in [`../validation/pdf-fidelity.md`](../validation/pdf-fidelity.md)
- iterative delivery in [`../plans/iterative-test-plan.md`](../plans/iterative-test-plan.md)

## Non-Goals

- This file does not fully specify `pro-whitepaper` or `pro-announcement`.
- This file does not require one-to-one visual reproduction of prototype HTML markup.
- This file does not declare a derived backend as the default implementation path.

## Acceptance Signals

- `veriff` class and its variants are clearly defined.
- Variant selection rules and default behavior are unambiguous.
- Legacy migration path is explicit and provides actionable guidance.
- A future implementer can tell exactly what belongs in slice 1 and what belongs in later semantic expansion.
- Every required visible structure has an identified authoring source or explicit phase deferral.
- The class reads as the chosen flagship direction, not as a generic academic or marketing template.
