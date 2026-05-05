# Pro Refresh Overdrive

## Context

`pro-refresh-overdrive` is the first real implementation target. It is the flagship professional article direction chosen after the prototype review.

This file is the authoritative contract for the first shipped class. If another file disagrees with it on first-class scope, this file wins.

## Visual Authority

The approved visual references currently live outside the repo in the prototype workspace at:

- `/Users/hubertbehaghel/tmp/veriff-article-prototypes/04-refresh-overdrive.html`
- `/Users/hubertbehaghel/tmp/veriff-article-prototypes/prototypes.css`
- `/Users/hubertbehaghel/tmp/veriff-article-prototypes/render-pdfs.js`

The implementation should treat these as design oracles, not as source templates to be copied.

From those references, the non-negotiable signals for this class are:

- warm paper tone rather than stark white or black
- strong orange-led accent system with grounded brown support
- visible but restrained pattern use
- clear title hierarchy
- a single shared hero-copy column for eyebrow, title, and dek content, with the dek sitting directly beneath the title rather than drifting beside it
- materially heavier display treatment for the title than for the dek during the current page-one fidelity phase
- branded but still readable PDF page chrome when that chrome is present; the current font-fidelity phase may temporarily suppress running header/footer furniture across `pro-refresh-overdrive` exports while typography catches up
- evidence-friendly content structures rather than a decorative marketing shell

## Class Intent

This class is for branded professional article and report output that should feel clearly refreshed, visibly Veriff-aligned, and still trustworthy over multiple pages.

It is not a general campaign poster class and it is not the calmer white-paper direction.

## Required Semantic Affordances

The class must consume the shared semantic layer from [`../semantic-layer.md`](../semantic-layer.md) and must not invent a separate authoring dialect.

| Visible structure | Expected Org source | Notes |
| --- | --- | --- |
| hero composition | title, subtitle, author, date, optional eyebrow | Class-owned composition; not a standalone shared block. |
| eyebrow | `#+EXPORT_EYEBROW:` | Optional but supported. |
| title | `#+TITLE:` | Required. |
| dek | `#+SUBTITLE:` | Optional in general, but expected for flagship review specimens. |
| body standfirst | `#+begin_standfirst` | Preferred for explicit lead paragraph styling. |
| quote | native quote block or `pullquote` where larger treatment is intended | Native first, custom only when emphasis requires it. |
| callout | `#+begin_callout` | Required later in full semantic coverage. |
| metrics | `#+begin_metrics` | Required later in full semantic coverage. |
| table | native Org table | Required later in full semantic coverage. |
| pillars or cards | `#+begin_pillars` | Required later in full semantic coverage. |
| code sample | source block | High-fidelity syntax highlighting via minted and tcolorbox. Supports line numbers via `-n`. |
| code theme | `#+EXPORT_CODE_THEME:` | Optional. Accepts `light` or `dark`. Defaults to `light`. |
| footer note | `#+EXPORT_FOOTER_NOTE:` | Optional but supported. |

For the current page-one approval slice, the class-owned hero composition must keep eyebrow, title, and dek in one shared left-column text block. The dek must align under the title in that same block, while the title keeps a visibly stronger display treatment than the dek. Hero metadata should read as part of that same column, separated by an internal divider in the hero-copy block rather than by full-width rules framing the entire hero. The hero must also include a distinct branded container background, the left-side pattern treatment, and the top-right logo treatment to establish the card feel from the prototype.

## Phase Expectations

### Slice 1: Narrow English Flagship Slice

Slice 1 is intentionally small.

Required in slice 1:

- class selection via `#+LATEX_CLASS: pro-refresh-overdrive`
- English locale selection path
- title, author, date
- one heading and one paragraph
- class-owned page tone and typography sufficient to prove the class exists
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
- quote and pullquote behavior
- callout behavior
- metrics cluster
- native tables
- pillars cluster
- code sample styling
- footer note

## Preamble and Export Expectations

- The default implementation path should use Org LaTeX class machinery, preamble hooks, and locale-layer configuration.
- The class should be expressible through `org-latex-classes`, LaTeX preamble configuration, and related exporter hooks before any derived-backend escalation is considered.
- `pro-refresh-overdrive` now assumes the high-fidelity XeLaTeX path as its only supported compiler path. If XeLaTeX or the required class assets are unavailable, export should fail loudly instead of degrading to a lower-fidelity fallback.
- The `.cls` file should own as much stable visual rendering as possible, including the page-level font setup and hero/title formatting primitives. Export-time Elisp should stay focused on metadata wiring, asset staging, and compile orchestration.
- Page header and footer behavior should remain consistent with the approved prototype cues once that running chrome is reintroduced.
- During the current font-fidelity phase, the implementation may temporarily suppress running header/footer furniture across `pro-refresh-overdrive` outputs to keep comparisons focused on typography, hierarchy, and hero composition.

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

- A future implementer can tell exactly what belongs in slice 1 and what belongs in later semantic expansion.
- Every required visible structure has an identified authoring source or explicit phase deferral.
- The class reads as the chosen flagship direction, not as a generic academic or marketing template.
