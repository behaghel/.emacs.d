# Target Mapping: Hugo Static Sites

## Purpose

This file maps the target-agnostic Org semantic layer to Hugo HTML output for
personal static sites, including sites created from the `hugo-ox-static-site`
template.

## Source of Truth

- Shared semantic contract: [../semantic-layer.md](../semantic-layer.md)
- Implementation target: `/Users/hubertbehaghel/nixos-config/templates/hugo-ox-static-site/`
- Prior art: `/Users/hubertbehaghel/ws/blog.behaghel.org/`

## Target Principles

1. Org remains the authoring source of semantics.
2. `ox-hugo` produces Markdown/front matter; Hugo layouts own web rendering.
3. SCSS owns presentation; generated HTML should carry semantic classes and
   structure, not inline styling.
4. Hugo render hooks/shortcodes may enrich native Org exports when Hugo needs
   information not represented by plain Markdown.
5. Semantic loss must be explicit in docs/tests; the template should not create
   a website-only authoring dialect unless recorded here.

## Asset Model

| Asset location | Hugo role | Processing | Intended use |
| --- | --- | --- | --- |
| `assets/img/...` | global resource | yes | shared processable images, responsive figures |
| page bundle resources | page resource | yes | images owned by one page/post |
| `static/...` | copied file | no | raw files, downloads, favicons, static HTML |

Images intended for responsive treatment should live in `assets/img/...` or in
page bundles. `static/` is a raw escape hatch.

## Mapping Table

| Semantic role | Org form | Hugo rendering | Support | Notes |
| --- | --- | --- | --- | --- |
| title | `#+TITLE:` | Hugo `.Title`, `<h1>`, `<title>`, OpenGraph where enabled | supported | `ox-hugo` front matter owns transfer. |
| subtitle / hero dek | `#+SUBTITLE:` | `.Params.subtitle` rendered in article header/hero when present | planned | Template should include header support. |
| author | `#+AUTHOR:` | `.Params.author` or site default in metadata/header | planned | Visual rendering can be quiet. |
| date | `#+DATE:` | `.Date` / `.Params.date` in article metadata | supported | Existing template has simple dates. |
| locale | `#+LANGUAGE:` | `lang` attribute and generated-label selection | planned | Start with pass-through/default. |
| optional eyebrow | `#+EXPORT_EYEBROW:` | `.Params.export_eyebrow` in article header | planned | Requires ox-hugo front matter support or raw keyword handling. |
| optional footer note | `#+EXPORT_FOOTER_NOTE:` | backmatter/footer-note block near article end | planned | Requires extraction path. |
| headings | Org headings | `<h1>`… with heading anchors | supported/planned anchors | Blog has render-heading prior art. |
| paragraphs | Org paragraphs | `<p>` styled by semantic typography SCSS | supported | No custom syntax. |
| lists | Org lists | `<ul>`/`<ol>` | supported | SCSS handles spacing. |
| checklist items | Org checkbox lists | task-list CSS removes bullets and preserves status text | supported | Prior art in blog CSS. |
| links | Org links | `<a>` | supported | Internal Org links resolved by ox-hugo. |
| footnotes | Org footnotes | Hugo/Goldmark footnotes with semantic footnote styling | supported | SCSS owns presentation. |
| tables | Org tables | `<table>` | supported | Wide tables may need future responsive wrapper. |
| figures/images | Org image link with caption/name/attrs | render hook emits `<figure>`/`<picture>` when processable | planned | Use page resource, then global `assets/`, then raw fallback. |
| code sample | Org source block | Hugo highlighted code or `pre > code` | supported | SCSS normalizes code blocks. |
| quotation | quote block | `<blockquote>` | supported | SCSS owns visual treatment. |
| quote attribution | `#+ATTR_QUOTE: :author ...` | `figure.quote > blockquote + figcaption` | planned | Needs exporter/filter/shortcode support. |
| emphasis | Org emphasis | native inline HTML (`em`, `strong`, `code`) | supported | SCSS styles inline code. |
| epigraph | `#+begin_epigraph` | `<section class="epigraph">` | planned | Blog CSS has visual prior art. |
| pullquote | `#+begin_pullquote` | `<aside class="pullquote">` or `<blockquote class="pullquote">` | planned | Distinct from quote. |
| callout | `#+ATTR_CALLOUT` + `#+begin_callout` | `<aside class="callout callout--TYPE">` | planned | Types should align across targets. |
| standfirst | `#+begin_standfirst` | `<section class="standfirst">` near article top | planned | May be pulled into header/hero later. |
| section break | Org horizontal rule | `<hr class="section-break">` | planned | CSS quiet ornament. |
| metrics cluster | `#+begin_metrics` | `<section class="metrics">` with metric cards | planned | Needs block translation. |
| pillars cluster | `#+begin_pillars` | `<section class="pillars">` with cards/columns | planned | CSS responsive grid. |
| graph/chart | `#+begin_graph` | `<figure class="graph">` with image/SVG fallback | planned | No browser-side graph DSL in v1. |

## Initial Template Implementation Scope

The first Hugo slice should port battery-included support for:

1. SCSS pipeline and semantic CSS modules.
2. Responsive/processable images and figure semantics.
3. Existing blog semantic classes that are already author-facing:
   - `small`
   - `fullwidth`
   - `right`
   - `centered`
   - `responsive`
   - `epigraph`
   - `encart` (to be reconciled with `callout`/`standfirst`)
   - `two-axis-table`
   - `underline`
4. Basic typographic defaults:
   - readable body measure and line height;
   - editorial heading scale;
   - blockquote, caption, table, footnote, and inline-code styling;
   - code block normalization.

Blog-specific behavior is intentionally excluded from the generic template by
default: webmentions, IndieAuth, analytics, social icon chrome, journal RSS,
and h-card footer.

## Semantic Loss / Degradation

| Semantic role | Degradation | Reporting requirement |
| --- | --- | --- |
| responsive image with remote URL | Render normal remote `<img>`/link; no processing | Documented convention; no build failure. |
| responsive image from `static/` | Render normal raw image; no processing | Documented convention. |
| quote attribution | Until implemented, attribution may be ignored by ordinary HTML export | Test once implemented. |
| graph/chart without image/SVG fallback | Render source content if exported, otherwise unsupported | Future preflight should warn/fail. |
| metrics/pillars/callouts without block support | Render generic HTML/custom block fallback if available | Future tests should assert supported block shapes. |

## Acceptance Signals

- A site created from `hugo-ox-static-site` can render processable responsive
  images from `assets/img/...` and page bundles.
- `hb-static-site-create-page` (`C-c w p`) creates leaf-bundle pages by
  default; `hb-static-site-create-basic-page` (`C-c w P`) remains available for
  flat page files.
- The template stylesheet is SCSS-based and includes the shared semantic classes
  above without requiring a theme.
- Static sites can use the same semantic authoring vocabulary as other Org
  targets.
- `static/` remains a documented raw-file escape hatch.
- Blog-specific chrome is not silently copied into the template.
