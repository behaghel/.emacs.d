# Typographic Semantics Contract

## Purpose

This document defines the author-facing typographic semantics used when writing Org documents in this repository.  The contract is intentionally backend-neutral: LaTeX/PDF, Confluence, and Google Docs are renderers of the same authoring model, not independent sources of truth.

The goal is to let an author write one Org source consistently and export or sync it while preserving as much semantic intent as each target can support.

## Principles

- **Authoring semantics come first.** Org source expresses meaning before visual style.
- **Renderers may differ visually.** PDF, Confluence, and Google Docs can style the same semantic differently.
- **Semantic loss must be explicit.** A backend that cannot preserve a semantic should classify it as degraded, deferred, or unsupported instead of silently flattening it.
- **Styling is a separate concern.** Typography, spacing, colors, syntax highlighting, callout chrome, and quote decoration are renderer-specific unless this contract names a semantic requirement.
- **Review collaboration is separate.** Review comments live in sidecar comments files and are not represented as typographic footnotes.

## Support vocabulary

| Status | Meaning |
| --- | --- |
| `semantic` | The construct carries author intent that should survive export/sync. |
| `styling` | The construct is mainly visual treatment; exact rendering is backend-specific. |
| `native` | The target has a first-class equivalent and should use it when possible. |
| `round-trip` | The construct should survive export and import back to Org. |
| `degraded` | The target can carry readable content but loses part of the semantic. |
| `deferred` | Intentional later work, usually because it is mostly styling or needs more design. |
| `unsupported` | The target cannot preserve this safely yet and should report that clearly. |

## Construct inventory

### Document metadata

| Semantic | Canonical Org source | Contract |
| --- | --- | --- |
| Title | `#+TITLE:` | Semantic. Renderers should preserve as document title or heading metadata. |
| Subtitle | `#+SUBTITLE:` when used | Semantic. Preserve as subtitle metadata or leading text. |
| Author | `#+AUTHOR:` | Semantic. Preserve where target metadata supports it; otherwise readable text is acceptable. |
| Date | `#+DATE:` and Org timestamps | Semantic. Preserve date value; localized visual formatting is styling. |
| Locale | `#+LANGUAGE:` | Semantic for export. Unsupported targets should degrade explicitly. |
| Eyebrow | `#+EXPORT_EYEBROW:` | Semantic document chrome. Target-specific visual rendering is styling. |
| Footer note | `#+EXPORT_FOOTER_NOTE:` | Semantic document note rendered by capable exporters. |

### Inline semantics

| Semantic | Canonical Org source | Contract |
| --- | --- | --- |
| Emphasis | `/italic/`, `*bold*`, `_underline_`, `+strike-through+` | Semantic inline emphasis. Preserve when target supports rich text. |
| Inline code | `~code~` and `=verbatim=` | Semantic literal text. Syntax color is styling. |
| External link | `[[https://example.com][label]]` | Semantic link target plus label. |
| Date mention | Org timestamps such as `<2026-07-03 Fri>` and inactive `[2026-07-03 Fri]` | Semantic date value. Remote dates imported from Confluence or Google Docs should become inactive Org timestamps by default so they do not pollute `org-agenda`. Google Docs date smart chips are the native target where available. |
| Person mention | Provisional `[[person:email@example.com][Name]]` | Semantic person identity plus display name. Google Docs people smart chips are the native target where available. |
| Status mention | Provisional `[[status:blocked][Blocked]]` | Semantic status value plus display label. Google Docs dropdown smart chips are the native target where available. |
| Footnote reference | `[fn:label]` | Semantic note anchor. Native footnotes are preferred where available. |

### Notes

| Semantic | Canonical Org source | Contract |
| --- | --- | --- |
| Ordinary note | `[fn:label] Body` | Semantic note body. In `hub-article` LaTeX, ordinary notes render as margin sidenotes. |
| Forced bottom footnote | Footnote definition with `HUB_NOTE_KIND: footnote` metadata | Semantic override. Preserves bottom-footnote behavior in renderers that distinguish side notes and footnotes. |
| Marginalia | Footnote definition with `HUB_NOTE_KIND: marginalia` metadata | Semantic marginal note. `packages/org-marginalia/` owns authoring helpers and metadata conventions. |
| Review comment | Sidecar `*.comments.org` | Not typographic. Must not be encoded as an Org footnote. |

### Blocks

| Semantic | Canonical Org source | Contract |
| --- | --- | --- |
| Source block | `#+begin_src LANG ... #+end_src` | Semantic code block with language. Google Docs code block building blocks are the native target where available. Syntax highlighting and line-number styling are renderer-specific. |
| Example block | `#+begin_example ... #+end_example` | Semantic preformatted block. |
| Quote block | `#+begin_quote ... #+end_quote` | Semantic quotation boundary and text. Visual quote styling is deferred per renderer. |
| Callout | `#+begin_callout` plus optional `#+ATTR_CALLOUT:` | Semantic admonition with type/title. Visual panel treatment is styling. |
| Standfirst | `#+begin_standfirst ... #+end_standfirst` | Semantic leading summary/standfirst for classes that support it. |
| Metric | `#+begin_metric ... #+end_metric` | Semantic metric block in Veriff-style documents. |
| Graph | `#+begin_graph ... #+end_graph` | Semantic graph block; layout/float treatment is styling. |

### Structure

| Semantic | Canonical Org source | Contract |
| --- | --- | --- |
| Headings | `*`, `**`, ... | Semantic hierarchy. Preserve levels where possible. |
| Section break | Org horizontal rule, e.g. `-----` | Semantic untitled transition or return to a wider section perspective. Renderers may style it as a dinkus, ornament, or degraded rule. |
| Paragraphs | Plain Org paragraphs | Semantic text flow. |
| Bullet lists | `- item` | Semantic unordered list. |
| Ordered lists | `1. item` | Semantic ordered list. |
| Definition lists | `- Term :: Definition` | Semantic term/definition pair. |
| Checkboxes | `- [ ]`, `- [X]`, `- [-]` | Semantic task state. Visual checkbox glyphs are styling. |
| Tables | Org tables | Semantic grid with optional header separator. Preserve cell text and header/body distinction where feasible. |

### Media

| Semantic | Canonical Org source | Contract |
| --- | --- | --- |
| Standalone image | Undescribed file link, e.g. `[[./img/foo.png]]` | Semantic embedded image. Targets should not degrade it to a literal path without warning. |
| Captioned figure | `#+CAPTION:` before standalone image | Semantic caption plus embedded image. Exact placement is styling. |
| Described image link | `[[./img/foo.png][Open image]]` | Semantic hyperlink, not an embedded image. |

## Backend expectations

### LaTeX/PDF

`modules/org/export-latex.el` currently carries the most complete renderer treatment. It defines class-specific behavior for:

- locale-aware dates;
- class-owned title/eyebrow/footer-note chrome;
- minted source blocks;
- class-owned inline code and emphasis macros;
- branded and article table wrappers;
- callout attributes;
- `hub-article` section-break ornaments from Org horizontal rules;
- standfirst placement validation;
- class-owned image wrappers;
- `hub-article` sidenotes, `HUB_NOTE_KIND: footnote` overrides, and marginalia metadata conventions;
- definition-list and checkbox rendering.

### Confluence

Confluence should preserve document semantics into storage XHTML when the platform has a reasonable equivalent. Existing exporter behavior is a useful reference for images, quote blocks, footnotes, tables, and callouts, but Confluence storage markup is not the canonical semantic model.

### Google Docs

Google Docs should implement this contract as far as its public APIs allow. Current priorities are:

1. classify all specimen constructs without silent loss;
2. preserve native footnotes where possible;
3. publish standalone local images as inline images;
4. preserve source-code block text and language identity, targeting native Google Docs code block building blocks where public APIs expose them, with syntax styling deferred;
5. preserve date/person/status semantics as explicit links/text first, importing remote dates as inactive Org timestamps, with smart chips or native mentions/dropdowns as native targets;
6. treat quote-block visible decoration as styling unless a reliable semantic round-trip is available.

## Specimen

The tracked specimen at `modules/org/specimens/typographic-semantics.org` is the living example of this contract. Exporter and sync tests should use it, or reduced fixtures derived from it, to verify semantic support and intentional degradation.
