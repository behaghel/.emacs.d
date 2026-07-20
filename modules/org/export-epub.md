# Spec: Org → EPUB Export

## Problem

Authors can already export Org documents to LaTeX/PDF and publish/sync selected semantics to other document targets, but there is no EPUB path for reflowable reading. EPUB export should let the same Org source produce a portable ebook while preserving the repository's backend-neutral typographic semantics where EPUB can represent them, making semantic loss explicit where it cannot.

## Context

- Domain: `authoring > documents` (`core`); specs for Org document export live under `modules/org/`.
- `modules/org/export-latex.el` is the architecture reference for integrating with Org export, handling metadata, routing generated artifacts, and keeping renderer-owned styling separate from authoring semantics.
- `modules/org/typographic-semantics.md` is the semantic contract. EPUB support must consume this contract rather than inventing a separate authoring dialect.
- `modules/org/specimens/typographic-semantics.org` is the living fixture for semantic coverage.
- `modules/org/README.md` requires document workflows to keep generated intermediates under managed paths in `var/` and to test export behavior from tracked specimens/textual assertions before visual inspection.
- Current LaTeX graph examples are LaTeX/TikZ pass-through inside `graph` blocks; there is no portable graph DSL yet.
- Pandoc is the packaging/compiler backend, but repository-owned code controls semantic XHTML generation, assets, CSS, preflight, chapter splitting, and author UX.
- `hub-article` can inspire the EPUB visual language, but EPUB rendering is owned by tracked CSS assets, not LaTeX classes, embedded fonts, or hard-coded inline styling.

## Decisions

| Decision | Choice | Rationale |
| --- | --- | --- |
| Export target | EPUB 3-compatible `.epub` artifact | EPUB 3 is the modern baseline for semantic HTML/CSS ebooks. |
| Pipeline | Org → repo-controlled XHTML/CSS/assets → Pandoc → EPUB | Prevents Pandoc from silently flattening repo-specific Org semantics. |
| Pandoc ownership | Runtime/system dependency, not added to this repo's `devenv.nix` | User's NixOS/Home Manager runtime owns Pandoc availability. |
| Pandoc discovery | `hub/org-epub-pandoc-executable` defcustom, falling back to `executable-find` | GUI Emacs may have a different `exec-path`; missing Pandoc gets an actionable error. |
| Pandoc invocation | Direct `process-file`/argument-list invocation | Avoid shell quoting issues with paths containing spaces. |
| Org integration | Derived Org/HTML backend plus orchestration command | Reuse Org export architecture while retaining control over multi-file EPUB packaging. |
| Module | `modules/org/export-epub.el`, providing `org/export-epub` | Mirrors existing Org export module layout. |
| Main command | `hub/org-epub-export-to-epub` | Repo-owned interactive command namespace. |
| Subtree export | Deferred | EPUB title page, cover, identifier, chapters, and TOC are book-level concerns. |
| Export dispatcher | Register with Org export dispatcher when technically reasonable | Improves discoverability and honors existing Org export architecture. |
| Strictness | Strict by default; explicit permissive/degraded mode | Semantic loss must be explicit; drafts can still export intentionally. |
| Preflight report | Structured internal data, human `*Org EPUB Preflight*` buffer, and `preflight.json` in work dir | Tests assert structure; users get readable diagnostics. |
| Styling owner | `etc/epub/reader.css` | Tracked renderer-owned CSS parallels `etc/latex/` without coupling to LaTeX. |
| Visual profile | One default v1 profile only | Avoid premature profile/configuration surface. |
| Visual target | Conservative cross-reader long-form article/essay reading | Kindle will matter soon; v1 manual smoke targets Apple Books first, Calibre second. |
| Fonts | Generic font families only; no embedded fonts | Maximizes reader compatibility/accessibility. |
| Color | Restrained, non-color-dependent styling | Reader themes/dark modes vary; semantics must not depend on color. |
| Body layout | Ragged-right, max-width around 40–44em | Better reflow on narrow readers; comfortable measure on wide readers. |
| Heading scale | Modest but editorial (`h1` ~1.9em, `h2` ~1.45em, `h3` ~1.2em) | Clear hierarchy without wasting reader screens. |
| Title page | Distinct generated title page, left-aligned editorial layout | Keeps document chrome separate from body chapters. |
| Title-page fields | Eyebrow, title, subtitle, author, date | Mirrors existing document chrome without inventing visible metadata. |
| Title-page metadata placement | Author/date directly under subtitle | Robust across readers; avoids fragile bottom positioning. |
| Cover | Optional `#+EPUB_COVER:` support in v1 | Cover support matters, but missing cover must not block export. |
| Cover page | If cover exists, use both EPUB cover metadata and first spine page | Ensures library cover and readable first page behavior. |
| Cover handling | Copy as-is, normalize copied name to `cover.<ext>`, no resizing | Avoid image-processing dependencies; CSS uses contain/no-crop display. |
| Standfirst | First document-level `standfirst` becomes its own page after title page and before TOC | Standfirst is semantic frontmatter, not ordinary body flow. |
| Standfirst multiplicity | Strict error on multiple standfirst blocks | V1 has only one lead/standfirst page. |
| Standfirst content | Paragraphs with inline markup/links only | Keeps frontmatter design controlled and brief. |
| Standfirst visual | Calm lead/abstract page, non-italic by default | Standfirst is not a pullquote. |
| TOC | Visible TOC page plus EPUB nav metadata | User wants TOC in reading flow; reader UI nav remains useful. |
| TOC depth | Visible TOC level 1 only; EPUB nav levels 1–3 | Clean in-book contents; richer reader navigation. |
| TOC title | `Contents` for English, `Table des matières` for French | Localized generated labels for common languages. |
| Generated labels | Small en/fr label table; unknown languages fall back to English with report | Keeps visible generated text respectful without building full i18n. |
| Chapter splitting | Split at level-1 Org headings by default | Matches ebook chapter expectations. |
| Split override | Reserve design space; do not implement v1 override | Avoid premature author-facing knobs. |
| Pre-heading body | If later level-1 chapters exist, generate visible `Introduction`; if no headings, create one body chapter without visible duplicate title | Avoid abrupt preface text while preventing title duplication in short essays. |
| Body title duplication | Do not repeat `#+TITLE:` as body H1 | Title page owns title semantics. |
| Chapter headings | Render each level-1 heading visibly as chapter title | Chapters are self-contained when opened directly. |
| Chapter numbering | No automatic numbering in v1 | Avoid duplicating author-supplied numbering; future CSS/option can add it. |
| Page breaks | Major sections use conservative CSS page-break hints | Title, standfirst, TOC, chapters, and footer note should behave as pages where readers honor it. |
| Footer note | `#+EXPORT_FOOTER_NOTE:` becomes backmatter at end on its own page | Footer note is semantic document note/chrome, not a footnote. |
| Final output root | `hub/org-epub-output-root`, default `~/Audiobooks/Hubert J. Behaghel/` | Finished EPUBs go to the user's personal library. |
| Final output layout | One title directory: `<root>/<Sanitized Title>/<Sanitized Title>.epub` | Leaves room for cover, metadata, and later Kindle artifacts. |
| Final adjacent files | Copy normalized cover if present and write `metadata.json`; do not copy source or intermediates | Keeps library useful without creating source drift. |
| Intermediates | Keep under `hub/org-epub-work-root`, default `var/org-epub/`, grouped by book stem and timestamped run | Debuggable without polluting final library. |
| Overwrite | Prompt interactively when final EPUB exists; auto-overwrite in batch/noninteractive | Safe interactive UX, convenient automation. |
| Metadata manifest | Write final `metadata.json` with compact metadata/preflight summary | Supports future Kindle conversion and stale-directory detection. |
| Identifier | Denote stem without tags when filename matches; otherwise require `#+EPUB_IDENTIFIER:` in strict mode | Stable across machines; tags are subjects, not identity. |
| Non-Denote permissive identifier | Permissive mode may generate temporary identifier with report | Draft export remains possible without stable identity. |
| Title fallback | `#+TITLE:`, then Denote title from filename, then strict failure | EPUB title is required and should be meaningful. |
| Author fallback | `#+AUTHOR:`, then `user-full-name`; strict failure if both empty | EPUB should have an author; inferred author is reported. |
| Publisher | `hub/org-epub-publisher`, default `Hubert J. Behaghel`, OPF metadata only | Useful catalog metadata; not visible on title page. |
| Subjects | Merge Denote tags and standard Org `#+KEYWORDS:` | Tags/keywords are metadata, not visible headings. |
| Description | `#+DESCRIPTION:`, else standfirst plain text, else omit | Explicit catalog description wins; standfirst is a good fallback. |
| Rights | Standard `#+COPYRIGHT:` maps to EPUB rights metadata only | Rights are not EPUB-specific; visible copyright belongs in body/footer note if desired. |
| Language | Pass through arbitrary language value to EPUB metadata/html; test en/fr | EPUB has no Babel-like package constraint. |
| Date | Display author-provided date string; write machine-readable metadata only when parseable | Avoid surprising locale reformatting in v1. |
| Callouts | `<aside>` with visible localized label/title and CSS-owned panel | Preserves admonition semantics portably. |
| Callout types | v1 allows `note`, `tip`, `warning`, `important`; default `note`; unknown type fails strict | Avoid silent semantic class drift. |
| Quote attribution | Attributed quotes render as `figure.quote > blockquote + figcaption` | Good semantic HTML shape for quote plus author. |
| Source blocks | Plain `pre > code` with language class; no syntax highlighting | Language is semantic; coloring is deferred styling. |
| Notes | Ordinary, forced footnotes, and marginalia all map to EPUB-native/readable endnotes or linked footnotes | Content and note relation survive; marginal placement is degraded but allowed. |
| Note placement | Chapter-local endnotes required if feasible; fallback only if Pandoc blocks it | Keeps notes near context. |
| Note backlinks | Required | EPUB note UX needs return links. |
| Section breaks | `hr.section-break` styled as quiet ornament/dinkus | Preserves transition semantics without default heavy rule. |
| Tables | Real HTML tables; report readability risk for wide tables | Preserve grid semantics while acknowledging small-screen risk. |
| Images | Copy local image assets as-is and constrain display with CSS | No resizing/transcoding in v1. |
| Remote standalone images | Strict failure; permissive may degrade to link | EPUB should be self-contained; no network fetch in v1. |
| Described image links | Remain hyperlinks, not embedded figures | Matches semantic contract. |
| Image alt text | `ATTR_EPUB :alt`, then caption, then filename fallback with report | Accessibility without inventing extra metadata. |
| Graphs | `ATTR_EPUB :image ./graph.png` fallback supported; package/render as graph figure | Current graph examples are LaTeX-only; EPUB needs explicit image fallback. |
| LaTeX-only graph content | Strict failure/degradation unless EPUB image fallback exists | Avoid silently dropping TikZ/PGFPlots content. |
| Placeholder graph prose | May render as degraded readable graph placeholder | Keeps specimen placeholder readable while reporting loss. |
| Metrics | `ATTR_EPUB :value` primary; `ATTR_LATEX :options [...]` compatibility fallback | EPUB should not depend on LaTeX attrs, but existing fixtures use them. |
| Metrics cluster | Render `metrics` as grouped section containing `metric` cards, stacked by default | Preserves KPI cluster semantics with Kindle-friendly layout. |
| `ATTR_EPUB` | Narrow v1 support for `:image`, `:value`, and `:alt` only | Backend-specific packaging/accessibility hints without styling knobs. |
| Raw export blocks | Strict v1 blocks raw HTML pass-through; LaTeX export blocks ignored outside semantic containers and unsupported inside them unless fallback exists | Keeps EPUB controlled and semantically accountable. |
| Internal links | Preserve headline/custom-ID links across split chapter files; broken links fail strict | Chapter splitting must not break navigation. |
| Drawers/properties | Omit from visible output; use specific properties for behavior only | Book output should not show authoring metadata. |
| Comments/planning/TODO/tags | Omit comments/planning; strip TODO keywords and tags from visible headings; informational report only | EPUB should read like a book, not an agenda export. |
| Local non-image links | Strict failure unless later specced | EPUB-packaged arbitrary files have unclear reader UX. |
| Embedded audio | Deferred; audio links treated like local non-image links | Audiobooks output path does not imply EPUB audio/media overlay support. |
| Success UX | Message final EPUB path only; no auto-open | Predictable, non-disruptive command behavior. |
| Open/reveal command | Deferred | Nice-to-have, not export correctness. |
| EPUB validation | No `epubcheck` dependency in v1; inspect package structure instead | Avoid Java/validator dependency; Pandoc should produce valid EPUB. |
| Manual smoke | Apple Books first, Calibre optional, Kindle deferred but future-friendly | Start simple while avoiding Kindle-hostile choices. |

## Acceptance Criteria

- [ ] AC-1: Given a metadata-rich Org file, when strict EPUB export runs, then title, subtitle, author, date, language, eyebrow, footer note, publisher, subjects, description, rights, and identifier are mapped according to the decisions above, with inferred/defaulted values reported.
- [ ] AC-2: Given an Org file with `#+EPUB_COVER:`, when EPUB export succeeds, then the final title directory contains the `.epub`, normalized `cover.<ext>`, and `metadata.json`; the EPUB has cover metadata and a first spine cover page using contain/no-crop styling.
- [ ] AC-3: Given an Org file without `#+EPUB_COVER:`, when strict EPUB export runs, then export succeeds and the preflight report records `cover: absent` informationally.
- [ ] AC-4: Given top-level Org headings and pre-heading body prose, when exporting, then output order is title page, optional standfirst page, visible TOC, optional visible Introduction, one chapter per level-1 heading, and optional footer-note backmatter page.
- [ ] AC-5: Given no level-1 headings, when exporting, then body prose becomes a single body chapter without repeating the title as a visible heading.
- [ ] AC-6: Given a first document-level `standfirst`, when exporting, then it becomes a standalone frontmatter page after title page and before TOC, is removed from normal body flow, and allows only paragraph/inline content in strict mode.
- [ ] AC-7: Given multiple `standfirst` blocks, when strict export runs, then export fails with an actionable preflight item.
- [ ] AC-8: Given English and French documents, when exporting, then generated visible labels use the en/fr table (`Contents` / `Table des matières`, etc.); unknown languages fall back to English with informational report.
- [ ] AC-9: Given ordinary Org structure from the typographic specimen, when exporting to EPUB, then headings, paragraphs, emphasis, inline code/verbatim, links, lists, definition lists, checkboxes, tables, horizontal rules, source blocks, example blocks, quote blocks, and images are represented as XHTML/EPUB content without literal Org syntax leakage.
- [ ] AC-10: Given `callout` blocks with allowed types and titles, when exporting, then they render as semantic asides with localized visible labels/titles and CSS-owned styling; unknown types fail strict export.
- [ ] AC-11: Given quote attribution metadata, when exporting, then attributed quotes render as `figure.quote` with `blockquote` and `figcaption`; unattributed quotes render as normal blockquotes.
- [ ] AC-12: Given ordinary notes, `HUB_NOTE_KIND: footnote`, and `HUB_NOTE_KIND: marginalia`, when exporting, then all become linked EPUB notes/endnotes with backlinks; marginalia placement degradation is reported but does not block strict export.
- [ ] AC-13: Given a standalone local image or captioned figure, when exporting, then the image asset is packaged and referenced relatively; described image links remain hyperlinks.
- [ ] AC-14: Given a standalone remote image URL, when strict export runs, then export fails; in permissive mode it may degrade to a hyperlink with report.
- [ ] AC-15: Given image or graph fallback alt metadata, captions, or neither, when exporting, then alt text follows the specified priority and filename fallback is reported.
- [ ] AC-16: Given a `graph` block with `#+ATTR_EPUB: :image ./graph.png`, when exporting, then the fallback image is packaged and rendered as a graph figure.
- [ ] AC-17: Given a `graph` block containing LaTeX-only export content and no EPUB image fallback, when strict export runs, then export fails or reports unsupported according to semantic-container handling.
- [ ] AC-18: Given `metric` and `metrics` blocks, when exporting, then metric values are read from `ATTR_EPUB :value` or compatibility `ATTR_LATEX :options`, and clusters render as grouped stacked metric cards.
- [ ] AC-19: Given source blocks, when exporting, then code renders as plain `pre > code` with language class and no syntax-highlighting dependency.
- [ ] AC-20: Given internal headline links and `CUSTOM_ID` links across split chapters, when exporting, then generated hrefs point to the correct chapter file and anchor; broken internal links fail strict export.
- [ ] AC-21: Given property drawers, TODO keywords, tags, comments, and planning lines, when exporting, then book output omits/strips them as specified without visible agenda metadata.
- [ ] AC-22: Given local non-image or audio file links, when strict export runs, then export fails with an actionable diagnostic.
- [ ] AC-23: Given missing Pandoc, when invoking EPUB export, then the user receives an actionable error mentioning `hub/org-epub-pandoc-executable` and runtime/system configuration.
- [ ] AC-24: Given an existing target EPUB, when exporting interactively, then the command prompts before overwriting; in batch/noninteractive, it overwrites automatically.
- [ ] AC-25: Given export success, when inspecting final outputs, then the final library path is `<hub/org-epub-output-root>/<Sanitized Title>/<Sanitized Title>.epub`, `metadata.json` exists, and intermediates remain under `hub/org-epub-work-root`.
- [ ] AC-26: Given export success, when inspecting generated EPUB/package inputs, then presentation comes from `etc/epub/reader.css` and not primary generated inline styling.
- [ ] AC-27: Given strict preflight success, degraded permissive success, and strict failure cases, when exporting interactively/batch, then structured report, human report, and `preflight.json` behavior match the UX decisions.
- [ ] AC-28: Given existing LaTeX/PDF export tests and batch load, when EPUB support is loaded, then existing LaTeX/PDF behavior and batch-load invariants still pass unchanged.

## Invariants

- Org authoring syntax remains backend-neutral; v1 adds only narrow EPUB-specific metadata/attributes where required for EPUB packaging/accessibility (`EPUB_COVER`, `EPUB_IDENTIFIER` fallback, `ATTR_EPUB` hints).
- `modules/org/typographic-semantics.md` remains the semantic source of truth.
- EPUB CSS owns EPUB presentation; LaTeX classes own PDF presentation.
- Runtime intermediates, unpacked inspection artifacts, and generated XHTML belong under `var/`.
- Final EPUB library artifacts belong under configurable output root, defaulting to `~/Audiobooks/Hubert J. Behaghel/`.
- Loading the EPUB module must be batch-safe and must not require Pandoc or touch the filesystem at load time.
- Missing Pandoc must fail with a clear user-facing diagnostic, not a raw process backtrace.
- Review sidecar comments are not typographic footnotes and must not be included as EPUB notes unless a later publishing spec explicitly requests that.
- No tracked generated EPUBs, unpacked EPUBs, screenshots, or visual diff artifacts.

## Scope

**May modify:**

- `modules/org/export-epub.el` or equivalent Org EPUB integration module.
- `modules/org/README.md` for cross-linking the EPUB spec/exporter.
- `modules/org/typographic-semantics.md` only to add an EPUB backend expectations section or clarify classification vocabulary.
- `modules/org/specimens/` only for reduced EPUB fixtures when the full specimen is too broad for a focused test.
- `etc/epub/reader.css` and related tracked EPUB static assets.
- `test/authoring/documents/*epub*test.el` and focused fixtures.
- Test helpers/stubs needed for Pandoc invocation and XHTML/package inspection.

**Must not modify:**

- Existing LaTeX class behavior or `etc/latex/` visual rules except for test isolation if strictly required.
- Google Docs or Confluence exporter behavior unless a shared helper extraction is separately specced.
- This repo's `devenv.nix` to add Pandoc for v1; Pandoc is a runtime/system dependency outside this repo.
- Sidecar review comments semantics.
- Generated EPUB files, unpacked EPUB directories, screenshots, or visual inspection artifacts as tracked golden files.

## Verification Plan

| Criterion | Method | Automated? |
| --- | --- | --- |
| AC-1 | ERT fixture exports metadata-rich Org and inspects generated metadata object, OPF/Pandoc metadata inputs, title page XHTML, and report. | Yes |
| AC-2, AC-3 | ERT covers cover-present and cover-absent paths; package/work-dir inspection verifies normalized cover copy and metadata behavior. | Yes |
| AC-4, AC-5 | ERT inspects generated spine-order list and XHTML files for heading/pre-heading scenarios. | Yes |
| AC-6, AC-7 | ERT covers single/multiple standfirst extraction and body-flow removal. | Yes |
| AC-8 | ERT covers en/fr/unknown generated labels. | Yes |
| AC-9 | ERT exports reduced fixture(s) from `typographic-semantics.org` and asserts no literal Org syntax leakage plus expected XHTML fragments. | Yes |
| AC-10, AC-11 | ERT asserts callout and attributed quote XHTML shapes. | Yes |
| AC-12 | ERT covers ordinary, forced footnote, and marginalia metadata paths plus backlinks/degradation report. | Yes |
| AC-13–AC-15 | ERT validates local image packaging, described-link behavior, remote-image strict failure, and alt priority. | Yes |
| AC-16, AC-17 | ERT covers graph fallback image, placeholder prose degradation, and LaTeX-only graph strict failure. | Yes |
| AC-18 | ERT covers metric value extraction from `ATTR_EPUB` and compatibility `ATTR_LATEX`, plus `metrics` grouping. | Yes |
| AC-19 | ERT asserts source block language class and no highlighter dependency. | Yes |
| AC-20 | ERT covers cross-chapter headline/custom-ID links and broken-link preflight failure. | Yes |
| AC-21, AC-22 | ERT covers property/TODO/tag/comment/planning stripping and local non-image/audio strict failure. | Yes |
| AC-23 | ERT stubs Pandoc discovery/invocation failure and expects actionable `user-error`. | Yes |
| AC-24, AC-25 | ERT stubs interactive/batch overwrite behavior and verifies final/work path layout. | Yes |
| AC-26 | Package/input inspection asserts `etc/epub/reader.css` is copied/referenced and no primary inline styling is generated. | Yes |
| AC-27 | ERT asserts structured report data, human renderer content, and `preflight.json` artifact. | Yes |
| AC-28 | Run focused existing export tests and load check after implementation. | Yes |
| Manual smoke | Open generated EPUB in Apple Books; optionally inspect in Calibre. Verify cover/title/standfirst/TOC/chapter/footer order, notes/backlinks, and readable CSS in normal reader settings. | No |

## Open Questions

1. Should permissive mode be exposed as a prefix argument on `hub/org-epub-export-to-epub`, a separate command, or both?
2. What exact schema/version should final `metadata.json` use?
3. If Pandoc rewrites internal EPUB paths, which internal package paths should tests treat as stable vs implementation-owned?
4. Should chapter-local endnotes be implemented before first Pandoc integration, or accepted as a fallback risk until the packaging slice proves feasibility?

## References

- `modules/org/export-latex.el`
- `modules/org/typographic-semantics.md`
- `modules/org/specimens/typographic-semantics.org`
- `modules/org/README.md`
- `spec/org-latex-pdf/semantic-layer.md`
