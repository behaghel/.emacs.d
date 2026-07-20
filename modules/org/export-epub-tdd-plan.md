# Iteration Plan: Org → EPUB Export

**Source spec:** `modules/org/export-epub.md`
**Vertical-slice rule:** every iteration keeps the user path centered on `M-x hub/org-epub-export-to-epub` from an Org buffer, producing observable files/reports or an actionable error. Unit helpers may be introduced inside a slice, but no slice is considered done unless the command-level path works.

## Iteration Table

| # | Slice Goal | User Interaction Path | Tests to Write First | Expected Red Signal | Minimal Green Target | Feedback Checkpoint |
|---|-----------|----------------------|---------------------|--------------------|--------------------|-------------------|
| 1 | Export the smallest book-shaped EPUB input through the command using a stubbed Pandoc invocation. | Open a simple Org file with title/author/body, run `M-x hub/org-epub-export-to-epub`, inspect final title directory and generated work XHTML. | `hub/org-epub-export-minimal-book-writes-title-directory`; `hub/org-epub-export-minimal-book-generates-title-toc-body-spine`; `hub/org-epub-export-invokes-pandoc-with-argument-list`. | `require` fails because `org/export-epub` does not exist; command undefined. | New module, defcustoms, command, metadata extraction for title/author, work dir, title page, visible TOC, one body XHTML, CSS copy placeholder, stub-friendly Pandoc invocation, final `.epub` placeholder/move behavior. | Show generated directory tree and minimal XHTML order; ask if the basic book shape feels right. |
| 2 | Resolve EPUB identity, title, author, output paths, overwrite behavior, and final metadata manifest. | Export Denote and non-Denote Org files; inspect `<root>/<Title>/<Title>.epub` and `metadata.json`. | `hub/org-epub-denote-stem-provides-identifier-title-subjects`; `hub/org-epub-requires-epub-identifier-without-denote`; `hub/org-epub-infers-author-from-user-full-name`; `hub/org-epub-prompts-before-interactive-overwrite`; `hub/org-epub-metadata-json-summarizes-export`. | Minimal slice uses hard-coded/weak metadata and no manifest/overwrite handling. | Denote stem parser, sanitized title dirs/files, `EPUB_IDENTIFIER` fallback, `user-full-name` fallback, metadata schema v1, interactive overwrite prompt vs batch overwrite. | Show two exports: Denote-derived and explicit identifier; verify library path and metadata shape. |
| 3 | Add frontmatter/backmatter structure: optional cover, standfirst page, localized TOC, footer-note page. | Export an Org file with cover, `standfirst`, language, and footer note; inspect spine order and copied cover. | `hub/org-epub-cover-is-packaged-and-visible-first-page`; `hub/org-epub-cover-absent-is-informational`; `hub/org-epub-standfirst-becomes-frontmatter-and-is-removed-from-body`; `hub/org-epub-multiple-standfirsts-fail-strict`; `hub/org-epub-footer-note-is-backmatter-page`; `hub/org-epub-localizes-visible-toc-title`. | No cover/standfirst/footer-specific pages; labels not localized. | Parse `EPUB_COVER`, copy `cover.<ext>`, generate cover page, extract one standfirst with content validation, localize label table, generate footer-note XHTML, update spine order/report. | Show ordered pages: cover → title → standfirst → TOC → body/chapter → footer. |
| 4 | Split body content into chapters and preserve basic navigation links. | Export a file with pre-heading prose, level-1/2/3 headings, and internal links; inspect chapter files and hrefs. | `hub/org-epub-splits-level-one-headings-into-chapters`; `hub/org-epub-preheading-prose-becomes-visible-introduction-only-before-chapters`; `hub/org-epub-no-heading-document-does-not-repeat-title`; `hub/org-epub-nav-depth-includes-levels-one-to-three`; `hub/org-epub-rewrites-headline-and-custom-id-links-across-chapters`; `hub/org-epub-broken-internals-fail-strict`. | Body is one file; links point nowhere or title repeats. | Chapter planner, generated introduction rules, visible TOC level 1, nav model levels 1–3, stable anchors, cross-file internal href rewrite, broken-link preflight. | Show multi-chapter XHTML and TOC/nav model; ask if chapter/read flow matches expectation. |
| 5 | Render core Org semantics from a reduced typographic fixture into controlled XHTML. | Export fixture with paragraphs, inline markup, lists, tables, code/example, quotes, horizontal rules, images; inspect XHTML. | `hub/org-epub-renders-basic-inline-and-block-org-semantics`; `hub/org-epub-source-blocks-use-pre-code-language-class`; `hub/org-epub-section-breaks-use-hr-section-break`; `hub/org-epub-tables-remain-html-tables-and-wide-risk-is-reported`; `hub/org-epub-omits-drawers-comments-planning-and-strips-todo-tags`. | Org syntax leaks or default exporter behavior is uncontrolled. | Derived HTML backend/transcoders or controlled tree renderer for common semantics; cleanup filters for TODO/tags/drawers; wide-table report. | Show specimen excerpt XHTML and report; confirm baseline reader semantics. |
| 6 | Add CSS-owned rendering and conservative visual asset contract. | Export any successful book; inspect copied `reader.css`, XHTML class hooks, and absence of primary inline style. | `hub/org-epub-copies-reader-css-from-etc-epub`; `hub/org-epub-xhtml-references-reader-css`; `hub/org-epub-reader-css-uses-generic-fonts-and-page-break-hints`; `hub/org-epub-generated-xhtml-avoids-primary-inline-styles`. | No CSS asset exists; styling is absent or inline. | Create `etc/epub/reader.css`; link it from generated XHTML; add hooks/classes for pages, headings, callouts, metrics, notes; keep CSS conservative. | Show CSS excerpts and one rendered XHTML page; ask if visual language direction is acceptable before richer semantics. |
| 7 | Preserve semantic custom blocks: callouts, attributed quotes, metrics, and graph image fallbacks. | Export fixture with allowed/unknown callouts, quote attribution, metrics cluster, graph fallback image, graph placeholder, LaTeX-only graph. | `hub/org-epub-callout-renders-aside-with-localized-label-title`; `hub/org-epub-unknown-callout-type-fails-strict`; `hub/org-epub-attributed-quote-renders-figure`; `hub/org-epub-metric-value-prefers-attr-epub-with-latex-options-fallback`; `hub/org-epub-metrics-cluster-renders-stacked-group`; `hub/org-epub-graph-image-fallback-packages-figure`; `hub/org-epub-latex-only-graph-fails-strict`. | Custom blocks are flattened or ignored. | Transcoders/preflight for `callout`, `quote`, `metric(s)`, `graph`; narrow `ATTR_EPUB` parser for `:value`, `:image`, `:alt`. | Show each semantic block's XHTML and degradation/failure report behavior. |
| 8 | Package local images accessibly and reject unsupported links/media. | Export images with captions/alt, described image links, remote standalone image, local non-image/audio links. | `hub/org-epub-packages-local-standalone-and-captioned-images`; `hub/org-epub-described-image-link-stays-hyperlink`; `hub/org-epub-alt-priority-uses-attr-caption-filename-report`; `hub/org-epub-remote-standalone-image-fails-strict`; `hub/org-epub-local-non-image-and-audio-links-fail-strict`. | Images are not copied or unsupported links pass silently. | Asset collector, relative refs, alt priority/reporting, strict link classifiers, permissive degradation for remote images if mode exists by then. | Show asset manifest and image XHTML; confirm accessibility/packaging tradeoffs. |
| 9 | Implement notes/endnotes with backlinks, including marginalia degradation reporting. | Export chapter(s) with ordinary footnotes, `HUB_NOTE_KIND: footnote`, and `marginalia`; click/inspect note refs/backlinks. | `hub/org-epub-ordinary-footnotes-render-linked-endnotes`; `hub/org-epub-forced-footnotes-render-linked-endnotes`; `hub/org-epub-marginalia-renders-endnote-with-degradation-report`; `hub/org-epub-notes-include-backlinks`; `hub/org-epub-prefers-chapter-local-endnotes`. | Notes are default HTML footnotes, no backlinks, marginalia silent. | Controlled note collection per chapter where feasible, backlinks, note-kind metadata parser, report degradation for marginalia placement. | Show one chapter with note refs/endnotes/backlinks; decide whether chapter-local behavior is good enough before Pandoc integration. |
| 10 | Add strict/permissive preflight UX and batch artifacts. | Run strict failure, permissive degraded export, and batch export; inspect `*Org EPUB Preflight*` and `preflight.json`. | `hub/org-epub-strict-failure-opens-human-preflight-buffer`; `hub/org-epub-permissive-export-continues-with-degradations`; `hub/org-epub-batch-writes-preflight-json`; `hub/org-epub-report-contains-status-counts-and-items`; `hub/org-epub-success-message-includes-final-path-only`. | Failures are raw errors or reports are ad hoc strings. | Report object, JSON writer, human renderer, prefix/separate permissive entry decision, user-facing messages. | Show strict/permissive report examples; ask if diagnostics are actionable. |
| 11 | Invoke real Pandoc when available and inspect generated EPUB package. | Export a representative small book with actual Pandoc; unzip/inspect EPUB package structure. | `hub/org-epub-real-pandoc-test-skips-when-pandoc-missing`; `hub/org-epub-missing-pandoc-errors-actionably`; `hub/org-epub-real-package-contains-css-cover-xhtml-metadata-nav`; `hub/org-epub-pandoc-receives-spine-in-order`. | Stub-only implementation; missing Pandoc may backtrace; package not generated. | Real Pandoc runner, metadata input, CSS/cover args, package inspection helper, skip real-Pandoc integration when missing. | Produce a real `.epub`; optionally manual-open in Apple Books. |
| 12 | Harden against regressions and integrate cleanly with existing Org export architecture. | Load full config, use Org export dispatcher if implemented, run focused existing LaTeX/PDF tests. | `hub/org-epub-backend-has-dispatch-menu`; load-check smoke; existing `org-latex-pdf-export-test.el`; full `org-epub-export-test.el`. | Module not loaded/registered; existing export behavior breaks. | Dispatcher registration, module load wiring, batch-safe load, test pruning/refactor, README/typographic-semantics cross-links. | Final acceptance demo: real EPUB, tests green, manual smoke checklist ready. |

## Planned Test Files

- Primary new ERT file: `test/authoring/documents/org-epub-export-test.el`.
- Optional focused fixtures: `test/fixtures/org-export/epub-*.org` only when inline test buffers become unreadable.
- Static CSS asset: `etc/epub/reader.css` introduced in Iteration 6.
- Implementation module: `modules/org/export-epub.el`.

## Execution Discipline

- Do not implement future semantic handling before its iteration's tests are red.
- Keep Pandoc invocation stubbed until Iteration 11; all earlier slices must still exercise `hub/org-epub-export-to-epub` end-to-end through a stubbed process boundary.
- Prefer inspecting generated XHTML/work directories over golden EPUB binaries.
- Run focused test command after each iteration, e.g. `devenv -q shell -- env HOME=$PWD emacs --batch -Q -L . -l ert -l test/test-helpers.el -l test/authoring/documents/org-epub-export-test.el -f ert-run-tests-batch-and-exit`.
- After touching module load wiring or integration points, also run `devenv -q shell -- env HOME=$PWD emacs --batch -l init.el --eval '(message "Loaded")' --kill`.

## Open Plan Risks

- Chapter-local endnotes may conflict with Pandoc packaging behavior; Iteration 9 should settle local XHTML behavior before Iteration 11 validates real packaging.
- Derived `html` backend may be insufficient for multi-file output; if so, keep Org parsing/transcoding helpers but let the command own file splitting explicitly.
- Internal package paths inside Pandoc-generated EPUB may be rewritten; tests should assert semantic presence unless path stability is explicitly controlled.
- The plan is intentionally broad; if any iteration becomes too large, split it before coding rather than skipping red-green-refactor.

## Execution Log

### Iteration 1: Minimal book-shaped export through stubbed Pandoc

#### Red

- Tests written:
  - `hub/org-epub-export-minimal-book-writes-title-directory`
  - `hub/org-epub-export-minimal-book-generates-title-toc-body-spine`
  - `hub/org-epub-export-invokes-pandoc-with-argument-list`
- Run result: failed because `org/export-epub` did not exist.

#### Green

- Added `modules/org/export-epub.el` with the first `hub/org-epub-export-to-epub` path.
- Added minimal metadata extraction, output/work directory creation, generated `titlepage.xhtml`, `toc.xhtml`, `body.xhtml`, and direct Pandoc process invocation.
- Added `test/authoring/documents/org-epub-export-test.el` with a stubbed `process-file` boundary.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 2: Identity, title, author, output paths, and metadata manifest

#### Red

- Tests written:
  - `hub/org-epub-denote-stem-provides-identifier-title-subjects`
  - `hub/org-epub-requires-epub-identifier-without-denote`
  - `hub/org-epub-infers-author-from-user-full-name`
  - `hub/org-epub-prompts-before-interactive-overwrite`
- Run result: failed on missing Denote title/identifier fallback, missing author fallback, and absent overwrite prompt.

#### Green

- Added minimal Denote filename parsing for title, stable identifier, and tags-as-subjects.
- Added `KEYWORDS` subject merging, `user-full-name` author fallback, strict non-Denote identifier requirement, and final `metadata.json` generation.
- Added interactive overwrite confirmation while preserving batch/noninteractive overwrite behavior.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 3: Cover, standfirst, localized TOC, and footer note

#### Red

- Tests written:
  - `hub/org-epub-cover-is-packaged-and-visible-first-page`
  - `hub/org-epub-cover-absent-is-informational`
  - `hub/org-epub-standfirst-becomes-frontmatter-and-is-removed-from-body`
  - `hub/org-epub-multiple-standfirsts-fail-strict`
  - `hub/org-epub-footer-note-is-backmatter-page`
  - `hub/org-epub-localizes-visible-toc-title`
- Run result: failed on missing cover copy/page, absent cover metadata, missing standfirst/footer pages, missing multiple-standfirst validation, and unlocalized TOC.

#### Green

- Added optional `EPUB_COVER` support with normalized `cover.<ext>` copy to final/work directories and first spine cover page.
- Added cover absence metadata, single standfirst extraction/removal, multiple standfirst strict failure, footer-note backmatter page, and en/fr TOC label selection.
- Updated spine ordering for cover/title/standfirst/TOC/body/footer-note.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 4: Chapter splitting and basic navigation links

#### Red

- Tests written:
  - `hub/org-epub-splits-level-one-headings-into-chapters`
  - `hub/org-epub-preheading-prose-becomes-visible-introduction-only-before-chapters`
  - `hub/org-epub-no-heading-document-does-not-repeat-title`
  - `hub/org-epub-rewrites-headline-links-across-chapters`
  - `hub/org-epub-broken-internals-fail-strict`
- Run result: failed on one-body-file output, missing introduction/chapter files, unrevised internal links, and no broken-link strict failure.

#### Green

- Added a level-one chapter planner using Org headline positions.
- Added introduction generation for pre-heading prose before chapters, no-title-repeat behavior for no-heading documents, chapter XHTML files with stable slug anchors, and simple cross-chapter headline link rewriting.
- Added strict failure for broken simple headline links.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 5: Core Org semantics from controlled XHTML

#### Red

- Tests written:
  - `hub/org-epub-renders-basic-inline-and-block-org-semantics`
  - `hub/org-epub-source-blocks-use-pre-code-language-class`
  - `hub/org-epub-omits-drawers-comments-planning-and-strips-todo-tags`
- Run result: failed on literal Org syntax leakage for inline markup/lists/tables/quotes/rules and source blocks.

#### Green

- Switched body/chapter content rendering to Org's HTML exporter in body-only mode, with EPUB postprocessing.
- Added semantic postprocessing for `strong`/`em`, section-break rules, and plain `pre > code` source blocks with language classes.
- Preserved cleanup of TODO keywords, tags, comments, planning lines, and drawers through export configuration.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 6: CSS-owned rendering and conservative visual assets

#### Red

- Tests written:
  - `hub/org-epub-copies-reader-css-from-etc-epub`
  - `hub/org-epub-xhtml-references-reader-css`
  - `hub/org-epub-reader-css-uses-generic-fonts-and-page-break-hints`
  - `hub/org-epub-generated-xhtml-avoids-primary-inline-styles`
- Run result: failed because `etc/epub/reader.css` did not exist, generated XHTML did not link a stylesheet, and work directories did not receive CSS.

#### Green

- Added tracked `etc/epub/reader.css` with conservative generic-font, page-break, heading, cover, quote, code, table, and section-break rules.
- Added module-owned reader CSS source resolution, work-dir CSS copy, and stylesheet links in generated XHTML documents.
- Preserved no-primary-inline-style behavior for generated wrapper XHTML.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 7: Semantic custom blocks

#### Red

- Tests written:
  - `hub/org-epub-callout-renders-aside-with-localized-label-title`
  - `hub/org-epub-unknown-callout-type-fails-strict`
  - `hub/org-epub-attributed-quote-renders-figure`
  - `hub/org-epub-metric-value-prefers-attr-epub-with-latex-options-fallback`
  - `hub/org-epub-graph-image-fallback-packages-figure`
  - `hub/org-epub-latex-only-graph-fails-strict`
- Run result: failed because Org HTML flattened custom semantics into generic `div`/`blockquote` output and did not package graph fallback images.

#### Green

- Added semantic replacement pass before Org HTML export for attributed quotes, callouts, metrics, and graph blocks.
- Added strict unknown-callout and LaTeX-only graph failures.
- Added `ATTR_EPUB` handling for metric values and graph image/alt fallback, plus compatibility fallback for metric `ATTR_LATEX :options`.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 8: Image assets and unsupported link policy

#### Red

- Tests written:
  - `hub/org-epub-packages-local-standalone-and-captioned-images`
  - `hub/org-epub-described-image-link-stays-hyperlink`
  - `hub/org-epub-alt-priority-uses-attr-caption-filename-report`
  - `hub/org-epub-remote-standalone-image-fails-strict`
  - `hub/org-epub-local-non-image-and-audio-links-fail-strict`
- Run result: failed because image files were not copied, alt text used Org defaults, and unsupported remote/local links passed silently.

#### Green

- Added local standalone/captioned image copying into the work directory and HTML image reference rewriting.
- Added alt priority from `ATTR_EPUB :alt`, caption text, then filename fallback.
- Preserved described image links as hyperlinks and added strict failures for standalone remote images plus local non-image/audio file links.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 9: Notes/endnotes with backlinks

#### Red

- Tests written:
  - `hub/org-epub-ordinary-footnotes-render-linked-endnotes`
  - `hub/org-epub-forced-footnotes-render-linked-endnotes`
  - `hub/org-epub-marginalia-renders-endnote-with-degradation-report`
  - `hub/org-epub-notes-include-backlinks`
  - `hub/org-epub-prefers-chapter-local-endnotes`
- Run result: failed because Org's default HTML footnotes used `fn.*` IDs/classes, leaked `HUB_NOTE_KIND`, and did not emit a marginalia degradation artifact.

#### Green

- Added EPUB note postprocessing for Org footnotes into endnote hooks with `noteref-*`/`note-*` IDs and backlink classes.
- Added internal `HUB_NOTE_KIND` line stripping before Org HTML export.
- Added marginalia degradation collection and a work-dir `notes-report.json` artifact.
- Preserved chapter-local footnote placement through existing per-chapter Org HTML export.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 10: Strict/permissive preflight UX and batch artifacts

#### Red

- Tests written:
  - `hub/org-epub-strict-failure-opens-human-preflight-buffer`
  - `hub/org-epub-permissive-export-continues-with-degradations`
  - `hub/org-epub-batch-writes-preflight-json`
  - `hub/org-epub-report-contains-status-counts-and-items`
  - `hub/org-epub-success-message-includes-final-path-only`
- Run result: failed because strict failures only raised raw `user-error`s, permissive mode did not exist, and no structured `preflight.json` artifact was written.

#### Green

- Added dynamic preflight item collection, JSON report writing, status/count calculation, and a human `*Org EPUB Preflight*` buffer for strict failures.
- Added optional permissive argument/prefix support for `hub/org-epub-export-to-epub`.
- Added permissive degradation for standalone remote images into ordinary hyperlinks with a warning report item.
- Promoted marginalia degradation into the structured preflight report while preserving the existing `notes-report.json` artifact.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 11: Real Pandoc package smoke checks

#### Red

- Tests written:
  - `hub/org-epub-real-pandoc-test-skips-when-pandoc-missing`
  - `hub/org-epub-missing-pandoc-errors-actionably`
  - `hub/org-epub-real-package-contains-css-cover-xhtml-metadata-nav`
  - `hub/org-epub-pandoc-receives-spine-in-order`
- Run result: initial package-inspection assertions were too filename-specific because Pandoc canonicalizes EPUB resource names (`stylesheet1.css`, media filenames) while preserving resource classes.

#### Green

- Kept the existing direct Pandoc runner and actionable missing-Pandoc error path.
- Added real-Pandoc smoke coverage that skips when Pandoc/unzip are unavailable, verifies a non-empty EPUB, and inspects the archive for stylesheet, media cover asset, OPF metadata, nav XHTML, and generated XHTML content.
- Added a spine-order assertion at the process boundary, including generated Introduction when pre-heading keyword/preamble content creates one.

#### Refactor

- Ran formatter, checkdoc, parser, and focused ERT for the touched files.
- No spec changes required.

### Iteration 12: Final integration and load/regression checks

#### Red

- Tests written:
  - `hub/org-epub-backend-has-dispatch-menu`
- Run result: before registration, `org/export-epub` had a command but no Org export backend menu entry and was not loaded by the interactive init path.

#### Green

- Registered a lightweight derived Org export backend `hub-epub` with an EPUB dispatcher menu entry that calls `hub/org-epub-export-to-epub`.
- Added `org/export-epub` to the interactive init load path alongside `org/export-latex`.
- Cross-linked the EPUB spec/command from `modules/org/README.md`.

#### Refactor

- Ran formatter, checkdoc, parser, focused EPUB ERT, existing `org-export-test.el`, and batch init load.
- Attempted `org-latex-pdf-export-test.el`; it failed before EPUB code on missing `org-comments` load-path under that isolated command.
