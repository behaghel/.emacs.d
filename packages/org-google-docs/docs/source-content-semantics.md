# Spec: Google Docs Source Content Semantics

## Problem

Org ↔ Google Docs body sync currently depends on upstream `benthamite/gdocs`, but real documents such as `/Users/hubertbehaghel/tmp/gdocs-test.org` expose semantic gaps for the repository's typographic semantics contract. Styling differences are expected and out of scope for this epic. The goal is to preserve authoring semantics for footnotes, images, source blocks, dates, people mentions, and other Org-native constructs without forking unnecessarily or duplicating Confluence-specific behavior.

## Context

- `packages/org-google-docs/` is the local adapter and should stay the home for wrapper/adaptation code.
- Upstream body sync lives in `straight/repos/gdocs/` and exposes an Org ↔ IR ↔ Google Docs request pipeline in `gdocs-convert.el`.
- `gdocs-convert-org-buffer-to-ir` already parses Org with `org-element` and creates an intermediate representation.
- The backend-neutral contract lives in `modules/org/typographic-semantics.md`, with a tracked specimen at `modules/org/specimens/typographic-semantics.org`.
- `modules/org/typographic-semantics.el` audits Org buffers into a plain semantic inventory; `packages/org-google-docs/org-google-docs-semantics.el` classifies that inventory against current Google Docs support without depending on local `hub-*` helpers.
- `packages/org-google-docs/org-google-docs-footnotes.el` extracts a push-safe native-footnote plan before any remote mutation. It supports named definitions in a conventional footnotes section, reports rich-body/repeated-reference degradations, blocks unsupported forms such as anonymous inline footnotes, missing definitions, marginalia, mixed footnotes-section content, or definitions outside the conventional section, and filters the Org-only footnotes section out of push-time IR when native footnotes are active.
- Native Google Docs footnote mutation needs two batchUpdate phases: first `createFootnote` at exact UTF-16 document indices, then `insertText` into the returned footnote segment IDs. The local request helpers implement this choreography and create footnotes from highest body index to lowest so repeated/multiple references do not shift later insertion points.
- The local upstream `gdocs-convert.el` seam preserves Org footnote references as zero-width semantic runs and calls `gdocs-convert-footnote-reference-handler` with each run's exact Google Docs UTF-16 body index during request generation, avoiding placeholder text search.
- The local upstream `gdocs-diff.el` seam treats those zero-width footnote runs as semantic paragraph changes and emits the footnote-reference request hook for formatting-only changes where visible text is unchanged.
- The seam is carried in a local `~/ws/gdocs` fork on branch `org-footnote-seam`; activation prefers that checkout and falls back to upstream only when the local fork is absent, so bootstrap no longer depends on mutating `straight/repos/gdocs` directly.
- The sample and specimen currently reveal:
  - Org footnote references become literal text such as `[fn:1]` in paragraphs.
  - Footnote definitions become separate `:type 'footnote` IR elements, but push emits them as literal `[fn:N] body` text rather than native Google Docs footnotes.
  - Plain local image links become plain text like `file:./veriff2026-logo.png`; imported Google Docs inline images become `:type 'image` IR elements that render back to empty Org text.
  - Source blocks must preserve code text and language identity. Current v1 support stores the source block language in a semantic named-range marker and attaches logical `gdocs-code` / `gdocs-source-block` styles before late-binding them to Google Docs properties. Google Docs code block building blocks remain the native visual target only if public APIs expose a reliable seam. Syntax highlighting is styling.
  - Dates, people mentions, and statuses need explicit semantic classification before deciding whether native smart chips, mentions, or dropdowns are feasible. Remote dates should import as inactive Org timestamps by default to avoid polluting `org-agenda`.
  - Quote blocks become `:style 'quote` paragraph IR and round-trip to Org quote blocks, but Google Docs request generation maps unknown styles to normal text. Visible quote decoration is therefore styling-deferred unless a reliable Docs semantic is found.
- Confluence provides useful patterns, not a storage model to copy:
  - image asset discovery and path validation in `org-confluence-export.el`;
  - standalone image-link detection and captions;
  - quote block semantic export;
  - footnote reference/definition handling.

## Decisions

| Decision | Choice | Rationale |
| --- | --- | --- |
| Upstream strategy | Wrap first, add upstream seams second, fork only if blocked | Keeps `benthamite/gdocs` usable as transport/sync engine while allowing local progress. |
| Primary package boundary | Put Google-specific semantic adapters in `packages/org-google-docs/` | Keeps activation thin and avoids editing vendored upstream until a seam is proven necessary. |
| Reuse strategy | Extract tiny provider-neutral helpers only when Confluence and Google genuinely share Org-level semantics | Avoids forcing Google Docs into Confluence storage/XHTML concepts. |
| Epic scope | Typographic contract semantics: footnotes, standalone local images, source blocks, dates, people mentions, statuses, and explicit degradation for other specimen constructs | Styling, callout visual design, exact spacing, typography, and rich layout are later epics. |
| Footnote target | Round-trip semantic preservation | Footnotes are a first-class Org semantic and native Google Docs footnotes exist. |
| Image target | Publish-first for standalone local images; pull preservation only where the API exposes enough data | Google Docs image insertion/upload and reverse mapping are trickier than text semantics. |
| Quote target | Preserve quote-block boundaries on round trip; treat visible quote formatting as styling unless Google exposes a reliable semantic equivalent | Upstream already preserves Org quote IR; request generation is the likely styling gap. |
| Upstream modifications | Prefer small, upstreamable hooks/IR extensions over large local monkey patches | The current IR pipeline is a natural extension point; exact footnote reference indices should come from conversion/diff, not placeholder searches. |
| Style architecture | Emit logical style names in `gdocs` and resolve them late to Google Docs properties | Google Docs exposes only fixed named styles, not arbitrary custom style names. Semantic conversion must not hardcode personal typography; local authoring modules can override logical style definitions. |

## Acceptance Criteria

- [x] AC-0: Given an Org buffer with named footnotes in a conventional `Footnotes` or `Notes de bas de page` section, when footnote preflight runs, then it returns a native footnote push plan with references, plain text bodies, section metadata, blocking diagnostics, and degradation notices before remote mutation.
- [x] AC-0a: Given planned footnote references with exact Google Docs document indices, when native request planning runs, then it creates `createFootnote` requests in descending index order and second-phase body `insertText` requests from returned footnote IDs without relying on placeholder text search.
- [x] AC-0b: Given an Org paragraph containing a footnote reference, when upstream `gdocs-convert` builds paragraph runs and requests, then the reference is represented as a zero-width semantic run and the footnote handler receives the exact Docs insertion index rather than literal `[fn:]` text.
- [x] AC-1: Given an Org paragraph containing `[fn:1]`, when pushed through the Google Docs adapter, then the Google Doc contains a native footnote reference at the corresponding text position rather than literal `[fn:1]` body text.
- [x] AC-2: Given an Org footnote definition `[fn:1] Body`, when pushed, then the native Google Docs footnote contains `Body` as plain text; supported inline text semantics inside footnotes are a documented follow-up degradation.
- [x] AC-2a: Given a conventional Org footnotes section is converted to native Google Docs footnotes, when push-time IR is generated, then the `Footnotes`/`Notes de bas de page` heading and footnote-definition IR are omitted from the Google Docs body without modifying the source Org buffer.
- [x] AC-2b: Given visible paragraph text changes and multiple zero-width footnote runs, when upstream diff generates requests, then it emits footnote-reference seam requests from the word-level modification path so native footnotes are not dropped.
- [x] AC-3: Given a Google Doc with native footnotes, when converted by the local `gdocs` pull seam, then the Org buffer contains corresponding ordinary footnote references and definitions under a conventional `* Footnotes` section without losing surrounding paragraph text.
- [x] AC-4: Given a standalone local image link such as `[[./img/foo.png]]`, when pushed, then the Google Doc receives an inline image instead of literal `file:./img/foo.png` text. The adapter uploads readable standalone images to Drive, grants an anyone-reader permission so Docs can fetch them, enriches image IR with a direct download URI, and relies on the upstream image request seam for `insertInlineImage`.
- [x] AC-4a: Given a Google Doc with an inline image whose object metadata exposes `sourceUri` or `contentUri`, when pulled, then the Org buffer receives an ordinary Org link to that URI instead of losing the image or rendering only an opaque object id.
- [x] AC-5: Given a standalone local image link with an Org caption, when pushed, then the caption is preserved as Org-visible semantic text after the inserted image. This is a v1 visible-text mapping rather than native Google Docs figure metadata.
- [x] AC-5a: Given a pushed caption is marked with the neutral `org-image-caption` semantic style name, when pulled, then the marked caption paragraph is re-associated with the preceding image and rendered as `#+CAPTION:` without relying on visual styling heuristics.
- [ ] AC-6: Given a described image link such as `[[./img/foo.png][Open image]]`, when pushed, then it remains a normal link and is not treated as a standalone image upload.
- [x] AC-7: Given an unsupported or missing local image file, when push preflight runs, then it fails before mutating the remote document with a clear actionable error.
- [x] AC-8: Given a source block with a language, when pushed and pulled, then code text and language identity survive via semantic markers; Google Docs code block building blocks are used only when public APIs expose a reliable seam, and syntax highlighting is explicitly deferred.
- [ ] AC-9: Given Org dates, provisional people links, and provisional status links, when pushed or preflighted, then Google Docs classifies them as native, degraded, or deferred rather than silently flattening their meaning; pulled remote dates become inactive Org timestamps by default.
- [ ] AC-10: Given a `#+begin_quote` block with multiple paragraphs, when pushed and pulled, then the Org quote block boundary and paragraph text survive round trip; visible quote decoration is styling-deferred unless a reliable semantic equivalent is implemented.
- [ ] AC-11: Given the specimen `modules/org/specimens/typographic-semantics.org`, when converted through the local semantic preflight/adapter tests, then every contract construct is classified as supported, degraded, deferred, or unsupported with diagnostics.

## Invariants

- Do not fork upstream `gdocs` as the first implementation move.
- Do not move heavy Google Docs logic into `modules/interactive/org/google-docs.el`.
- Do not regress current upstream body sync commands: create, push, pull, open, status.
- Do not include styling parity in this epic.
- Do not break comments workflows or `org-comments` backend behavior.
- Do not print credentials or auth tokens during diagnostics.
- Native footnote push must fail preflight before invoking upstream `gdocs-push` when blocking diagnostics are present.

## Scope

**May modify:**

- `packages/org-google-docs/`
- `packages/org-google-docs/test/`
- `modules/org/typographic-semantics.md`, `modules/org/typographic-semantics.el`, and `modules/org/specimens/typographic-semantics.org`
- tiny shared Org semantic helpers under `packages/` only if reused by both Confluence and Google Docs
- upstream `straight/repos/gdocs/` only for minimal, well-isolated extension seams after wrapper feasibility is exhausted
- docs/spec files for Google Docs source-content semantics

**Must not modify:**

- `modules/interactive/org/google-docs.el` except thin activation or key/dispatch wiring
- Confluence storage behavior except to extract clearly reusable Org-level helpers
- auth/credential storage behavior
- styling/callout visual parity beyond explicit deferral notes

## Verification Plan

| Criterion | Method | Automated? |
| --- | --- | --- |
| AC-1, AC-2 | Unit tests over Org → IR/request adapter using sample footnote references/definitions, plus manual smoke checklist in `docs/native-footnotes-smoke.md` | Yes |
| AC-3 | Upstream `gdocs-convert` fixture test over Docs JSON/native footnote payload → Org output | Yes |
| AC-4, AC-4a, AC-6, AC-7 | Image classification/preflight tests modeled after Confluence image asset tests plus upstream image IR/request/pull tests, plus manual smoke checklist in `docs/native-images-smoke.md` | Yes |
| AC-5, AC-5a | Caption fixture tests and manual smoke checklist in `docs/native-images-smoke.md`; v1 preserves captions as visible text, records a neutral semantic caption style marker, and reconstructs `#+CAPTION:` from that marker on pull | Yes |
| AC-8 | Org → IR/request and IR/JSON → Org source-block fixture tests, plus manual live smoke before declaring native visual parity | Yes |
| AC-9 | Semantic classification tests for dates and people links | Yes |
| AC-10 | Org → IR/request and IR/JSON → Org quote fixture tests | Yes |
| AC-11 | Specimen semantic audit test/report using `modules/org/specimens/typographic-semantics.org` | Yes |

## References

- Typographic contract: `modules/org/typographic-semantics.md`
- Typographic specimen: `modules/org/specimens/typographic-semantics.org`
- Sample file: `/Users/hubertbehaghel/tmp/gdocs-test.org`
- Upstream conversion: `straight/repos/gdocs/gdocs-convert.el`
- Upstream sync orchestration: `straight/repos/gdocs/gdocs-sync.el`
- Google Docs adapter facade: `packages/org-google-docs/org-google-docs.el`
- Native image smoke checklist: `packages/org-google-docs/docs/native-images-smoke.md`
- Confluence image/quote/footnote exporter inspiration: `packages/org-confluence/org-confluence-export.el`
- Confluence exporter tests: `test/publishing/confluence/org-confluence-export-test.el`
