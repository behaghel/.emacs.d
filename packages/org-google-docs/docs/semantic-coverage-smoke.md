# Mixed semantic coverage smoke check

Non-normative acceptance procedure for the [supported profile](source-content-semantics.md).
A passing normalized diff alone is not fidelity evidence.

## Setup

- Use only synthetic disposable content, with explicit `personal` account,
  automatic push disabled and automatic mutation retries disabled.
- Use reviewed conversion sources in batch Emacs with installed authenticated
  transport, or explicitly install reviewed versions. Testing does not authorize
  a production SDK reload, installation, merge or workspace retirement.
- The SDK fixtures `test/fixtures/disposable-coverage-presentation.org` and
  `test/fixtures/disposable-coverage-presentation-edited.org` provide the initial
  and edited sources. Copy them into a disposable directory. The `coverage.png`
  image is a runtime asset, not a tracked visual golden; obtain the public image
  from `https://www.gstatic.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png`.
- The public-image URI may be explicitly supplied as test enrichment instead of
  uploading to Drive. Record that limit: this checks inline-image/caption
  integration, not another live test of Drive asset upload.
- Share the actual matching Org source when sharing the browser Doc. Preserve
  sent plans, returned snapshots and pipeline traces under fresh filenames.

## Checks

1. Run `org-google-docs-debug-pipeline` before mutation. Ensure only document title
   metadata is present; `#+AUTHOR:`/`#+LANGUAGE:` inside literal blocks must remain
   code/example content, not document metadata.
2. Create the mixed Doc through native-footnote preparation, including reference
   creation and segment filling. Inspect PDF and browser: rich text, list nesting
   and continuations, tasks, description terms, quote/callout/center/verse groups,
   adjacent and terminal rules, literal blocks, table, image/caption and rich note.
   Source/example panels have an unshaded outside gap while authored internal blank
   lines remain inside their respective panels. Definition terms need no redundant
   bold markup, yet terms remain bold in Docs through their logical role. Org
   `status:` links display badges. Google Docs uses lowercase glyph inputs with
   uniform small caps, while pull restores the original source spelling through
   provenance. Example surfaces use `#C8B59A` with dark text; source surfaces stay
   unchanged.
3. Inspect actual native runs: two day-date chips with active/inactive provenance,
   complete literal time-bearing timestamp text, intended hyperlink labels and
   targets (including person identity), an image and its caption, and one rich
   native footnote. No unlinked space may acquire a URL.
4. Fetch and independently plan an unchanged push. Require zero requests **and**
   no false modification operations: inline provenance must not become paragraph
   identity or cause unchanged native objects to be recreated beside an edit.
5. Apply the edited specimen: prose, child list, center and verse text, source text,
   table cell, image caption and footnote body. Date chips must not be reinserted;
   the existing native note segment receives its edit without recreating its
   reference. Fetch, inspect rich native runs again and require an empty repeat.
6. Reparse the pulled Org as a fresh local source and require another empty plan.
7. In the browser, change `updated` in the opening bold phrase to `reviewed`.
   Pull again: bold Unicode text, link targets, date kinds, block boundaries,
   image/caption and note semantics remain intact. Require both independent and
   pulled-source repeats to be empty.
8. In isolated linked-buffer tests, concurrent local/remote edits in center-only,
   verse-only and native-date documents stop before automatic text merge and
   preserve local content/revision. Word edits with stable geometry remain
   pullable. Structural loss stops ordinary replacement; confirmed replace-local
   recovery remains an explicit separate operation.

## Negative profile checks

Create, push and sync-current must reject unsupported quote attribution, document
metadata, special blocks and note/asset forms before remote mutation. Image-bearing
sources also pass SDK validation before upload. Unknown native inline objects and
unsupported native date values must block import rather than disappear or become
invented dates. Audit counters without declared mappings remain visible as
unsupported. Do not expand the supported profile merely to get the full authoring
specimen through publication.
