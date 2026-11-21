# Mixed semantic coverage smoke check

Non-normative acceptance procedure for the [supported profile](source-content-semantics.md).
A passing normalized diff alone is not fidelity evidence.

## Setup

- Use only synthetic disposable content, with explicit `personal` account,
  automatic push disabled and automatic mutation retries disabled.
- Use reviewed conversion sources in batch Emacs with installed authenticated
  transport, or explicitly install reviewed versions. Testing does not authorize
  a production `gdocs` library reload, installation, merge or workspace retirement.
- The `gdocs` library fixtures `test/fixtures/disposable-coverage-presentation.org` and
  `test/fixtures/disposable-coverage-presentation-edited.org` provide the initial
  and edited sources. Copy them into a disposable directory. The `coverage.png`
  image is a runtime asset, not a tracked visual golden; obtain the public image
  from `https://www.gstatic.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png`.
- The public-image URI may be explicitly supplied as test enrichment instead of
  uploading to Drive. Record that limit: this checks inline-image/caption
  integration, not another live test of Drive asset upload.
- Share the actual matching Org source when sharing the browser Doc. Preserve
  sent plans, returned snapshots and pipeline traces under fresh filenames.

## Tracked offline driver

Run these commands **inside the project development shell** in the task worktree
(`as <task-slug> run sh` opens one for a human operator). Select reviewed `gdocs` library
**source**, including its `test/` fixtures, and existing dependencies explicitly:

```sh
export GDOCS_PATH=/path/to/reviewed/gdocs
export STRAIGHT_BUILD_PATH=/path/to/straight/build
export GDOCS_DISPOSABLE_THEME=veriff-working
export GDOCS_DISPOSABLE_IMAGE_URI=https://www.gstatic.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png
export GDOCS_DISPOSABLE_SOURCE="$GDOCS_PATH/test/fixtures/disposable-coverage-polished.org"
export GDOCS_DISPOSABLE_REMOTE="$GDOCS_PATH/test/fixtures/live-coverage-polished.json"
export GDOCS_DISPOSABLE_ARTIFACTS="$(mktemp -d)"
GDOCS_DISPOSABLE_ACTION=coverage-verify emacs --batch -Q \
  -l packages/org-google-docs/test/org-google-docs-disposable-preflight.el
```

Expected summary: `:requests 0`, `:pulled-repeat-requests 0`, `:native-dates 2`,
`:native-footnotes 1`, `:native-link-targets 7`, `:linked-whitespace 0`.
This replays a captured native fixture; it is not a new live acceptance run.
The public URI enriches image requests directly, so this command needs no local
PNG and does not exercise upload. The named theme matches this captured fixture.

The same entry point exposes every planning phase. Set
`GDOCS_DISPOSABLE_ACTION` and use a **new empty directory** for
`GDOCS_DISPOSABLE_ARTIFACTS` on every invocation:

| Action | Additional input | Output |
|---|---|---|
| `coverage-create` | Unset `GDOCS_DISPOSABLE_REMOTE` and `GDOCS_DISPOSABLE_REPLY` | `create.requests.json`, `pipeline.org`; body contains native-note sentinels |
| `coverage-native` | `GDOCS_DISPOSABLE_REMOTE`: fetched post-create sentinel snapshot, including `revisionId` | `native.requests.json`: sentinel deletion, native reference creation and provenance |
| `coverage-fill` | `GDOCS_DISPOSABLE_REPLY`: saved reference-creation response | `fill.requests.json`: native segment content and styling |
| `coverage-inspect` | `GDOCS_DISPOSABLE_REMOTE`: latest native snapshot; source may contain intended edits | `inspect.requests.json`, `returned.org`, `pulled-repeat.requests.json` |
| `coverage-verify` | Latest native snapshot and independently maintained matching source | `verify.requests.json`, `returned.org`, `pulled-repeat.requests.json`; nonzero exit if either repeat is nonempty or raw native checks fail |

Each successful phase also emits `summary.json`. Fill takes its revision from the
creation response's `writeControl`; if absent, supply a **fresh post-reference**
snapshot via `GDOCS_DISPOSABLE_REMOTE`. The driver refuses native/fill plans without
a revision. Keep the original source and all snapshots/replies with the artifacts.
A failed repeat verification retains its diagnostic plans; rerunning must use a
new directory, not overwrite that evidence.

These commands only write local plans. They never authenticate, fetch, create a
Google Doc or execute a batch update. Sending plans remains a separately approved
operation through the owned disposable transport, with explicit account and no
mutation retries. No helper definitions or state from an earlier Emacs session
are required for planning. The current mixed canary supports exactly one named
footnote reference. Browser/PDF review and the live transport ownership checks
remain separate; do not recreate a missing ownership record from a document ID.

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
sources also pass `gdocs` library validation before upload. Unknown native inline objects and
unsupported native date values must block import rather than disappear or become
invented dates. Audit counters without declared mappings remain visible as
unsupported. Do not expand the supported profile merely to get the full authoring
specimen through publication.
