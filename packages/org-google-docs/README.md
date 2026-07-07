---
domain: publishing.google-docs
status: draft
last-reviewed: 2026-07-02
---

# Spec: Org Google Docs Adapter

## Problem

This repository has a mature Org ↔ Confluence publishing and sync workflow, but no equivalent Google Docs workflow. The desired Google Docs integration should let Org buffers publish to, pull from, and open Google Docs with a familiar command surface, while reusing upstream work where possible instead of rebuilding Google Docs sync from scratch.

## Context

- `packages/org-confluence/` is the reference publishing package boundary: reusable package code lives under `packages/`, while personal activation lives in `modules/interactive/org/confluence.el`.
- `packages/org-comments/` owns the generic sidecar comment model and backend protocol. Google Docs comments should eventually integrate through that protocol.
- Upstream [`benthamite/gdocs`](https://github.com/benthamite/gdocs) already provides Org ↔ Google Docs body sync, OAuth, linking, push/pull, status, conflict handling, and comment-aware push preservation.
- Upstream `gdocs` currently does not provide this repository's Confluence-like `org-comments` sidecar UX.
- Source-content semantic gaps are tracked in [`docs/source-content-semantics.md`](docs/source-content-semantics.md); styling parity is intentionally a later epic.
- `org-google-docs-semantics.el` classifies a typographic semantic audit into Google Docs `supported`, `degraded`, `unsupported`, and `deferred` buckets without depending on local `hub-*` modules.
- `org-google-docs-footnotes.el` extracts a native-footnote push plan for named Org footnotes before any Google Docs mutation, wires the `gdocs` footnote-reference seam into `org-google-docs-push`, filters Org-only footnote definitions out of push-time IR, and provides two-phase request helpers for descending-index `createFootnote` requests followed by body insertion into returned footnote segments.
- `patches/gdocs-footnote-reference-seam.patch` records the minimal upstream `gdocs` seam needed to expose exact footnote-reference indices without placeholder text search.
- `patches/gdocs-footnote-diff-seam.patch` records the complementary upstream diff change needed for zero-width footnote runs to count as semantic paragraph changes.
- `patches/gdocs-footnote-pull-seam.patch` records the upstream conversion change that imports native Google Docs footnotes as ordinary Org footnote references and definitions.
- `patches/gdocs-standalone-image-ir-seam.patch` records the upstream conversion change that preserves standalone Org image links as image IR instead of literal file-link text.
- `patches/gdocs-image-uri-insert-seam.patch` records the upstream conversion change that emits `insertInlineImage` requests for image IR with fetchable URIs.
- `patches/gdocs-drive-public-permission-helper.patch` records the upstream API helper used to make uploaded Drive image files fetchable by Google Docs.
- `patches/gdocs-drive-raw-multipart-upload.patch` records the upstream upload fix needed for Drive media uploads to send raw multipart bodies with a final boundary.
- `patches/gdocs-image-caption-text.patch` records the upstream conversion change that renders image captions as visible text after inserted images.
- `patches/gdocs-image-caption-diff-key.patch` records the upstream diff change that treats image caption changes as semantic image changes.
- `patches/gdocs-inline-image-pull-uri.patch` records the upstream conversion change that preserves Google Docs inline image source/content URIs on pull.
- `patches/gdocs-image-caption-marker-pull.patch` records the upstream conversion change that marks pushed captions with a neutral semantic style name and reconstructs `#+CAPTION:` from that marker on pull.
- `patches/gdocs-image-block-spacing.patch` records the upstream conversion change that keeps image blocks separated from surrounding paragraphs in pulled Org output.
- `patches/gdocs-image-caption-diff-range.patch` records the upstream conversion fix that makes a marked caption paragraph part of the image element's document range so repeated pushes replace rather than duplicate captions.
- `patches/gdocs-image-merge-segment-spacing.patch` records the upstream sync fix that preserves image block spacing when pull uses the three-way merge segment path.
- Patch files are audit/export artifacts for upstream review; current active seam work is developed and tested directly on the local `~/ws/gdocs` branch.
- `docs/native-footnotes-smoke.md` records manual smoke-test scenarios for native footnote push/pull, including repeated references and UI caveats.
- `docs/native-images-smoke.md` records manual smoke-test scenarios for standalone image and caption push, including debug-pipeline checkpoints.
- The activation module prefers a local `~/ws/gdocs` checkout on branch `org-image-seam`; this makes the seam repeatable without mutating `straight/repos/gdocs` directly.
- This domain is supporting: specs are expected when the package boundary or integration contract changes, but the first implementation should stay small and reversible.

## Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Package boundary | `packages/org-google-docs/` | Keeps Google Docs adapter behavior extractable and mirrors `packages/org-confluence/`. |
| Activation boundary | `modules/interactive/org/google-docs.el` | Personal config, Straight dependency declaration, auth-source policy, and future leader keys belong outside the package. |
| Upstream strategy | Wrap upstream `gdocs` via Straight | Body sync is already solved upstream; avoid reimplementing Google Docs sync unless this path fails. |
| Package dependencies | Extractable, no `hub-*` dependencies | Preserves local package discipline and allows reuse outside this config. |
| Metadata source | Use upstream `gdocs` metadata as-is | Avoid duplicate `#+GOOGLE_DOCS_*` metadata and drift with upstream linking state. |
| Sync default | Manual push/pull by default | Avoid surprising remote edits; users can opt into auto-push/auto-pull later. |
| Load behavior | Safe-load facade with command-time preflight | Batch loads and package tests should not fail merely because upstream `gdocs` or credentials are absent. |
| Command surface | Thin `org-google-docs-*` facade plus upstream `gdocs-*` availability | Provides Confluence-like muscle memory while keeping upstream docs useful. |
| Dispatch key | Package-native `C-c C-x G` initially | Leader bindings belong in activation modules, not package defaults. |
| Credentials | Auth-source/pass-backed `gdocs-accounts` setup | Secrets must not be committed; activation may provide glue or a documented hook point. |
| Comments v1 | Import-first sidecar workflow, then explicit actions on imported threads | Body sync is delegated upstream; local adapter now owns comment import, replies, resolve, and reconciliation while still deferring new root creation. |
| Comment anchoring | Quoted-text exact/fuzzy matching with stale/ambiguous states | Avoid deep dependency on upstream internals until needed. |
| Upstream contribution | Upstream-friendly split | Generic missing primitives can be prepared for upstream; repo-specific `org-comments` UX remains local. |

## Milestone 1: Body Sync Wrapper

Milestone 1 proves that upstream `gdocs` can support the desired Google Docs publishing workflow in this repository with minimal local code.

### Acceptance Criteria

- [ ] AC-1: Given `domains.yaml`, when resolving `packages/org-google-docs/`, then it resolves to `publishing > google-docs`.
- [ ] AC-2: Given `packages/org-google-docs/` on `load-path`, when `(require 'org-google-docs)` runs without upstream `gdocs` installed, then the package loads without error.
- [ ] AC-3: Given upstream `gdocs` is missing, when a facade command requiring it is invoked, then it fails with an actionable `user-error` mentioning the missing dependency.
- [ ] AC-4: Given upstream `gdocs` is available, when `org-google-docs-create`, `org-google-docs-push`, `org-google-docs-pull`, `org-google-docs-open`, or `org-google-docs-status` is invoked, then it delegates to the corresponding upstream command.
- [ ] AC-5: Given an Org buffer, when `org-google-docs-sync-current` is invoked, then it performs the intended wrapper sync behavior for a linked buffer or reports a clear preflight error.
- [ ] AC-6: Given `org-google-docs-dispatch` is invoked, then it exposes document sync, browser/status, auth/setup, and doctor actions.
- [ ] AC-7: Given `modules/interactive/org/google-docs.el` loads in an interactive config, then it adds this package to `load-path`, declares upstream `gdocs` via Straight, sets manual sync defaults, and does not put reusable behavior in the activation module.
- [ ] AC-8: Given no Google credentials are configured, when Emacs loads the activation module, then startup is silent; when `org-google-docs-doctor` runs, then it reports missing account setup.
- [ ] AC-9: Given relevant load/test commands run, then the new package and activation module do not break existing batch checks.

### Verification Plan

| Criterion | Method | Automated? |
|---|---|---|
| AC-1 | `domain_tree_resolve packages/org-google-docs/README.md` | Yes |
| AC-2 | ERT safe-load test with `packages/org-google-docs/` on `load-path` and no `gdocs` feature provided | Yes |
| AC-3 | ERT command preflight test asserting `user-error` | Yes |
| AC-4 | ERT tests stubbing upstream `gdocs-*` commands and asserting delegation | Yes |
| AC-5 | ERT unit test for wrapper behavior plus manual linked-buffer smoke test after OAuth setup | Partial |
| AC-6 | ERT or load test ensuring dispatch command is defined; manual transient inspection | Partial |
| AC-7 | Batch load smoke check for config; source inspection for thin activation | Partial |
| AC-8 | ERT/unit test for doctor account detection where practical; manual no-credentials startup check | Partial |
| AC-9 | `devenv -q shell -- ./scripts/elisp-parse ...`, targeted ERT, and load check | Yes |

## Milestone 2: Read-Only Google Comment Import

Milestone 2 adds the first useful comment parity layer without mutating remote Google comments.

### Acceptance Criteria

- [x] AC-10: Given a linked Google Docs Org buffer, when importing comments, then active/open Google comments are written to the adjacent `.comments.org` sidecar.
- [x] AC-11: Given import is run with an include-resolved option, then resolved comments are imported or updated as well.
- [x] AC-12: Given an imported Google comment already exists locally, when re-importing, then machine-owned remote metadata/body is updated while user-owned notes are preserved.
- [x] AC-13: Given `quotedFileContent` uniquely matches source Org text, when comments are imported, then the sidecar records an anchor that `org-comments` can display in source/context-panel UI.
- [x] AC-14: Given no unique source match exists, when comments are imported, then the sidecar marks the comment as stale/ambiguous instead of guessing.
- [x] AC-15: Given a Google Docs comment sidecar entry, when open-remote is requested, then a browser opens the best available Google Docs URL for that document/comment context.
- [x] AC-16: Given the Google Docs backend is registered with `org-comments`, when backend detection runs in a linked `gdocs` buffer, then it can select the Google Docs backend for supported read-only operations.

## Current Comment Workflow

Google Docs comment support is built on top of the generic `org-comments`
sidecar model.  The supported workflow is:

1. Link or create a Google Doc through upstream `gdocs` / `org-google-docs` body
   commands.
2. Run `org-google-docs-comments-import` or generic `org-comments-pull` from the
   linked Org buffer to populate `SOURCE.comments.org`.
3. Review imported comments in the source buffer or `org-comments-panel-mode`.
4. Use generic DWIM commands from source or panel rows:
   - `org-comments-open-remote` opens the Google Doc focused on the remote
     comment when possible;
   - `org-comments-reply` creates a local sidecar reply;
   - `org-comments-push` pushes a local reply or a pending local root resolve;
   - `org-comments-mark-resolved` resolves an imported Google root comment
     remotely;
   - `org-comments-sync` pushes pending local resolves, then pulls remote
     comment state.

Re-import reconciles remote roots and replies: existing remote-owned bodies and
metadata are updated, new remote replies are appended, missing roots/replies are
marked with `ORG_COMMENTS_REMOTE_STATE: missing`, and local notes or unsynced
local replies are preserved.  Import and push/sync commands report concise
provider-neutral summaries such as `added replies 2`, `remote missing 1`,
`pushed replies 1`, or `resolved 1`.

Current limitations:

- local-only root comment creation/push is intentionally unsupported until
  Google offers public API support for Docs comments that render as native inline
  anchored comments in the Google Docs UI;
- unanchored Drive comments and comments-as-content/styled inline notes are
  intentionally not used as substitutes for native anchored comments;
- reopening Google comments is not implemented;
- Google API visibility can make "missing" mean deleted, hidden by filtering, or
  otherwise absent from the current API payload;
- body sync remains upstream `gdocs` responsibility, separate from comments-only
  `org-comments-sync`.

## Confluence Authoring And Collaboration Parity Roadmap

The target is not feature-for-feature cloning of Confluence APIs; it is parity for
the authoring and review loop this configuration supports today.

| Capability | Confluence workflow | Google Docs status | Parity gap |
|---|---|---|---|
| Create/publish document | Create/update page from Org | Delegated to upstream `gdocs-create`/`gdocs-push` | Needs more facade polish, but usable. |
| Pull remote body | Pull page into Org | Delegated to upstream `gdocs-pull` | Needs real-world confidence and clearer conflict guidance. |
| Open remote document | Open Confluence page | `org-google-docs-open` | Usable. |
| Manual sync command | `org-confluence-sync-current` | `org-google-docs-sync-current` currently pushes | Needs safer stale-aware semantics or documented upstream behavior. |
| Comment import | Remote comments into `.comments.org` | Active/resolved import, root/reply update, and missing reconciliation implemented | Usable; deleted-vs-hidden semantics remain Google API dependent. |
| Side panel review | `org-comments` context panel | Imported Google comments and replies appear in side panel | Usable. |
| Open remote comment | Browser opens focused Confluence comment | Google Docs backend opens `?disco=COMMENT_ID` URL | Usable. |
| Reply to reviewer | Local sidecar reply pushed to Confluence | Local replies under imported roots push to Drive replies API | Usable; duplicate pushes are skipped. |
| Resolve comment | Status change pushed to Confluence | Imported roots resolve via Drive reply action; pending local `RESOLVED` state syncs | Usable; reopening is deferred. |
| Create new anchored remote comment | Local sidecar draft pushed to Confluence | Unsupported by policy until Google exposes reliable native Docs anchoring | Do not implement unanchored comments or comments-as-content as substitutes. |
| Sync status dashboard | Confluence sync status report | Provider-neutral push/import feedback implemented for Google actions | Basic summaries usable; richer history/dirty-state remains deferred. |
| Child pages/folders | Pull child pages | Upstream has Drive folder concepts | Lower priority for authoring parity. |
| Suggestions/tracked changes | Out of current Confluence parity scope | Not implemented | Explicitly deferred. |

### Next Best Slice: Existing-Comment Collaboration Actions

The next slice should make imported Google comments actionable without tackling
new anchored comment creation yet.  This gives the largest Confluence-like
collaboration gain with the least Google Docs anchoring risk.

Planned behavior:

- push a local sidecar reply under an imported Google comment to the remote
  Google Drive comment thread;
- mark an imported Google comment resolved from Emacs;
- keep remote mutation commands explicit and command-driven;
- preserve read-only import behavior for remote root comments;
- fail clearly when the sidecar entry is not linked to a Google remote comment.

Acceptance criteria:

- [x] AC-17: Given a sidecar reply under an imported Google comment, when the
  reply push command runs, then a Google Drive reply is created under the remote
  comment and the sidecar reply records its remote id.
- [x] AC-18: Given an imported Google comment, when the resolve command runs,
  then the remote Google comment is marked resolved and local sidecar metadata is
  updated to `resolved`.
- [x] AC-19: Given a local-only sidecar root comment with no remote id, when a
  Google Docs push/resolve command is requested, then the command fails with a
  clear error explaining that new anchored remote comment creation is not yet
  supported.
- [x] AC-20: Given a Google Docs source buffer, when generic `org-comments`
  backend actions dispatch `:reply` or `:set-status`, then they route to the
  Google Docs backend for supported operations.
- [x] AC-21: Given upstream Google API support is missing for an operation, when
  the adapter implements the minimum REST wrapper locally, then the wrapper is
  isolated and shaped for a possible upstream `gdocs` contribution.

Verification:

| Criterion | Method | Automated? |
|---|---|---|
| AC-17 | ERT with stubbed Drive replies API plus manual reply push against a test Doc | Partial |
| AC-18 | ERT with stubbed `gdocs-api-resolve-comment` plus manual resolve against a test Doc | Partial |
| AC-19 | ERT sidecar command error test | Yes |
| AC-20 | ERT backend dispatch test | Yes |
| AC-21 | Source review and package-local unit tests around REST wrapper, if needed | Yes |

## Invariants

- Reusable adapter behavior lives under `packages/org-google-docs/`; activation and personal preferences live under `modules/interactive/org/google-docs.el`.
- Package files must not require `hub-*` libraries.
- Package load must remain safe when upstream `gdocs`, Google credentials, or OAuth tokens are absent.
- Secrets must not be committed. OAuth client secrets belong in auth-source/pass or private untracked configuration.
- Upstream `gdocs` remains the source of truth for document link/sync metadata in v1.
- Manual sync is the default in this repository unless explicitly changed later.
- Google comment root creation is unsupported until Google exposes reliable public API support for native inline anchored Docs comments; imported remote roots are actionable, but local-only root comments cannot be pushed.
- Google comment mutations stay explicit and command-driven: replies and resolves are never pushed implicitly except through comments-only sync of pending local resolved state.
- Local sidecar user notes and unsynced replies must not be overwritten by remote comment re-import.

## Scope

**May modify:**

- `domains.yaml`
- `packages/org-google-docs/`
- `modules/interactive/org/google-docs.el`
- `init.el` only to load the activation module when ready
- `test/publishing/google-docs/` or package-local tests under `packages/org-google-docs/test/`
- `straight/versions/default.el` if adding/pinning upstream `gdocs` requires it

**Must not modify without a separate decision:**

- `packages/org-confluence/` behavior
- `packages/org-comments/` backend protocol, except for narrowly required extension points agreed before Milestone 2
- Confluence keybindings or metadata conventions
- `private/setup.el` with tracked secrets
- upstream `gdocs` vendoring/forking

## Implementation Plan

1. Add `publishing > google-docs` domain mapping.
2. Create `packages/org-google-docs/` with this spec and a safe-loading facade.
3. Add small command/preflight helpers that delegate to upstream `gdocs` only at command time.
4. Add `org-google-docs-doctor` and `org-google-docs-dispatch`.
5. Add focused ERT tests for safe load, missing dependency errors, and delegation.
6. Add thin activation module with Straight-managed `gdocs`, manual sync defaults, and auth-source/pass-backed account hook point.
7. Wire activation into interactive Org loading once tests pass.
8. Validate with parse/load checks before moving to comment import.

## References

- `packages/org-confluence/README.md`
- `packages/org-confluence/README.org`
- `modules/interactive/org/confluence.el`
- `packages/org-comments/README.md`
- Upstream `gdocs`: https://github.com/benthamite/gdocs
- Upstream `gdocs` manual: https://stafforini.com/notes/gdocs/
- Related prior art: https://docs.rs/tftio-org-gdocs/latest/org_gdocs/
