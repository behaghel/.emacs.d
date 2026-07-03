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
| Comments v1 | Defer to second milestone; read-only sidecar import first | Body sync must be validated before comment UX. Initial comments should import/list/open only, not mutate remote comments. |
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

### Planned Acceptance Criteria

- [ ] AC-10: Given a linked Google Docs Org buffer, when importing comments, then active/open Google comments are written to the adjacent `.comments.org` sidecar.
- [ ] AC-11: Given import is run with an include-resolved option, then resolved comments are imported or updated as well.
- [ ] AC-12: Given an imported Google comment already exists locally, when re-importing, then machine-owned remote metadata/body is updated while user-owned notes are preserved.
- [ ] AC-13: Given `quotedFileContent` uniquely matches source Org text, when comments are imported, then the sidecar records an anchor that `org-comments` can display in source/context-panel UI.
- [ ] AC-14: Given no unique source match exists, when comments are imported, then the sidecar marks the comment as stale/ambiguous instead of guessing.
- [ ] AC-15: Given a Google Docs comment sidecar entry, when open-remote is requested, then a browser opens the best available Google Docs URL for that document/comment context.
- [ ] AC-16: Given the Google Docs backend is registered with `org-comments`, when backend detection runs in a linked `gdocs` buffer, then it can select the Google Docs backend for supported read-only operations.

## Confluence Authoring And Collaboration Parity Roadmap

The target is not feature-for-feature cloning of Confluence APIs; it is parity for
the authoring and review loop this configuration supports today.

| Capability | Confluence workflow | Google Docs status | Parity gap |
|---|---|---|---|
| Create/publish document | Create/update page from Org | Delegated to upstream `gdocs-create`/`gdocs-push` | Needs more facade polish, but usable. |
| Pull remote body | Pull page into Org | Delegated to upstream `gdocs-pull` | Needs real-world confidence and clearer conflict guidance. |
| Open remote document | Open Confluence page | `org-google-docs-open` | Usable. |
| Manual sync command | `org-confluence-sync-current` | `org-google-docs-sync-current` currently pushes | Needs safer stale-aware semantics or documented upstream behavior. |
| Comment import | Remote comments into `.comments.org` | Active/resolved import implemented | Needs missing/deleted reconciliation and better update reporting. |
| Side panel review | `org-comments` context panel | Imported Google comments appear in side panel | Usable once package stack is loaded. |
| Open remote comment | Browser opens focused Confluence comment | Google Docs backend opens `?disco=COMMENT_ID` URL | Needs real-world confirmation across comment types. |
| Reply to reviewer | Local sidecar reply pushed to Confluence | Not implemented | High-value next collaboration gap. |
| Resolve comment | Status change pushed to Confluence | Not implemented | High-value next collaboration gap; lower anchoring risk than creating comments. |
| Create new anchored remote comment | Local sidecar draft pushed to Confluence | Not implemented | Harder; Google Drive comment anchoring is less predictable. |
| Sync status dashboard | Confluence sync status report | Not implemented | Useful after bidirectional comment actions exist. |
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

- [ ] AC-17: Given a sidecar reply under an imported Google comment, when the
  reply push command runs, then a Google Drive reply is created under the remote
  comment and the sidecar reply records its remote id.
- [ ] AC-18: Given an imported Google comment, when the resolve command runs,
  then the remote Google comment is marked resolved and local sidecar metadata is
  updated to `resolved`.
- [ ] AC-19: Given a local-only sidecar root comment with no remote id, when a
  Google Docs push/resolve command is requested, then the command fails with a
  clear error explaining that new anchored remote comment creation is not yet
  supported.
- [ ] AC-20: Given a Google Docs source buffer, when generic `org-comments`
  backend actions dispatch `:reply` or `:set-status`, then they route to the
  Google Docs backend for supported operations.
- [ ] AC-21: Given upstream Google API support is missing for an operation, when
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
- Google comment support is read-only in the first comment milestone: import/list/open only; no create/reply/resolve.
- Local sidecar user notes must not be overwritten by remote comment re-import.

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
