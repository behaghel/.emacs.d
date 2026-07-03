---
domain: authoring.comments
status: draft
last-reviewed: 2026-06-21
---

# Org Comments Package Refactor

> User-facing package documentation lives in `README.org` and the Info-ready
> manual at `docs/org-comments.org`. This file records the refactor plan,
> architecture decisions, and migration status.

## Problem

The current Org comments implementation works, but its reusable model is spread across personal configuration, large files, and Confluence-specific integration points. To prepare it for standalone open-source use and for multiple backends, the generic comment model, storage, UI, and backend protocol need to move into `packages/org-comments/` with neutral defaults and clear extension boundaries.

## Context

Current implementation locations:

- `packages/org-comments/`: generic sidecar storage/model/link/anchor logic, commands, overlays, panel rendering, and backend protocol.
- `packages/org-comments/docs/ux-parity-audit.md`: UX parity contract and audit checklist for Confluence and Google Docs comment workflows.
- `packages/org-comments/org-context-panel.el`: reusable provider-based Org context panel mechanics.
- `modules/interactive/org/comments.el`: personal interactive command layer and Evil/Bépo bindings.
- `modules/interactive/org/context-panel.el`: personal context-panel activation, UI adapters, visual-fill docking, and optional filter extensions.
- `packages/org-confluence/`: Confluence publishing and remote comment sync through public package APIs/backend adapters.
- `test/authoring/org/` and `packages/org-comments/test/`: integration and package-local tests for comments and context panels.

The target package must support:

- local Org sidecar comments today,
- remote Confluence comments today,
- Google Docs comments through the `org-google-docs` backend,
- Org-native default keybindings,
- personal Evil bindings only in `modules/`.

## Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Package location | `packages/org-comments/` | Keeps reusable package code extractable and separate from personal activation. |
| Package namespace | `org-comments-*` | Avoids the former personal namespace and avoids the existing `org-commentary` package name. |
| Domain classification | `authoring > comments`, shared-kernel | The comment model and backend contract are consumed by authoring UI and remote publishing adapters. |
| Compatibility strategy | Hard namespace migration, no new `hub/` aliases | Keeps the package extractable and avoids extending the former personal API surface. |
| Default package keys | Org-native `C-c ...` bindings | Standalone package should feel natural in Org and not require Evil. |
| Evil bindings | Keep in `modules/interactive/org/` | Evil/Bépo bindings are personal configuration, not package defaults. |
| Backend design | Backend protocol with local Org sidecar as default backend | Confluence and Google Docs should plug into the same operations instead of coupling to sidecar internals. |
| Test placement | Tests mirror package source layout | Improves package extraction and makes each source slice independently testable. |
| File size target | Prefer files under ~700 LOC | Prevents another monolith while allowing cohesive modules. |

## Target Package Layout

```text
packages/org-comments/
├── org-comments.el                 ; umbrella entrypoint
├── org-comments-core.el            ; groups, customs, generic utilities
├── org-comments-model.el           ; comment/thread/target records
├── org-comments-target.el          ; target text, ranges, hashes
├── org-comments-anchors.el         ; exact/fuzzy matching and stale anchors
├── org-comments-sidecar.el         ; .comments.org read/write/format storage
├── org-comments-store.el           ; persistence-facing API
├── org-comments-backend.el         ; backend protocol and registry
├── org-comments-backend-org.el     ; local Org sidecar backend
├── org-comments-links.el           ; org-comment: links
├── org-comments-commands.el        ; backend-neutral interactive commands
├── org-context-panel.el            ; reusable Org context panel primitives
├── org-comments-context-panel.el   ; comments provider glue
├── org-comments-overlays.el        ; public overlay activation facade
├── org-comments-panel.el           ; context panel mode and public commands
├── org-comments-panel-render.el    ; panel rendering helpers
├── org-comments-panel-filter.el    ; panel filtering state/predicates
├── org-comments-panel-actions.el   ; panel jump/edit/reply/status/delete actions
├── org-comments-compose.el         ; compose buffer mode and submit/cancel
├── org-comments-page.el            ; page-level comments and panel
└── test/
    ├── org-comments-core-test.el
    ├── org-comments-model-test.el
    ├── org-comments-target-test.el
    ├── org-comments-anchors-test.el
    ├── org-comments-sidecar-test.el
    ├── org-comments-backend-test.el
    ├── org-comments-links-test.el
    ├── org-comments-commands-test.el
    ├── org-context-panel-test.el
    ├── org-comments-context-panel-test.el
    ├── org-comments-overlays-test.el
    ├── org-comments-panel-test.el
    └── org-comments-compose-test.el
```

Personal activation remains in `modules/interactive/org/` and should stay limited to load-path setup, package requires, hooks, and personal extensions. Comments panel mode/keymaps are owned by `org-comments-panel-mode`.

## UX Parity Contract

Remote comment providers should share the same author-facing command language,
comments panel look'n'feel, report grammar, and capability-gated failure style.
Confluence and Google Docs parity is tracked in
[`docs/ux-parity-audit.md`](docs/ux-parity-audit.md), with the visual contract in
[`docs/wireframes/provider-parity-panel.svg`](docs/wireframes/provider-parity-panel.svg).
Provider API differences remain legitimate, but they should appear as clear
capability limits inside the shared UX rather than as different interaction
models.

## Backend Contract

Backends expose capabilities and operations through `org-comments-backend.el`.

Initial capabilities:

- `:list-comments`
- `:create-inline`
- `:create-page`
- `:reply`
- `:edit`
- `:delete`
- `:set-status`
- `:open-remote`
- `:sync`
- `:push`
- `:pull`

Initial operations:

- `org-comments-backend-list`
- `org-comments-backend-create`
- `org-comments-backend-update`
- `org-comments-backend-delete`
- `org-comments-backend-reply`
- `org-comments-backend-set-status`
- `org-comments-backend-open-remote`
- `org-comments-backend-push`
- `org-comments-backend-pull`
- `org-comments-backend-sync`
- `org-comments-register-backend-detector`
- `org-comments-backend-detect`

The local Org sidecar backend is the default backend. Confluence registers a detector so Org source buffers with `#+CONFLUENCE_PAGE_ID` default remote comment operations to the Confluence backend. Google Docs implements the same protocol for linked `gdocs` buffers.

## Keybinding Policy

The package may define only Org-native and Emacs-native defaults, such as `C-c` prefixed commands appropriate for Org buffers and sidecar buffers.

Package defaults must not assume Evil, general.el, leader keys, or Bépo-specific layout choices.

Personal Evil bindings stay in:

- `modules/interactive/org/core.el`
- `modules/interactive/org/comments.el` while it remains as activation glue
- other personal interactive modules as needed

## Test Architecture Policy

Tests should mirror source architecture:

- package unit tests live under `packages/org-comments/test/`, next to the package they validate;
- each source file should have a corresponding focused test file when behavior warrants it;
- broad authoring integration tests may remain under `test/authoring/org/` during migration;
- new backend contract tests should use a fake backend so command/panel behavior does not assume sidecar or Confluence;
- Confluence-specific comment tests belong under `packages/org-confluence/test/` or `test/publishing/confluence/`, not in generic org-comments tests.

## Acceptance Criteria

- [ ] AC-1: Given `emacs -Q` with `packages/org-comments/` on `load-path`, when `(require 'org-comments)` runs, then the package loads without private `hub-*` libraries.
- [ ] AC-2: Given an Org buffer, when local sidecar comments are created/read/updated/deleted through the new API, then behavior matches the current implementation.
- [ ] AC-3: Given existing repo modules/tests, when they are migrated to package APIs, then they continue to pass without personal compatibility aliases.
- [ ] AC-4: Given package files under `packages/org-comments/`, when file sizes are checked, then no implementation file exceeds the agreed size target without an explicit documented exception.
- [ ] AC-5: Given the standalone package defaults, when keybindings are inspected, then defaults are Org-native/Emacs-native and no Evil bindings are installed by the package.
- [ ] AC-6: Given personal configuration under `modules/interactive/org/`, when Evil bindings are inspected, then Evil-specific comment bindings live only there.
- [ ] AC-7: Given a fake backend implementing the backend protocol, when generic commands/panel tests run, then they pass without sidecar or Confluence assumptions.
- [ ] AC-8: Given Confluence comment integration, when it is migrated, then it uses public `org-comments-*` APIs/backend registration and does not call private `org-comments--*` or former personal private internals.
- [ ] AC-9: Given package tests, when the test tree is inspected, then tests mirror the package source layout.

## Invariants

- Source Org buffers remain clean; local comments are stored in sidecar files unless another backend is explicitly selected.
- Remote backend metadata must not leak into the core model except through neutral fields.
- Package defaults must be neutral and suitable for open-source distribution.
- Personal preferences, secrets, account defaults, Evil bindings, and project-specific activation stay outside `packages/org-comments/`.
- Backends must fail with clear user-facing errors when an unsupported capability is requested.
- No new `hub/` compatibility aliases should be added for package APIs.

## Scope

May modify during this refactor:

- `domains.yaml`
- `packages/org-comments/`
- removed legacy personal comments library references when found
- `modules/interactive/org/comments.el`
- `modules/interactive/org/context-panel.el`
- `modules/interactive/org/core.el` for personal bindings
- `packages/org-confluence/` only to migrate comment integration to public APIs/backend protocol
- relevant tests under `packages/org-comments/test/`, `test/authoring/org/`, and Confluence test locations

Must not modify without a separate decision:

- unrelated Org authoring behavior,
- unrelated Confluence publishing/export semantics,
- unrelated Evil state exceptions,
- private secrets or local-only setup.

## Verification Plan

| Criterion | Method | Automated? |
|---|---|---|
| AC-1 | Batch-load package with `emacs -Q -L packages/org-comments --batch -l org-comments` | Yes |
| AC-2 | Sidecar-focused ERT tests mirrored under `packages/org-comments/test/` | Yes |
| AC-3 | Existing authoring integration tests under `test/authoring/org/` | Yes |
| AC-4 | `wc -l packages/org-comments/*.el` or script check | Yes |
| AC-5 | Inspect package keymaps/tests asserting no Evil dependency | Partly |
| AC-6 | Grep package for Evil/general symbols; inspect module bindings | Partly |
| AC-7 | Fake backend ERT suite | Yes |
| AC-8 | Grep Confluence for private comment internals; run Confluence tests | Yes |
| AC-9 | Compare source/test file names under `packages/org-comments/` | Partly |

## Minor-Mode-Centric Direction

`org-comments-mode` is the package's primary standalone deliverable.  Enabling
the minor mode should provide the complete generic buffer-local comments
experience without requiring personal configuration or a rich context panel:

- Org-native command keybindings and DWIM public commands that work from source buffers and comments panel rows;
- inline target overlays and page-comment markers;
- buffer-local refresh hooks for comment state that can be derived from local
  sidecars or registered backends;
- explicit extension hooks for optional richer UIs such as the current personal
  context panel.

Keep this scope focused on generic package behavior.  Personal Evil/Bépo
bindings, Confluence-specific sync markers, account lookup defaults, and visual
layout preferences remain outside `packages/org-comments/`.

Nice-to-have Emacs package affordances such as an Easy Menu, mode-line counts,
`imenu` integration, or a `completing-read` jump command are valid future work,
but they should not interrupt the migration slices below unless they unblock
extraction.

## Implementation Phases

1. Create package skeleton, README/spec, and domain coverage. ✅
2. Move current generic implementation into package APIs with a hard namespace migration. ✅
3. Split model, target, sidecar, anchor, and link modules. ✅
4. Introduce backend protocol and local Org backend. ✅
5. Move backend-neutral commands into the package with Org-native default keys. ✅
6. Make `org-comments-mode` minor-mode-centric with package-owned overlays. ✅
7. Split the remaining context panel into package modules in small slices:
   panel mode/window lifecycle ✅, rendering ✅, filters ✅, actions ✅, page
   panel ✅, and compose/reply ✅.
8. Migrate Confluence to a backend adapter using public APIs:
   backend registration/capability seam ✅, remote open ✅, push ✅, pull ✅,
   comments-only sync ✅, backend detection ✅, `org-comments-sync` command ✅,
   `org-comments-open-remote` command ✅, `org-comments-push` command ✅,
   `org-comments-pull` command ✅, panel remote-open action ✅, panel push action ✅,
   panel pull/sync actions ✅, Confluence open-current backend delegation ✅,
   package-native Confluence import/push/sync helpers ✅, Confluence comments facade ✅,
   full Confluence sync reuses comments helper ✅, context-panel remote-open backend dispatch ✅,
   package-owned push-at-point command ✅, context-panel push adapter binding ✅,
   package-owned remote-open-at-point command ✅, context-panel remote-open adapter binding ✅,
   package-owned status-at-point commands ✅, context-panel status adapter binding ✅,
   package-owned delete-at-point command ✅, package-owned edit-at-point command ✅,
   package-owned reply-at-point command ✅, package-owned jump-at-point command ✅,
   package-owned page-open-at-point command ✅, package-owned close-current-ui command ✅,
   package-owned item navigation commands ✅, package-owned generic filter commands ✅,
   package-owned help-current-ui command ✅, direct package action bindings ✅,
   backend-neutral collaboration model spec ✅, collaboration normalization helpers ✅,
   package-owned public filter registry ✅, collaboration-aware built-in filters ✅,
   source-buffer-scoped package filter state ✅, hub context-panel filter predicates use package registry ✅,
   eager sidecar collaboration normalization ✅, Confluence import-shaped records normalize to package collaboration fields ✅,
   reduced hub context-panel normalization adapter ✅, explicit legacy sidecar migration module ✅,
   package and Confluence sidecar metadata use `ORG_COMMENTS_*` ✅,
   broader hub/context-panel/Confluence fixtures use `ORG_COMMENTS_*` ✅,
   Confluence comment code calls package-native org-comments APIs directly ✅,
   interactive Org comment/context modules no longer require the former personal comments library ✅,
   broad hub/context tests no longer require the former personal comments library ✅,
   and remaining private API cleanup.
9. Add fake backend tests to validate Google Docs readiness.
10. Shrink personal modules to activation, adapter registration, and
    Evil/keybinding preferences.

## References

- Backend-neutral collaboration model: `packages/org-comments/COLLABORATION.md`
- Current authoring Org spec: `modules/interactive/org/README.md`
- Current generic implementation: `packages/org-comments/`
- Current personal commands: `modules/interactive/org/comments.el`
- Current panel implementation: `modules/interactive/org/context-panel.el`
- Current Confluence integration: `packages/org-confluence/`
