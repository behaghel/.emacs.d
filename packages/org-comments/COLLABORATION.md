# Org Comments Collaboration Model

## Problem

`org-comments` must treat remote collaborative comments as a first-class package concern, not as a backend-specific extension. Confluence and future Google Docs support need a shared model for remote presence, authorship, local edits, synchronization state, and filters. Without that model, package filters and UI actions either leak backend names or remain trapped in personal configuration.

## Context

The package already has a backend registry and generic actions for push, pull, sync, open-remote, panel actions, filters, and UI adapters. Existing sidecar records also carry remote-oriented metadata such as remote ids, parent ids, remote author ids, and missing-state markers. The next step is to normalize those facts into backend-neutral fields and make filters consume the normalized model.

## Decisions

| Decision | Choice | Rationale |
| --- | --- | --- |
| Collaboration state belongs in `org-comments` | Define backend-neutral record fields in the package | Remote comments, authorship, and sync state are common to Confluence, Google Docs, and future collaborative backends. |
| Filters consume normalized fields | Filters must not branch on backend names | The same missing/mine/draft/actionable filters should work for Confluence and Google Docs records. |
| Backends map native metadata into package fields | Backend adapters translate provider-specific ids/states | Keeps package UI stable while allowing provider-specific APIs behind adapters. |
| Identity is a package seam | Package exposes current-user and person-resolution functions | “Mine” depends on author identity, but the source of truth may be local config, backend accounts, or future directory integrations. |
| Local and remote state are separate | Track local edit/push state separately from remote presence | A comment can be local-only, pending push, remotely missing, or remotely present independently of its Org TODO status. |
| Resolved state is normalized | Store `:resolved` explicitly and derive it with precedence | Filters should not parse Org TODO keywords or provider-specific fields. Backend metadata wins, then Org `:status`, then nil. |
| Local state is a flag set | Store `:local-state` as a list of flags | Local records can be edited, pending push, and in error at the same time. |
| Remote state and sync state are enums | Store each as one normalized value | Remote presence and aggregate sync health are mutually exclusive enough to render and filter as single states. |
| Identity records are backend-agnostic people with backend-qualified aliases | Comment records store raw backend ids plus `:backend`; people records map those ids to humans | The same person can participate across Confluence, Google Docs, and other backends. |
| Filter state is source-buffer scoped | Panels read and update the source buffer's filter state | Filters describe how a document's comments are viewed and should remain consistent across panels for that document. |
| Actionability is explicit-or-derived | `:actionable` true marks a record actionable; otherwise package derives attention state from sync, remote, and local state | Backends can flag assigned/mentioned/review-needed comments while the package still catches conflicts, errors, missing records, and pending pushes. |
| Remote deletion archives locally | Confirmed remote deletion archives the sidecar subtree with Org-native archive mechanics | Deletion is non-destructive and Org-native while keeping deleted records out of normal active comment views. |
| Filters use a public registry | Built-in filters are registered through the same small public filter API as extension filters | Future backends and personal config can add filters without depending on package internals. |
| Normalization is eager | Records are normalized at sidecar collection and backend import boundaries | Filters and renderers should consume backend-neutral fields without parsing provider-specific properties. |
| Persisted properties are package-native only | Durable sidecar writes use `ORG_COMMENTS_*`; legacy `HUB_COMMENT_*` requires explicit migration | Open-source package code should not carry permanent personal-format compatibility. |

## Backend-neutral record fields

Records may include these fields. Backends and stores should preserve unknown fields.

| Field | Values | Meaning |
| --- | --- | --- |
| `:backend` | symbol such as `org`, `confluence`, `google-docs` | Backend that owns remote operations for the record. |
| `:remote-id` | string or nil | Provider comment id for a root comment or reply. |
| `:remote-parent-id` | string or nil | Provider id of the parent remote comment for replies. |
| `:remote-thread-id` | string or nil | Provider thread/conversation id when distinct from comment id. |
| `:remote-state` | `present`, `missing`, `deleted`, `unconfirmed`, `unknown`, nil | Whether the record is known to exist remotely. |
| `:remote-author-id` | string or nil | Raw provider account/user id for the remote author. Resolve with the record's `:backend`. |
| `:remote-author-name` | string or nil | Provider display name for the remote author. |
| `:local-state` | list of flags or nil | Local lifecycle flags independent of remote presence. See below. |
| `:sync-state` | `clean`, `dirty`, `conflict`, `error`, `unknown`, nil | Aggregate synchronization state between local and remote. |
| `:sync-error` | string or nil | Last backend sync/push error summary for this record. |
| `:resolved` | boolean or nil | Backend-neutral resolved state derived from status or remote metadata. |
| `:actionable` | boolean or nil | Explicit backend-neutral flag for records requiring user attention. Derived actionability may also apply. |
| `:archived` | boolean or nil | Non-nil when the record is represented by an Org-archived sidecar subtree. |

Existing fields such as `:status`, `:sync-kind`, `:author`, `:created-at`, `:replies`, and source/anchor fields remain valid. The collaboration fields add remote and local collaboration semantics; they do not replace source anchoring or Org TODO status.

### Local state flags

` :local-state` is a list of zero or more flags:

| Flag | Meaning |
| --- | --- |
| `:local-only` | Exists only in the local sidecar; no remote id has been assigned. |
| `:draft` | Saved as an unfinished local draft and not ready to push. |
| `:edited` | Local content differs from the last known remote content. |
| `:pending-push` | Ready or queued to be sent to the remote backend; it has not successfully landed there yet. |
| `:push-error` | The last push attempt failed. Error details live in `:sync-error`. |

Examples: `(:local-only :draft)`, `(:edited :pending-push)`, and `(:pending-push :push-error)`.

### Resolved state precedence

` :resolved` is stored as a normalized boolean for filters and rendering. It is derived in this order:

1. Explicit backend remote resolved/unresolved metadata when available.
2. Local Org `:status` when backend metadata is absent: `RESOLVED` means true; `OPEN` and `TODO` mean false.
3. Nil/false when neither source provides a resolved signal.

### Actionability precedence

A record is actionable when either:

1. `:actionable` is explicitly non-nil; or
2. derived state needs attention:
   - `:sync-state` is `conflict` or `error`;
   - `:remote-state` is `missing` or `deleted`;
   - `:local-state` contains `:pending-push` or `:push-error`;
   - future backend metadata indicates assignment, mention, review, or another attention signal.

Explicit `:actionable nil` means there is no explicit backend/user actionability flag. It does not suppress derived actionability.

## Identity seam

The package should expose two extension points:

| Function | Responsibility |
| --- | --- |
| `org-comments-current-user-id-function` | Return the current backend-agnostic person id or nil. |
| `org-comments-resolve-person-function` | Resolve a backend/raw user id pair to a plist such as `(:id ID :name NAME :me BOOL)`. |

A local people directory may represent backend-agnostic people with backend-qualified aliases:

```elisp
(:id "person-hubert"
 :name "Hubert"
 :me t
 :identities ((:backend confluence :id "abc123")
              (:backend google-docs :id "people/xyz")))
```

Comment records store raw backend ids plus `:backend`, not fully qualified identity plists in each field. The package-level “mine” predicate should use normalized author identity:

1. Prefer `:remote-author-id` with the record's `:backend` when present.
2. Fall back to `:author` only for local-only records.
3. Treat a record as mine when the identity resolver returns `:me` or the resolved person id matches the current user id.

## Filter model

Generic filters should be package-owned and registry-based. Filter state is stored per source buffer; panel buffers read and update the source buffer's state so all views of one document stay consistent.

The first implementation should expose a small public registry, with built-in filters registered through the same API as extension filters:

- `org-comments-register-filter`
- `org-comments-filter-predicate`
- `org-comments-filter-apply`
- `org-comments-toggle-filter`

Built-in filters:

- `:show-resolved`
- `:show-missing`
- `:mine`
- `:drafts`
- `:actionable`

| Filter id | Default | Predicate intent |
| --- | --- | --- |
| `:show-resolved` | true | Hide records whose backend-neutral resolved state is true when false. |
| `:show-missing` | true | Hide records with `:remote-state` of `missing` when false. |
| `:show-deleted` | false | Show collected archived/deleted records when true. Collection must opt into archived records separately. |
| `:mine` | false | Include only records authored by the current user when true. |
| `:drafts` | false | Include only local-only, draft, edited, pending-push, or push-error records when true. |
| `:actionable` | false | Include only records marked actionable or records with conflict/error/missing/pending state when true. |

Root comments should remain visible when a visible reply matches a filter, so conversation context is not lost. Reply collections may be filtered independently inside the visible root record.

## Backend mapping requirements

Backends should map provider-native comment metadata into the collaboration fields:

- Confluence import maps provider comment ids, parent ids, author account ids/display names, and missing/deleted remote state.
- Google Docs import should map comment ids/thread ids, reply parentage, author ids/display names, resolved state, and deleted/missing state into the same fields.
- Local Org sidecar records use `:backend 'org`; local-only records without remote ids should derive `:local-state` from sidecar metadata and local edit/push markers.
- Confirmed remote deletion should archive the local sidecar subtree using Org-native archive mechanics after recording deleted metadata.

Backends must not require package filters to know provider-specific property names.

### Remote deletion and archive behavior

`missing` and `deleted` are intentionally different:

- `missing` means the record was expected remotely but absent or uncertain in the latest sync. It stays active and actionable.
- `deleted` means the backend positively reported deletion. The sidecar subtree should be archived by default, not destructively removed.

Normal collection hides archived/deleted comments by default. Archive visibility uses two layers:

- collection option `:include-archived` controls whether archived sidecar subtrees are read at all; default nil.
- filter `:show-deleted` controls whether collected archived/deleted records are visible; default nil.

This keeps the normal path simple and lets explicit audit/recovery views opt in.

## Sidecar property migration

Durable sidecar storage should use only package-native `ORG_COMMENTS_*` properties. Legacy `HUB_COMMENT_*` properties are migration input, not a permanent readable format.

Policy:

1. New writes use `ORG_COMMENTS_*` properties.
2. Normal collection may detect legacy `HUB_COMMENT_*` properties and report that migration is required, but should not silently treat them as the durable format.
3. Migration is explicit, not automatic on normal load.
4. Migration code is isolated in a removable module such as `org-comments-migrate.el`.
5. Future cleanup may remove the migration module after user data has moved.

Example property names:

- `ORG_COMMENTS_ID`
- `ORG_COMMENTS_TARGET_TEXT`
- `ORG_COMMENTS_REMOTE_ID`
- `ORG_COMMENTS_REMOTE_STATE`
- `ORG_COMMENTS_LOCAL_STATE`
- `ORG_COMMENTS_SYNC_STATE`

## Acceptance Criteria

- [ ] AC-1: Given a record from any backend, when it is normalized eagerly at collection/import time, then provider-specific remote metadata is available through backend-neutral collaboration fields.
- [ ] AC-2: Given Confluence and Google Docs shaped records, when generic filters evaluate them, then filter behavior does not branch on backend names.
- [ ] AC-3: Given a record with a visible matching reply and a non-matching root, when filters are applied, then the root remains visible with replies filtered to matching replies.
- [ ] AC-4: Given identity resolver configuration, when the `:mine` filter is enabled, then records authored by the current user are included regardless of backend.
- [ ] AC-5: Given remote-missing, local draft, pending-push, conflict, and resolved records, when corresponding filters are toggled, then inclusion follows the backend-neutral fields.
- [ ] AC-6: Given unknown provider-specific fields, when records pass through package collection/render/action paths, then unknown fields are preserved.
- [ ] AC-7: Given confirmed remote deletion, when import/sync applies the update, then the local sidecar subtree is archived rather than destructively deleted.
- [ ] AC-8: Given built-in and custom registered filters, when filters are applied, then both use the same public registry path.
- [x] AC-9: Given a sidecar containing legacy `HUB_COMMENT_*` properties, when normal collection encounters it, then the user is told explicit migration is required rather than silently reading the legacy format.
- [x] AC-10: Given a legacy sidecar, when the migration command is run, then properties are rewritten to `ORG_COMMENTS_*` and no comment content is lost.

## Invariants

- Backend `:sync` remains comments-only and must not trigger full document/page sync.
- Package filters must not contain backend-specific names or provider-specific property keys.
- Local Org sidecar behavior remains usable without any remote backend configured.
- Existing package action commands continue to use adapter seams for personal or richer UIs.
- Existing Org TODO status remains available and should not be replaced by remote collaboration fields.

## Scope

May modify:

- `packages/org-comments/` model, store, backend, filter, and tests.
- Backend adapters that normalize provider metadata into package fields.
- Personal UI adapters only to consume package fields or register package filters.

Must not modify in this design slice:

- Full document/page sync behavior.
- Provider API clients beyond metadata mapping.
- Personal keybinding policy except where binding package commands directly.

## Verification Plan

| Criterion | Method | Automated? |
| --- | --- | --- |
| AC-1 | Unit tests for normalization from provider-shaped plists | Yes |
| AC-2 | Fake Confluence/Google Docs records through package filter predicates | Yes |
| AC-3 | Filtered root-with-replies fixture test | Yes |
| AC-4 | Stub identity resolver and test `:mine` predicate | Yes |
| AC-5 | Table-driven filter inclusion tests for collaboration states | Yes |
| AC-6 | Round-trip store/normalization test preserving unknown plist keys | Yes |
| AC-7 | Sidecar fixture sync/import test archives deleted record | Yes |
| AC-8 | Built-in and custom filter registry tests | Yes |
| AC-9 | Legacy sidecar detection test | Yes |
| AC-10 | Legacy sidecar migration fixture test | Yes |

## Open Questions

1. Should implementation start with the collaboration normalization layer, the filter registry, or the sidecar property migration command?
2. Should the first migration command operate on one sidecar, one source file, or all discovered project sidecars?
3. Which existing personal/context-panel filters should be migrated first onto the package registry after the model layer lands?
