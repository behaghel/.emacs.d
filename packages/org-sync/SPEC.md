---
domain: publishing.org-sync
status: draft
last-reviewed: 2026-08-11
---

# Spec: Org Sync Status and Remote Tracking

## Problem

Org synchronization currently exposes provider-specific status UX, especially in `org-confluence-sync-status-*`. That makes status, fetch/pull/push language, window management, comment tracking, and future Google Docs parity harder to keep consistent.

`org-sync` should become the shared kernel for a Git/Magit-inspired Org sync status workflow. Provider packages should keep provider API behavior, but status modeling, remote-tracking sidecars, source markers, bottom-panel UI, and content/comment domain comparison should be provider-neutral.

## Goals

- Provide one `org-sync-status` bottom panel UX for Confluence now and Google Docs later.
- Model remote tracking like Git:
  - `g` refreshes local status without network.
  - `f` fetches remote refs into tracking state without mutating source/comments.
  - pull applies remote to local.
  - push publishes local to remote.
- Track status per first-class domain: `content` and `comments`.
- Store durable tracking/base/fetched refs in `source.sync.org`.
- Keep source Org files and comments sidecars free of volatile global sync status/hashes.
- Remove old Confluence sync-status display/cache/render/marker modules after migration.

## Non-goals for v1

- Assets/attachments tracking.
- Multiple remotes for one Org source file.
- Generic conflict resolution or force pull/push for diverged domains.
- Auto-fetch when opening or enabling sync mode.
- Direct mutation of `.comments.org` by `org-sync`.
- Backward-compatible Confluence `org-confluence-sync-status-*` modules or command names.

## Package Dependencies

`org-sync` is a stand-alone UX/shared-kernel package and should hard depend on:

- `org`
- `context-panels`
- `magit-section`
- `org-comments`

Provider packages depend on `org-sync`; `org-sync` must not depend on Confluence or Google Docs packages.

## Data Ownership

### Source Org file

Stores provider identity and human-authored document metadata only, for detection without reading sync sidecars.

Examples:

- Confluence page id / space / base URL / title metadata.
- Future Google Docs document id / account metadata.

It must not store volatile status, ahead/behind, fetched remote refs, base refs, or sync hashes.

### Comments sidecar (`.comments.org`)

Stores durable local comment records only:

- local comment ids
- remote ids
- statuses/resolution state
- anchors/target data
- bodies/replies
- provider metadata needed for mutation

It must not store global sync base/fetched refs.

### Sync sidecar (`.sync.org`)

Stores provider-neutral remote-tracking state:

- provider kind/id/account/url/title when safe
- per-domain base local refs
- per-domain base remote refs
- per-domain fetched remote refs
- last fetched timestamps and fetch errors
- minimal remote comment refs/summaries, not full comment bodies by default
- identity consistency metadata

`source.sync.org` should be safe and intended for version control by default. It must not include OAuth tokens, raw private API payloads, or full remote comment bodies unless an explicit debug command writes non-committed diagnostics elsewhere.

## Domains

V1 supports exactly:

- `content`
- `comments`

### Content refs

- Current local content refs are recomputed on every local refresh.
- Providers may override local content ref computation to match their publish/export scope.
- Generic fallback is a canonical Org buffer hash.
- Remote content refs are provider-supplied.
  - Confluence: page version plus optional storage/content hash.
  - Google Docs later: revision id plus optional Docs JSON/IR hash.

### Comment refs

- Local comment refs are computed from normalized `org-comments` records filtered for the active provider.
- Providers adapt remote comments into a shared normalized remote comment shape.
- `org-sync` canonicalizes/sorts/hashes normalized comment records consistently.
- Comment body hashes may be stored; full bodies should not be stored by default.
- Copilot/local-only comments are excluded unless the provider adapter marks them syncable for that provider.

## Status Model

Each domain has:

- current local ref: recomputed now
- fetched remote ref: last fetched remote-tracking ref from `.sync.org`
- base local ref: local ref accepted as matching base
- base remote ref: remote ref accepted as matching base

Statuses:

- `clean`: current local ref equals base local ref and fetched remote ref equals base remote ref.
- `ahead`: current local ref changed and fetched remote ref is unchanged.
- `behind`: current local ref unchanged and fetched remote ref changed.
- `diverged`: current local ref changed and fetched remote ref changed.
- `unknown`: missing valid identity, base, or fetched data.
- `conflicted`: provider/generic model reports a blocking, non-mergeable condition.
- `fetch-error`: latest fetch failed for that domain; previous fetched refs may remain available but status must show the error.

Overall status derives from domain statuses. Blocking/conflicted/diverged states dominate aggregate mutation actions.

## Baseline Semantics

`B` baseline means: “accept current local files and currently fetched remote refs as corresponding.”

Baselining records a pair per domain:

- `base-local-ref = current local ref`
- `base-remote-ref = fetched remote ref`

After baselining, the domain is clean by definition until local or remote refs change.

If no baseline exists after fetch, aggregate pull/push remain disabled until `B` is run.

## Fetch/Pull/Push Semantics

### Refresh

`g` recomputes local refs and re-renders using existing tracking state. It performs no network I/O.

### Fetch

`f` fetches all supported provider domains and writes fetched refs/issues into `.sync.org`. It must not mutate:

- source Org file
- `.comments.org`
- remote provider state

Partial fetch failures are per-domain. Successful domain refs are preserved/written; failed domains keep previous usable refs where possible and show `fetch-error`/`unknown` with an issue.

### Pull

Pull mutates local state from fetched/remote provider data through provider callbacks.

- `F`: aggregate pull for eligible behind domains.
- `C`: pull content.
- `M`: pull comments.

If multiple domains are behind, aggregate `F` may prompt/select domains. Diverged/conflicted/unknown domains are refused in generic v1.

### Push

Push mutates remote provider state from local state through provider callbacks.

- `p`: aggregate push for eligible ahead domains.
- `c`: push content.
- `m`: push comments.

Diverged/conflicted/unknown domains are refused in generic v1.

### Base advancement

- `fetch` updates fetched remote refs only; base refs are unchanged.
- successful pull updates local state, recomputes local refs, and advances base refs for the pulled domains.
- successful push obtains/fetches new remote refs and advances base refs for pushed domains.
- aggregate operations advance only domains that succeeded.

## Identity and Reset

Provider detection returns one active document descriptor for the current source buffer. V1 supports exactly one provider/remote per source.

If source identity and `.sync.org` identity disagree, status is blocking/unknown and fetch/pull/push are refused.

`R` / `org-sync-reset-tracking` explicitly resets tracking for the current source identity:

- archive or backup old `.sync.org`
- initialize provider identity
- clear base/fetched refs
- do not mutate source/comments/remote
- leave status `unknown` until `f` and `B`

## Provider Adapter Contract

Providers register adapters with `org-sync`.

Required adapter fields:

- `:kind` — provider symbol, e.g. `confluence`, `google-docs`.
- `:detect` — `(SOURCE-BUFFER) -> descriptor-or-nil`.
- `:fetch` — `(DESCRIPTOR DOMAINS) -> fetch-result`.

Recommended action callbacks:

- `:local-content-ref` — provider-specific local content ref, optional fallback to generic hash.
- `:local-comment-record-p` — predicate for provider-relevant local comments.
- `:pull-content`
- `:pull-comments`
- `:push-content`
- `:push-comments`
- `:open-remote`

Descriptor shape:

```elisp
(:kind confluence
 :remote-id "123"
 :account "optional-account"
 :remote-url "https://..."
 :title "Remote title"
 :source-file "/path/source.org")
```

Fetch result shape:

```elisp
(:provider confluence
 :remote-id "123"
 :remote-url "https://..."
 :fetched-at "2026-08-11T10:20:00+0200"
 :domains
 ((content
   :remote-ref (:version "42" :hash "abc" :title "Page title")
   :summary "remote v42")
  (comments
   :remote-records (...normalized remote comment records...)
   :remote-ref (:hash "def" :count 12 :updated-at "...")
   :summary "12 remote comments"))
 :issues (...))
```

Providers adapt raw remote comments; `org-sync` owns canonicalization and hashing.

## Commands

- `org-sync-mode`: source-buffer minor mode.
  - detects provider
  - reads tracking sidecar
  - computes local refs
  - updates source marker
  - registers bottom panel provider
  - no auto-fetch
- `org-sync-status`: open the bottom status panel for the current source, enabling `org-sync-mode` if needed.
- `org-sync-refresh`: `g`, local refresh only.
- `org-sync-fetch`: `f`, network fetch only into tracking state.
- `org-sync-pull`: `F`, aggregate pull.
- `org-sync-push`: `p`, aggregate push.
- `org-sync-pull-content`: `C`.
- `org-sync-pull-comments`: `M`.
- `org-sync-push-content`: `c`.
- `org-sync-push-comments`: `m`.
- `org-sync-baseline`: `B`.
- `org-sync-reset-tracking`: `R`.
- `org-sync-help`: `?`.
- `org-sync-close`: `q`.

No primary generic `sync` command is part of the v1 UX. Existing provider sync commands may exist until deleted/migrated, but the user-facing target is fetch/pull/push/status.

## Bottom Panel UX

`org-sync` owns one generic `context-panels` bottom provider.

- Provider name: `org-sync`.
- View id: `org-sync-status`.
- Buffer name: `*Org Sync*`.
- Rendering uses `magit-section` as a hard dependency.

Preferred layout:

```text
Org Sync: confluence DOC-123  main.org
Head: local 8f3a21c  Origin: v42 fetched 2026-08-11 10:20

Unpushed changes
  Content       local changed since v42                 c push content
  Comments      2 local comments pending push           m push comments

Unpulled changes
  Content       remote v43 available                    C pull content
  Comments      3 remote comment updates                M pull comments

Conflicts
  (empty when none)

Issues
  ⚠ People cache stale                                  r resolve people
  ⚠ 1 dangling remote anchor                            a repair anchors

Actions: g refresh, f fetch, p push, F pull, c/m push domain, C/M pull domain, B baseline, R reset, ? help, q close
```

Sections:

- `Unpushed changes`
- `Unpulled changes`
- `Conflicts`
- `Issues`
- optionally compact `Clean` / `Unknown`

Only sections with content should be expanded by default where practical.

## Source Marker UX

`org-sync-mode` owns a generic compact source marker.

Marker examples:

- `⇅ clean`
- `⇅ ?`
- `⇅ ↑`
- `⇅ ↓`
- `⇅ ↑↓`
- `⇅ !`
- `⇅ ↑2 ↓1`

Counts are domain counts, not comment/item counts. Tooltip/help text explains domain details, e.g. `Content ahead; Comments behind`. Clicking/RET opens `org-sync-status`. The marker uses only tracking/local cache data and performs no network fetch.

## Confluence Migration

Create `packages/org-confluence/org-confluence-org-sync.el`.

It registers the Confluence adapter and owns Confluence-specific behavior:

- detect Confluence source metadata
- fetch page content refs
- fetch/normalize remote comments
- Confluence pull/publish callbacks
- Confluence comment import/push callbacks
- Confluence-specific issues such as people cache, anchors, remote page availability

Migrate/delete old modules after replacement:

- `org-confluence-sync-status-cache.el`
- `org-confluence-sync-status-collect.el` generic parts
- `org-confluence-sync-status-display.el`
- `org-confluence-sync-status-marker.el`
- `org-confluence-sync-status-render.el`
- `org-confluence-sync-status-actions.el`
- `org-confluence-sync-status.el`

No Confluence status command names need to be preserved. Rename user-facing command to `org-sync-status` and update keybindings/docs.

Future Google Docs adapter should be named `org-google-docs-org-sync.el`.

## Acceptance Criteria

1. Given an Org buffer with Confluence metadata, when `org-sync-mode` is enabled, then exactly one `confluence` adapter is detected and a compact source marker is shown without network I/O.
2. Given no valid provider metadata, when `org-sync-status` runs, then it fails clearly with no provider detected.
3. Given multiple providers detect a source, when status is requested, then v1 fails clearly with multiple providers detected.
4. Given no `.sync.org`, when status opens, then content and comments domains are `unknown` and push/pull are disabled.
5. Given `f` fetch succeeds, when status refreshes, then `.sync.org` stores fetched remote refs for content/comments and source/comments files are unchanged.
6. Given fetched refs exist but no base refs, when `B` runs, then current local refs and fetched remote refs are recorded as base refs and domains become `clean`.
7. Given local content changes after baseline and remote content is unchanged, when status refreshes, then content is `ahead`.
8. Given fetched remote content changes and local content is unchanged from baseline, when status refreshes, then content is `behind`.
9. Given both local and fetched remote content changed since baseline, when status refreshes, then content is `diverged` and aggregate pull/push refuse that domain.
10. Given local provider-relevant comments change after baseline, when status refreshes, then comments are `ahead`.
11. Given fetched remote comments change after baseline, when status refreshes, then comments are `behind`.
12. Given aggregate `F` runs with only behind eligible domains, then provider pull callbacks run for those domains and base refs advance only for successful domains.
13. Given aggregate `p` runs with only ahead eligible domains, then provider push callbacks run for those domains and base refs advance only for successful domains.
14. Given source identity differs from `.sync.org` identity, when status opens, then a blocking identity mismatch is shown and fetch/pull/push are refused until `R` reset.
15. Given `R` reset is confirmed, then old `.sync.org` is archived/backed up, new tracking identity is initialized, and source/comments/remote are unchanged.
16. Given Confluence status migration is complete, then no active code requires `org-confluence-sync-status-*` modules and keybindings/docs use `org-sync-status`.
17. Given Google Docs later registers an adapter, then it can reuse the same bottom status panel, source marker, sidecar tracking, command keys, and content/comments status model without Confluence dependencies.

## Verification Plan

- Add `packages/org-sync/test/` ERT coverage for:
  - model status derivation
  - sync sidecar read/write/reset/baseline
  - provider detection and ambiguity
  - local content/comment ref calculation
  - fake provider fetch/pull/push callbacks
  - bottom panel rendering/keybindings
  - source marker rendering
- Add Confluence adapter tests for:
  - detection from existing Confluence metadata
  - fetch result normalization
  - content/comment callbacks delegating to existing Confluence APIs
  - deletion of old sync-status module usage
- Keep `packages/org-comments/test/*` green for comments hashing/filtering integration.
- Add a later Google Docs adapter test proving provider reuse without Confluence dependencies.
