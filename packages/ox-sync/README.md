---
domain: publishing/org-sync
status: draft
---

# Org Export Synchronization Shared Kernel

## Boundary

`ox-sync` owns provider-neutral synchronization planning, local and remote reference modeling, tracking sidecars, status presentation, and Org asset semantics. Provider packages own authentication, network APIs, remote mutation, and translation from provider payloads.

## Tracking Model

A source document may have one active provider identity and a colocated `.sync.org` tracking sidecar. Tracking data contains provider identity plus per-domain base and fetched references; it contains no credentials or raw private payloads.

The synchronization domains are `content` and `comments`. Each domain compares current local, fetched remote, base local, and base remote references and derives `clean`, `ahead`, `behind`, `diverged`, `unknown`, `conflicted`, or `fetch-error`.

A baseline is the pair of local and remote references last accepted as corresponding, not a backup copy of the document. Explicit baseline acceptance and successful pull/push advance that pair for the affected domains. Fetch alone does not advance the baseline.

## Operation Semantics

- Refresh recomputes local references without network access.
- Fetch updates remote-tracking references without changing source content, comments, or remote state.
- A successful fetch records its local completion time, the domains whose references were returned, and whether those references changed. Unchanged references still constitute a successful fetch. Failed fetches or results without usable requested-domain references do not advance the last-success metadata; local refresh does not change it.
- Fetch provides immediate progress/completion feedback. Status shows the last successful fetch and outcome, plus each domain's current local, fetched remote, and baseline reference pair. Numbered versions and opaque revision IDs are displayed when available; hashes are abbreviated.
- Baseline accepts current local and fetched references as corresponding.
- Pull applies eligible remote changes through provider callbacks.
- Push applies eligible local changes through provider callbacks.
- Diverged, conflicted, and unknown domains block generic pull and push.
- Successful operations advance bases only for domains that completed successfully.
- Push requires a fetch callback before remote mutation. After publishing, it fetches the affected remote references before advancing their bases; a later fetch does not misclassify the just-published revision as an unpulled change.

## Provider Contract

Providers register detection, fetch, reference, pull, push, and open-remote callbacks. Raw provider comments are normalized before `ox-sync` canonicalizes and hashes them. The kernel never depends on Confluence or Google Docs packages.

## Asset Contract

The package detects standalone Org image links, resolves local paths relative to the source buffer, extracts captions, creates stable generated filenames, and reports missing sources. Providers decide how remote assets are uploaded, downloaded, or reused.

## Invariants

- A source has at most one active provider identity.
- Source identity is detected afresh. Provider kind, remote ID, and optional account identify a tracked document; identity mismatches display a blocking status and refuse fetch, baseline, pull, and push until explicitly reset.
- Tracking files contain no OAuth tokens, credentials, or full private API payloads.
- Fetch never mutates authored source or comment sidecars.
- Provider-neutral logic remains deterministic and testable without network access.
- Status UI reads local and tracking state without implicit fetches.

## Package Boundary

- The public package, libraries, and functions use the `ox-sync` namespace. Remote provider IDs, `.sync.org` filenames, the version-1 `org_sync_sidecar_version` field, and the default `assets/org-sync` cache location remain stable.
- The context-panels provider ID remains `org-sync`; it is an integration identifier, not a legacy library or function alias.
- Loading requires Org, org-comments, context-panels, Magit Section, and Transient, but no provider packages, personal configuration, accounts, or credentials. Activation is explicit and does not fetch.
- Status adapters and the URL-oriented remote-document registry are separate interfaces. A remote-document adapter does not implicitly provide status fetch/pull/push callbacks.

Installation, dependency setup, adapter usage, and the offline workflow are documented in the [package manual](README.org).

## Related Material

- Status-model delivery specification: [`SPEC.md`](SPEC.md)
- Asset-planning delivery history: [`assets.plan.md`](assets.plan.md)
