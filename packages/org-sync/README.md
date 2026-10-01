---
domain: publishing/org-sync
status: draft
---

# Org Sync Shared Kernel

## Boundary

`org-sync` owns provider-neutral synchronization planning, local and remote reference modeling, tracking sidecars, status presentation, and Org asset semantics. Provider packages own authentication, network APIs, remote mutation, and translation from provider payloads.

## Tracking Model

A source document may have one active provider identity and a colocated `.sync.org` tracking sidecar. Tracking data contains provider identity plus per-domain base and fetched references; it contains no credentials or raw private payloads.

The synchronization domains are `content` and `comments`. Each domain compares current local, fetched remote, base local, and base remote references and derives `clean`, `ahead`, `behind`, `diverged`, `unknown`, `conflicted`, or `fetch-error`.

## Operation Semantics

- Refresh recomputes local references without network access.
- Fetch updates remote-tracking references without changing source content, comments, or remote state.
- Baseline accepts current local and fetched references as corresponding.
- Pull applies eligible remote changes through provider callbacks.
- Push applies eligible local changes through provider callbacks.
- Diverged, conflicted, and unknown domains block generic pull and push.
- Successful operations advance bases only for domains that completed successfully.

## Provider Contract

Providers register detection, fetch, reference, pull, push, and open-remote callbacks. Raw provider comments are normalized before `org-sync` canonicalizes and hashes them. The kernel never depends on Confluence or Google Docs packages.

## Asset Contract

The package detects standalone Org image links, resolves local paths relative to the source buffer, extracts captions, creates stable generated filenames, and reports missing sources. Providers decide how remote assets are uploaded, downloaded, or reused.

## Invariants

- A source has at most one active provider identity.
- Identity mismatches block network and mutation operations until explicitly reset.
- Tracking files contain no OAuth tokens, credentials, or full private API payloads.
- Fetch never mutates authored source or comment sidecars.
- Provider-neutral logic remains deterministic and testable without network access.
- Status UI reads local and tracking state without implicit fetches.

## Related Material

- Status-model delivery specification: [`SPEC.md`](SPEC.md)
- Asset-planning delivery history: [`assets.plan.md`](assets.plan.md)
