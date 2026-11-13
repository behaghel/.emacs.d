---
domain: publishing/google-docs
status: draft
---

# Org Google Docs Adapter

Source setup and local diagnostics are documented in the [package manual](README.org).

## Boundary

The adapter publishes and synchronizes Org documents with Google Docs while delegating base document transport, OAuth, and link metadata to upstream `gdocs`. Reusable adapter behavior lives in this package; personal activation, account selection, and keybindings live in `modules/interactive/org/google-docs.el`.

## Dependency Source

The repository configuration uses the semantic-capable
[`behaghel/gdocs`](https://github.com/behaghel/gdocs) fork, pinned through Straight.
An ordinary installation does not depend on a checkout under `~/ws/` or select a
less capable upstream implementation when that directory is absent.

A nonempty `GDOCS_PATH` or an explicitly configured
`hub/org-google-docs-gdocs-repository` selects a local development checkout.
Invalid overrides report an error instead of falling back. Leaving the override
unset uses the managed package. Style request tests use `GDOCS_PATH` when set,
otherwise using the managed build and its dependencies.

## Document Synchronization

- Facade commands create, push, pull, open, and inspect linked Google Docs through upstream `gdocs`.
- Package loading remains safe when `gdocs` or credentials are unavailable; commands that need them fail with actionable diagnostics.
- Manual synchronization is the default.
- Explicit replace-local recovery uses Google's current body without merging local edits. It preserves local file metadata and document identity, validates conversion before one confirmation, and backs up the entire current buffer before replacement. Editing remains available during fetch; edits made during fetch are included in the backup. A rejected confirmation, failed fetch, failed conversion, or failed backup leaves the local body unchanged. Recognized remote constructs that cannot be preserved block recovery. Recovery does not write to Google.
- Recognized Org semantics are classified before mutation. Unsupported constructs fail closed rather than degrading silently.
- Native footnotes, standalone images, captions, and other supported semantic structures use explicit conversion seams and round-trip handling.
- Uniform bold across an imported heading is heading presentation, not Org inline emphasis. Partial bold that contrasts with the rest of the heading and other visible emphasis remain content. Invisible authored bold distinctions flattened by heading typography are not reconstructed.
- Static `status:` and `confluence-status:` labels retain label text, colour, original protocol, and provider marker identity across conversion and diffing. Their presentation is configurable; they do not imply native dropdown option sets. Unsupported colours or rich-label combinations block mutation.
- Rendered Org separates semantic blocks from adjacent text and lists while keeping consecutive list items compact. Repeated conversion retains those boundaries.

## Comment Integration

Google Docs comments use the provider-neutral [`org-comments`](../org-comments/README.md) model.

- Pull imports remote roots and replies into the local sidecar while preserving local notes and unsynchronized replies.
- Repeated imports update remote-owned fields and mark absent remote records without deleting local history.
- Replies and resolve actions are explicit remote mutations routed through backend capabilities.
- Local-only root creation remains unsupported while Google lacks a reliable public API for native anchored Docs comments.
- Opening a linked comment uses the best available remote document URL.

## Asset and Semantic Safety

- Org-level asset planning is provided by [`ox-sync`](../ox-sync/README.md); Drive upload and download remain provider responsibilities.
- Credentials and OAuth tokens never enter tracked files or diagnostic output.
- Async callbacks preserve the originating buffer and account context.
- A failed preflight leaves the remote document unchanged.
- Nested lists inside shaded blocks remain unsupported and block mutation rather than flattening silently.

## Standalone Boundary

The facade declares org-comments and ox-sync as required dependencies. The
semantic-capable gdocs implementation is optional for loading and necessary for
conversion and synchronization commands. Its source and transitive dependencies
are selected explicitly; the adapter does not initialize personal account or
style configuration.

Package classification tests consume plain audit fixtures. Tests that load this
repository's typographic auditor and specimen belong to the consumer integration
suite, not the reusable package. Offline diagnostics use real local conversion
and request planning without authentication or remote mutation. Live conformance
requires the exact explicit flag `ORG_GOOGLE_DOCS_LIVE_TESTS=1`; other values do
not authorize it. Live account setup consumes explicit environment values or
already configured gdocs accounts, never private consumer callbacks. Forced pull
previews bind upstream auto-sync/integrity controls dynamically even when the
facade is compiled before the optional client is loaded.

## Invariants

- Package files do not require private `hub-*` libraries.
- Upstream `gdocs` remains the source of truth for document identity and base sync metadata.
- Remote mutations are explicit and command-driven.
- Ordinary pull and import preserve user-owned local content. Explicit replace-local recovery discards the local body only after confirmation and a successful complete backup.
- Tests use fixtures and mocked provider calls unless an opt-in live conformance run is requested.

## Related Material

- Source semantic contract: [`docs/source-content-semantics.md`](docs/source-content-semantics.md)
- Delivery history: [`implementation.plan.md`](implementation.plan.md)
