---
domain: publishing/google-docs
status: draft
---

# Org Google Docs Adapter

## Boundary

The adapter publishes and synchronizes Org documents with Google Docs while delegating base document transport, OAuth, and link metadata to upstream `gdocs`. Reusable adapter behavior lives in this package; personal activation, account selection, and keybindings live in `modules/interactive/org/google-docs.el`.

## Document Synchronization

- Facade commands create, push, pull, open, and inspect linked Google Docs through upstream `gdocs`.
- Package loading remains safe when `gdocs` or credentials are unavailable; commands that need them fail with actionable diagnostics.
- Manual synchronization is the default.
- Recognized Org semantics are classified before mutation. Unsupported constructs fail closed rather than degrading silently.
- Native footnotes, standalone images, captions, and other supported semantic structures use explicit conversion seams and round-trip handling.

## Comment Integration

Google Docs comments use the provider-neutral [`org-comments`](../org-comments/README.md) model.

- Pull imports remote roots and replies into the local sidecar while preserving local notes and unsynchronized replies.
- Repeated imports update remote-owned fields and mark absent remote records without deleting local history.
- Replies and resolve actions are explicit remote mutations routed through backend capabilities.
- Local-only root creation remains unsupported while Google lacks a reliable public API for native anchored Docs comments.
- Opening a linked comment uses the best available remote document URL.

## Asset and Semantic Safety

- Org-level asset planning is provided by [`org-sync`](../org-sync/README.md); Drive upload and download remain provider responsibilities.
- Credentials and OAuth tokens never enter tracked files or diagnostic output.
- Async callbacks preserve the originating buffer and account context.
- A failed preflight leaves the remote document unchanged.
- Nested lists inside shaded blocks remain unsupported and block mutation rather than flattening silently.

## Invariants

- Package files do not require private `hub-*` libraries.
- Upstream `gdocs` remains the source of truth for document identity and base sync metadata.
- Remote mutations are explicit and command-driven.
- Pull and import preserve user-owned local content.
- Tests use fixtures and mocked provider calls unless an opt-in live conformance run is requested.

## Related Material

- Source semantic contract: [`docs/source-content-semantics.md`](docs/source-content-semantics.md)
- Delivery history: [`implementation.plan.md`](implementation.plan.md)
