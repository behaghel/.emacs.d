---
domain: publishing
status: draft
last-reviewed: 2026-06-10
---

# Confluence Publishing

## Ubiquitous Language

- **Confluence publishing**: Sending authored Org content to Confluence Storage Format and page APIs.
- **Package boundary**: Reusable `org-confluence` code that should remain extractable from this Emacs configuration.
- **Activation policy**: Personal configuration that wires the reusable package into this repository's interactive workflows.

## Invariants

- Reusable package code under `packages/org-confluence/` should keep defaults neutral and extractable.
- Personal activation belongs in the surrounding configuration module, not in the package boundary.
- Publishing must preserve authoring semantics while translating to Confluence-specific storage/API contracts.
- See `README.org` for package-level usage and implementation notes.
