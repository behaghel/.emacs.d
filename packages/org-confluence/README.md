---
domain: publishing/confluence
status: draft
---

# Confluence Publishing

## Ubiquitous Language

- **Confluence publishing**: Sending authored Org content to Confluence Storage Format and page APIs.
- **Package boundary**: Reusable `org-confluence` code that should remain extractable from this Emacs configuration.
- **Activation policy**: Personal configuration that wires the reusable package into this repository's interactive workflows.

## Invariants

- Reusable package code under `packages/org-confluence/` should keep defaults neutral and extractable.
- Personal activation belongs in the surrounding configuration module, not in the package boundary.
- The package declares its org-comments and ox-sync dependencies; reusable customization belongs under Org, not a configuration-specific customization group.
- Local Storage XHTML export/import does not require a Confluence account, credentials, or the cfl executable, and previews do not rewrite the source. Import requires native libxml2 support and diagnoses its absence before conversion.
- Live CLI and authenticated REST operations are separate from local conversion. Loading the package and enabling local mode integration do not implicitly perform remote operations.
- Package tests use an owned temporary home and runtime caches, explicit offline fixtures, and fail-closed guards against unmocked credential reads, networking, and external processes. Best-effort error handling cannot turn unexpected I/O into a passing test.
- Missing-CLI diagnostics name the executable and configuration remedy without dumping PATH values. The offline demo reports this expected failure in place, preserving the selected window, buffer, and point.
- Publishing must preserve authoring semantics while translating to Confluence-specific storage/API contracts.
- See `README.org` for package-level usage and implementation notes.
