---
domain: knowledge-writing
status: draft
last-reviewed: 2026-06-10
---

# Interactive Writing

## Ubiquitous Language

- **Prose workflow**: Editing behavior for natural-language text, including wrapping, spell checking, readability, and focused writing modes.
- **Snippet**: A reusable expansion template managed by yasnippet.
- **Template**: An auto-insert skeleton for new writing-oriented files.
- **Markup mode**: A text authoring mode such as Markdown or AsciiDoc configured for writing and preview/export ergonomics.

## Invariants

- Writing configuration belongs to the knowledge-writing domain, not the generic editing foundation.
- Snippets and auto-insert templates must keep their assets under the configured snippet/template directories.
- Prose and markup bindings should conform to the shared Evil leader semantics.
- Writing modules are interactive-only and must not be exposed in ordinary batch loads.
