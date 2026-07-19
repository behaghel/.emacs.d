---
domain: configuration-system
status: draft
last-reviewed: 2026-06-08
---

# Shared Libraries

## Ubiquitous Language

- **Hub helper**: Repo-owned helper named with the `hub/` prefix.
- **Shared helper**: Reusable behavior consumed by more than one module or domain.
- **Key semantic**: A stable meaning attached to a leader/localleader key family.
- **Structural helper**: Navigation or manipulation behavior over syntax, Tree-sitter nodes, or balanced expressions.

## Invariants

- Shared helpers must be small, explicit, and safe to load from multiple domains.
- Any module that calls shared `hub/` helpers must require the relevant helper library explicitly.
- Shared libraries must not introduce broad interactive side effects merely by being required.
- Key semantics should remain discoverable and aligned with documented leader/localleader conventions.
- Shared code should avoid depending on optional interactive packages unless guarded or moved into the consuming domain.
- Org marginalia model helpers are pure collection/layout functions: they may parse Org buffers, but they must not open windows, modify source text, or perform export side effects.
- Org quote metadata helpers expose canonical quote attributes such as `#+ATTR_QUOTE: :author ...` without owning exporter styling or interactive insertion behavior.
- Org comment storage helpers keep source documents clean by writing region-targeted review comments to colocated `*.comments.org` sidecars; UI commands live in the interactive Org domain.

## Integration Notes

This domain is a shared kernel.  Changes can affect runtime, editing, writing, email, and development workflows.  Keep the shared surface narrow; behavior that serves only one domain belongs in that domain instead.
