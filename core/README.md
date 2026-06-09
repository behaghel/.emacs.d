---
domain: configuration-runtime
status: draft
last-reviewed: 2026-06-08
---

# Configuration Runtime

## Ubiquitous Language

- **Interactive layer**: User-facing configuration that should not leak into ordinary batch loads.
- **Batch load**: Non-interactive Emacs execution used for smoke checks, linting, tests, and CI.
- **Force full load**: Explicit CI-like path that loads interactive modules to catch integration failures.
- **Predicate**: A small environment test such as interactive, batch, GUI, TTY, or CI.
- **Startup hot path**: Work that runs before Emacs becomes usable.

## Invariants

- Batch sessions must stay safe and predictable.
- Interactive-only modules must not be exposed on `load-path` during ordinary batch loads.
- Required runtime failures must remain visible in CI; fallbacks may be explicit but must not silently hide broken required paths.
- Runtime state should be routed under `var/` or package-specific managed locations rather than polluting the repo root.
- Package bootstrap must remain reproducible through pinned `straight.el` versions.

## Integration Notes

The runtime is the supplier of environment predicates, package bootstrap, path conventions, and load ordering.  Other domains should consume these contracts rather than duplicating environment detection or package bootstrap behavior.
