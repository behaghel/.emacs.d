---
domain: configuration-system/runtime
status: draft
---

# Configuration Runtime

## Ubiquitous Language

- **Interactive layer**: User-facing configuration that should not leak into ordinary batch loads.
- **Batch load**: Non-interactive Emacs execution used for smoke checks, linting, tests, and CI.
- **Force full load**: Explicit CI-like path that loads interactive modules to catch integration failures.
- **Predicate**: A small environment test such as interactive, batch, GUI, TTY, or CI.
- **Startup hot path**: Work that runs before Emacs becomes usable.

## Invariants

- Tracked `.el` files are the authoritative executable configuration.
  Documentation and validation workflows do not generate or overwrite them
  from Org documents.
- Batch sessions must stay safe and predictable.
- Interactive-only modules must not be exposed on `load-path` during ordinary batch loads.
- Required runtime failures must remain visible in CI; fallbacks may be explicit but must not silently hide broken required paths.
- Runtime state should be routed under `var/` or package-specific managed locations rather than polluting the repo root.
- Package bootstrap must remain reproducible through pinned `straight.el` versions.
- `core/core-packages.el` is the single package bootstrap implementation used by
  startup and package maintenance commands. A bootstrap failure stops startup
  with the original error rather than attempting another implementation.
- `emacs -Q` provides a configuration-free recovery session.

## Context Panels Dependency

- The default `context-panels` source is
  [`behaghel/context-panels`](https://github.com/behaghel/context-panels), managed
  by Straight and pinned in `straight/versions/default.el`.
- A nonempty `CONTEXT_PANELS_PATH` explicitly selects a local development
  directory instead. The directory contains readable `context-panels.el` and
  `context-panels-org.el` files; invalid overrides signal an actionable
  source-selection error rather than silently selecting a remote recipe.
- An unset or empty override uses the managed dependency. Merely having a
  checkout under `~/ws/` does not change the selected source.
- The ERT runner uses the same override, otherwise using the managed build
  under `STRAIGHT_BUILD_PATH` (default `straight/build`).
- Selecting a source does not activate context panels in any buffer.

For a local development session, start Emacs with
`CONTEXT_PANELS_PATH="$HOME/ws/context-panels" emacs`. Tests use the same variable:
`CONTEXT_PANELS_PATH="$HOME/ws/context-panels" devenv -q shell -- ./scripts/elisp-ert`.

## Integration Notes

The runtime is the supplier of environment predicates, package bootstrap, path conventions, and load ordering.  Other domains should consume these contracts rather than duplicating environment detection or package bootstrap behavior.
