---
domain: configuration-system/quality
status: draft
---

# Quality System

## Ubiquitous Language

- **Load check**: Batch smoke check that loads `init.el`.
- **Full load**: CI-like load that forces interactive modules to catch integration failures.
- **Architecture lint**: Static check enforcing layer and module-boundary rules.
- **Hook**: Pre-commit gate for formatting, checkdoc, parsing, or tests.
- **Measurement script**: Tool that records timings without changing runtime behavior.

## Invariants

- Checks should run through the declarative development environment.
- Formatting/checkdoc/parse failures should be visible before commit.
- Full-load checks must not hide required module failures.
- Measurement tooling must not change runtime behavior or claim improvements without before/after data.
- CI cache and package version behavior should remain reproducible.

## Architecture Lint

- Modules and local libraries explicitly require `hub-utils` when referencing
  helpers defined by that library. Defining module-owned `hub/*` names does not
  create that dependency; comments and strings are not executable references.
- Dependency checks read source forms without evaluating them and reject
  unreadable source rather than silently skipping it.
- Domain tests live below `test/<domain>/`; only test files directly inside
  `test/` violate this boundary.

## Integration Notes

The quality system verifies contracts supplied by runtime, modules, specs, and governance docs. Scripts may encode checks, but durable behavioral expectations should be captured in specs or architecture documentation first.
