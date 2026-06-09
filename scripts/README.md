---
domain: quality-system
status: draft
last-reviewed: 2026-06-08
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

## Integration Notes

The quality system verifies contracts supplied by runtime, modules, specs, and governance docs.  Scripts may encode checks, but durable behavioral expectations should be captured in specs or architecture documentation first.
