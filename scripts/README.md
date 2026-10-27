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
- Entering the development shell installs hooks but does not run test or
  pre-commit suites. Checks run on commit, in CI, or through explicit commands.

## Coding-agent Environment

- The development environment provides the shared `agent-session` launcher and
  its `as` shorthand from the locked `nixos-config` input in `devenv.yaml`.
- Managed worktrees live under the ignored `.worktrees/` subtree. The launcher
  defaults to Pi and runs one-off commands through quiet devenv in the selected
  worktree, preserving arguments and exit status.
- Local links are restricted to ignored `devenv.local.nix`, `devenv.local.yaml`,
  and `.envrc.local` overrides. Emacs private settings, credentials, package
  caches, Pi histories, and runtime state are not shared through these links.
- Agent workflow and safety rules are defined in
  [repository instructions](../AGENTS.md#coding-agent-sessions-pi).

## ERT Test Coverage

- The full ERT invocation discovers `*-test.el` files under both `test/` and
  package-local `packages/*/test/` directories, in deterministic order.
- Package source changes select that package's tests; explicit test paths remain
  runnable individually. Existing configuration-specific selections are preserved.
- `scripts/elisp-ert --list [changed-path ...]` reports the same selection used
  for execution, without loading tests or starting Emacs.
- Configuration tests and each package's tests run in separate batch Emacs
  processes so independent ERT names and test stubs do not collide. All selected
  suites are attempted even when one fails.
- The ERT runner clears inherited `GIT_INDEX_FILE` before starting Emacs. Git
  operations in nested package repositories and test fixtures use their own
  indexes, not the committing worktree's staged index. The parent hook environment
  and other Git configuration are preserved.
- Test failures and load failures produce a nonzero exit status. Skipped tests
  remain visible as skips, not passes; live-service tests require explicit opt-in.
- CI runs the full ERT invocation on pushes and pull requests, including changes
  confined to package code or test tooling.
- Dependency load paths follow the runtime's
  [context-panels source contract](../core/README.md#context-panels-dependency).

## Standalone Package Checks

- `scripts/elisp-package-standalone` shares source-only copying, temporary-home
  isolation, dependency checks, ERT, strict compilation, and interactive samples
  for the verified comments, suggestions, marginalia, and static-site workflows. The focused
  commands below retain their `--batch` and `--interactive` interfaces.
- `scripts/org-comments-standalone` verifies the comments package with only
  Emacs built-ins and its declared context-panels dependency. Dependency paths
  come from `CONTEXT_PANELS_PATH` or the configured Straight build directory;
  checks do not fetch dependencies or initialize the personal configuration.
- `scripts/org-suggestions-standalone` verifies the suggestions package with
  Emacs built-ins only, including the
  [executable public workflow](../packages/org-suggestions/examples/standalone.org).
  Its `--interactive` mode opens that walkthrough in clean Emacs for explicit
  create, reload, preview, accept, and undo operations.
- `scripts/cp-org-marginalia-standalone` verifies native note collection, metadata,
  read-only panel display, and jumps to prose references. The collector also has
  a model-only loading check without context-panels. Setup and manual acceptance
  are documented in the [package manual](../packages/cp-org-marginalia/README.org).
- `scripts/hb-static-site-standalone` verifies project-local section/page creation,
  validation, source-root resolution, and actionable missing-dependency errors
  with Emacs and Org built-ins only. Its interactive mode opens a disposable Hugo
  project with directory-local activation, without Denote, ox-hugo, or
  context-panels. Setup and limits are documented in the
  [package manual](../packages/hb-static-site/README.org).
- Source and package-local tests run from disposable copies outside the checkout,
  with a temporary home and no sibling package or shared helper load paths.
  Emacs network connections are disabled. Failures in ERT or strict compilation
  return a nonzero status, and temporary files are removed on exit.
- The comments check's `--interactive` mode opens a disposable source document for its documented
  [create/reopen/navigate workflow](../packages/org-comments/README.org).

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
