# Spec: Performance Discipline for Emacs Configuration

## Problem

Performance work on this Emacs configuration must improve responsiveness without
quietly changing the interactive experience.  Prior optimization attempts showed
that deferring load indiscriminately can break important UX, such as the startup
dashboard.  Future work needs a verifiable contract: measure first, preserve
observable behavior unless explicitly approved, and prefer standard Emacs,
`use-package`, and `straight.el` mechanisms before adding custom machinery.

## Context

Relevant existing pieces:

- `early-init.el` already applies standard startup optimizations for GC,
  file-name handlers, native compilation warnings, and macOS redisplay.
- `init.el` is the main entry point and currently loads package management,
  core predicates, language modules, interactive modules, dashboard, Org, email,
  and local integrations.
- `core/core-packages.el` bootstraps `straight.el` and `use-package`.
- `modules/interactive/ui/gui.el` configures the startup dashboard.  Its visible
  startup behavior is user-facing and must not disappear as a side effect of
  performance work.
- `modules/interactive/dev/common.el` enables `direnv-mode`; cross-project file
  visits can currently block while environments reload.
- `devenv.nix` defines the project command environment; checks should be run
  through `devenv -q shell -- ...`.

## Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| UX preservation | Performance changes must not materially change visible UX without approval | Responsiveness matters, but so does established workflow, especially dashboard startup |
| Measurement | Every optimization needs before/after measurements before claiming improvement | Avoids cargo-cult performance changes and makes regressions visible |
| Mechanism preference | Prefer native Emacs, then existing packages (`use-package`, `straight.el`), then custom code only when necessary | Keeps the configuration maintainable and aligned with community tooling |
| Dashboard | Dashboard remains expected startup UX unless explicitly changed | Its disappearance is an unacceptable regression |
| Cross-project direnv | File display must take priority over environment readiness | It is acceptable for code assistance to catch up after the file is visible |
| Packaging direction | Consider converting coherent subsets of local config into straight-managed local packages | Lets `straight.el` generate autoloads/build artifacts and makes module boundaries explicit |

## Acceptance Criteria

- [ ] AC-1: Given a proposed performance change, when it is presented for implementation, then it includes the UX behavior it may affect and waits for approval when behavior changes are possible.
- [ ] AC-2: Given a proposed optimization, when it is evaluated, then baseline and post-change measurements are recorded before any improvement claim is made.
- [ ] AC-3: Given normal GUI startup, when Emacs opens, then the startup dashboard still appears unless a separate approved UX change says otherwise.
- [ ] AC-4: Given a cross-project file visit into a `direnv` project, when the target file is selected, then the desired future behavior is that the file becomes visible before environment reload/code assistance completes.
- [ ] AC-5: Given a need for startup/load optimization, when choosing an implementation mechanism, then built-in Emacs facilities, `use-package`, and `straight.el` options/autoloads/builds are considered before custom scheduling/loading code.
- [ ] AC-6: Given a new performance-oriented runtime change, when it is implemented, then it does not introduce new custom abstractions unless the plan documents why existing mechanisms are insufficient.
- [ ] AC-7: Given future module boundary work, when a coherent subset of config is repeatedly optimized for loading, then it is evaluated as a candidate local package managed through `straight.el` and `use-package`.

## Invariants

- Dashboard startup behavior must not disappear accidentally.
- Existing keybindings and Evil behavior must remain stable unless explicitly
  changed.
- Batch loading and CI-like full loading must continue to catch broken modules.
- No performance work should hide required dependency failures in CI/batch.
- New scripts/docs must not modify runtime behavior unless explicitly stated.

## Scope

**May modify during measurement-only phase:**

- `docs/performance-plan.md`
- `docs/performance-spec.md`
- measurement scripts under `scripts/`

**Must not modify during measurement-only phase:**

- Runtime load order in `init.el`
- Dashboard behavior in `modules/interactive/ui/gui.el`
- `direnv` behavior in `modules/interactive/dev/common.el`
- Package declarations or straight configuration

Runtime files may be modified only in later phases after a specific proposal and
approval.

## Verification Plan

| Criterion | Method | Automated? |
|-----------|--------|------------|
| AC-1 | Review proposed change notes for UX impact/approval checkpoint | No |
| AC-2 | Compare recorded before/after output from measurement commands | Partly |
| AC-3 | Manual GUI startup check: dashboard appears | No |
| AC-4 | Manual cross-project file visit timing once direnv work begins | No |
| AC-5 | Plan review includes native/use-package/straight option analysis | No |
| AC-6 | Code review checks for new abstractions and rationale | No |
| AC-7 | Plan review identifies local-package candidates | No |

## References

- Plan: `docs/performance-plan.md`
- Package bootstrap: `core/core-packages.el`
- Startup entry point: `init.el`
- Dashboard configuration: `modules/interactive/ui/gui.el`
- Dev/common and direnv: `modules/interactive/dev/common.el`
