# Emacs Performance Plan

This is the evolving plan for making the configuration faster while preserving
Hubert's established Emacs experience.  The normative rules live in
`docs/performance-spec.md`; this document tracks the sequence of work and what
we learn as measurements accumulate.

## Current operating rules

- Discuss any change that can materially affect UX before implementing it.
- Preserve the startup dashboard unless an explicit approved UX change says
  otherwise.
- Measure before and after every optimization.
- Prefer native Emacs mechanisms first, then existing package mechanisms
  (`use-package`, `straight.el`), and only then custom code.
- Treat `straight.el` as a first-class performance tool: autoloads, build cache,
  package boundaries, local packages, lock/freeze state, and modification checks
  are all in scope for analysis.
- Cross-project navigation should prioritize showing the file.  `direnv` reload
  and code assistance may complete later.
- Separate definitions, configuration, and activation:
  - definitions: functions, commands, exporters, package extensions, local
    libraries;
  - configuration: variables, hooks, keybindings, account/site preferences;
  - activation: load now, autoload, enable mode, defer until package/user action.
- Loading a local file should define capabilities, not activate heavyweight
  behavior, unless the file is explicitly an activation module.
- Package declarations should be lazy by default.  Proactive loading is the
  exception and must be justified by visible startup UX or required global
  behavior.

## Baseline measurements

Record measurements here before and after each change.  Use the same machine,
Emacs build, and command where possible.

### 2026-06-08: restored baseline after undoing first attempt

Command shape:

```sh
devenv -q shell -- env HOME=$PWD HUB_SKIP_SERVER=1 emacs --batch -l init.el --eval '...' --kill
devenv -q shell -- env HOME=$PWD HUB_FORCE_FULL_LOAD=1 HUB_CI_SKIP_OPTIONALS=1 HUB_SKIP_SERVER=1 emacs --batch -l init.el --eval '...' --kill
```

| Scenario | Result |
|----------|--------|
| Batch init load, ad hoc command | `real 1.78s` |
| Forced full load, optionals skipped, ad hoc command | `real 4.08s` |
| Batch init load, `scripts/perf-measure init.el` | `real 2.21s` |
| Forced full load, optionals skipped, `scripts/perf-measure init.el` | `real 13.91s` |
| Visit `init.el` after init, `scripts/perf-measure init.el` | `visit-file-time 0.079s`, `real 1.84s` |

Notes:

- The dashboard is not represented by these batch timings; it needs a manual GUI
  startup measurement/check.
- Full-load timings vary significantly depending on package/native-comp cache
  state and external probes.  Treat this variability itself as something to
  understand before optimizing.
- Org version mismatch warnings appear during CI-like full load and may be a
  cleanup/performance target later.

## Measurement phase

Goal: add measurement capability and gather numbers without changing runtime UX.

### Automated/batch measurements

Use `scripts/perf-measure` for repeatable non-GUI timings:

```sh
devenv -q shell -- ./scripts/perf-measure
```

Optional file-open timings:

```sh
devenv -q shell -- ./scripts/perf-measure init.el /path/to/other/project/file
```

The script should remain measurement-only: it must not change load order,
package configuration, dashboard behavior, or direnv behavior.

### Manual GUI measurements/checks

Because dashboard behavior is user-facing, record these manually for now:

| Scenario | What to record |
|----------|----------------|
| GUI cold startup | Time until frame appears; time until dashboard visible; whether typing/switching feels blocked |
| Daemon + client | Time until client frame is usable; dashboard behavior if applicable |
| Cross-project file visit with direnv | Time until file is visible; time until environment/code assistance is ready; whether Emacs blocks |
| First `magit-status` | Time from command to usable Magit buffer |
| First `org-agenda` | Time from command to agenda visible |
| First email/mu4e command | Time from command to usable email UI |

## Investigation backlog

### 1. straight.el and package boundary audit

Status: first measurement pass complete; no runtime change applied.

Questions:

- What is the current value and behavior of `straight-check-for-modifications`?
- Is startup paying for modification checks or builds?
- Are all local modules byte/native compiled effectively?
- Which local modules are coherent candidates for local packages managed by
  `straight.el` recipes?
- Would local packages improve autoload generation and load boundaries without
  changing UX?

Findings from 2026-06-08:

- Current `straight-check-for-modifications` after loading `init.el` is
  `(find-at-startup find-when-checking only-once)`.
- `straight.el` documents `find-at-startup` as a startup-time cost and says it
  must be configured before bootstrap.
- `straight-cache-autoloads` is already `t`.
- `straight-disable-native-compile` is `nil`, so package native compilation is
  enabled.
- `straight-use-package-by-default` is `t`, and use-package integration is
  enabled.
- Built package inventory at measurement time:
  - `straight/build`: 25M
  - `straight/repos`: 690M
  - `straight/build-cache.el`: 572K
  - built package directories: 138
  - generated autoload/compiled files counted under `straight/build`: 844
  - `eln-cache`: 171M

Experiment, measurement-only via command-line pre-init `--eval` values:

| Scenario | Runs | Observation |
|----------|------|-------------|
| Batch load, current default | `1.00s`, `1.03s`, `0.92s` | baseline for this run |
| Batch load, no `find-at-startup` (`(find-when-checking only-once)`) | `0.68s`, `0.69s`, `0.73s` | ~250-350ms faster |
| Batch load, `nil` checks | `0.68s`, `0.74s`, `0.69s` | no material gain beyond removing `find-at-startup` |
| Forced full load, current default | `3.25s`, `3.10s` | baseline for this run |
| Forced full load, no `find-at-startup` | `2.49s`, `2.33s` | ~600-900ms faster |

Interpretation:

- The first obvious straight-native optimization is to remove `find-at-startup`
  while preserving explicit modification checking through `find-when-checking`.
- Setting checks to `nil` is not justified by the measurement because it does not
  materially outperform `(find-when-checking only-once)` in this sample.
- This is a low-UX-risk candidate because it changes package modification
  detection timing, not visible editing/dashboard behavior; nevertheless it
  should be approved before changing runtime config.

Applied runtime change on 2026-06-08:

```elisp
(unless (boundp 'straight-check-for-modifications)
  (setq straight-check-for-modifications '(find-when-checking only-once)))
```

Placed before straight bootstrap in `core/core-packages.el` and in the fallback
bootstrap path in `init.el` so both paths share the same behavior.  The `unless`
preserves earlier CI overrides that deliberately disable modification checks.
Manual package change detection remains available through `straight-check-package`
and `straight-check-all`; startup no longer pays the `find-at-startup` scan.

Post-change measurements:

| Scenario | Runs | Observation |
|----------|------|-------------|
| Batch load after change | `0.94s`, `0.77s`, `0.70s` | variable, aligned with prior no-`find-at-startup` experiment |
| Forced full load after change | `2.64s`, `2.20s`, `2.56s` | materially faster than prior default sample |
| CI-like `scripts/ci-load-all.el` after change | `2.102s` total load time | passes; existing Org mismatch warnings remain |

Straight/use-package startup options to consider next:

- `straight-cache-autoloads` is already enabled and should remain enabled; it
  reduces package autoload IO from O(number of packages) to O(1).
- For packages used only conditionally, straight recommends
  `straight-register-package` plus a conditional `use-package` form, because
  `:when` does not prevent `:straight` registration/loading behavior when
  `straight-use-package-by-default` is active.
- `straight-use-package-lazy` can register a package and defer installation if
  the repo is absent; useful for optional tools, but not a direct startup win
  for already-installed packages.
- Per-recipe `:build (:not autoloads)`, `:build (:not compile)`, or
  `:build (:not native-compile)` exist, but straight's docs frame autoload
  suppression as premature unless a package has unusually heavy autoloads.
- `:straight nil` should be used deliberately for built-in packages or external
  packages such as environment-provided mu4e to avoid unnecessary straight work.
- Local config packages are worth evaluating because straight can generate
  autoloads, build artifacts, and explicit boundaries for coherent subsets of
  this repository.

Candidate package groups:

- `hub-core`: `core/` plus shared predicates/package/path startup pieces.
- `hub-lib`: shared helpers under `lisp/`.
- `hub-editing`: `modules/interactive/editing/`.
- `hub-completion`: `modules/interactive/completion/`.
- `hub-ui`: `modules/interactive/ui/`, preserving dashboard behavior.
- `hub-org`: Org authoring/export integration.
- `hub-email`: email/mu4e modules.
- `hub-dev`: programming/dev/common/language integration.

### 2. use-package eager-load audit

Status: first audit pass complete; no runtime change applied.

Method:

```sh
devenv -q shell -- env HOME=$PWD HUB_FORCE_FULL_LOAD=1 HUB_CI_SKIP_OPTIONALS=1 HUB_SKIP_SERVER=1 \
  emacs --batch --eval '(setq use-package-compute-statistics t use-package-minimum-reported-time 0.001)' \
  -l init.el --eval '...' --kill
```

High-signal observations from the forced full-load sample:

| Candidate | Approx use-package time | Current shape | UX risk | Notes / possible next move |
|-----------|------------------------:|---------------|---------|-----------------------------|
| `citeproc` | 230ms | `(use-package citeproc)` in `org/core` | medium | Eagerly loaded when Org core loads; likely tied to Org export/citation. Review whether it can be `:after oc`/export command or whether dashboard/agenda needs it. |
| `eglot` | 167ms | `(use-package eglot)` in `dev/common` | low-medium | Built-in-ish code assistance should not be required until language hooks need it. Could use built-in recipe and defer/configure after load. Must preserve language hooks. |
| `yasnippet-snippets` | 98ms | `(use-package yasnippet-snippets)` in `init.el` | low | Eagerly loads while `yasnippet` itself is deferred. Log says snippets prepared just-in-time but no snippets found. Best first candidate. |
| `evil` family | 145ms for `evil`, plus related packages | `evil` demanded by core interactive UX | high | Core UX; do not defer casually. Possible later work is narrowing `evil-collection-init`, not removing Evil startup. |
| `dired` + `dired-aux` + friends | ~60ms | Dired module required in full interactive load | medium | Dired behavior is user-facing but not dashboard-critical. Later candidate after command availability review. |
| `diff-hl` | 40ms | `:demand t` in VCS module | medium | Demand-loading is explicit. Could move to hooks, but this changes when gutters become available. Needs UX discussion. |
| `perspective` | 23ms | `:defer t`, but module config invokes behavior | medium-high | Perspectives are a daily workflow. Measure project switching before changing. |
| `smartparens`/`evil-smartparens` | ~31ms | global modes after `:defer 2` in `init.el` | medium | Runtime cost may matter more than startup. Needs global-mode audit. |
| `kind-icon` | 17ms | `:after corfu` | low-medium | Icons can affect completion rendering; not first target. |
| `mu4e-dashboard` | 17ms | `:if locate-library`, `:after mu4e` | high | Email dashboard/sidebar is visible UX; do not change before email-specific review. |
| `modus-themes` | 11ms | `:demand t` in GUI/TTY UI | high | Theme is startup UX; preserve unless explicitly changed. |

Straight/use-package audit findings:

- `:when`/`:if` do not prevent straight work when `straight-use-package-by-default`
  is active. For conditional packages, straight recommends
  `straight-register-package` plus conditional `use-package`.
- Built-ins already handled in several places with `:straight (:type built-in)`
  or `:straight nil`; continue expanding this only where a package is genuinely
  built-in or externally provided.
- Avoid per-recipe autoload suppression until a package is proven to have heavy
  autoloads and a small manually needed command surface.
- The first low-risk runtime candidate is `yasnippet-snippets` because it is
  eager while `yasnippet` is deferred and it is not part of visible startup UX.

Applied runtime change on 2026-06-08:

```elisp
(use-package yasnippet-snippets
  :after yasnippet)
```

This preserves snippets once Yasnippet is loaded while avoiding eager startup
loading of the snippets package.

Post-change measurements:

| Scenario | Runs | Observation |
|----------|------|-------------|
| Batch load after change | `0.84s`, `0.63s`, `0.67s`, `0.62s`, `0.72s` | no eager `yasnippet-snippets` log during init |
| Forced full load after change | `2.68s`, `2.82s`, `2.43s`, `2.38s`, `2.36s` | similar overall full-load range, but snippets package no longer loads eagerly |
| `use-package` stats for `yasnippet-snippets` after change | `:use-package-secs` around 0.1ms | prior sample spent ~98ms while loading it eagerly |
| CI-like `scripts/ci-load-all.el` after change | `1.908s` total load time | passes; existing Org mismatch warnings remain |

Behavior check:

- After `init.el`, `(featurep 'yasnippet-snippets)` is nil.
- After `(require 'yasnippet)`, `yasnippet-snippets` loads and
  `yas-snippet-dirs` includes `yasnippet-snippets-dir` plus the repo-local
  snippets directory.

### 3. Dashboard-preserving optimization

Status: GUI/dashboard instrumentation added on 2026-06-08; no dashboard behavior
change intended.

The dashboard must continue to appear at startup.  Instrumented milestones now
log to `*Messages*` when `hub/performance-log-startup` is non-nil:

- `ui/gui load start`
- `dashboard config start`
- `dashboard recentf ready`
- `dashboard seed projects (N)`
- `dashboard known projects ready`
- `dashboard startup hook installed`
- `dashboard startupify lists`
- `dashboard denote section`
- `dashboard agenda section`
- `dashboard config complete`
- `ui/gui load complete`
- `emacs-startup-hook`

Manual GUI measurement procedure:

1. Launch Emacs normally.
2. Wait until the dashboard is visible.
3. Open `*Messages*`.
4. Copy the `[perf]` lines into this plan or the conversation.
5. Record whether the dashboard appeared as usual.

Manual GUI sample from 2026-06-08:

- `exec-path-from-shell` took 2.354s to configure and emitted its slow-startup warning.
- `eglot` load took 0.963s before GUI/dashboard loading.
- `ui/gui load start`: +4.572s.
- `dashboard config start`: +4.809s.
- `dashboard recentf ready`: +4.812s, only 0.003s for recentf in this sample.
- Project seeding failed quickly: `No applicable method: project-root, "/Users/hubertbehaghel/ws/AIProPrompter/"`.
- `ui/gui load complete`: +4.823s.
- Dashboard list rendering happened later during startup hook after additional modules loaded.
- `dashboard denote section`: +7.640s, 0.016s.
- `dashboard agenda section`: +9.684s, 2.044s.
- `dashboard startupify lists`: +9.684s, 2.079s.
- `emacs-startup-hook`: +9.689s.

Interpretation:

- The perceived launch-to-dashboard time is dominated by real GUI startup work,
  not the batch metrics.
- The biggest measured single blockers are `exec-path-from-shell` (~2.35s),
  Eglot load (~0.96s), and dashboard agenda rendering (~2.04s).
- Dashboard configuration itself is fast once reached; the dashboard becomes
  visible late because rendering happens on startup hook after more modules load,
  and agenda generation blocks that render.

Applied runtime/environment change on 2026-06-08:

- Removed Emacs' dependency on `exec-path-from-shell` during interactive startup.
- Added explicit Emacs `exec-path` construction from:
  - known Nix/Home Manager/profile/system directories, and
  - the inherited `PATH`.
- Updated `nixos-config/modules/home/darwin-only.nix` so launchd GUI apps get a
  richer explicit PATH including Home Manager/profile bins and core tooling.

Validation checklist after activating nixos-config and relaunching GUI Emacs:

```elisp
(executable-find "git")
(executable-find "rg")
(executable-find "fd")
(executable-find "pass")
(executable-find "mu")
(executable-find "gpg")
(executable-find "direnv")
(executable-find "devenv")
(getenv "PATH")
```

Expected result: all essential commands resolve without any
`exec-path-from-shell` log entry or warning.  If project-specific tools are
missing globally, prefer adding them to that project's devenv and letting direnv
provide them on project visit.

Post-activation user check:

- `git`, `rg`, `fd`, `pass`, `mu`, `gpg`, `direnv`, and `devenv` all resolve
  from `/etc/profiles/per-user/hubertbehaghel/bin/`.
- `*Messages*` contains no `exec-path-from-shell` startup output.
- Inherited PATH includes the expected explicit profile/system directories.
- Follow-up cleanup opportunity: PATH contains duplicate entries with and without
  trailing slashes; normalize path entries before deduplication if this becomes
  noisy.

Post-change GUI sample:

- `ui/gui load start`: +3.254s, down from +4.572s.
- `dashboard config start`: +3.647s, down from +4.809s.
- `ui/gui load complete`: +3.663s, down from +4.823s.
- `dashboard denote section`: +6.690s, down from +7.640s.
- `dashboard agenda section`: +8.739s, still 2.050s.
- `dashboard startupify lists`: +8.740s, still 2.090s.
- `emacs-startup-hook`: +8.744s, down from +9.689s.

Result: removing `exec-path-from-shell` improved the measured dashboard-visible
path by roughly 0.9-1.3s while preserving UX.  Remaining high-signal blockers in
this sample are Eglot eager loading (~1.1s before GUI), dashboard agenda rendering
(~2.05s), and later optional modules before dashboard startup hook rendering.

Applied runtime change on 2026-06-08:

```elisp
(use-package eglot
  :defer t
  :commands (eglot eglot-ensure))
```

This keeps language hooks intact while avoiding eager Eglot loading before the
GUI/dashboard path.  Expected UX: code assistance starts when a language buffer
or explicit Eglot command needs it; dashboard and editing startup should not wait
for LSP client loading.

Post-change GUI sample:

- No early `Loading package eglot...` entry.
- `ui/gui load start`: +1.385s, down from +3.254s after the PATH fix and +4.572s
  baseline.
- `dashboard config start`: +1.623s, down from +3.647s after the PATH fix and
  +4.809s baseline.
- `ui/gui load complete`: +1.646s, down from +3.663s after the PATH fix and
  +4.823s baseline.
- `dashboard denote section`: +4.858s, down from +6.690s after the PATH fix and
  +7.640s baseline.
- `dashboard agenda section`: +7.760s, down from +8.739s after the PATH fix and
  +9.684s baseline, but section rendering itself rose to 2.902s in this sample.
- `emacs-startup-hook`: +7.773s, down from +8.744s after the PATH fix and
  +9.689s baseline.

Result: Eglot deferral improved the measured dashboard-visible path by roughly
another 1s, with no observed startup UX regression.

Applied dashboard first-paint change on 2026-06-08:

- Keep `dashboard-items` configured with the full desired layout.
- Temporarily replace generators for deferred sections (`denote`, `agenda`) with
  cheap placeholder generators before the first dashboard render.
- Schedule an idle refresh after first paint, restore the real item generators,
  and rerender the dashboard with Denote/agenda data.
- Keep heavy dependencies inside item generators.  Therefore, if dashboard is
  disabled or an expensive section is removed from `dashboard-items`, that
  section's dependencies such as `org-agenda` are not loaded for dashboard.

Expected UX: dashboard appears with fast sections and "Loading ..." placeholders,
then refreshes once deferred data is ready.

Post-change GUI sample:

- `ui/gui load start`: +1.291s.
- `dashboard config start`: +1.541s.
- `ui/gui load complete`: +1.560s.
- First `dashboard startupify lists`: +4.420s, 0.020s render duration.  This is
  the first dashboard paint with deferred placeholders.
- `emacs-startup-hook`: +4.425s.
- Deferred Denote section: +4.715s, 0.044s.
- Deferred agenda section: +7.466s, 2.751s.
- Full deferred refresh: +7.471s, 2.801s.

Result: first dashboard paint moved from +7.760s to +4.420s in the latest
sample, while the expensive agenda work moved after the dashboard is already
visible.  The deferred refresh still blocks for roughly 2.8s while agenda data is
computed on the main Emacs thread; further improvement should either make agenda
cheaper/cached or delay refresh until the user has had more idle time.

Applied lazy-load follow-up on 2026-06-08:

- Increased dashboard deferred refresh idle delay from 0.2s to 1.0s, so the user
  gets a longer usable window after first paint before agenda refresh starts.
- Changed `citeproc` to `:defer t`; it should not load during startup merely
  because `org/core` was loaded.
- Changed `org-ai` to command-based loading via `org-ai-mode` and
  `org-ai-global-mode`; it no longer enables itself for every Org buffer at
  startup.
- Changed `whisper` to command-based loading via `whisper-run`, `whisper-file`,
  and `whisper-select-language`.
- Changed `mu4e-dashboard` to command/mode-based loading and removed `:after
  mu4e`; it should load only when the mail sidebar/dashboard is actually used.

Policy decision: package declarations should be lazy by default.  Proactive
loading should be the exception and should be justified by visible startup UX or
required global behavior.

Applied dashboard first-paint ordering change on 2026-06-08:

- Dashboard no longer relies on the package's default `after-init-hook` /
  `emacs-startup-hook` render path, because that waits for all later init modules
  to finish loading.
- `ui/gui` still installs dashboard resize behavior, but removes the default
  delayed render hooks.
- `init.el` now calls `hub/dashboard-first-paint` immediately after loading
  `ui/gui`, before navigation/VCS/Dired/Org/mail modules load.
- The first paint still uses deferred placeholders for agenda/Denote, and the
  real expensive sections refresh later from the idle timer.
- Dashboard config now requires the cheap built-in `bookmark` library before
  rendering to avoid the first-paint `bookmark-all-names` void-variable warning.

Post-change GUI sample:

- `ui/gui load start`: +1.419s.
- `dashboard config start`: +2.043s.
- `ui/gui load complete`: +2.103s.
- First placeholder dashboard render: +2.123s, 0.018s.
- `dashboard first paint`: +2.132s, down from +4.341s after deferred sections,
  +7.760s before deferred sections, and +9.684s baseline.
- `emacs-startup-hook`: +6.020s, now after dashboard is already visible.
- Deferred refresh still completes around +9.562s due agenda work, but after
  first paint.

Result: dashboard first paint is now approximately 7.5s faster than the original
baseline and 2.2s faster than the previous deferred-only iteration.

Applied deferred-refresh split on 2026-06-08:

- Replaced the single dashboard deferred refresh delay with per-section delays:
  - Denote: 1.0s idle delay.
  - Agenda: 5.0s idle delay.
- The first deferred refresh restores only Denote; agenda remains a placeholder.
- The second deferred refresh restores agenda, so the expensive Org agenda scan
  should happen later and no longer coincide with the lighter Denote refresh.
- Fixed project seeding by using `project-remember-projects-under` for each
  configured workspace root instead of passing raw directory strings to
  `project-remember-project`.

Post-change GUI sample:

- Project seed error is gone; 3 projects were found in `~/ws/`.
- First paint remains early at +2.805s in this sample.
- Denote refresh is split from agenda and completes at +8.213s with only 0.070s
  of refresh cost.
- Agenda refresh did not appear in the provided trace, consistent with its later
  5s idle delay.
- Follow-up cleanup: suppress noisy `project-remember-projects-under` "Found ..."
  messages during dashboard seed; the dashboard perf line is enough.
- Remaining issue investigated via staged startup bisect.  Culprit was the
  startup-time `use-package treemacs-magit` declaration: merely declaring the
  optional Treemacs/Magit bridge caused the generic `Symbol's value as variable
  is void: symbol` message after `emacs-startup-hook`.  Replaced it with a lazy
  `with-eval-after-load` bridge that requires `treemacs-magit` only after both
  `magit` and `treemacs` are actually loaded.
- Recentf is not the bottleneck in this sample.
- Project seeding has a correctness issue, but not a time issue in this sample.

Batch validation sample from explicitly requiring `ui/gui`:

```text
[perf] +2.411s ui/gui load start
[perf] +2.470s dashboard config start
[perf] +2.471s dashboard recentf ready (0.001s)
[perf] +2.471s dashboard seed projects (0) (0.000s)
[perf] +2.472s dashboard known projects ready (0.000s)
[perf] +2.472s dashboard startup hook installed (0.000s)
[perf] +2.472s dashboard config complete
[perf] +2.472s ui/gui load complete (0.061s)
```

This batch sample does not represent your 9s GUI perceived startup, but it
confirms the instrumentation loads and logs without changing dashboard setup.

Potential areas to measure before changing:

- workspace project seeding in `hub/dashboard-seed-project-list`
- `recentf-load-list`
- agenda section generation
- Denote recent notes scanning
- icon/font setup

Potential acceptable approaches, subject to measurement and approval:

- cache dashboard data while preserving the visible dashboard layout
- display the dashboard shell earlier, render fast sections immediately, then
  refresh individual expensive sections as their data becomes available
- shorten expensive sections with explicit placeholders
- use package-provided dashboard hooks rather than custom startup deferral

### 4. direnv/cross-project responsiveness

Problem: visiting a file in another project can block while `direnv` reloads.

Desired UX:

1. User selects/opens file.
2. File appears immediately.
3. Environment reload happens in the background or after the file is visible.
4. Eglot/code assistance may be temporarily unavailable.
5. Completion/diagnostics become ready when the environment is ready.

Investigation questions:

- Does the installed `direnv` package expose an async or non-blocking update
  option?
- Can `direnv-mode` be configured to avoid blocking `find-file`?
- Can environment update be moved to idle/project-switch hooks without custom
  scheduling?
- How should Eglot wait for or restart after direnv environment changes?

### 4. Global mode audit

Measure before proposing changes to:

- `smartparens-global-mode` / `smartparens-global-strict-mode`
- `global-whitespace-mode`
- `global-whitespace-cleanup-mode`
- `global-auto-revert-mode`
- `flyspell-prog-mode` in `prog-mode-hook`
- `treesit-font-lock-level 4`
- `diff-hl :demand t`

### 5. Org version mismatch cleanup

Status: CI-specific mismatch warning fixed on 2026-06-09; slow full-load still
needs investigation.

Findings:

- Web/prior-art check confirmed the classic straight.el guidance: if using
  straight's newer Org, call `(straight-use-package 'org)` immediately after
  straight bootstrap so straight's Org takes load-path precedence before bundled
  Org can be pulled in.
- `core/core-packages.el` already makes straight package load paths available
  before `org/core` configures Org in normal init.
- The CI-like mismatch had a different trigger: `scripts/ci-load-all.el` tried
  `(require 'mu4e nil 'noerror)` while setting up test stubs.  In devenv, real
  mu4e is available; loading it also loads `mu4e-org`, which pulled in bundled
  Org before `init.el` bootstrapped straight.

Applied fix:

- `scripts/ci-load-all.el` now installs CI mu4e stubs without requiring or
  providing real `mu4e`.
- `email/core` skips real mu4e discovery when `hub/ci-stubbed-mu4e` is set.
- `email/core` also avoids initializing `evil-collection-mu4e` against CI stubs.
- `email/dashboard` avoids declaring/loading `mu4e-dashboard` when CI stubs are
  active.

Validation:

- `devenv -q shell -- ci:load-all` completed without Org version mismatch
  warnings.  The run still took `310.484s`, so slow forced full-load remained a
  separate performance target.

Slow full-load follow-up on 2026-06-09:

- Instrumented the final interactive require chain and found the dominant delay
  in `email/view`: top-level `(require 'ffap)` took about `404.5s` in one CI-like
  run.
- `ffap` is only needed by `hub/copy-url-at-point-dwim`, an interactive command,
  so loading it at email module load time was unnecessary.
- Removed the eager `ffap` require from `modules/interactive/email/view.el` and
  require it inside `hub/copy-url-at-point-dwim` only when the command needs the
  fallback URL detector.
- Also removed the eager optional `shr` require from `email/view`; the command
  already checks `fboundp` for `shr-url-at-point`.
- Post-change `devenv -q shell -- ci:load-all` completed in `1.020s` with no Org
  version mismatch warnings.

### 6. Package-boundary and definition/configuration refactor

Goal: make performance properties obvious by separating capability definitions
from configuration and activation.

Current problem pattern:

- `init.el` and some modules mix function definitions, package declarations,
  local variables, account/site configuration, autoloads, and activation.
- Merely declaring a package or requiring a local module can accidentally trigger
  startup work, as seen with Magit and `treemacs-magit`.
- Ill-shaped integrations such as Confluence export need clearer homes so
  command availability does not imply eager exporter/package activation.

Target shape:

```text
lisp/ or packages/       definitions, commands, helpers, reusable package code
modules/interactive/...  user configuration and activation policies
init.el                  orchestration only
```

Refactor candidates:

1. **Confluence export** — first candidate.  It should be self-contained enough
   to move to its own Git repository later.  The Emacs config should only set
   user/site defaults, autoload commands, and decide when the export backend is
   registered.

   Applied first extraction step on 2026-06-09:

   - Moved Confluence activation out of `init.el` into
     `modules/interactive/org/confluence.el`.
   - `init.el` now only requires `org/confluence` as part of the interactive Org
     module set.
   - The activation module owns load-path registration for
     `packages/org-confluence/`, command autoloads, user/site defaults,
     and delayed export-backend registration after `ox` loads.
   - Exporter/API/command implementation files remain self-contained under
     `packages/org-confluence/` as the future standalone package/repository
     candidate.
2. **UI/dashboard** — split `ui/gui.el` into performance instrumentation, theme,
   fonts/emoji, and dashboard activation.

   Applied first split on 2026-06-09:

   - `modules/interactive/ui/gui.el` is now an orchestration-only GUI entrypoint.
   - `modules/interactive/ui/performance.el` owns startup milestone logging.
   - `modules/interactive/ui/theme.el` owns the graphical Modus theme setup.
   - `modules/interactive/ui/fonts.el` owns icons, emoji/symbol fallback, and
     face font defaults.
   - `modules/interactive/ui/dashboard.el` owns dashboard activation, first
     paint, deferred section refresh, and dashboard-specific instrumentation.
   - No intended UX or load-order behavior change: `ui/gui` still requires the
     same GUI capabilities in the same sequence and still provides
     `hub/dashboard-first-paint` via the dashboard module.
3. **VCS/Magit** — keep Magit autoload-only at startup; isolate Diff-HL,
   ssh-agent, and Magit bridge configuration.

   Applied first split on 2026-06-09:

   - `modules/interactive/vcs/git.el` is now the Git/VCS entrypoint for generic
     VCS state, shared keybindings, and startup-debug stage orchestration.
   - `modules/interactive/vcs/magit.el` owns Magit command autoloads,
     Magit-specific keybindings, and post-load configuration.  It deliberately
     avoids `use-package`/`straight-use-package` so Magit remains unloaded during
     startup.
   - `modules/interactive/vcs/ssh.el` owns the graphical `ssh-agency` askpass
     integration.
   - `modules/interactive/vcs/diff-hl.el` owns the eager Diff-HL activation and
     gutter/hunk keybindings.  It remains `:demand t`; changing gutter
     activation timing is still a future UX/performance decision.
   - Replaced the Magit gitman `defadvice` with `define-advice`, removing one
     obsolete-advice warning without changing the intended behavior.
4. **Org** — split core Org editing from agenda, capture, babel, export,
   citations, and integrations.

   Applied first extraction step on 2026-06-09:

   - Moved Org-adjacent package declarations from `modules/interactive/org/core.el`
     to `modules/interactive/org/integrations.el`.
   - The integrations module now owns `evil-org`, `org-re-reveal`, `ox-clip`,
     `org-cliplink`, `anki-editor`, `org-download`, `org-drill`, and `citeproc`
     declarations.
   - `org/core` still owns the main `use-package org` configuration and requires
     `org/integrations` afterward, preserving the previous declaration order and
     avoiding a runtime behavior change.

   Applied broader split on 2026-06-09:

   - `modules/interactive/org/settings.el` owns the shared `hub/org` group and
     `org-directory` default.
   - `modules/interactive/org/agenda.el` owns agenda file defaults, missing-file
     pruning, and agenda display setup.
   - `modules/interactive/org/capture.el` owns capture templates, the Veriff
     template helper, blog capture integration, and refile targets.
   - `modules/interactive/org/babel.el` owns Babel evaluation/source-block
     behavior and source language mappings.
   - `modules/interactive/org/export.el` owns citation/export defaults,
     reveal.js root, and local LaTeX class discovery helpers.
   - `modules/interactive/org/authoring.el` owns semantic authoring shortcuts,
     Confluence status link faces, image/callout/footnote snippets, and Tempo
     shortcut registration.
   - `modules/interactive/org/core.el` is now an editing/orchestration entrypoint
     that wires the split setup functions into the main `use-package org` config.

Performance rule for refactors:

- autoload commands early;
- set cheap variables early if needed;
- attach cheap hooks only when they do not block first paint/file visit;
- load heavy packages only on command invocation, mode hook, after first paint,
  or explicit user action.

## Proposed next steps

Current state after the 2026-06-09 work:

- startup responsiveness was improved without removing the dashboard;
- dashboard first paint is intentionally fast, with Denote and agenda refreshed later;
- `exec-path-from-shell`, Eglot, Magit, `org-ai`, `whisper`, `mu4e-dashboard`, and
  several other integrations no longer load eagerly on the startup/dashboard path;
- `packages/org-confluence/` is now the local package boundary for the Confluence
  exporter, while `modules/interactive/org/confluence.el` owns activation and
  personal workflow configuration;
- `private/setup.el` is reserved for secrets/truly uncommittable values, not
  ordinary personal configuration;
- `ui/gui` and `vcs/git` are now smaller orchestration entrypoints with their
  theme/fonts/dashboard and Magit/SSH/Diff-HL responsibilities split out;
- `org/core` is now a smaller orchestration entrypoint with agenda, capture,
  Babel, export, authoring, settings, and integrations split out.

Recommended next session priorities:

1. Investigate the Org version mismatch warning and slow forced full-load path.
2. Consider whether Diff-HL should stay eager or move to hook/idle activation;
   discuss UX first because gutter availability is visible.
3. Consider adding a doc-drift check for generated package docs, following the
   `eve.el` guardrail pattern.

Before claiming further performance wins, rerun the GUI/dashboard measurements
from this plan and compare against the recorded dashboard first-paint samples.
