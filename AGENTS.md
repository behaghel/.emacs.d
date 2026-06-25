# Repository Guidelines

## Purpose
- This repository is an Emacs configuration written mostly in Emacs Lisp.
- Optimize for safe batch loading, predictable module boundaries, stable interactive UX, and small local changes over broad rewrites.

## Agent Inputs
- No Cursor rules were found in `.cursor/rules/`.
- No `.cursorrules` file was found.
- No Copilot instructions file was found at `.github/copilot-instructions.md`; treat this `AGENTS.md` as the canonical agent guidance.

## Project Layout
- `init.el`: main entry point; sets `user-emacs-directory`, bootstraps packages, loads modules.
- `early-init.el`: early startup tuning.
- `core/`: always-on infrastructure such as paths, package bootstrap, predicates, startup, and keybind foundations.
- `modules/lang/`: language-specific configuration; `modules/interactive/`: interactive-only modules grouped by category like `editing/`, `navigation/`, `email/`, `ui/`.
- `modules/writing/`: writing helpers that are still loaded from the legacy path.
- `packages/`: local package-style libraries intended to be reusable/extractable; keep defaults neutral and activate/configure them from modules.
- `lisp/`: shared local libraries such as `hub-utils.el`, `hub-keys.el`, `hub-struct.el`.
- `test/`: ERT tests and test helpers.
- `scripts/`: repo-local validation helpers used by hooks and CI; `etc/`: static assets; `var/`: runtime state files.
- `private/setup.el`: gitignored sensitive local overrides only; it is not the normal place for personal configuration and should ideally remain empty.

## Architecture Rules
- Keep runtime layers explicit: `core` is always available; `interactive` is loaded only outside batch unless forced.
- Batch sessions must not expose `modules/interactive` on `load-path`; CI lint checks this.
- Keep package/library defaults neutral and reusable. Place extractable local packages under `packages/`; put normal personal configuration and activation in tracked config modules under the relevant domain/category; use `private/setup.el` only for secrets or values that truly cannot be committed.
- Do not add new tracked files under the retired `settings/` directory.
- Module feature names should be namespaced by category, not by `hub/`; examples: `navigation/perspective-auto`, `email/view`.
- Repo-specific function and variable names should use the `hub/` prefix.
- Put generic helpers in `lisp/` only when they are shared by multiple modules.
- Keep interactive-only dependencies out of always-on code unless guarded.
- Evil normal state is the default for all modes. Emacs state is exception-based; each exception is registered in `editing/evil.el` with a justifying comment. Bépo ctsr rotation requires normal or motion state and does not apply in emacs state.

## Environment And Setup
- Preferred workflow: enable `direnv` and run `direnv allow` at repo root.
- Use `devenv shell -- <command>` for one-off commands to match local hooks and CI.
- `devenv.nix` defines convenience scripts and pre-commit hooks; check it before inventing new commands.
- When a general tool dependency is missing from the development environment, install it through `devenv.nix` instead of reimplementing that capability inside repo code.
- Exception: LaTeX packages expected by Emacs document/export workflows must be added in `~/nixos-config/`, not only in this repo's `devenv.nix`. The canonical Home Manager linkage lives in `~/nixos-config/modules/home/emacs/texlive.nix`, re-exported by `~/nixos-config/modules/home/texlive/default.nix`, so GUI Emacs and system activation get the same TeX Live closure.
- CI and many local checks run with `HOME=$PWD` so state stays inside the repository.
- Environment predicates live in `core/core-predicates.el`: `hub/interactive-p`, `hub/batch-p`, `hub/gui-p`, `hub/tty-p`, `hub/ci-p`.

## Build, Lint, And Test Commands
- Full batch load smoke check: `devenv shell -- env HOME=$PWD emacs --batch -l init.el --eval '(message "Loaded")' --kill`
- Equivalent devenv shortcut: `devenv run load-check`
- Force a fuller CI-like load: `devenv shell -- ci:load-all`
- Tangle literate config: `devenv shell -- tangle`
- Format all tracked Elisp: `devenv shell -- elisp:format-all`
- Checkdoc all tracked Elisp: `devenv shell -- elisp:checkdoc-all`
- Run all configured pre-commit gates: `devenv shell -- pre-commit:all`
- Run pre-commit directly: `devenv shell -- pre-commit run -a`
- Parse changed or selected files: `devenv shell -- ./scripts/elisp-parse path/to/file.el`
- Checkdoc selected files: `devenv shell -- ./scripts/elisp-checkdoc path/to/file.el`
- Format selected files: `devenv shell -- ./scripts/elisp-format path/to/file.el`
- Run all ERT tests under `test/`: `devenv shell -- ./scripts/elisp-ert`
- Run smoke tests only: `devenv shell -- env HOME=$PWD emacs --batch -l ert -l test/smoke.el -f ert-run-tests-batch-and-exit`
- Run one test file: `devenv shell -- env HOME=$PWD emacs --batch -Q -L . -l ert -l test/test-helpers.el -l test/perspective-auto-test.el -f ert-run-tests-batch-and-exit`
- Run a single named ERT test: `devenv shell -- env HOME=$PWD emacs --batch -Q -L . -l ert -l test/test-helpers.el -l test/perspective-auto-test.el --eval '(ert-run-tests-batch-and-exit (quote hub/persp-auto-creates-and-switches))'`
- Alternate single-test selector by regexp: `devenv shell -- env HOME=$PWD emacs --batch -Q -L . -l ert -l test/test-helpers.el -l test/eve-test.el --eval '(ert-run-tests-batch-and-exit "eve-delete-word-updates-segment")'`

## What CI Enforces
- `.github/workflows/emacs.yml` runs on pushes and pull requests.
- CI sets up Emacs `30.2`.
- CI performs architecture lint, full config load, formatting, checkdoc, tangling, init load, and ERT tests.
- On PRs, CI may limit module-test execution based on changed files; on pushes it runs all module tests present.
- Straight cache keys depend on Emacs version and `straight/versions/default.el`.
- Architecture lint also rejects core files requiring namespaced module features and modules using `hub/*` helpers without `(require 'hub-utils)`.

## Pre-commit Hooks
- Hooks are configured from `devenv.nix` and installed via `pre-commit`.
- `elisp-format`: indents and runs `whitespace-cleanup`; if it modifies files, the commit fails and you must restage.
- `elisp-checkdoc`: fails on remaining checkdoc warnings after filtering known structural exceptions.
- `elisp-parse`: batch-reads forms to catch syntax and reader errors.
- `elisp-ert`: runs all tests in `test/*-test.el`.
- After editing hook configuration in `devenv.nix`, reload the environment and reinstall hooks.

## Elisp File Conventions
- New `.el` files should start with a standard header line and `lexical-binding: t`.
- Use the common section structure: header, `Commentary`, `Code`, `provide`, footer.
- Keep filenames aligned with the provided feature and directory namespace.
- Prefer one responsibility per module.
- Use docstrings for functions, macros, variables, `defcustom`, and tests when appropriate.

## Imports And Dependencies
- Use `require` for hard dependencies that must exist.
- Use `require ... nil 'noerror` only for genuinely optional packages.
- Prefer `with-eval-after-load` when extending package maps or behavior after package init.
- In tests, add minimal stubs for optional packages instead of requiring the full stack.
- Keep local helper requires near the top of the file.
- Existing files often put local `hub-*` requires first, then built-ins, then package-specific modules; preserve nearby style rather than reordering noisily.
- If a module calls `hub/*` helpers, include `(require 'hub-utils)` explicitly; CI checks this convention.
- Do not silently swallow required-module failures; avoid `ignore-errors` around required code paths unless the fallback is intentional and documented.

## Formatting And Whitespace
- Use spaces, not tabs, in Elisp; `.editorconfig` sets `indent_size = 2`.
- Keep lines and forms readable; prefer multi-line formatting over dense one-liners for complex logic.
- Let Emacs indentation decide alignment.
- Preserve final newlines and trim trailing whitespace.
- Do not hand-format against the repo style; run the formatter when you touch `.el` files.

## Naming Conventions
- Prefix repo-owned functions, variables, commands, macros, and groups with `hub/`.
- Internal helpers should use `hub/name--private-helper` style.
- Predicates should end in `-p`.
- Buffer-local internals commonly use `defvar-local` and `--` private markers.
- Constants in tests may use `*-test--sample-data` style.
- Feature names should mirror paths, e.g. `modules/interactive/email/view.el` provides `email/view`.

## Types, Data, And State
- Emacs Lisp is dynamically typed here; express invariants through naming, docstrings, `defcustom :type`, and careful helper boundaries.
- Prefer `defcustom` for user-facing configuration and `defvar` or `defvar-local` for internal mutable state.
- Use `setq-local` for buffer-local behavior.
- Prefer alists and plists consistently within a given module; do not mix representations casually.
- When persisting data, write into `var/` under `user-emacs-directory`.

## Error Handling
- Use `user-error` for interactive misuse and invalid user actions.
- Use `condition-case` around operations that can fail externally, such as optional packages, filesystem work, rendering, or parsing.
- Use `ignore-errors` sparingly and only when failure is acceptable and non-fatal.
- Prefer explicit fallbacks over silent suppression.
- Avoid breaking batch load because an optional interactive package is absent.

## Hooks, Keymaps, And UX
- This repo cares about keybinding consistency; read `docs/keybinding-semantics.md` before changing keymaps.
- Prefer leader/localleader semantics and preserve established Bepo-aware bindings.
- Re-assert fragile bindings in mode hooks or `with-eval-after-load` when package init order can overwrite them.
- For mode-specific setup, use hooks and idempotent setup functions.
- Avoid global side effects in modules that should only affect one mode or one environment.
- Keep discovery-friendly labels and DWIM bindings aligned with `modules/interactive/editing/keys.el` and `lisp/hub-keys.el`.

## Package Configuration Style
- `use-package` is the standard package declaration pattern.
- Prefer built-in packages with `:straight (:type built-in)` when that is already the repo pattern.
- Use `:commands`, `:hook`, `:after`, and `:config` conservatively; keep startup implications clear.
- If a package is interactive-only, keep it out of batch-only paths.
- When changing dependencies, review whether `straight/versions/default.el` also needs updating.

## Testing Style
- Put ERT tests in `test/*-test.el`.
- Load `test/test-helpers.el` when the test relies on repo helper macros or temp project setup.
- Prefer focused unit tests over broad integration tests when validating a single module behavior.
- Stub optional external packages with minimal APIs rather than making tests network- or package-install-dependent.
- Name tests descriptively with the `hub/...` or module-specific prefix style already used in the file.

## Change Discipline
- Keep edits minimal and scoped to the task.
- Do not rewrite unrelated formatting or move modules around unless required.
- Never store secrets or truly sensitive values in tracked files; reserve `private/setup.el` for those rare uncommittable values only. Normal personal configuration belongs in tracked modules under the appropriate domain/category.
- When introducing mutable paths, route them through `var/` or `etc/` according to whether they are runtime state or static assets.
- Before handoff, run the most relevant load/test commands for the touched area and mention any checks you could not run.

## Common Pitfalls
- Do not add interactive-only requires to always-on files unless they are guarded for batch safety.
- Do not put new tracked content under `settings/`; use `modules/`, `lisp/`, `etc/`, `var/`, or `private/setup.el` as appropriate.
- Do not use `ignore-errors` to hide required dependency failures; prefer explicit optional loading or `condition-case` with a real fallback.
- Do not forget `test/test-helpers.el` or `-Q -L .` when running isolated ERT tests; many tests rely on that setup.
- Do not handwave formatting/checkdoc; the pre-commit hooks will fail and require restaging if they modify files.
