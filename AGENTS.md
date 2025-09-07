# Repository Guidelines

## Project Structure & Module Organization
- `init.el`: entry point; sets `user-emacs-directory` and loads modules.
- `modules/`: layered modules by runtime context (see below).
- `modules/lang/`: language tooling (Scala, JS/TS, Python, etc.).
- `modules/interactive/dev/common.el`: shared development defaults.
- `lisp/`: custom helpers (e.g., `eshell-autojump.el`, `hub-utils.el`).
- `snippets/`, `insert/`: editor assets and templates.
- `etc/`: static assets and configuration resources (e.g., CSS, templates).
- `var/`: runtime state and caches (recentf, savehist, treemacs, project list, persp.state, etc.).
- Eshell: aliases tracked at `modules/interactive/shell/alias.eshell`; state under `.cache/eshell/`.
- `.githooks/post-commit`: formats changed `*.el` files and runs `checkdoc`.

### Environment Layers (Architecture)
- Layers: structure modules by runtime context for predictability and speed.
  - `core`: always-on, no UI side effects (e.g., paths, packages).
  - `interactive`: loaded only when not batch (TTY or GUI) — DX features, keymaps.
  - `gui`: GUI-only adornments (icons, fringes, fancy faces).
  - `tty`: TTY-friendly alternatives and tweaks.
  - `batch`: CI/batch-only optimizations (optional).
- Paths and features:
  - Modules live under `modules/<layer>/<category>/...` and provide category features like `editing/evil`, `navigation/treemacs`, `completion/core`.
  - The legacy `settings/` folder is retired. No tracked files should remain there; CI fails if any do.
  - Convention: keep non-versioned, variable files under `var/` and static assets under `etc/`. Avoid putting ephemeral state files at the repo root — route them to `var/` in code.
- Enforcement:
  - `init.el` filters `load-path` by layer: batch sessions do not see `modules/interactive`, so interactive-only modules cannot load in batch.
  - GUI/TTY specializations can be added similarly (only one on `load-path`).
- Predicates: `core/predicates.el` defines helpers used across modules:
  - `hub/interactive-p`, `hub/batch-p`, `hub/gui-p`, `hub/tty-p`, `hub/ci-p`.

## Build, Test, and Development Commands
- Enter dev shell (Emacs, EditorConfig, Git preconfigured):
  - `nix develop` (or `nix develop .`).
  - Important: run all project commands from the devenv shell. This ensures consistent Emacs/pre-commit versions.
- Quick load check (mirrors CI):
  - `HOME=$PWD emacs --batch -l init.el --eval '(message "Loaded")' --kill`.
- Check a file with checkdoc:
  - `emacs --batch path/to/file.el -l checkdoc --eval '(checkdoc-file "path/to/file.el" t)'`.
  - Compatibility: if your Emacs only supports one-arg `checkdoc-file`, use `(checkdoc-file "path/to/file.el")`.

## Coding Style & Naming Conventions
- Indentation: spaces, 2 spaces (see `.editorconfig`); no tabs in Lisp.
- File naming: language modules in `modules/lang/*.el`; shared dev defaults in `modules/interactive/dev/common.el`.
- Elisp identifiers: prefix repo-specific helper functions/commands with `hub/` (e.g., `hub/transpose-params`).
- Module features and paths: do not use `hub/` in feature names or on-disk paths; use category namespaces like `editing/evil`, `navigation/treemacs`, `completion/core` under `modules/<layer>/<category>/...`.
- Formatting: post-commit hook auto-indents and runs `whitespace-cleanup`; fix any `checkdoc` warnings reported.

## Testing Guidelines
- CI loads the config on push/PR (`.github/workflows/emacs.yml`). Keep it green.
- Local smoke test before pushing: same command as above to batch-load `init.el`.
- Optional ERT tests: place under `test/` as `*-test.el`. Run with:
  - `emacs --batch -l ert -l test/your-test.el -f ert-run-tests-batch-and-exit`.

## Commit & Pull Request Guidelines
- Commit messages: short, imperative subject; include scope when useful, e.g. `[init.el] Load without error`.
- PRs: describe the change, affected modules (e.g., `modules/...`, `lisp/...`), and any user-visible behavior; link related issues.
- Screenshots/gifs welcome for UI changes (themes, layouts), but not required.

## Security & Configuration Tips
- Secrets and machine-specific settings go in `private/setup.el` (gitignored) and are loaded optionally. For backward compatibility, `settings/setup-private.el` is loaded if present.
- GPG/Pass: this config uses `auth-source-pass` and `pinentry`; ensure your keychain is set up.
- Package management uses `straight.el` on demand; do not commit cache/vendor directories.

## Git Discipline & Branching Workflow

- Branching: start each task on a fresh branch from the current base (`main` or the agreed feature base). Use prefixes like `feat/…`, `fix/…`, `chore/…`, or `refactor/…`.
- Commits: commit early and often with focused diffs; use short, imperative subjects and include scope when helpful, e.g., `[modules/lang/js.el] Describe change`.
- Push & CI: push branches to `origin` regularly and verify GitHub Actions status. Fix CI breaks before continuing related work.
- Remotes: use SSH remotes (e.g., `git@github.com:behaghel/.emacs.d.git`). Avoid interactive GitHub logins; no HTTPS remotes.
- Flow for larger efforts: create a meta/setup branch first (e.g., `chore/git-discipline`), push it, then branch the long‑running migration work from it (e.g., `refactor/migration-base`).
- WIP management: if needed, `git stash push -u` to move in‑progress changes between branches cleanly.
- Agent etiquette: the agent announces new branches, commits frequently, and requests approval before network actions (e.g., `git push`).

## Pre-commit Enforcement (devenv)

- Hooks: formatting and `checkdoc` run as pre-commit hooks using devenv’s `pre-commit` integration.
- What runs:
  - `elisp-format`: formats changed `*.el` (indent + `whitespace-cleanup`). If it modifies files, the commit is blocked; re-stage and commit again.
  - `elisp-checkdoc`: runs `checkdoc` on changed `*.el`. Any warnings fail the commit. We ignore purely structural header/footer warnings (e.g., package first-line, Commentary/Code headings, footer provide) to accommodate our module naming.
- Usage:
  - Enter the dev shell: `nix develop`. It auto-installs the pre-commit hooks.
  - Commit as usual; hooks will run on staged `*.el` files.
  - To install manually: `pre-commit install --install-hooks`.
  - To validate everything proactively before committing: `pre-commit run -a`.
  - Convenience (devenv):
    - `devenv run elisp:format-all` to format all tracked `.el` files.
    - `devenv run elisp:checkdoc-all` to run checkdoc on all tracked `.el` files.
    - `devenv run pre-commit:all` to run all pre-commit checks on all files.
- Requirements: `emacs` available in the shell (provided via devenv); `pre-commit` is included.
- CI: mirror checks can be added later; for now local hooks enforce cleanliness before pushing.

## Dependency Pinning (straight.el)

- Rationale: pinning produces a stable `straight/versions/default.el` so CI caches hit reliably and local installs are reproducible.
- Pin versions:
  - `devenv run freeze` (runs Emacs batch and writes `straight/versions/default.el`).
  - Commit the generated file as part of the change that introduced or updated packages.
- CI cache keys: include Emacs version and the hash of `straight/versions/default.el` to avoid unnecessary rebuilds while allowing intentional updates to refresh caches.
- Updating deps: run `devenv run freeze` after making changes, review the diff in `straight/versions/default.el`, and commit it.

## CI Emacs Versions

- Runner setup: CI installs Emacs via `jcs090218/setup-emacs@v1` and pins a specific version (currently `30.2`). This uses GitHub’s toolcache to speed up runs and ensures parity across checks.
- Why: consistent Emacs across CI runs keeps caches effective and avoids version‑specific regressions.
- Bump policy: update the `version:` in `.github/workflows/emacs.yml` when adopting a new major/minor. If package changes are involved, run `devenv run freeze` and commit the updated `straight/versions/default.el`.
- Test multiple versions (matrix):
  - Example workflow fragment:
    - `strategy.matrix.emacs: ["29.4", "30.2"]`
    - Setup step:
      - `uses: jcs090218/setup-emacs@v1`
      - `with: version: ${{ matrix.emacs }}`
  - Our cache key already includes the detected Emacs version, so caches stay distinct per version.
- Local tip: develop with a matching Emacs when validating version bumps (the dev shell provides `emacs-nox`; you can pin nixpkgs if exact versions are needed).

## Agent Devenv Discipline

- Always run commands inside `nix develop` shells for this repo.
- Keep one or two `nix develop` shells open during a session to avoid start-up overhead and ensure hooks are installed.
- When automating from scripts, prefer `nix develop -c <command>` or `devenv run <task>` to preserve environment parity with CI.
