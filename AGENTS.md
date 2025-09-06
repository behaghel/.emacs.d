# Repository Guidelines

## Project Structure & Module Organization
- `init.el`: entry point; sets `user-emacs-directory` and loads modules.
- `settings/`: modular configuration (`setup-*.el`) and language tooling under `settings/dev/`.
- `lisp/`: custom helpers (e.g., `eshell-autojump.el`).
- `snippets/`, `insert/`, `eshell/`: editor assets and templates.
- `.githooks/post-commit`: formats changed `*.el` files and runs `checkdoc`.

## Build, Test, and Development Commands
- Enter dev shell (Emacs, EditorConfig, Git preconfigured):
  - `nix develop` (or `nix develop .`).
- Quick load check (mirrors CI):
  - `HOME=$PWD emacs --batch -l init.el --eval '(message "Loaded")' --kill`.
- Check a file with checkdoc:
  - `emacs --batch path/to/file.el -l checkdoc --eval '(checkdoc-file "path/to/file.el" t)'`.

## Coding Style & Naming Conventions
- Indentation: spaces, 2 spaces (see `.editorconfig`); no tabs in Lisp.
- File naming: `settings/setup-<topic>.el`, language modules in `settings/dev/`.
- Elisp: prefer `hub/` prefix for repo-specific helpers and commands.
- Formatting: post-commit hook auto-indents and runs `whitespace-cleanup`; fix any `checkdoc` warnings reported.

## Testing Guidelines
- CI loads the config on push/PR (`.github/workflows/emacs.yml`). Keep it green.
- Local smoke test before pushing: same command as above to batch-load `init.el`.
- Optional ERT tests: place under `test/` as `*-test.el`. Run with:
  - `emacs --batch -l ert -l test/your-test.el -f ert-run-tests-batch-and-exit`.

## Commit & Pull Request Guidelines
- Commit messages: short, imperative subject; include scope when useful, e.g. `[init.el] Load without error`.
- PRs: describe the change, affected modules (`settings/...`), and any user-visible behavior; link related issues.
- Screenshots/gifs welcome for UI changes (themes, layouts), but not required.

## Security & Configuration Tips
- Secrets and machine-specific settings go in `settings/setup-private.el` (gitignored) and are loaded optionally.
- GPG/Pass: this config uses `auth-source-pass` and `pinentry`; ensure your keychain is set up.
- Package management uses `straight.el` on demand; do not commit cache/vendor directories.

## Git Discipline & Branching Workflow

- Branching: start each task on a fresh branch from the current base (`main` or the agreed feature base). Use prefixes like `feat/…`, `fix/…`, `chore/…`, or `refactor/…`.
- Commits: commit early and often with focused diffs; use short, imperative subjects and include scope when helpful, e.g., `[settings/setup-foo.el] Describe change`.
- Push & CI: push branches to `origin` regularly and verify GitHub Actions status. Fix CI breaks before continuing related work.
- Remotes: use SSH remotes (e.g., `git@github.com:behaghel/.emacs.d.git`). Avoid interactive GitHub logins; no HTTPS remotes.
- Flow for larger efforts: create a meta/setup branch first (e.g., `chore/git-discipline`), push it, then branch the long‑running migration work from it (e.g., `refactor/migration-base`).
- WIP management: if needed, `git stash push -u` to move in‑progress changes between branches cleanly.
- Agent etiquette: the agent announces new branches, commits frequently, and requests approval before network actions (e.g., `git push`).

## Pre-commit Enforcement (devenv)

- Hooks: formatting and `checkdoc` run as pre-commit hooks using devenv’s `pre-commit` integration.
- What runs:
  - `elisp-format`: formats changed `*.el` (indent + `whitespace-cleanup`). If it modifies files, the commit is blocked; re-stage and commit again.
  - `elisp-checkdoc`: runs `checkdoc` on changed `*.el`. Any warnings fail the commit.
- Usage:
  - Enter the dev shell: `nix develop`. It auto-installs the pre-commit hooks.
  - Commit as usual; hooks will run on staged `*.el` files.
  - To install manually: `pre-commit install --install-hooks`.
- Requirements: `emacs` available in the shell (provided via devenv); `pre-commit` is included.
- CI: mirror checks can be added later; for now local hooks enforce cleanliness before pushing.
