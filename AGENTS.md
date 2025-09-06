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

