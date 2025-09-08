{ pkgs, lib, ... }:
{
  # Developer shell packages
  packages = [
    pkgs.editorconfig-core-c
    pkgs.git
    pkgs.ripgrep
    pkgs.emacs-nox
    pkgs.pre-commit
  ];

  env.EDITOR = "emacs";
  env.FLOW = ''
  Project commands (run inside `nix develop`):
    - devenv run tangle              : Tangle docs/config.org
    - devenv run load-check          : Batch-load init.el
    - devenv run freeze              : Freeze straight packages
    - devenv run elisp:format-all    : Format all tracked .el files
    - devenv run elisp:checkdoc-all  : Run checkdoc on all tracked .el files
    - devenv run pre-commit:all      : Run all pre-commit checks on all files

  Tips:
    - Always commit from this shell (pre-commit runs here).
    - Run `pre-commit run -a` before pushing.
    - CI mirrors these checks and caches straight.el/eln.
  '';

  # Install pre-commit hooks on shell entry
  enterShell = ''
    echo "$FLOW"
  '';

  # Developer scripts (run with: devenv run <name>)
  scripts = {
    tangle.exec = ''
      emacs --batch -l org --eval '(org-babel-tangle-file "docs/config.org")'
    '';
    load-check.exec = ''
      HOME=$PWD emacs --batch -l init.el --eval '(message "Loaded init.el")' --kill
    '';
    freeze.exec = ''
      emacs --batch -l core/packages.el --eval '(core/packages-freeze)' --kill
    '';
    "elisp:format-all".exec = ''
      chmod +x scripts/elisp-format || true
      mapfile -t el_files < <(git ls-files "*.el")
      if [ ''${#el_files[@]} -gt 0 ]; then
        ./scripts/elisp-format "''${el_files[@]}"
      fi
    '';
    "elisp:checkdoc-all".exec = ''
      chmod +x scripts/elisp-checkdoc || true
      elisp-parse = {
        enable = true;
        name = "elisp-parse";
        entry = "./scripts/elisp-parse";
        language = "system";
        files = "\.el$";
        pass_filenames = true;
      };
      elisp-ert = {
        enable = true;
        name = "elisp-ert";
        entry = "./scripts/elisp-ert";
        language = "system";
        files = "(modules/.+\.el|test/.+\.el)";
        pass_filenames = false;
      };
      mapfile -t el_files < <(git ls-files "*.el")
      if [ ''${#el_files[@]} -gt 0 ]; then
        ./scripts/elisp-checkdoc "''${el_files[@]}"
      fi
    '';
    "pre-commit:all".exec = ''
      pre-commit run -a || exit 1
    '';
  };

  # Enforce formatting and checkdoc via pre-commit
  pre-commit = {
    hooks = {
      elisp-format = {
        enable = true;
        name = "elisp-format";
        entry = "./scripts/elisp-format";
        language = "system";
        files = "\\.el$";
        pass_filenames = true;
      };
      elisp-checkdoc = {
        enable = true;
        name = "elisp-checkdoc";
        entry = "./scripts/elisp-checkdoc";
        language = "system";
        files = "\\.el$";
        pass_filenames = true;
      };
    };
  };
}
