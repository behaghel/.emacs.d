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

  # Install pre-commit hooks on shell entry
  enterShell = ''
    if command -v pre-commit >/dev/null 2>&1; then
      pre-commit install --install-hooks >/dev/null 2>&1 || true
    fi
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
