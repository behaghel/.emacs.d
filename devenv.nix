{ pkgs, lib, ... }:
{
  # Developer shell packages
  packages = [
    pkgs.editorconfig-core-c
    pkgs.git
    pkgs.ripgrep
  ];

  env.EDITOR = "emacs";

  enterShell = ''
    git config core.hooksPath .githooks
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
}
