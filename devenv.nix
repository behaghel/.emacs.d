{ pkgs, lib, config, ... }:
let
  texliveEnv = pkgs.texliveSmall.withPackages (ps: [
    ps.collection-latexrecommended
    ps.collection-fontsrecommended
    ps.pgf
    ps.pgfplots
    ps.geometry
    ps.hyperref
    ps.needspace
    ps.ulem
    ps.fontspec
    ps.wrapfig
    ps."capt-of"
    ps.tcolorbox
    ps.minted
    ps.fvextra
  ]);
in
{
  # Developer shell packages
  packages = [
    pkgs.editorconfig-core-c
    pkgs.fd
    pkgs.git
    pkgs.pass
    pkgs.ripgrep
    pkgs.pre-commit
    pkgs.ghostscript
    pkgs.poppler-utils
    texliveEnv
  ];

  env.DEEPSEEK_API_KEY = config.secretspec.secrets.DEEPSEEK_API_KEY or "";
  env.EDITOR = "emacs";
  env.OSFONTDIR = "/Library/Fonts:/Users/hubertbehaghel/Library/Fonts:/System/Library/Fonts";
  env.FLOW = ''
  Project commands:
    - CI full load       : devenv shell -- ci:load-all
    - Format all elisp   : devenv shell -- elisp:format-all
    - Checkdoc all elisp : devenv shell -- elisp:checkdoc-all
    - Tangle config      : devenv shell -- tangle
    - Load check         : devenv shell -- load-check
    - Freeze packages    : devenv shell -- freeze
    - Pi (DeepSeek)      : pi [args...]   # defaults to --provider deepseek --model deepseek-v4-flash
    - Pre-commit (all)   : devenv shell -- pre-commit:all
    - devenv shell -- <command>     : Run one-off commands in the dev shell
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
    ''
    ;
    pi.exec = ''
      mapfile -t pi_candidates < <(which -a pi 2>/dev/null || true)
      self_path="$(command -v pi || true)"
      upstream_pi=""

      for candidate in "''${pi_candidates[@]}"; do
        if [ -n "$candidate" ] && [ "$candidate" != "$self_path" ]; then
          upstream_pi="$candidate"
          break
        fi
      done

      if [ -z "$upstream_pi" ]; then
        printf '%s\n' "Could not find an upstream 'pi' binary beyond the devenv wrapper. Install pi on your host or adjust the wrapper target." >&2
        exit 1
      fi

      has_provider=0
      has_model=0
      for arg in "$@"; do
        case "$arg" in
          --provider|--provider=*)
            has_provider=1
            ;;
          --model|--model=*)
            has_model=1
            ;;
        esac
      done

      cmd=("$upstream_pi")
      if [ "$has_provider" -eq 0 ]; then
        cmd+=(--provider deepseek)
      fi
      if [ "$has_model" -eq 0 ]; then
        cmd+=(--model deepseek-v4-flash)
      fi
      cmd+=("$@")

      exec "''${cmd[@]}"
    '';
    "ci:load-all".exec = ''
      HOME=$PWD HUB_FORCE_FULL_LOAD=1 emacs --batch -l scripts/ci-load-all.el
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
  git-hooks = {
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

    };
  };
}
