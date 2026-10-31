{ pkgs, lib, config, ... }:
let
  # Released CAVA needs a loopback device on macOS.  This revision supports
  # CoreAudio system-output taps without changing the user's audio routing.
  cavaVersion = "0.10.6-unstable-2026-10-28";
  # Keep iniparser's dependency tests enabled with newer Clang's C++ checks.
  unityForCava = pkgs.unity-test.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + ''
      substituteInPlace src/unity_internals.h \
        --replace-fail 'UnityFloatToPtr(expected),' '(const UNITY_FLOAT*)UnityFloatToPtr(expected),' \
        --replace-fail 'UnityDoubleToPtr(expected),' '(const UNITY_DOUBLE*)UnityDoubleToPtr(expected),'
    '';
  });
  cavaPackage = if pkgs.stdenv.hostPlatform.isDarwin then
    (pkgs.cava.override {
      iniparser = pkgs.iniparser.override { unity-test = unityForCava; };
    }).overrideAttrs (old: {
      version = cavaVersion;
      src = pkgs.fetchFromGitHub {
        owner = "karlstav";
        repo = "cava";
        rev = "d272bb61d28a2e91217e927a1f099670290f59e0";
        hash = "sha256-7VRYZ4045CIy2h8QMrSWgEPVFgPjMlX9m4tIDShaKZM=";
      };
      preAutoreconf = "echo ${cavaVersion} > version";
      buildInputs = old.buildInputs ++ [ (pkgs.darwinMinVersionHook "14.2") ];
      postConfigure = (old.postConfigure or "") + ''
        if ! grep -q -- '-DCOREAUDIO_TAP' Makefile; then
          echo "CAVA requires a macOS SDK with CoreAudio tap support (14.2+)" >&2
          exit 1
        fi
      '';
    })
  else pkgs.cava;
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
    ps.pdfcol
    ps.minted
    ps.fvextra
    ps.lettrine
    ps.enumitem
  ]);
in
{
  # Shared launcher; implementation and shell integration come from nixos-config.
  agentSession = {
    enable = true;
    agentCommand = [ "pi" ];
    # Never link private Emacs settings, package caches, or Pi/runtime state.
    localLinks = [ "devenv.local.nix" "devenv.local.yaml" ".envrc.local" ];
  };

  # Developer shell packages
  packages = [
    cavaPackage
    pkgs.editorconfig-core-c
    pkgs.emacsPackages.package-lint
    pkgs.fd
    pkgs.git
    pkgs.pass
    pkgs.ripgrep
    pkgs.pre-commit
    pkgs.ghostscript
    pkgs.graphviz
    pkgs.poppler-utils
    pkgs.texinfo
    texliveEnv
  ];

  env.EDITOR = "emacs";
  env.OSFONTDIR = "/Library/Fonts:/Users/hubertbehaghel/Library/Fonts:/System/Library/Fonts";
  env.FLOW = ''
  Project commands:
    - Agent worktree     : as <task-slug> create --json
    - Run in worktree    : as <task-slug> run <command> [args...]
    - Session diagnostics: as <task-slug> doctor --json
    - CI full load       : devenv shell -- ci:load-all
    - Format all elisp   : devenv shell -- elisp:format-all
    - Checkdoc all elisp : devenv shell -- elisp:checkdoc-all
    - Byte compile pkgs  : devenv shell -- elisp:byte-compile-packages
    - Package lint pkgs  : devenv shell -- elisp:package-lint-packages
    - Confluence docs    : devenv shell -- docs:confluence
    - Org comments docs  : devenv shell -- docs:org-comments
    - Load check         : devenv shell -- load-check
    - Freeze packages    : devenv shell -- freeze
    - Pre-commit (all)   : devenv shell -- pre-commit:all
    - devenv shell -- <command>     : Run one-off commands in the dev shell
  '';

  dotenv.enable = true;

  # Install pre-commit hooks on shell entry
  enterShell = ''
    echo "$FLOW"
  '';

  # Developer scripts (run with: devenv run <name>)
  scripts = {
    load-check.exec = ''
      HOME=$PWD emacs --batch -l init.el --eval '(message "Loaded init.el")' --kill
    '';
    freeze.exec = ''
      emacs --batch -Q -L core --eval '(setq user-emacs-directory (expand-file-name default-directory))' \
        -l core/core-packages.el --eval '(core/packages-freeze)' --kill
    ''
    ;
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
    "elisp:byte-compile-packages".exec = ''
      chmod +x scripts/elisp-byte-compile || true
      ./scripts/elisp-byte-compile
    '';
    "elisp:package-lint-packages".exec = ''
      chmod +x scripts/elisp-package-lint || true
      ./scripts/elisp-package-lint
    '';
    "docs:confluence".exec = ''
      ./scripts/elisp-package-docs \
        --load packages/org-confluence/org-confluence.el \
        --prefix org-confluence- \
        --command-prefix org-confluence- \
        --out packages/org-confluence/docs/generated
    '';
    "docs:org-comments".exec = ''
      chmod +x scripts/org-comments-gen-docs scripts/org-comments-check-doc-drift || true
      ./scripts/org-comments-gen-docs
      emacs --batch -Q -L packages/org-comments -l ox-texinfo \
        --visit packages/org-comments/docs/org-comments.org \
        --funcall org-texinfo-export-to-texinfo
      makeinfo --no-split packages/org-comments/docs/org-comments.texi \
        -o packages/org-comments/docs/org-comments.info
      install-info --dir=packages/org-comments/docs/dir \
        packages/org-comments/docs/org-comments.info
    '';
    "docs:org-comments:check".exec = ''
      chmod +x scripts/org-comments-gen-docs scripts/org-comments-check-doc-drift || true
      ./scripts/org-comments-gen-docs
      ./scripts/org-comments-check-doc-drift
    '';
    lint.exec = ''
      chmod +x scripts/elisp-parse scripts/elisp-checkdoc || true
      mapfile -t el_files < <(git ls-files "*.el")
      if [ ''${#el_files[@]} -gt 0 ]; then
        ./scripts/elisp-parse "''${el_files[@]}"
        ./scripts/elisp-checkdoc "''${el_files[@]}"
      fi
      load-check
    '';
    ci.exec = ''
      lint
      docs:confluence
      docs:org-comments:check
      pre-commit:all
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
        files = "(core/.+\.el|init\.el|lisp/.+\.el|modules/.+\.el|packages/.+\.el|scripts/.+\.el|test/.+\.el)";
        pass_filenames = true;
        require_serial = true;
      };

    };
  };
}
