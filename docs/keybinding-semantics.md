# Keybinding Semantics: Consistent, Discoverable, Mode-Aware

This document proposes a cohesive keybinding semantic for our Emacs configuration that:

- Drives consistency across packages and languages.
- Centralizes declaration so bindings are easy to find and audit.
- Works with Evil (modal), GUI/TTY, and both global and mode-local contexts.
- Scales to language environments so similar actions share the same keys.
- Is discoverable at runtime via which‑key and hydra palettes.

The design borrows from excellent setups (Doom, Spacemacs, Prot’s dotemacs) while fitting our comma leader and module architecture.

---

## Goals

- Consistency: the same category prefix across modes (files, buffers, windows, project, VCS, search, code, test, debug, repl, docs, toggles, apps).
- Single source of truth: each module declares bindings in one place using a standard API.
- Mode layering: global leader for universal actions; localleader for mode/language‑specific actions.
- Discoverability: which‑key shows categories; hydra palettes offer a visual/interactive cheat sheet.
- Minimal surprises: keep existing mental model where it helps; reduce global pollution.

## High‑Level Semantics

- Leader: `,` in Evil normal state (primary global prefix).
- Localleader: `;` (single‑key, per major‑mode domain). For insert/motion, keep minimal targeted bindings.
- Category prefixes under leader:
  - `,f`: Files (open/save/recent/rename)
  - `,b`: Buffers (switch/kill/rename/reopen)
  - `,w`: Windows (split/move/resize/tabs)
  - `,p`: Project (switch/find/search/shell)
  - `,g`: VCS (Magit, hunk nav, diff)
  - `,s`: Search (ripgrep/consult, in‑buffer, xref)
  - `,e`: Evaluate/Execute (eval/compile/shell)
  - `,c`: Code (LSP actions, format, refactor)
  - `,t`: Test/Toggle (tests; UI toggles under `,t u`)
  - `,d`: Debug (dap/gud; language‑specific debug)
  - `,r`: REPL/Run (start/send buffer/region)
  - `,h`: Help/Docs (describe, docsets, symbols)
  - `,o`: Open/Apps (Treemacs, Elfeed, mail, terminals)
  - `,x`: Text/Transform (align/sort/narrow)
  - `,y`: Yank/Copy helpers (paths, URLs)
  - `,q`: Quit/Session (workspace/session management)
  - `,?`: Cheat sheet (hydra/which‑key landing)

Notes:
- Mode‑specific items go under localleader by default (`;` …). If a command is universal (e.g., `consult-line`), place it under leader (`,s`), not localleader.
- Conflicts resolved via per‑mode maps and which‑key labels; avoid grabbing common keys in insert state.

## Language & Programming Semantics

For programming modes, consistency matters most. Reserve these under leader and localleader:

- `,c` Code:
  - `,c a` LSP/Code Action
  - `,c f` Format buffer/region
  - `,c r` Refactor (rename/symbol)
  - `,c i` Inspect/type info
  - `,c g` Goto (def/refs/impl)
- `,t` Test:
  - `,t t` Test current (file/it)
  - `,t a` Test all
  - `,t r` Rerun
  - `,t s` Toggle test/target buffer
- `,r` REPL/Run:
  - `,r r` Start/attach REPL
  - `,r b` Send buffer
  - `,r e` Send region/sexp
  - `,r l` Load file
- `,d` Debug:
  - `,d d` Start debug
  - `,d b` Toggle breakpoint
  - `,d c` Continue/step controls

Localleader (`;` …) mirrors the same namespaces for mode‑specific extras that don’t warrant global exposure (e.g., language tool menus, generators).

## Structural Motions & Text Objects

Structural navigation and editing should be consistent across languages and modes. We unify:

1) Evil text objects and motions
- Retain core motions/objects and add semantic ones (function/class/block/arg/etc.).

2) Tree‑sitter semantics (Emacs 29+)
- Provide motions and objects using Tree‑sitter:
  - Motions: `g n/p` (next/prev node), `g u` parent, `g d` child, `g f/F` next/prev function, `g c/C` class, `g b/B` block.
  - Text objects: `i f`/`a f` (function), `i c`/`a c` (class), `i b`/`a b` (block), `i a`/`a a` (arg/param), `i s`/`a s` (string), `i C`/`a C` (comment), etc.
- Implementation strategy: prefer built‑ins (treesit‑beginning/end‑of‑thing), complement with evil text‑object helpers where needed. For languages lacking treesit, fall back to imenu/regex heuristics.

3) Smartparens pairs
- Slurp/barf/unwrap consistently; available via a structural hydra and localleader `;` → `s` group to avoid clutter.

Org‑mode mapping
- Apply the same mental model to Org elements: headline/subtree, src block, list item, table. Reuse `g` motions and `i/a` objects accordingly.

Discovery hydra
- `,c s` (and/or localleader `;` → `s`) summons a per‑mode structural hydra showing motions and objects with which‑key labels.

## Visual Discovery

- which‑key: Required. Shows category names and command labels for prefixes. Category prefixes will be labeled (e.g., `,g` → “VCS/Git”, `,c` → “Code”).
- Hydra palettes:
  - `,?` Global cheat sheet hydra showing top‑level categories and most common sub‑bindings.
  - Category hydras (optional) under each prefix, e.g., `,w` pops a window‑management hydra; `,g` offers common Magit actions.
- Optional: embark integration for context‑aware actions.

Must‑have packages
- embark: action layer everywhere (minibuffer, buffers, lists). Bind under leader (`,e a`) and enable embark‑collect on common sources; wire embark‑consult.
- marginalia: rich minibuffer annotations. Enabled by default; ensure which‑key labels match marginalia categories.

## Declaration Pattern (per module)

- Use `general.el` definers to centralize leaders and apply to Evil states.
- Each module declares all its bindings in a single `:general` block (or a dedicated “keybindings” section), not scattered `define-key` calls.
- Global vs mode:
  - Global: `:keymaps 'global` with the leader definer for universal actions.
  - Mode: `:keymaps 'major-mode-map` with the localleader definer.

Example (illustrative; actual API provided by our helper macros):

```elisp
;; Central (hub-keys.el)
(general-create-definer hub/leader :states '(normal visual) :prefix ",")
(general-create-definer hub/localleader :states '(normal visual) :prefix ".")

;; Package module
(use-package magit
  :commands (magit-status)
  :general
  (hub/leader
    ",g s"  #'magit-status
    ",g b"  #'magit-branch-checkout
    ",g /"  #'vc-git-grep)
  :config
  ;; Fine‑grained mode maps can still be added here if needed
  )

(use-package eglot
  :commands (eglot)
  :general
  (hub/leader
    ",c a"  #'eglot-code-actions
    ",c r"  #'eglot-rename
    ",c g d" #'eglot-find-declaration
    ",c g r" #'xref-find-references
    ",c f"  #'eglot-format)
  (hub/localleader :keymaps 'eglot-mode-map
    "k" #'eldoc))
```

Guidelines:
- Prefer `:general` over `:config` keymap mutations. If a package needs a mode map, use `:keymaps` in `:general` with `:after` or the mode symbol.
- Avoid global `define-key evil-normal-state-map` in package modules. Keep those centralized in editing/evil if truly global.
- Don’t bind in insert state unless there is a strong precedent.

Repository layout for keys
- Keep global keymaps in a dedicated file (e.g., `modules/interactive/editing/keys.el`), separate from Evil setup (`editing/evil.el`).
- `hub-keys.el` in `lisp/` provides the leader/localleader definers and shared helpers (DWIM, speed‑dial, hydra entry points).

## DWIM (Do What I Mean) Philosophy

Bindings should adapt to context while keeping the same chord:

- `,f f` find‑file DWIM: if in a project → `project-find-file`, else → `find-file`.
- `,s s` search DWIM: project ripgrep if in project, else `consult-line` (or ripgrep in current dir if large buffer).
- `,b s` switch buffer DWIM: prefer `consult-project-buffer` in project, else `consult-buffer`.
- `,c f` format DWIM: prefer Eglot formatter if managed, else fallback (apheleia/indent-region).
- `,t t` test DWIM: run nearest/file based on mode; rerun if already failed.
- `,r r` REPL DWIM: attach to existing REPL if present, else start appropriate one.
- `,g /` VCS grep DWIM: use `consult-git-grep` if in Git repo, else `consult-ripgrep`.

Implementation approach
- Provide a small library of `hub/dwim-*` helpers (in `hub-keys.el`) and use them throughout `:general` bindings.
- Make DWIM functions first‑class in the migration: replacing direct command bindings with DWIM wrappers.

Open brainstorm for further DWIM dimensions
- OS/open‑with (browse URL vs eww); window reuse vs pop; symbol at point defaulting; region vs buffer target.

## Naming & Labels

- which‑key names every prefix with a readable title:
  - `,f` “Files”, `,b` “Buffers”, `,w` “Windows”, `,p` “Project”, `,g` “VCS/Git”, `,s` “Search”,
    `,e` “Eval/Exec”, `,c` “Code”, `,t` “Test/Toggle”, `,d` “Debug”, `,r` “REPL/Run”,
    `,h` “Help/Docs”, `,o` “Open/Apps”, `,x` “Text/Transform”, `,y` “Yank/Copy”, `,q` “Quit/Session”.
- Use consistent sub‑keys across languages; e.g., `,t t` is always “test current”, `,c a` is always “code action”.

## Conflict & Mode Policy

- Mode maps can shadow global leader keys for better UX, but must retain category semantics.
- Packages integrate via mode‑local bindings under `, m …` unless they fit a global category.
- Never redefine a category prefix to mean a completely different domain within a mode.

## Migration Plan

1. Core helpers: add `hub-keys.el` with leader/localleader definers and which‑key labels.
2. Global keys: move truly global bindings into `editing/keys.el` (separate file) using `hub/leader`.
3. Package pass 1: convert scattered `define-key` to `:general` in each module, aligning to categories.
4. Language pass: ensure `,c`, `,t`, `,r`, `,d` actions are present and consistent for JS/TS, Python, Ruby, Scala, etc.
5. Discovery: add `,?` global hydra; optional hydras for `,w`, `,g`, `,p`.
6. Docs: keep this spec as the source of truth; update when categories evolve.

## Backwards Compatibility

- Keep legacy aliases for most‑used chords (e.g., `,vs` → `,g s`) for one cycle with deprecation notices in which‑key labels.
- Respect TTY constraints: avoid sequences that render poorly; provide fallbacks.

## Appendix: Suggested Default Global Map

- `,f`: find file (`consult-find`), recent (`recentf`), save, rename.
- `,b`: switch (`consult-buffer`), kill, rename, revert.
- `,w`: split v/h, move focus, resize, balance, close.
  - Speed Dial: quick open/switch entries under `,w o KEY`; hydra on `,w O` to browse/edit dials.
- `,p`: project switch/find file/grep/shell/eshell/compile.
- `,g`: `magit-status`, next/prev hunk, stage/unstage hunk, blame.
- `,s`: `consult-line`, `consult-ripgrep`, isearch→consult, xref.
- `,e`: eval buffer/region/last, compile, `shell-command`.
- `,c`: LSP/goto/format/rename/code action.
  - Default to Eglot functions where applicable.
- `,t`: tests (file/all/last), toggles (UI) under `,t u`.
- `,d`: debugger controls.
- `,r`: start REPL, send buffer/region.
- `,h`: describe*, `helpful-*`, man/info/docs.
- `,o`: Treemacs, mail, Elfeed, terminals.
- `,x`: align, sort, narrow/widen.
- `,y`: copy file path, URL at point, symbol.
- `,q`: quit, save session, workspace state.

---

This semantic aims to be both opinionated and pragmatic: the top‑level categories are fixed to build muscle memory, while mode‑local leaders offer flexibility. The migration will make bindings consistent and discoverable, and the declaration surface small and obvious in each module.
### Speed Dial (`,o` …)

Purpose: quick access to frequently used workspaces/files/apps with consistent perspective behavior.

- Location: under the Open/Apps category (`,o`), single keys per destination (e.g., `,o m` for mail, `,o e` for Emacs config).
- Behavior:
  - Perspective: switches/creates the named perspective.
  - File target: opens the file in that perspective.
  - Command target: runs a command (e.g., `mu4e`, `elfeed`).
  - Sidebar: entries can opt-in to ensuring Treemacs is open as a sidebar; focus remains on the primary buffer.
- Configuration:
  - Declared in `navigation/perspective.el` via `hub/speed-dial-items`.
  - Supported forms:
    - `(KEY perspective NAME)` → just switch perspective.
    - `(KEY file PATH NAME)` → switch perspective and find file.
    - `(KEY file-with-tree PATH NAME)` → switch, open file, ensure Treemacs sidebar.
    - `(KEY command FN NAME)` → switch perspective and run command.
  - Example bindings (MVP):
    - `,o m`: Mail — switch to "mails" perspective, run `mu4e`, show mu4e-dashboard as a sidebar.
    - `,o e`: Emacs config — switch to ".emacs.d", open `init.el`, ensure Treemacs sidebar.
    - `,o t`: Treemacs, `,o f`: Elfeed, etc.
- Rationale: keeps speed access under a single, memorable prefix while integrating with the structural sidebar (Treemacs) consistently.
