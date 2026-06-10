---
domain: development-workbench
status: draft
last-reviewed: 2026-06-10
---

# Programming Defaults

## Ubiquitous Language

- **Development default**: Cross-language programming behavior applied to `prog-mode` or development buffers.
- **Project environment**: Directory-local command environment, usually provided by direnv/devenv, applied without blocking file display.
- **Code assistance**: Formatting, diagnostics, navigation, Tree-sitter, LSP, or editor affordances that help modify code.
- **Structural editing**: S-expression or delimiter-aware editing, currently provided by Smartparens for programming buffers.

## Invariants

- Programming defaults must remain cross-language; language-specific behavior belongs under `modules/lang/`.
- File visits and project switches should show buffers before environment or LSP readiness catches up.
- Formatting, whitespace, Tree-sitter, LSP, and REPL behavior should be separable modules orchestrated by `dev/common` or `dev/core`.
- Development modules conform to interactive keybinding conventions and are not loaded in ordinary batch sessions.
