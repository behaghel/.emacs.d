---
domain: development
status: draft
last-reviewed: 2026-06-08
---

# Development Workbench

## Ubiquitous Language

- **Language module**: Per-language configuration under `modules/lang/`.
- **Development default**: Cross-language programming behavior applied by the common development module.
- **Project environment**: Directory-local tool environment, often supplied by direnv/devenv.
- **Code assistance**: Completion, diagnostics, navigation, formatting, or LSP/Eglot behavior.
- **Grammar**: Tree-sitter parser used by a major mode.

## Invariants

- Language-specific behavior should live in language modules, not in always-on runtime code.
- Code assistance may become ready after a file is visible; navigation must not be held hostage by environment reloads.
- Cross-project file visits should prioritize showing the file before direnv/Eglot readiness.
- External tools should come from the declarative development environment where practical.
- Language modules should fail gracefully when optional tools are absent, while required behavior remains visible in CI.

## Integration Notes

The development workbench conforms to interactive key/navigation conventions and consumes runtime package/bootstrap contracts.  Its environment integration is a major performance-sensitive boundary and should be measured before changing.
