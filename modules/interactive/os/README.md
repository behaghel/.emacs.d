---
domain: interactive-foundation
status: draft
last-reviewed: 2026-06-10
---

# OS Integration

## Ubiquitous Language

- **OS integration**: Operating-system-specific behavior that adapts Emacs to platform conventions and constraints.
- **Platform tweak**: A narrowly scoped adjustment for keyboard, shell, window, or system integration on one operating system.

## Invariants

- OS-specific behavior should stay isolated from portable interactive configuration.
- Platform tweaks must preserve shared keybinding semantics unless explicitly documented.
- Missing platform facilities should not break startup on other systems.
