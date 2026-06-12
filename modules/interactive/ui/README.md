---
domain: interactive-foundation
status: draft
last-reviewed: 2026-06-10
---

# Presentation

## Ubiquitous Language

- **Presentation**: Visual and terminal display behavior, including dashboard, theme, fonts, GUI, and TTY paths.
- **First paint**: The point at which the Emacs frame/buffer is visible and usable.
- **Dashboard**: Startup or sidebar presentation that summarizes relevant activity without blocking interaction.

## Invariants

- Presentation should make startup and common workflows legible without delaying usable interaction unnecessarily.
- GUI and TTY behavior may differ, but each path should remain intentional and usable.
- Dashboard and theme behavior should conform to established navigation and window-management conventions.
