---
domain: personal-apps
status: draft
last-reviewed: 2026-06-08
---

# Personal Apps

## Ubiquitous Language

- **App module**: Self-contained interactive workflow that is not part of foundational editing, writing, email, or development behavior.
- **Feed workflow**: Reading and managing subscriptions from inside Emacs.
- **Video workflow**: Media-oriented command flow integrated into the editor.

## Invariants

- App modules should not impose startup cost unless their UI is explicitly part of startup.
- App-specific keybindings should fit the established leader/localleader model.
- External services or media tools must be optional or clearly declared in the development/runtime environment.
- App modules should keep their state under managed runtime locations rather than the repo root.

## Integration Notes

These workflows conform to the interactive experience domain.  They should remain isolated enough that failures or missing external tools do not degrade core editing, writing, or startup behavior.
