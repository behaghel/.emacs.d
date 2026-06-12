---
domain: interactive-foundation
status: draft
last-reviewed: 2026-06-08
---

# Interactive Experience

## Ubiquitous Language

- **Leader binding**: Discovery-oriented Evil keybinding rooted at comma.
- **Localleader binding**: Mode-specific command surface with stable semantic families.
- **Bépo rotation**: Keyboard-layout-aware Evil movement/key translation policy.
- **Perspective**: Activity or project context used to organize buffers and navigation state.
- **First paint**: The point at which the Emacs frame/buffer is visible and usable.

## Invariants

- Evil normal state is the default unless a documented exception justifies another state.
- Keybinding changes must preserve established leader/localleader semantics unless explicitly approved.
- Interactive UX changes that affect startup screens, dashboard visibility, or command availability require discussion before implementation.
- GUI and TTY behavior may differ, but each path should remain intentional and usable.
- Navigation, completion, and editing features should optimize responsiveness without surprising the user.

## Integration Notes

Other user-facing domains conform to this domain's command surface, Evil conventions, and presentation expectations.  When a feature needs a new global interaction pattern, update the shared key semantics rather than inventing a private convention.
