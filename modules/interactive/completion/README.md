---
domain: interactive-foundation
status: draft
last-reviewed: 2026-06-10
---

# Completion

## Ubiquitous Language

- **Completion framework**: Minibuffer and in-buffer completion behavior for command, symbol, file, and buffer selection.
- **Candidate source**: A backend that contributes completion candidates or metadata.
- **Preview**: Temporary display of the selected candidate's target or context.

## Invariants

- Completion belongs to the interactive foundation because it shapes how commands and destinations are selected across outcomes.
- Completion keybindings and previews should conform to the shared interaction model.
- Completion packages should not block ordinary batch load or unrelated startup behavior.
