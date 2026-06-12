---
domain: development
status: draft
last-reviewed: 2026-06-10
---

# Shell

## Ubiquitous Language

- **Interactive shell workflow**: Running project commands from inside Emacs with editor-integrated navigation and history.
- **Eshell state**: History, aliases, and prompt state managed under repository runtime/cache locations.
- **Visual command**: A command that should run in a terminal-like buffer rather than through ordinary Eshell output handling.

## Invariants

- Shell modules belong to the development workbench and should use project/environment context when available.
- Shell state must live under managed runtime paths, not the repository root.
- Shell keybindings should conform to the shared navigation and execution command surface.
- Eshell customizations should remain isolated from non-shell startup behavior.
