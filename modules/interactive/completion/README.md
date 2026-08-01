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
- File-path entry should work structurally for ordinary file readers before individual commands are patched.

## File Path Completion

- File completion should prefer path-native styles, so directories, relative paths, home paths, and TRAMP prefixes remain predictable.
- Raw minibuffer prompts that do not expose a completion table may still offer file completion through `completion-at-point`.
- Commands that semantically require a file path should use Emacs file-reading APIs, such as `read-file-name`; patch individual entry points only when the structural layer cannot infer file intent.
