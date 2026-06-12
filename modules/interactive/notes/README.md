---
domain: authoring
status: draft
last-reviewed: 2026-06-10
---

# Notes

## Ubiquitous Language

- **Note workflow**: Capturing, finding, and maintaining personal knowledge from inside Emacs.
- **Brain**: The local note-taking workflow that integrates Denote-style note storage with Org authoring behavior.
- **Capture target**: A file or command destination used to create a new note or inbox item.

## Invariants

- Note workflows are part of authoring because they create and organize knowledge artifacts.
- Note modules should preserve established Org editing behavior and keybindings.
- Note storage locations should be configurable and must not hard-code sensitive local paths.
- Optional note-taking packages should not break ordinary startup when unavailable.
