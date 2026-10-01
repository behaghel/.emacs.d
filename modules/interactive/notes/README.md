---
domain: authoring/notes
status: draft
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
- Personal, work, and blog note commands always use their configured destinations, independent of directory-local settings in the invoking buffer.
- `,nn` creates a personal note, `,no` opens or creates a personal note, `,nw` opens or creates a work note, `,nW` creates a work note, and `,nj` creates a blog journal note.
- Optional note-taking packages should not break ordinary startup when unavailable.
