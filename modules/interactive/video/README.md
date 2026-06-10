---
domain: personal-apps
status: draft
last-reviewed: 2026-06-10
---

# Video Workflows

## Ubiquitous Language

- **Video workflow**: Media-oriented command flow integrated into the editor.
- **Transcript workflow**: Commands and buffers for working with speech, captions, or extracted text from media.
- **Media artifact**: A local file or generated output used by video-oriented workflows.

## Invariants

- Video workflows are standalone personal apps and should not affect core editing or startup when unused.
- External media tools should be optional or clearly declared by the development/runtime environment.
- Generated media artifacts and runtime state must not pollute the repository root.
- Video commands should conform to the established interactive keybinding and window-management conventions.
