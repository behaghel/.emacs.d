---
domain: publishing
status: draft
last-reviewed: 2026-06-10
---

# Blog Publishing

## Ubiquitous Language

- **Publishing workflow**: Export or synchronization path from authored content to an external medium.
- **Blog post**: Authored content prepared for a static-site publishing destination.
- **Capture template**: A command surface for creating a publishable post skeleton.

## Invariants

- Blog workflows consume authored content; they should not own general authoring semantics.
- Blog-specific capture or export helpers should remain isolated from unrelated Org authoring behavior.
- Publishing destinations and local paths should be configurable or private when machine-specific.
