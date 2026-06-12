---
domain: development
status: draft
last-reviewed: 2026-06-10
---

# Version Control

## Ubiquitous Language

- **VCS command surface**: Leader bindings and commands for repository history, diffs, blame, grep, staging, and commits.
- **Repository context**: The current Git repository or file revision context used by VC, Magit, diff-hl, and related tools.
- **SSH integration**: Git transport behavior that depends on SSH agent or host configuration.

## Invariants

- Version-control modules belong to the development workbench but conform to the shared interactive keybinding model.
- Magit, diff highlighting, and SSH integrations should remain separate modules so startup behavior is auditable.
- VCS state and caches should not pollute the repository root.
- Missing optional integrations should not break ordinary editing or startup.
