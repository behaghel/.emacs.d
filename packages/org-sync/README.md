---
domain: publishing.org-sync
status: draft
last-reviewed: 2026-07-05
---

# Spec: Org Sync Shared Kernel

## Problem

Confluence and Google Docs both synchronize Org buffers with remote document providers. They need the same Org-level asset semantics: standalone image detection, local path resolution, stable generated filenames, imported/missing-source handling, and preflight diagnostics. Provider packages should not reimplement this logic differently.

## Decisions

- Keep provider-neutral Org synchronization helpers in `packages/org-sync/`.
- Keep provider API behavior outside this package: Confluence attachment upload and Google Drive upload/download remain in provider packages.
- Preserve existing Confluence-compatible generated image filename semantics so migration can be incremental.
- Prefer data-returning planners and small diagnostics over provider-specific side effects.

## Initial Scope

- Standalone Org image link detection.
- Caption extraction from standalone image paragraphs.
- Local image path resolution relative to the source Org buffer.
- Stable generated image filenames.
- Missing imported/generated asset detection for providers that can reuse a remote asset without a local source.
- Buffer image asset planning.

## Acceptance Criteria

- [x] Given an undescribed standalone local image link, when image assets are planned, then org-sync returns source link, local source path, generated filename, and caption when present.
- [x] Given a described image link, when image assets are planned, then it is ignored as a normal link.
- [x] Given a missing non-imported local image, when strict path resolution is requested, then org-sync reports a clear error.
- [x] Given a generated/imported image filename with no local source, when image assets are planned with imported reuse enabled, then the asset is marked `:missing-source` instead of failing.
- [ ] Given Google Docs image preflight runs, when it detects standalone images, then it uses org-sync asset planning for Org-level detection/path/caption behavior.
- [x] Given Confluence image asset behavior is migrated, when existing Confluence tests run, then generated filenames and missing-source semantics remain unchanged.

## Verification

- Package-local ERT tests in `packages/org-sync/test/` cover the shared behavior.
- Provider migrations keep their existing provider-specific test suites green.
