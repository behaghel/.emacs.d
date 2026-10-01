# org-marginalia Spec

## Problem

Org footnote/sidenote marginalia currently lives in personal configuration under
`packages/org-marginalia/` and `modules/interactive/org/context-panel.el`.
This prevents the context-panel module from collapsing to personal activation and
keybindings only.

## Context

`org-context-panel` provides generic context-panel mechanics and already supports
multiple providers. `org-comments` is one provider. Marginalia should become a
sibling provider rather than personal module logic or an `org-comments` feature.

## Decisions

- Create standalone package code under `packages/org-marginalia/`.
- Use clean `org-marginalia-*` names; do not keep `hub/` aliases.
- Preserve existing `HUB_NOTE_*` property drawer semantics for now.
- Provide independent activation via `org-marginalia-context-panel-mode`.
- Integrate with `org-context-panel` through provider descriptors.
- Render marginalia rows in the shared side panel using source/viewport order.
- Provide jump behavior to the source footnote reference.
- Do not include comments, filters, compose, help, edit/delete/toggle actions, or
  personal Evil/Bépo bindings.

## Acceptance Criteria

- Native Org footnotes are collected with reference and definition positions.
- `HUB_NOTE_KIND` controls `sidenote` vs `footnote` kind.
- Layout preserves deterministic non-overlapping display rows.
- Provider items compose with `org-comments` in a shared `org-context-panel`.
- Marginalia rows render readable Org-fontified body text.
- Jumping from a marginalia row moves to its source reference.
- The former personal marginalia implementation is removed after callers migrate.
- `modules/interactive/org/context-panel.el` no longer owns marginalia provider
  rendering logic.

## Verification

- Add focused `packages/org-marginalia/test/*-test.el` tests.
- Keep `packages/org-comments/test/org-context-panel-test.el` composition tests.
- Run authoring Org tests and package suites.
