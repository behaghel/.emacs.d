# Spec: Consistent Context Panel and Comment Pipeline

## Problem
Org comment UX currently has multiple paths that look equivalent but do different work: whole-panel vs per-item rendering, compatibility `org-context-panel-*` vs native `context-panels-*`, heading-title author resolution vs panel-record author resolution, Copilot-owned side-panel remnants vs unified comments rows, and fallback UI paths. These parallel paths repeatedly produce inconsistent behavior.

## Goal
Make context/comment UI behavior boring and consistent by enforcing one canonical pipeline:

`raw provider collection -> item normalization/enrichment -> filtering -> viewport projection -> per-item rendering -> shared actions`

No backward compatibility is required. Legacy code and compatibility shims should be deleted rather than preserved.

## Non-goals
- Preserving old `org-context-panel-*` APIs for external callers.
- Preserving Copilot's retired persistent side comment panel.
- Runtime support for legacy sidecar formats except optional explicit one-shot migration tooling if still useful.
- Whole-panel body rendering for providers.

## Decisions
1. `context-panels` is the only side-panel rendering engine.
2. Providers render individual rows only via `:render-side-item`.
3. Generic side-panel body text is provider rows only: no headers, source labels, counters, empty text, or fallback prose.
4. Provider records/items are normalized before rendering and before action dispatch.
5. UI renderers must consume normalized records and not perform backend lookup or business logic.
6. `org-comments` owns comment action semantics; source, page, and panel UI paths call the same commands.
7. `org-suggestions` owns executable suggestion semantics.
8. `org-copilot` owns chat, prompt, diff, and transient suggestion preview surfaces only; not persistent comment row UI.
9. Compatibility shims, compatibility tests, and retired panel tests are deleted.

## Required contracts

### `context-panels` provider contract
A side provider must declare:
- `:name` symbol;
- `:icon` non-empty string;
- `:collect-side-items` function when it contributes side rows;
- `:render-side-item` function when it contributes side rows.

A provider must not declare `:render-side-panel`.

### `context-panels` item contract
Normalized side items must have:
- stable identity (`:id` or equivalent item key source);
- effective `:provider` from registration;
- effective `:icon` from item override or provider default;
- source anchor metadata when the row should align to source text;
- optional normalized action context.

### `org-comments` record contract
Comment records passed to panel rendering/actions must already include:
- provider icon (`:icon`);
- resolved display author fields when a resolver/cache is available;
- normalized remote/local state fields;
- source/sidecar location for actions;
- reply records normalized with the same author/display rules as roots.

## Acceptance criteria
1. No active code path calls `org-context-panel-*`; active code uses `context-panels-*` directly.
2. `packages/org-comments/org-context-panel.el` compatibility shim is removed, or reduced to non-loaded migration documentation if absolutely needed.
3. `context-panels` rejects providers that declare `:render-side-panel`.
4. No provider in this repository declares `:render-side-panel`.
5. Side-panel render tests assert body text contains rows only and no header/fallback text.
6. Comment author resolution is applied during comment record enrichment, not only heading generation.
7. Comment action commands use one current-context resolver across source/panel/page UI.
8. Retired Copilot persistent side-panel rendering functions/tests are removed.
9. Legacy sidecar runtime modules are deleted or isolated behind explicit migration commands; normal collection does not depend on legacy runtime requires.
10. Tests enforce provider contract, normalized item shape, unified action dispatch, and no whole-panel rendering.

## Verification
- `~/ws/context-panels` full ERT.
- Focused org-comments context/panel/action tests.
- Focused org-copilot context tests after deleting retired side-panel expectations.
- `devenv -q shell -- ./scripts/elisp-parse` on touched files.
- `devenv -q shell -- ./scripts/elisp-checkdoc` on touched files.
- Manual smoke: open an Org/Confluence document with comments via `,cc`; verify rows align, mode-line counters show, authors resolve, reply/push/open-remote work.
