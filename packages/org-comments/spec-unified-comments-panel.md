# Spec: Unified Comments Panel for Copilot, Suggestions, and Confluence

## Problem
Org Copilot currently renders AI comments in a dedicated side panel that duplicates and diverges from the `org-comments`/Confluence comment experience. This wastes space, repeats comment text, uses ambiguous icons, and splits comment lifecycle/actions across provider-specific UI. Comments should have one compact, source-ordered panel regardless of whether they are local, Confluence-backed, or Copilot-created; executable suggestion actions should be owned by `org-suggestions`, not `org-copilot`.

## Context
`org-comments` owns durable comment sidecars, collection, rendering, navigation, and source highlighting. `org-confluence` integrates by producing/importing normal `org-comments` records. `org-copilot` already persists Copilot-created review comments as normal `org-comments` records with provider/session/suggestion metadata, but still has a separate `*Org Copilot*` side-panel renderer and Copilot-specific overlays. `org-suggestions` owns executable edit candidates and already links threads/candidates to comments through `ORG_COMMENTS_SUGGESTION_THREAD_ID` and `ORG_COMMENTS_SUGGESTION_IDS`.

## Decisions
| Decision | Choice | Rationale |
|---|---|---|
| Canonical comment UI | Unified `org-comments` panel | One source of truth for comment rendering, sorting, navigation, and highlighting. |
| Copilot side panel | Retire as a normal comments UI | Copilot comments are durable comments; a separate side list creates duplicated UX. |
| Provider identity | Provider icon on first line | Compact and immediately scannable without spelling provider names. |
| Provider icons | `🤖` Copilot, `☁️` Confluence/remote, `✍️` local | Distinct enough while fitting narrow panels. |
| Generic comment icon | Remove speech-bubble prefix | The panel context already says these are comments. |
| Semantic badges | Second line with timestamp | Keeps first line focused on provider/status/target and avoids crowding. |
| Suggestion badge | `✏️` means linked executable suggestion | Separates provider identity from edit semantics. |
| Card density | Fixed compact rows, one truncated body line, no autofill | The panel is an index; full context comes from source highlight and sidecar/chat/diff. |
| Sorting | Source order first | Reading comments in document order matters more than provider grouping. |
| Source highlight | Generic `org-comments`/context-panel highlight | Confluence/local/Copilot comments must focus source text identically. |
| Suggestion actions | Public `org-suggestions` comment-record API | `org-comments` should delegate linked-suggestion semantics instead of depending on Copilot. |
| Refine action | Deferred | Refinement is provider-specific and should later use a neutral extension point. |
| Help | `org-comments` owns `?` help | One contextual help surface can include extension-provided actions. |

## Card Format
A unified comment row uses this compact shape:

```text
🤖 [OPEN] “** Ad hoc automation…"
2026-08-06 20:22 ✏️
Expand execution plan
```

Rules:
- Line 1: provider icon, local status, quoted target preview.
- Line 2: author when useful, timestamp, semantic/sync badges.
- Line 3: one truncated body/summary line.
- Do not render inline action hints.
- Do not duplicate the body as both title and content.
- Do not render full replacement text or verbose Copilot chat responses in the card.

Provider and badge meanings:
- `🤖`: provider is `org-copilot`.
- `☁️`: remote/Confluence-linked comment.
- `✍️`: local comment without remote/provider identity.
- `✏️`: linked executable suggestion exists.
- `⚠️`: stale, missing, dangling, or otherwise blocked comment/suggestion state.
- `❓`: unconfirmed anchor.

## Acceptance Criteria
- [ ] AC-1: Given local, Confluence-linked, and Copilot-created comments in one source buffer, when the comments side panel opens, then all visible comments appear in one unified `org-comments` panel sorted by source position.
- [ ] AC-2: Given a Copilot-created comment with provider metadata, when rendered in the unified panel, then its first line starts with `🤖`, not a generic speech-bubble icon or provider text label.
- [ ] AC-3: Given a Confluence-linked comment, when rendered in the unified panel, then its first line starts with `☁️` and preserves existing remote author/timestamp metadata on the metadata line.
- [ ] AC-4: Given a local-only comment, when rendered in the unified panel, then its first line starts with `✍️` and does not use `✍️` to mean edited/unpushed state.
- [ ] AC-5: Given a comment linked to `ORG_COMMENTS_SUGGESTION_THREAD_ID` or `ORG_COMMENTS_SUGGESTION_IDS`, when rendered, then the metadata line includes `✏️` and no executable replacement text is shown in the comment card.
- [ ] AC-6: Given any comment body longer than one display line, when rendered in the unified panel, then the body is truncated to one compact line without autofill or expansion controls.
- [ ] AC-7: Given point moves onto an anchored inline comment row, when the panel focus changes, then the full resolved target range is highlighted in the source buffer.
- [ ] AC-8: Given point moves onto a scope comment row, when the panel focus changes, then the heading line is highlighted unless a more precise scope range exists.
- [ ] AC-9: Given point moves onto a stale or unanchored comment row, when the panel focus changes, then no source range is highlighted and the row displays a warning badge.
- [ ] AC-10: Given `org-copilot-open` or `org-copilot-open-panels` is invoked, when panels are shown, then the side panel is the unified `org-comments` panel and the bottom panel is Copilot chat; no normal `*Org Copilot*` side comment panel is created.
- [ ] AC-11: Given a selected comment row has suggestion link metadata, when suggestion actions are invoked from the unified panel, then `org-comments` delegates to public `org-suggestions` APIs using the selected comment record and source buffer.
- [ ] AC-12: Given a selected comment row has no suggestion link metadata, when suggestion actions are invoked, then the command fails with a clear non-Copilot-specific user error.
- [ ] AC-13: Given `?` is pressed in the unified comments panel, when a comment row is selected, then `org-comments` shows contextual help that includes base comment actions and includes `org-suggestions` actions only when the selected row links to a suggestion.
- [ ] AC-14: Given normal Copilot chat or diff buffers are used, when this unified panel work is complete, then bottom chat and transient diff/suggestion preview behavior still work without relying on the retired Copilot side comment panel.

## Invariants
- `org-comments` remains the owner of comment rendering, source-order navigation, source highlighting, and comment lifecycle display.
- `org-comments` must not implement suggestion accept/diff/undo semantics; it delegates linked suggestion actions to `org-suggestions` public APIs.
- `org-suggestions` must not depend on `org-copilot` for core suggestion actions.
- `org-copilot` may create comments and suggestions and may open chat/diff flows, but it must not own a parallel persistent comment panel.
- Copilot-created comments remain normal durable `org-comments` records with provider/session/link metadata.
- Pure Copilot chat answers remain only in the Copilot transcript and must not become comments.
- `refine` is out of initial scope and must not be introduced as a hard Copilot dependency from `org-suggestions`.

## Scope
**May modify:**
- `packages/org-comments/` panel rendering, focus/highlight, contextual help, and extension delegation APIs.
- `packages/org-suggestions/` public comment-record action APIs and tests.
- `packages/org-copilot/` panel-opening behavior and removal/retirement of duplicate side-panel comment rendering.
- Related tests under `packages/org-comments/test/`, `packages/org-suggestions/test/`, and `packages/org-copilot/test/`.

**Must not modify:**
- `org-confluence` publishing or remote API behavior except tests/adapters needed to confirm unified rendering metadata.
- gptel transport or Copilot model prompting.
- source mutation logic outside `org-suggestions`.
- private setup/secrets.

## Verification Plan
| Criterion | Method | Automated? |
|---|---|---|
| AC-1 | Panel render test with mixed local/remote/Copilot comments sorted by source position | Yes |
| AC-2–AC-5 | Row formatting unit tests for provider icons and semantic badges | Yes |
| AC-6 | Row formatting test with long body and fixed one-line truncation | Yes |
| AC-7–AC-9 | Context-panel focus tests asserting highlighted source ranges and stale badge behavior | Yes |
| AC-10 | Copilot open command test asserting unified side panel + bottom chat and no Copilot side comment buffer | Yes |
| AC-11–AC-12 | Delegation tests with stubbed `org-suggestions` comment APIs | Yes |
| AC-13 | Help rendering test with and without suggestion metadata | Yes |
| AC-14 | Existing Copilot chat/diff tests plus focused regression test around panel retirement | Yes |

## References
- `packages/org-comments/SPEC.md`
- `packages/org-suggestions/SPEC.md`
- `packages/org-copilot/SPEC.md`
- `packages/org-confluence/`
