# Spec: Org Comments Suggestion-Link Extension

Status: implemented for provider/session/link metadata, public target resolution, panel indicators, provider-session archive, and provider-session delete helper.

## Problem
Org comments are the durable anchored review/discussion substrate. Copilot-generated and future tool-generated suggestions need short, discoverable, anchored comments that act like commit messages for linked executable suggestion threads, without moving edit semantics into `org-comments` itself.

## Context
`org-comments` already owns source-adjacent `SOURCE.comments.org` sidecars, anchor metadata, overlays, context-panel rendering, and comment collection. `org-suggestions` will depend on `org-comments` for linked comments and reusable anchoring APIs. This spec is intentionally an extension/API contract, not a full restatement of all existing comment behavior.

## Decisions
| Decision | Choice | Rationale |
|---|---|---|
| Comment role | Short anchored description for review/suggestion threads | Keeps Copilot conversation separate from durable comment/commit-message text. |
| Suggestion semantics | Not owned by `org-comments` | Section replacement and multi-hunk apply logic belong to `org-suggestions`. |
| Link properties | Minimal named suggestion links in comment records | Makes sidecars understandable and navigable without adding edit logic. |
| API boundary | `org-suggestions` uses public `org-comments` APIs only | Prevents direct coupling to comments sidecar internals. |
| Copilot comments | Model-produced targeted/scope review comments persist as normal comments with provider metadata | Eliminates long-term Copilot in-memory comment storage. |
| Archive behavior | Use native Org archive commands/configuration | Keeps sidecars uncluttered without custom archive storage. |
| Delete behavior | Provider/session hard-delete helper for explicit erase flows | Supports destructive session cleanup without giving consumers sidecar internals. |

## Required Comment Metadata
Copilot/tool-created comments may include:

- `ORG_COMMENTS_PROVIDER`: producer package, e.g. `org-copilot`.
- `ORG_COPILOT_SESSION_ID`: Copilot session that created the comment, when applicable.
- `ORG_COMMENTS_SUGGESTION_THREAD_ID`: linked `org-suggestions` thread id.
- `ORG_COMMENTS_SUGGESTION_IDS`: optional space-separated linked candidate ids.

These fields are links and provenance only. They do not grant `org-comments` authority over suggestion lifecycle or source mutation.

## Public API Contract
`org-comments` should expose public APIs sufficient for extension packages to:

- create an anchored comment with body, author/provider metadata, and arbitrary whitelisted extension properties;
- update extension link properties on an existing comment;
- fetch a comment by id;
- resolve a comment target using existing anchor recovery semantics;
- update comment body/status through public functions;
- archive or delete provider-created comments by session id through public commands or reusable helpers.

If existing functions already provide these capabilities, implementation should reuse them rather than add duplicates.

## Acceptance Criteria
- [x] AC-1: Given a provider-created comment request with suggestion-thread metadata, when stored, then `SOURCE.comments.org` contains one normal comment entry with provider/session/link properties and no embedded executable suggestion body.
- [x] AC-2: Given an extension package needs anchor recovery, when it calls the public API, then it can resolve comment anchors without reading private sidecar internals.
- [x] AC-3: Given a comment linked to an active suggestion candidate, when the context panel renders the comment, then it shows a compact suggestion indicator and delegates suggestion actions to `org-suggestions`.
- [x] AC-4: Given a pure Copilot review comment with no suggestion, when persisted, then it appears as a regular comment row and remains independent of `org-suggestions`.
- [x] AC-5: Given a Copilot session id, when clearing the session with artifact archival enabled, then matching Copilot-created comments can be archived using native Org archive behavior.
- [x] AC-6: Given a comment sidecar with unknown extension properties, when collected, then existing comment behavior remains stable and unknown properties do not break loading.

## Invariants
- `org-comments` must not apply source edits, accept suggestions, or implement section replacement.
- Comment TODO/status remains discussion lifecycle, separate from suggestion candidate lifecycle.
- A linked comment anchors the primary/first hunk of a suggestion thread by default; individual hunks keep their own anchors in `org-suggestions`.
- Pure chat answers are not comments.
- Avoid direct sidecar edits from consumers; add public APIs when necessary.
- Provider/session archive/delete helpers affect only matching provider and session metadata.

## Scope
**May modify:**
- `packages/org-comments/` APIs, sidecar formatting/parsing, and panel rendering needed for link indicators
- `packages/org-comments/test/`

**Must not modify:**
- suggestion apply logic
- Copilot model transport
- publishing backend behavior except where tests reveal existing comment API regressions

## Verification Plan
| Criterion | Method | Automated? |
|---|---|---|
| AC-1, AC-6 | Sidecar formatting/collection fixture tests | Yes |
| AC-2 | Public anchor API unit tests | Yes |
| AC-3 | Context-panel render test with linked suggestion stub | Yes |
| AC-4 | Pure provider comment persistence test | Yes |
| AC-5 | Archive helper/command test with temp sidecar | Yes |

## References
- `packages/org-suggestions/SPEC.md`
- `packages/org-copilot/SPEC.md`
- `packages/org-copilot/spec-persistent-suggestions.md`
