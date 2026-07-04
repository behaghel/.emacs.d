---
domain: authoring.copilot
status: draft
last-reviewed: 2026-07-02
---

# Org Copilot MVP

## Problem

Org authors need AI assistance that feels like native document review rather than opaque text generation. The assistant should comment on the work, propose concrete changes, let the author inspect suggested edits as diffs, and support a focused dialogue without mutating the source document unexpectedly.

## Context

`org-copilot` lives in `packages/org-copilot/` as a reusable Org authoring package. It depends on `org-context-panel` for source overlays, right-side context panels, and bottom views. It may optionally interoperate with `org-comments`, but AI review artifacts are not ordinary persisted human comments in the MVP.

Relevant neighboring packages:

- `packages/org-comments/`: durable Org-native human/remote comments stored in `.comments.org` sidecars.
- `packages/org-comments/org-context-panel.el`: reusable provider-based Org context panel mechanics.
- `modules/interactive/org/`: personal activation, keybindings, and layout policy.

Prior art that informs the design:

- `gptel`: Emacs-native LLM access and scriptable Org-compatible chats.
- `gptel-aibo`: AI writing/editing assistant with structured edit suggestions and diff-style acceptance.
- `code-review`: review workflow precedent for comments, replies, suggestions, and accept/reject actions.
- `org-remark`: Org-oriented annotation/highlight precedent.

## Product Definition

`org-copilot` is an Org-native AI authoring assistant. The LLM acts primarily as an AI reviewer. It creates AI comments, optional concrete edit suggestions, and participates in a source-buffer-scoped chat.

The core UX principle is:

> AI output appears as review artifacts first, not silent document mutations.

## Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Package location | `packages/org-copilot/` | Keeps reusable AI authoring behavior separate from durable comments and personal config. |
| Package namespace | `org-copilot-*` | Names the product as an authoring assistant rather than a comments storage feature. |
| Required dependency | `org-context-panel` | Reuses the existing Org panel, overlay, marker, provider, and bottom-view substrate. |
| Optional dependencies | `org-comments`, `gptel` | Comments integration and LLM backends should not be hard-wired into core state. |
| Primary AI role | Reviewer-first, with explicit rewrite/chat commands | Preserves author control while allowing useful generation flows. |
| AI artifact name | AI comments | Future persistence is possible; the name should not overfit to ephemeral notes. |
| MVP persistence | Ephemeral per-buffer session state | Avoids mixing AI suggestions into `.comments.org`; future persistence should use a dedicated AI sidecar. |
| Review scope | Active region, else current subtree; whole document explicit later | Keeps requests bounded, fast, and anchorable. |
| Diff UX | Dedicated read-only diff buffer | Simple, reliable prose diff visualization before source mutation. |
| Chat scope | One chat per source buffer, with full-document, section, and comment contexts | Keeps one session while making model context explicit. |
| Chat context markers | `🌐`, `§`, and comment-status emojis | Reinforces whether chat is about the full document, a section, or one AI comment. |
| Section suggestions | Accept-capable AI comments replacing section body only | Preserves Org structure while allowing concrete section rewrites. |
| Full-document suggestions | Left-side preview only | Avoids dangerous whole-buffer apply semantics before they are explicitly designed. |
| LLM integration | Adapter protocol, with `gptel` as the first real adapter | Keeps core testable and avoids a hard dependency. |
| Initial response format | Adapter-defined; strict JSON for structured `gptel` suggestions | Structured enough for comments/suggestions, but not a public durable format. |

## User-Facing Scope

### Review command

`org-copilot-review-dwim` reviews:

1. the active region, when present;
2. otherwise the current Org subtree.

Whole-document review is intentionally outside the MVP command set unless added as an explicit command later.

### AI comments

AI review output may include:

- anchored inline AI comments;
- concrete replacement suggestions attached to an AI comment;
- page/subtree-level AI comments for scope-wide feedback.

AI comments have these lifecycle states:

- `active`: visible and actionable;
- `accepted`: suggestion has been applied and remains visible until session clear;
- `dismissed`: removed from the current in-memory session;
- `stale`: source range is invalid or the current source text no longer matches the reviewed target text.

### Panel UX

AI comments appear in a right-side context panel using the same mechanics as other `org-context-panel` providers. Rows are compact and use markers:

- `💬`: active comment without suggestion;
- `✏️`: active inline suggestion;
- `§✏️`: active section suggestion;
- `✅`: accepted suggestion;
- `⚠️`: stale suggestion.

Panel actions for AI comments include:

- `d`: view inline diff;
- `v`: view/reopen section suggestion preview;
- `a`: accept suggestion;
- `x`: dismiss comment;
- `c`: open/focus chat about this comment;
- `G`: switch chat to full-document context.

### Diff UX

`org-copilot-view-diff-at-point` opens a dedicated read-only diff buffer for a suggestion. The diff buffer exposes accept and dismiss commands for the suggestion it displays.

`org-copilot-accept-at-point` may apply a suggestion directly from the panel only when the source range and reviewed target text are still valid. If validation fails, the AI comment becomes stale and the command refuses to modify the source.

### Bottom chat

`org-copilot-chat` opens a bottom chat view for the current source buffer session. Chat has explicit contexts:

- `🌐 Full document`: model sees the full source content;
- `§ Section: <heading>`: model sees the full source content plus the focused subtree as the requested focus;
- comment context: model sees the selected AI comment and suggestion.

The prompt is prefixed with the same context marker, for example `🌐 You:` or `§ You:`. Sending to an async adapter scrolls the pending/new Copilot response to the top of the chat viewport. `M-<up>` recalls the last submitted prompt in the current context.

Follow-up chat can modify the visible AI comment list only through explicit user intent or command flow, never automatically because the model decided to do so. Structured section suggestions are an exception only in the sense that they become review artifacts, not source mutations.

### Section and full-document suggestions

In section or full-document chat, a `gptel` response may include JSON:

```json
{
  "message": "brief explanation",
  "suggestion": "suggested text",
  "heading_line": "optional exact target heading line",
  "section_title": "optional exact target title",
  "section_path": ["optional", "outline path"]
}
```

If a section anchor is present, or if chat is already in section context, the suggestion becomes an accept-capable AI comment for that section body. The target section heading is preserved; the suggestion replaces only the body below that heading. Suggestions may include subsections or lower-level headings inside the replacement body.

Unanchored full-document suggestions open a read-only left-side preview and are preview-only; they do not create side-panel rows and cannot be accepted yet.

## MVP Command Set

Core commands:

- `org-copilot-review-dwim`
- `org-copilot-open`
- `org-copilot-open-panels`
- `org-copilot-chat`
- `org-copilot-chat-full-document`
- `org-copilot-chat-section`
- `org-copilot-view-diff-at-point`
- `org-copilot-view-suggestion-at-point`
- `org-copilot-accept-at-point`
- `org-copilot-dismiss-at-point`
- `org-copilot-clear-session`

Source/side-panel prefix bindings use the Org-friendly prefix `C-c C-x /`:

- `C-c C-x / o`: open side panel and chat;
- `C-c C-x / g`: full-document chat;
- `C-c C-x / s`: section chat;
- `C-c C-x / c`: comment/current chat;
- `C-c C-x / a`: accept focused suggestion;
- `C-c C-x / d`: dismiss focused comment;
- `C-c C-x / n`, `C-c C-x / p`: next/previous comment;
- `C-c C-x / u`: undo accepted suggestion.

Chat-local convenience bindings include:

- `RET` / `C-c C-c`: send prompt;
- `/`: context-aware slash command completion when prompt is empty;
- `M-a`, `M-d`, `M-n`, `M-p`, `M-u`: accept/dismiss/next/previous/undo;
- `M-g`: full-document context;
- `M-s`: section context;
- `M-<up>`: recall last prompt in the current context.

## Initial Architecture

Files:

```text
packages/org-copilot/
├── org-copilot.el
├── org-copilot-model.el
├── org-copilot-session.el
├── org-copilot-context-panel.el
├── org-copilot-diff.el
├── org-copilot-suggestion.el
├── org-copilot-chat.el
├── org-copilot-llm.el
├── org-copilot-gptel.el
└── test/
```

Responsibilities:

- `org-copilot-model.el`: AI comment/suggestion data validation and helpers.
- `org-copilot-session.el`: ephemeral per-source-buffer session state.
- `org-copilot-context-panel.el`: `org-context-panel` provider integration and row rendering.
- `org-copilot-diff.el`: diff buffer rendering and safe suggestion application.
- `org-copilot-suggestion.el`: left-side previews and section suggestion anchoring.
- `org-copilot-chat.el`: source-buffer chat UI and context switching.
- `org-copilot-llm.el`: adapter protocol and normalized review request/response API.
- `org-copilot-gptel.el`: optional `gptel` adapter.
- `org-copilot.el`: public entry point and commands.

## Data Model

The normalized AI comment model is plist-shaped in MVP.

Required or common keys:

- `:id`: stable session-local id;
- `:type`: `inline` or `scope`;
- `:status`: one of `active`, `accepted`, `dismissed`, or `stale`;
- `:source-start`, `:source-end`: source buffer bounds when anchored;
- `:target-text`: exact text reviewed by the model;
- `:line-start`, `:line-end`: approximate line range from the model or request context;
- `:body`: AI comment text;
- `:suggestion`: optional replacement text;
- `:rationale`: optional explanation;
- `:created-at`: session timestamp;
- `:metadata`: adapter-specific diagnostics or raw ids.

Section suggestions also commonly carry:

- `:heading-line`: exact heading line used for anchoring;
- `:section-title`: exact heading title;
- `:section-path`: outline path used to disambiguate duplicate titles.

Anchoring uses both approximate line/range and quoted target text. Emacs verifies the exact text before accepting a suggestion.

## Invariants

- Source Org buffers remain clean unless the user explicitly accepts a suggestion.
- MVP AI comments are not written to `.comments.org`.
- Future persisted AI comments should use a dedicated AI sidecar, not the human comments sidecar.
- Core `org-copilot` must not hard-depend on `gptel`.
- Core package code must not contain personal `hub/` symbols, Evil bindings, Bépo assumptions, secrets, or local account defaults.
- Suggestions must never be applied when their reviewed target text no longer matches the source.
- Model responses must be validated before becoming visible AI comments.

## Acceptance Criteria

- [ ] AC-1: Given an Org buffer with `org-copilot` enabled, when `org-copilot-review-dwim` receives a normalized review response, then AI comments appear in the right context panel as AI-labeled review rows.
- [ ] AC-2: Given an AI comment with a replacement suggestion, when the user invokes `org-copilot-view-diff-at-point`, then a read-only diff buffer shows the reviewed text and proposed replacement.
- [ ] AC-3: Given a valid active suggestion, when the user invokes `org-copilot-accept-at-point`, then the source range is replaced, the AI comment becomes `accepted`, and it remains visible until session clear.
- [ ] AC-4: Given a suggestion whose source range or reviewed text no longer matches, when the user tries to accept it, then the command refuses to change the source and marks the AI comment `stale`.
- [ ] AC-5: Given an active AI comment, when the user invokes `org-copilot-dismiss-at-point`, then the comment is removed from the current in-memory session.
- [ ] AC-6: Given a source buffer session, when the user invokes `org-copilot-chat`, then a bottom chat view opens for that source buffer.
- [ ] AC-7: Given a chat opened from an AI comment, when the bottom chat opens, then it is focused around that comment while remaining part of the source-buffer session.
- [ ] AC-8: Given `gptel` is unavailable, when core `org-copilot` files are loaded, then they load without requiring `gptel`; real LLM review commands fail with a clear adapter error until configured.
- [ ] AC-9: Given package files under `packages/org-copilot/`, when inspected, then no package code defines personal keybindings or depends on private configuration modules.

## Verification Plan

| Criterion | Method | Automated? |
|---|---|---|
| AC-1 | Context-panel provider ERT with fake normalized LLM response | Yes |
| AC-2 | Diff buffer ERT asserting read-only contents and action metadata | Yes |
| AC-3 | Safe apply ERT on temp Org buffer | Yes |
| AC-4 | Stale apply ERT on modified temp Org buffer | Yes |
| AC-5 | Session dismissal ERT | Yes |
| AC-6 | Bottom view ERT or smoke test | Yes |
| AC-7 | Chat focus metadata ERT | Yes |
| AC-8 | Batch load without `gptel` on `load-path` | Yes |
| AC-9 | Grep/checkdoc/manual inspection | Partly |

## Out of Scope for MVP

- Persisting AI comments to disk.
- Writing AI suggestions into `.comments.org`.
- Whole-document automatic review as the default path.
- Accept/apply for unanchored full-document suggestions.
- Inline ghost-text or in-place diff overlays.
- Ediff integration.
- Automatic chat-driven mutation of the AI comment list.
- Remote collaboration or publishing of AI comments.
- Personal activation policy, Evil/Bépo bindings, or leader-key integration.
