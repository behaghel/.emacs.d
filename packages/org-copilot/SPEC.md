# Spec: Org Copilot Durable Sessions and Suggestion Orchestration

Status: implemented through durable sidecars, restore, linked suggestions, clear/archive/erase, and slash-command UX. Remaining known gap: sidecar mtime conflict protection (AC-8).

## Problem
Org Copilot needs to behave like a durable authoring assistant rather than an ephemeral in-memory comment store. Chat continuity, model-generated review comments, and executable suggestions should survive reloads, remain visible through the standard comment/suggestion systems, and avoid stale conflicts after accepted section rewrites.

## Context
`org-copilot` owns LLM prompting, response parsing, ChatGPT OAuth/gptel integration, chat UI, and orchestration. Durable targeted/scope comments belong in `org-comments`. Executable edit proposals belong in `org-suggestions`. Copilot sidecars store conversation/session metadata and events, not source-edit payloads that are already owned by suggestions/comments.

## Decisions
| Decision | Choice | Rationale |
|---|---|---|
| In-memory comments | Remove long-term Copilot-managed comment store | Reduces duplicate code and lets durable comments/suggestions be source of truth. |
| Copilot sidecar | Source-adjacent Org file `SOURCE.copilot.org` | Human-readable durable transcript/events beside document. |
| Session model | Schema supports multiple sessions; MVP uses one current/default session | Avoids migration pain while limiting UI scope. |
| Transcript restore | Restore visually when chat opens; do not auto-open windows | Provides continuity without startup noise. |
| Source of truth for model context | Live source + restored transcript + Copilot events + current comments/suggestions | Source text remains canonical; transcript/events explain conversation history. |
| Raw prompts | Not persisted by default; debug traces are explicit opt-in | Balances debuggability and privacy. |
| Model metadata | Persist symbolic backend/model per assistant response/event, never credentials | Useful for debugging without leaking tokens. |
| Suggestion schema | `suggestion_threads` is canonical; top-level `suggestion` is legacy compatibility only | Clean path avoids ambiguity while old flows migrate out. |
| Prompt policy | Conservative: one thread/one suggestion by default; complex threads only when warranted | Avoids leading model into unnecessary complexity. |
| Clear session | Default archives current session Copilot entries and Copilot-created comments/suggestions; `C-u` preserves durable artifacts | Keeps sidecars tidy without hard deletion. |
| Erase session | Hard-delete Copilot sidecar data and Copilot-created comments/suggestions after confirmation | Explicit destructive cleanup command. |
| Chat session commands | `/clear`, `/clear-ui`, and `/erase` | Makes durable session lifecycle available where the user already works. |
| Section resolution performance | Targeted heading/path/title scans before broad data construction | Reduces post-response latency on large Org files. |

## Copilot Sidecar Shape
For `draft.org`, the default sidecar is `draft.copilot.org`.

```org
#+title: Copilot for draft.org
#+source: draft.org
#+org_copilot_schema_version: 1
#+org_copilot_current_session: session-1

* Session session-1
:PROPERTIES:
:ORG_COPILOT_SESSION_ID: session-1
:ORG_COPILOT_CREATED_AT: 2026-07-18T12:00:00+02:00
:ORG_COPILOT_UPDATED_AT: 2026-07-18T12:12:00+02:00
:END:

** Messages
*** USER msg-1
:PROPERTIES:
:ORG_COPILOT_MESSAGE_ID: msg-1
:ORG_COPILOT_CREATED_AT: 2026-07-18T12:01:00+02:00
:ORG_COPILOT_CONTEXT_ID: section:Intro
:END:
Make this section clearer.

*** ASSISTANT msg-2
:PROPERTIES:
:ORG_COPILOT_MESSAGE_ID: msg-2
:ORG_COPILOT_CREATED_AT: 2026-07-18T12:01:12+02:00
:ORG_COPILOT_MODEL: gpt-5.5
:ORG_COPILOT_BACKEND: chatgpt-oauth
:ORG_COPILOT_CONTEXT_ID: section:Intro
:ORG_COPILOT_COMMENT_IDS: cmt-1
:ORG_COPILOT_SUGGESTION_THREAD_IDS: ai-thread-1
:ORG_COPILOT_SUGGESTION_IDS: ai-1
:END:
I drafted a replacement for the section.

** Events
*** ACCEPTED ai-1
:PROPERTIES:
:ORG_COPILOT_EVENT_ID: evt-1
:ORG_COPILOT_CREATED_AT: 2026-07-18T12:03:00+02:00
:ORG_COPILOT_EVENT_TYPE: accepted
:ORG_COPILOT_SUGGESTION_ID: ai-1
:END:
Accepted suggestion ai-1 into live source.
```

Message bodies are plain Org content. Structured edit payloads live in `SOURCE.suggestions.org`; anchored short comments live in `SOURCE.comments.org`.

## Model Response Protocol
The model response is JSON with conservative structure:

```json
{
  "intent": "answer|review|edit",
  "message": "Conversational answer shown in chat.",
  "suggestion_threads": [
    {
      "intent": "rewrite_section|revise_suggestion|insert_text|mixed_edit",
      "summary": "Short comment/commit-message style description.",
      "suggestions": [
        {
          "id": "optional-provider-id",
          "label": "optional candidate label",
          "hunks": [
            {
              "kind": "section-replace",
              "primary": true,
              "section_title": "Intro",
              "section_path": ["Doc", "Intro"],
              "replacement": "Replacement body only."
            }
          ]
        }
      ]
    }
  ],
  "comments": []
}
```

Rules:
- Top-level `intent` gates artifact installation: `answer` installs no comments/suggestions; `review` may install comments; `edit` may install suggestion threads/comments.
- Top-level `suggestion` is invalid and must be dropped with a chat warning.
- `message` is conversational and must not be used as the linked suggestion comment body.
- `summary` is the short linked comment body; if absent, Copilot creates a neutral fallback.
- Parser accepts multiple threads/hunks, but prompt discourages complexity unless explicitly warranted.
- Invalid hunks are skipped with factual warning; valid hunks in the same response may still install.

## Acceptance Criteria
- [x] AC-1: Given a Copilot chat response with `intent: answer`, when parsed, then no comments or suggestions are installed even if deprecated top-level `suggestion` appears; chat shows a warning for the invalid field.
- [x] AC-2: Given a chat response with one valid `suggestion_threads` entry, when installed, then Copilot writes one assistant message to `SOURCE.copilot.org`, one linked short comment to `SOURCE.comments.org`, and one suggestion thread/candidate/hunk to `SOURCE.suggestions.org`.
- [x] AC-3: Given a chat response with a verbose `message` and short `summary`, when installed, then the verbose text appears only in Copilot transcript and the comment body uses the short summary.
- [ ] AC-4: Given a model response with mixed valid and invalid hunks, when installed, then valid hunks install and skipped hunks are reported factually in chat.
- [x] AC-5: Given an accepted section suggestion and live edits afterward, when the user asks for a refinement in the same chat, then the request includes live source and compact thread/event history, and the new response creates a new active candidate under the same suggestion thread.
- [ ] AC-6: Given an accepted candidate `ai-1`, when a refinement candidate `ai-1.1` is accepted, then Copilot records an accepted event and `org-suggestions` records supersession metadata without relying on stale old section text.
- [x] AC-7: Given a source file with existing `SOURCE.copilot.org`, when `org-copilot-chat` opens, then the restored transcript renders in the bottom chat without auto-opening chat during mode enable.
- [ ] AC-8: Given sidecar mtime changed externally during a live session, when Copilot would write, then it warns and refuses overwrite until reload/force-save.
- [x] AC-9: Given `org-copilot-clear-session` without prefix, when confirmed, then current session Copilot entries and Copilot-created comments/suggestions for that session are archived using native Org archive behavior.
- [x] AC-10: Given `C-u org-copilot-clear-session`, when confirmed, then Copilot chat/session state is cleared and durable comments/suggestions are preserved with metadata unchanged.
- [x] AC-11: Given `org-copilot-erase-session`, when confirmed, then Copilot sidecar entries and Copilot-created comments/suggestions for the session are hard-deleted.
- [x] AC-12: Given core `org-copilot` files load without gptel installed, when loaded in batch, then they still do not hard-require gptel.

Implementation notes:
- `/clear` archives current-session artifacts; `/clear-ui` passes prefix-preserve semantics; `/erase` hard-deletes after confirmation.
- Durable linked suggestion accept delegates source mutation and lifecycle persistence to `org-suggestions`.
- Legacy top-level `suggestion` remains compatibility code in some paths, but `suggestion_threads` is canonical.

## Invariants
- Copilot must not persist credentials, OAuth tokens, request headers, or raw prompts in normal sidecars.
- Live source buffer content is always canonical for model context; transcript and events are explanatory context.
- Copilot-created targeted/scope review comments are durable `org-comments` records, not a parallel persistent Copilot comment store.
- Copilot-created executable edits are durable `org-suggestions` records, not embedded in comments or Copilot transcript.
- Pure chat answers remain only in the Copilot transcript.
- Clear/erase commands operate on the current session id and do not affect artifacts from other sessions.
- DRY rule: Copilot wraps `org-comments`/`org-suggestions` APIs and must not duplicate anchoring, sidecar, lifecycle, or source mutation logic.

## Scope
**May modify:**
- `packages/org-copilot/`
- Copilot-facing modules under `modules/interactive/org/` and `modules/interactive/tools/`
- tests under `packages/org-copilot/test/`

**Must not modify:**
- generic comments/suggestions internals except through their planned integration slices
- gptel upstream package code
- private setup/secrets

## Verification Plan
| Criterion | Method | Automated? |
|---|---|---|
| AC-1–AC-6 | Parser/install/orchestration ERT tests with fake adapter | Yes |
| AC-7 | Sidecar restore + chat render ERT test | Yes |
| AC-8 | Temp-file mtime conflict test | Yes |
| AC-9–AC-11 | Clear/archive/erase temp sidecar tests | Yes |
| AC-12 | Batch load test without gptel hard dependency | Yes |

## References
- `packages/org-copilot/spec-persistent-suggestions.md`
- `packages/org-suggestions/SPEC.md`
- `packages/org-comments/SPEC.md`
