---
domain: authoring.copilot
status: draft
last-reviewed: 2026-07-21
---

# Org Copilot

Org-native AI authoring assistance for review, chat, and executable edit suggestions.

Org Copilot keeps AI output visible as review artifacts. It does not silently mutate the source document: proposed edits become comments/suggestions first, and source text changes only when you accept a suggestion.

## What it stores

For `draft.org`, Copilot uses source-adjacent Org sidecars:

- `draft.copilot.org` — chat transcript/session data.
- `draft.comments.org` — anchored short review comments and suggestion links via `org-comments`.
- `draft.suggestions.org` — executable edit candidates via `org-suggestions`.

Pure Q&A answers stay in the Copilot transcript. Executable edits live in `org-suggestions`; comment rows are short descriptions/entry points, not patch payloads.

## Main commands

- `org-copilot-chat` — open the bottom chat for the current Org buffer.
- `org-copilot-chat-full-document` — chat about the whole document.
- `org-copilot-chat-section` — chat about the current subtree.
- `org-copilot-review-dwim` — review active region, else current subtree.
- `org-copilot-open-panels` — open side panel + chat.
- `org-copilot-view-diff-at-point` — preview focused suggestion.
- `org-copilot-view-suggestion-at-point` — reopen section suggestion preview.
- `org-copilot-accept-at-point` — accept focused suggestion.
- `org-copilot-dismiss-at-point` — dismiss focused legacy in-session comment.
- `org-copilot-clear-session` — archive current Copilot session artifacts; with prefix, clear UI only.
- `org-copilot-erase-session` — hard-delete current Copilot session artifacts after confirmation.

## Chat slash commands

In the chat prompt, type `/` on an empty prompt for context-aware completion.

- `/accept` — accept focused suggestion.
- `/dismiss` — dismiss focused comment.
- `/undo` — undo accepted legacy in-session suggestion when rollback data exists.
- `/next`, `/prev` — navigate focused comments/suggestions.
- `/doctor` — append a local health report.
- `/clear` — archive current session chat/comments/suggestions.
- `/clear-ui` — clear only the visible/ephemeral UI state.
- `/erase` — hard-delete current session chat/comments/suggestions after confirmation.

## Personal Evil bindings in this config

Package code is Evil-neutral. This repository’s personal Org bindings use `,a` for AI:

- `,aa` chat
- `,ar` review DWIM
- `,ao` open panels
- `,ag` full-document chat
- `,as` section chat
- `,an` / `,ap` next/previous
- `,aA` accept
- `,ad` diff preview
- `,av` suggestion preview panel
- `,au` undo
- `,ax` dismiss
- `,ac` clear/archive session
- `,aC` erase session

## Suggested workflow

1. Open an Org document.
2. Run `org-copilot-chat` or `org-copilot-review-dwim`.
3. Ask naturally: “rewrite this section”, “review this”, “make this clearer”.
4. Inspect side-panel rows and previews.
5. Accept only concrete executable suggestions you want.
6. Use `/clear` when done to archive the Copilot session, or `/clear-ui` to keep artifacts visible for later.

## Model response contract

Adapters should return structured JSON. The modern edit path uses `suggestion_threads`, not the legacy top-level `suggestion` field.

Minimal shape:

```json
{
  "intent": "answer|review|edit",
  "message": "Conversational answer shown in chat.",
  "suggestion_threads": [
    {
      "summary": "Short linked comment text.",
      "suggestions": [
        {
          "id": "ai-1",
          "hunks": [
            {
              "kind": "section-replace",
              "primary": true,
              "section_title": "Intro",
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

- `intent: answer` installs no comments or suggestions.
- `intent: review` may install comments.
- `intent: edit` may install suggestion threads/comments.
- `suggestion` means executable text only and is legacy compatibility, not the canonical path.
- Section suggestions replace the live section body while preserving the heading.

## Architecture

- `org-copilot-chat.el` — chat UI, context switching, slash commands.
- `org-copilot-session.el` — source-buffer session state and clear/erase orchestration.
- `org-copilot-sidecar.el` — `.copilot.org` transcript persistence.
- `org-copilot-llm.el` — adapter-neutral parsing/normalization.
- `org-copilot-gptel.el` — optional gptel/ChatGPT OAuth adapter.
- `org-copilot-context-panel.el` — side-panel provider and overlays.
- `org-copilot-diff.el` — preview/accept actions, delegating durable suggestions to `org-suggestions`.
- `org-copilot-suggestion.el` — legacy/preview section suggestion support.

See also:

- `SPEC.md`
- `persistent-suggestions.plan.md`
- `packages/org-comments/SPEC.md`
- `packages/org-suggestions/SPEC.md`
