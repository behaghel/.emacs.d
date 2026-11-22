---
domain: authoring/copilot
status: draft
---

# Org Copilot

Org-native AI authoring assistance for review, chat, and executable edit suggestions.

Source setup and the offline workflow are documented in the [package manual](README.org).

Org Copilot keeps AI output visible as review artifacts. It does not silently mutate the source document: proposed edits become comments/suggestions first, and source text changes only when you accept a suggestion.

## Activation

Org Copilot is opt-in per source buffer. Opening an Org file, switching windows,
or using another AI assistant does not enable Copilot, restore its artifacts, or
configure its adapters. Explicitly enabling `org-copilot-mode` or opening Copilot
chat/panels activates the integration. Disabling the mode removes its provider and
source overlays. Window tracking belongs to context-panels, not a global Copilot
selection hook. Opening or refreshing panels for an already enabled source does
not rerun activation or replace its explicitly selected adapter.

## Package boundary

The facade declares Context Panels, org-comments, and org-suggestions as package
dependencies. Its source-only workflow runs without personal modules, gptel,
authentication, or a model service. Concrete LLM adapters remain opt-in;
unconfigured review reports its missing adapter rather than choosing a provider.

Reusable chat mode does not select an Evil state. Personal adapter activation,
authentication, model choices, and modal editing policy belong to consumer
configuration. Package-local tests do not load that configuration.

Offline scripted replies use the real chat, parsing, comment, suggestion, and
sidecar paths. Linked suggestion previews delegate to org-suggestions and show
the actual source edits, not merely a comment anchor or primary-hunk summary.
Proposals preserve source text until explicit acceptance and do not claim
live-provider conformance. Authored sidecar formats and identifiers remain
stable.

Section proposals use the [Org Suggestions source-context contract](../org-suggestions/README.md#source-context-and-reconciliation).
A chat request's immutable `:source-content` is the accessible Org text supplied
to the producer; document and section chat do not silently widen that text.
The request separately captures local `:suggestion-source-context` evidence with
`org-suggestions-capture-source-context`. Installation passes that evidence to
`org-suggestions-candidate-with-source-context`, preserving full outline identity
and canonical baselines while honoring request-time narrowing. The widened local
snapshot is not sent in the prompt. Installation never substitutes callback-time
source or a model-supplied baseline. Org
Suggestions owns conflict comparison, deliberate reconciliation and revalidation
at acceptance. Adapters need no second revision policy. Unchanged targets remain
acceptable after unrelated edits; missing request evidence requires reconciliation.

Prepared chat contributions enter through `org-copilot-complete-chat-request`,
independently of the generation transport or prose parsing. Completion and
artifact installation retain the captured request's source/session fence.
Re-delivering an existing candidate identity preserves its status and acceptance
history rather than appending or reviving it; incompatible content under that
identity is refused. The editor's existing sidecars remain the projection store.

Prepared anchored comments can retain their producer's durable receipt identity without creating an executable suggestion. Prose-parsed model IDs do not acquire this identity. Redelivery preserves existing comment content and human resolution; incompatible content under that identity is refused. Retained anchors must still match uniquely and exactly in live text rather than falling back to guessed positions after an intervening edit.

The Review action captures the selected region or subtree (the whole accessible document before the first heading), its source witness and moving bounds. An adapter can submit that captured request through the ordinary chat submission boundary, retaining its existing request fence, native conversation and readable turn. Review does not reset editing focus or prescribe findings, research or rewriting. Anchored comments remain within the captured review span; creating a comment grants no source-edit authority.

## Conversation and editing focus

A conversation is independent of its editing focus. Within the chosen
conversation, selecting a section or comment changes the text and target for the
current request, not the conversation or its history. Documents remember the
conversation last selected for them; several documents can refer to the same
conversation. Starting another conversation is explicit, not a consequence of
focus navigation.

The selected document, section or comment discussion determines both the visible exchanges and where input goes. The document discussion contains only document-level exchanges. A comment's retained text opens its discussion, followed by the recorded replies. It is not a fabricated assistant turn. A submitted exchange establishes a document or section discussion; a contributed comment establishes its discussion immediately. Opening an empty scope or typing a draft does not add it to the ring. `/next` and `/prev` cycle through established discussions in source order, starting with the document when established and excluding dismissed comments. A section without a uniquely matching location remains accessible under its recorded scope. A Review reply follows the captured reviewed scope: an individual section, or the document for whole-document or cross-section Review. These views do not fork native history: model requests retain the whole chosen conversation and document context. Transient pending markers are UI
state, not authored conversation turns. Earlier focus metadata is retained when
formatting model history so an earlier reference to “this section” is not
silently attributed to the newly selected section.

Each request keeps its captured text, focus and proposal evidence. Moving focus
neither cancels that request nor retargets its reply or proposals. Explicit
conversation/session reset and source lifetime protections remain independent
of navigation. A model reply does not silently replace the current editing focus.

## What it stores

For `draft.org`, Copilot uses source-adjacent Org sidecars:

- `draft.copilot.org` — chat transcript/session data.
- `draft.comments.org` — anchored short review comments and suggestion links via `org-comments`.
- `draft.suggestions.org` — executable edit candidates via `org-suggestions`.

Pure Q&A answers stay in the Copilot transcript. Executable edits live in `org-suggestions`; comment rows are short descriptions/entry points, not patch payloads.

Copilot side cards use a configurable compact body preview, with a right arrow (`→`) available directly on each row header. Full findings are already present at the response that produced them, including after reopening; subsequent turns remain below them. The arrow opens that comment as a discussion thread in place of the document transcript and focuses the ordinary typing area. There is no separate reply link or intermediate Details panel. The input targets the discussion visible directly above it, with the document draft retained separately. The comment stays visible above its replies; the source and sidebar remain available. A finding remains an attached contribution, not another authored assistant turn or a new model request.

The same arrow behavior applies to older comments without a recorded producing response: their retained rationale opens the thread without guessing historical provenance or fabricating a past assistant turn. Suggestion diffs retain their existing preview path. Neither adapter request history nor native conversation identity changes. The preview limit changes only sidebar presentation, not stored rationale or source text. Conversation messages retain oldest-first order and show their recorded local time before the speaker, such as `[09:11] You:` or `[09:12] Copilot:`. Unrecorded or invalid times are visibly unknown, not invented from the current clock. Missing or dismissed comments fail visibly rather than opening another comment. Opening an explanation preserves the conversation and unsent input.

Chat content renders Org emphasis and descriptive links without running Org mode hooks in the panel or formatting buffers. Links use Org's normal explicit opening and confirmation rules in the originating source context. New authoring responses use concise Org content: the card identifies the useful change, the bottom discussion carries necessary evidence and qualifications, and source references link the relevant prose directly (for example, a linked “committee minutes”), rather than appending a Sources section. Descriptive links replace repeated elaboration where appropriate. Rendering does not rewrite old native history or heuristically convert Markdown into Org.

## Transcript persistence

Copilot transcript append, archive and erase use the same visiting-buffer owner.
A successful write updates both its contents and visited-file metadata, so a
new session can persist after archive even while its transcript remains open.
Persistence preserves the user's point and narrowing and saves complete Org
contents without bypassing native file locking or supersession checks.

Unsaved transcript edits or an external file change cause persistence to refuse
before mutation. The user must save or reconcile/reload that transcript before
retrying. Copilot neither saves unrequested human edits nor automatically reverts
or kills the visiting buffer. Conversation schemas and native Org archives are
unchanged. This contract belongs to the Copilot transcript; comments and
suggestions retain their own persistence contracts.

## Request and session lifetime

Every chat or review submission belongs to one source buffer, source file and
session instance. Its pending state, answer, error and deferred installation
belong to that exact request, not merely the most recent message in its context.
Overlapping requests can complete independently and out of order.

Clear (including UI-only clear), confirmed erase, transcript replacement, source
retargeting, source closure and request cancellation invalidate obsolete work.
Late responses and errors cannot recreate conversation, comments or suggestions,
consume another request's pending state, or append into a later session. Existing
sidecar archive/preserve/delete semantics and restoration remain unchanged.
Navigation between document, section and comment contexts does not cancel work
or redirect its captured response. Review completion does not change a newly
selected chat context.

Copilot owns this lifecycle independently of provider termination. Adapters use
supported cancellation when available, but a cancellation attempt is never proof
that late callbacks have stopped. Request/session identities are ephemeral and
do not change the durable `default` session identifier or sidecar schemas.

## Contributor attribution

New AI comment and proposal cards persist the originating request's host-recorded
`:contributor` in the existing comment-author field. The gptel transport captures
its effective backend's display name when sending; changing a backend before a
reply arrives or before reopening a document does not change stored authorship.
Model-supplied author fields do not establish this attribution. Adapters can
supply their own known contributor, such as `Herbert`, without changing models,
accounts, request fencing or the existing history owner.

Absent contributor metadata uses the generic `Copilot` label, never an inference
from the current adapter. Existing human comment authors are not rewritten when
a proposal is linked to their thread. Cards use the shared author-first comment
layout; attribution labels do not confer authentication or effect authority.

## Main commands

- `org-copilot-chat` — open the bottom chat for the current Org buffer.
- `org-copilot-chat-full-document` — chat about the whole document.
- `org-copilot-chat-section` — chat about the current subtree.
- `org-copilot-review-dwim` — review active region, else current subtree.
- `org-copilot-open-panels` — open side panel + chat.
- `org-copilot-view-diff-at-point` — preview focused suggestion.
- `org-copilot-visualize-at-point` — DWIM visualisation for focused comments and linked durable suggestions.
- `org-copilot-accept-at-point` — accept focused durable suggestion.
- `org-copilot-dismiss-at-point` — resolve focused durable comment and dismiss linked suggestions.
- `org-copilot-cancel-requests` — cancel current source requests from source or chat without deleting conversation or artifacts.
- `org-copilot-clear-session` — archive current Copilot session artifacts; with prefix, clear UI only.
- `org-copilot-erase-session` — hard-delete current Copilot session artifacts after confirmation.

## Chat slash commands

In the chat prompt, type `/` on an empty prompt for context-aware completion.

- `/accept` — accept focused suggestion.
- `/dismiss` — dismiss focused comment.
- `/undo` — reserved for durable suggestion undo flows; legacy comment-local undo is retired.
- `/next`, `/prev` — cycle through established document, section, and comment discussions.
- `/doctor` — append a local health report.
- `/clear` — archive current session chat/comments/suggestions.
- `/clear-ui` — clear only the visible/ephemeral UI state.
- `/erase` — hard-delete current session chat/comments/suggestions after confirmation.

## Copilot Babel blocks

`ob-copilot` adds a `copilot` Babel language for reviewable AI-maintained results:

```org
#+begin_src copilot :results value raw replace :exports results :eval never-export
Write an executive summary of this document.
#+end_src
```

The gptel generator requires an explicitly configured effective backend and
model before making a request. It does not delegate missing configuration to
gptel's default-provider selection. Authentication route and model selection
remain consumer-owned; offline/custom generators do not require gptel.

Evaluation stores raw Org in `#+RESULTS:` with hidden freshness metadata and an explicit result boundary. Freshness hashes the effective document or subtree context, excluding the current block and its complete generated result, including multiple paragraphs. Regeneration preserves surrounding human text. Missing result boundaries refuse inspection/replacement rather than guessing an extent; older unbounded results keep their conservative paragraph extent and old fingerprints become stale. Export preflight does not re-evaluate fresh blocks; missing/stale blocks ask before generation and ask again before exporting unreviewed content.

For generated diagrams/code, use `:output src:LANG` and optional `:file`:

```org
#+begin_src copilot :output src:dot :file flow.svg :eval never-export
Maintain a Graphviz flow diagram for this section.
#+end_src
```

The model returns only inner `LANG` code; Emacs wraps the generated source block. This config enables the Babel language, enables built-in Graphviz DOT Babel support, and adds Org Tempo shortcut `<cp TAB`.

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
- `,aD` debug trace buffer
- `,av` suggestion preview panel
- `,au` undo
- `,ax` dismiss
- `,ac` clear/archive session
- `,aC` erase session

## Window behavior

The chat transcript scrolls independently above a fixed typing area. The typing
area has a subtly lighter theme-derived background, a top separator and a
coloured `You: ` label. Replies and viewport changes do not replace an unsent
draft. Each source owns its typing buffers, so switching documents does not move
a draft to a different target; hiding and reopening its views retain the same
buffer. Each document, section and comment discussion retains an independent typing draft. Switching discussions restores the corresponding draft rather than retargeting unsent text. Draft text is not conversation history and is submitted only by an explicit send action. A synchronous send failure leaves the draft intact.

`org-copilot-chat-goto-prompt` focuses the visible typing area. The reusable
package does not select an Evil state or redefine personal window-navigation
keys. Input remains an owned part of the existing bottom view: closing the view
also closes its input window, without killing the draft buffer.

Copilot panels are source-owned context panels. If the source Org window is
replaced by a non-Org buffer, the side panel and bottom chat hide instead of
staying attached to a stale file. If an eligible Org file becomes visible again,
the visible/desired side panel and bottom chat restore and retarget to that
source. Explicitly closing the panels disables automatic restoration until you
open them again.

Delivering a proposal refreshes source highlights and the comments panel without
requiring focus/navigation first, respecting the panel-opening preference and
viewport filtering. Killing a source closes its owned side, chat, and diff views
even when its sidecars remain visible. Sidecar buffers/files and views retargeted
to another source are left intact. Teardown is source-local, not a global
selection hook.

Transient suggestion/diff previews are not auto-restored; reopen them from the
focused artifact when needed. A diff preview keeps the chat and its fixed
controls visible and usable. Splitting or closing ordinary source/preview
windows preserves the native panel layout.

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
- Top-level `suggestion` and comment-local `:suggestion` are invalid for executable edits and are ignored/fail closed.
- Section rewrites must be encoded as `suggestion_threads` hunks.

## Architecture

- `org-copilot-chat.el` — chat UI, context switching, slash commands.
- `org-copilot-session.el` — source-buffer chat state, request/session lifetime and cancellation, durable comment/suggestion adapters, and clear/erase orchestration.
- `org-copilot-sidecar.el` — `.copilot.org` transcript persistence.
- `org-copilot-llm.el` — adapter-neutral parsing/normalization.
- `org-copilot-gptel.el` — optional gptel/ChatGPT OAuth adapter.
- `org-copilot-context-panel.el` — side-panel provider and overlays.
- `org-copilot-diff.el` — preview/accept actions, delegating durable suggestions to `org-suggestions`.

See also:

- `SPEC.md`
- `persistent-suggestions.plan.md`
- `packages/org-comments/SPEC.md`
- `packages/org-suggestions/SPEC.md`
