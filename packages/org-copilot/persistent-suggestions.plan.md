---
domain: authoring.copilot
spec: packages/org-copilot/spec-persistent-suggestions.md
status: draft
---

# Plan: Persistent Copilot Suggestions

This plan implements the coordinated specs:

- `packages/org-copilot/spec-persistent-suggestions.md`
- `packages/org-suggestions/SPEC.md`
- `packages/org-comments/SPEC.md`
- `packages/org-copilot/SPEC.md`

The plan is intentionally DRY: each slice should add the smallest public API needed in the owning package, then have consumers call that API rather than duplicate storage, anchoring, lifecycle, or source mutation code.

## Package Milestones

| Package | Milestone |
|---|---|
| `org-suggestions` | New reusable package with sidecar, thread/candidate/hunk model, accept/stale/supersede, preview/undo APIs. |
| `org-comments` | Public extension APIs, provider/session metadata, suggestion-thread links, compact panel indicators, archive helpers. |
| `org-copilot` | New response protocol, durable transcript/events sidecar, orchestration through comments/suggestions APIs, no long-term in-memory comments. |

## Iteration 1: Standalone `org-suggestions` section replacement

- Slice goal: A user/tool can persist one suggestion thread with one active candidate and one `section-replace` hunk, reload it, preview it, and accept it against the current live section body.
- User interaction path: In an Org buffer, create a suggestion via package API, inspect `SOURCE.suggestions.org`, reload suggestions, run accept, and observe the current section body replaced.
- Tests to write first:
  - `org-suggestions-sidecar-writes-section-replace-thread`
  - `org-suggestions-sidecar-loads-thread-candidate-hunk`
  - `org-suggestions-accept-section-replace-uses-current-body`
  - `org-suggestions-preview-diff-uses-live-section-body`
- Expected red signal: missing package/files/functions; no sidecar path/write/load/apply behavior.
- Minimal green target: `org-suggestions.el`, model helpers, sidecar path/write/load for the documented shape, section resolver, candidate accept for single `section-replace`, live diff renderer.
- Feedback checkpoint: Show a temp `*.suggestions.org` and a source-buffer accept-after-edit smoke path.

## Iteration 2: Add `replace`, `insert`, all-or-nothing, stale, and lifecycle

- Slice goal: `org-suggestions` supports all MVP hunk kinds and candidate lifecycle rules.
- User interaction path: Create candidates with `replace`, `insert`, and multi-hunk operations; accept valid candidates; see stale behavior when one hunk cannot apply.
- Tests to write first:
  - `org-suggestions-accept-replace-requires-resolved-original`
  - `org-suggestions-accept-insert-uses-anchor-placement`
  - `org-suggestions-accept-multi-hunk-is-all-or-nothing`
  - `org-suggestions-alternative-accept-supersedes-siblings`
  - `org-suggestions-accepted-revision-records-superseded-by`
  - `org-suggestions-undo-accepted-session-local`
- Expected red signal: only section replacement exists; missing lifecycle transitions.
- Minimal green target: hunk-kind dispatch, anchor recovery reuse from `org-comments` where possible, atomic source mutation transaction, session-local rollback, status updates.
- Feedback checkpoint: Demonstrate one valid multi-hunk accept and one stale all-or-nothing refusal.

## Iteration 3: `org-comments` link APIs and panel indicator

- Slice goal: A suggestion thread can create/use one linked durable comment as its short anchored description, and the comments panel shows a compact suggestion indicator.
- User interaction path: Create a linked suggestion thread with summary; inspect `SOURCE.comments.org`; open context panel and see a normal comment row with active suggestion indicator.
- Tests to write first:
  - `org-comments-create-provider-comment-with-suggestion-link`
  - `org-comments-update-suggestion-link-properties`
  - `org-comments-public-resolve-anchor-for-extension`
  - `org-comments-context-panel-renders-suggestion-indicator`
  - `org-comments-archives-provider-session-comments`
- Expected red signal: missing public APIs/link rendering; extensions must currently know internals.
- Minimal green target: public create/update/fetch/resolve helpers, link metadata formatting/collection, panel indicator hook/delegation point, archive helper using native Org archive behavior.
- Feedback checkpoint: Show linked comment sidecar entry and panel row without embedding suggestion text in comments.

## Iteration 4: Copilot schema installs durable comments and suggestions

- Slice goal: A fake Copilot adapter response using `suggestion_threads` creates durable transcript message, linked comment, and suggestion thread/candidate/hunks.
- User interaction path: Send a Copilot chat request with fake response; inspect all three sidecars; open panel and suggestion preview.
- Tests to write first:
  - `org-copilot-parse-suggestion-threads-response`
  - `org-copilot-drops-deprecated-top-level-suggestion`
  - `org-copilot-installs-one-thread-comment-and-suggestion`
  - `org-copilot-message-vs-thread-summary-are-separated`
  - `org-copilot-skips-invalid-hunks-with-chat-warning`
  - `org-copilot-review-comments-persist-without-suggestions`
- Expected red signal: parser only understands legacy top-level suggestions / in-memory comments.
- Minimal green target: new parser normalization, conservative prompt text, installer that calls `org-comments` and `org-suggestions` public APIs, factual install receipt/warnings.
- Feedback checkpoint: Run a fake model response end-to-end; verify no Copilot-specific persistent comment duplicate is created.

## Iteration 5: Follow-up revision against live source

- Slice goal: After accepting a section candidate and editing the source, a follow-up Copilot refinement in the same chat creates `ai-1.1` under the same suggestion thread without immediate staleness.
- User interaction path: Ask for section rewrite, accept, edit section, ask “make it more practical,” preview/accept new candidate.
- Tests to write first:
  - `org-copilot-request-includes-live-source-after-accept`
  - `org-copilot-request-includes-compact-thread-event-history`
  - `org-copilot-refinement-creates-active-revision-candidate`
  - `org-copilot-accept-refinement-supersedes-accepted-parent-by-metadata`
- Expected red signal: no durable thread history / focus mapping from previous candidate to new candidate.
- Minimal green target: focus context references suggestion thread/candidate ids, compact history builder, accepted-event recording, revision id generation, same-thread install.
- Feedback checkpoint: Live smoke in an Org buffer matching the original stale bug.

## Iteration 6: Copilot sidecar persistence and restore

- Slice goal: Copilot transcript/events persist to `SOURCE.copilot.org` and restore when chat opens, without auto-opening UI windows.
- User interaction path: Have a Copilot exchange, kill/reopen buffer or reload session, run `org-copilot-chat`, and see transcript continuity.
- Tests to write first:
  - `org-copilot-sidecar-writes-session-messages-events`
  - `org-copilot-sidecar-restores-transcript-on-chat-open`
  - `org-copilot-sidecar-skips-unknown-newer-schema`
  - `org-copilot-sidecar-detects-mtime-conflict`
  - `org-copilot-sidecar-omits-raw-prompts-and-secrets`
- Expected red signal: chat messages are buffer/session-local only.
- Minimal green target: sidecar path/read/write, schema version checks, mtime guard, restore into chat render model, no raw prompt persistence.
- Feedback checkpoint: Inspect readable `*.copilot.org` and restored bottom chat.

## Iteration 7: Remove Copilot in-memory comment store from artifact UI

- Slice goal: Copilot-created comments/suggestions are sourced from `org-comments`/`org-suggestions`; Copilot keeps only chat/session state.
- User interaction path: Run review/chat flows; panel rows come from comments with suggestion indicators; accept/view actions delegate to suggestions.
- Tests to write first:
  - `org-copilot-does-not-store-persistent-ai-comments-in-session`
  - `org-copilot-panel-uses-linked-comment-suggestion-state`
  - `org-copilot-next-previous-navigate-durable_artifacts`
  - `org-copilot-accept-action-delegates-to-org-suggestions`
  - `org-copilot-load-check-without-gptel-hard-dependency`
- Expected red signal: commands still read/write Copilot comment plists.
- Minimal green target: compatibility shims only where needed, command delegation, removal/refactor of duplicate comment render/state paths.
- Feedback checkpoint: Compare behavior to current Copilot UI; verify fewer duplicated rows and no loss of targeted AI comments.

## Iteration 8: Clear, archive, and erase sessions

- Slice goal: Users can tidy a noisy Copilot session by archiving current-session artifacts or erase them explicitly.
- User interaction path: Run default `org-copilot-clear-session`, `C-u org-copilot-clear-session`, and `org-copilot-erase-session` in temp copies; inspect sidecars/archive behavior.
- Tests to write first:
  - `org-copilot-clear-session-archives-current-session-artifacts`
  - `org-copilot-clear-session-prefix-preserves-comments-suggestions`
  - `org-copilot-erase-session-hard-deletes-after-confirmation`
  - `org-copilot-clear-session-does-not-touch-other-session-artifacts`
- Expected red signal: clear currently only affects ephemeral state or lacks session-scoped durable cleanup.
- Minimal green target: session-id metadata queries, archive orchestration through package APIs/native Org archive, confirmation prompts, hard-delete helpers.
- Feedback checkpoint: Show tidy active sidecars and archived artifacts according to native Org config.

## Iteration 9: Hardening and migration cleanup

- Slice goal: The new architecture is stable, documented, and old legacy suggestion paths are removed or explicitly deprecated.
- User interaction path: Run full relevant test suite and one live smoke checklist: section rewrite → accept → edit → refine → accept → clear/archive.
- Tests/checks to run:
  - all `packages/org-suggestions/test/*-test.el`
  - all `packages/org-comments/test/*-test.el` touched by API changes
  - all `packages/org-copilot/test/*-test.el`
  - `devenv -q shell -- ./scripts/elisp-format ...`
  - `devenv -q shell -- ./scripts/elisp-checkdoc ...`
  - `devenv -q shell -- ./scripts/elisp-parse ...`
  - `devenv -q shell -- env HOME=$PWD emacs --batch -l init.el --eval '(message "Loaded")' --kill`
- Expected red signal: legacy assumptions in tests/docs or stale duplicated functions remain.
- Minimal green target: README quick-start updates, SPEC sync, remove dead code, retain only compatibility wrappers that have tests and removal notes.
- Feedback checkpoint: Ask for approval to commit/merge the architecture migration.

## Approval Checkpoint

Do not implement until this plan is approved. Start with Iteration 1 and follow red → green → refactor, with a human checkpoint after every iteration.
