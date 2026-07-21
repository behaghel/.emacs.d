# Spec: Persistent Copilot Suggestions Across Comments, Suggestions, and Chat

## Problem
Scoped Copilot suggestions can become stale immediately after an accepted edit because Copilot currently treats section rewrites like diff-backed comments and does not reliably feed accepted live edits back into follow-up model context. More broadly, Copilot duplicates comment/suggestion state in memory, making review artifacts fragile, non-durable, and harder to keep tidy after back-and-forth sessions.

## Context
This feature spans three packages:

- `org-comments`: durable anchored comments, the short review/commit-message surface.
- `org-suggestions`: new reusable executable-edit package, the structured patch-like suggestion surface.
- `org-copilot`: LLM conversation/session orchestration, model protocol, and provider integration.

The target architecture is DRY: anchoring belongs in comments/shared helpers, executable apply semantics belong in suggestions, and Copilot should orchestrate via public APIs instead of duplicating stores or mutation logic.

## Decisions
| Decision | Choice | Rationale |
|---|---|---|
| Durable artifacts | Copilot review comments persist to `SOURCE.comments.org`; executable edits persist to `SOURCE.suggestions.org`; transcript/events persist to `SOURCE.copilot.org` | Separates discussion, executable edits, and conversation. |
| Comments role | One short linked comment per Copilot-created suggestion thread | Comment acts like a commit message anchored to primary hunk, not the full conversational reply. |
| Suggestions role | Thread → candidate → hunks | Matches commit/patch analogy while supporting alternatives and revisions. |
| Copilot role | Provider/orchestrator only; no long-term in-memory comment store | Reduces duplicated UI/model code and improves continuity. |
| Section stale rule | Section replacement stales only when target section cannot resolve | Recent body edits are irrelevant because section replacement replaces current body. |
| Follow-up revisions | Same chat/session and same suggestion thread; new active candidate `ai-1.1` | Preserves conversation continuity and accepted history. |
| Model context | Live source + transcript + events + comments/suggestions summary | Prevents model from proposing against stale source. |
| Prompt complexity | Conservative default; complex suggestion threads only when explicitly warranted | Avoids over-leading the model into multi-thread/multi-hunk output. |
| Cleanup | Clear archives current session artifacts by default; prefix preserves artifacts; erase hard-deletes | Keeps sidecars tidy without accidental data loss. |
| DRY implementation | Use public package APIs and shared helpers | Avoids duplicate sidecar, anchor, lifecycle, and apply code. |

## Acceptance Criteria
- [ ] AC-1: Given a user asks Copilot for a section rewrite, when the model returns one valid suggestion thread, then the system persists: a chat assistant message in `SOURCE.copilot.org`, a short linked comment in `SOURCE.comments.org`, and one active candidate with a `section-replace` hunk in `SOURCE.suggestions.org`.
- [ ] AC-2: Given the user accepts a `section-replace` candidate after editing the section body, when accept runs, then the current live section body is replaced and no stale error occurs solely because body text changed.
- [ ] AC-3: Given the user accepts `ai-1` and then asks Copilot for a refinement in the same chat, when the next request is built, then it includes live source content, transcript history, and compact suggestion-thread/event context.
- [ ] AC-4: Given a refinement response for accepted `ai-1`, when installed, then it creates active candidate `ai-1.1` under the same suggestion thread, leaves `ai-1` accepted, and focuses the latest active candidate in chat/UI.
- [ ] AC-5: Given alternative candidates in the same alternative group, when one is accepted, then other active alternatives are marked `SUPERSEDED` and hidden from default actionable views.
- [ ] AC-6: Given accepted candidate `ai-1` is superseded by accepted revision `ai-1.1`, then `ai-1` remains historically `ACCEPTED` with `SUPERSEDED_BY: ai-1.1` metadata.
- [ ] AC-7: Given a source buffer is reopened, when Copilot and comment/suggestion sidecars exist, then comments/suggestions restore automatically without UI windows and chat transcript appears when the chat is opened.
- [ ] AC-8: Given pure Q&A chat, when the response is parsed, then no comments or suggestions are created, and deprecated top-level `suggestion` is dropped with a warning.
- [ ] AC-9: Given a review response with targeted comments but no executable edits, when installed, then durable `org-comments` records are created without `org-suggestions` records.
- [ ] AC-10: Given a response with a verbose conversational message and a short suggestion thread summary, when installed, then the verbose message stays in Copilot transcript and the summary becomes the linked comment body.
- [ ] AC-11: Given invalid hunks mixed with valid hunks, when installed, then valid hunks persist and invalid hunks are skipped with a factual chat footer.
- [ ] AC-12: Given the user runs default `org-copilot-clear-session`, when confirmed, then current-session Copilot entries and Copilot-created comments/suggestions are archived using native Org archive behavior.
- [ ] AC-13: Given the user runs `C-u org-copilot-clear-session`, when confirmed, then chat/session state is cleared while durable comments/suggestions remain unchanged.
- [ ] AC-14: Given the user runs `org-copilot-erase-session`, when confirmed, then current-session Copilot sidecar entries and Copilot-created comments/suggestions are hard-deleted.
- [ ] AC-15: Given package code is inspected, then Copilot does not duplicate sidecar parsing, anchor recovery, suggestion lifecycle, or source mutation logic already owned by comments/suggestions.

## Invariants
- `suggestion` means executable edit text only; advice/reference prose must not be installed as suggestions.
- Copilot `message` is conversational; linked comments use short summaries/fallbacks.
- Suggestion ids are unique per source suggestions sidecar and may be provider-controlled.
- A suggestion thread may contain multiple candidate alternatives/revisions; a candidate may contain multiple hunks.
- Candidate acceptance is all-or-nothing in MVP.
- Source mutations are performed by `org-suggestions`, not `org-copilot`.
- Comments and suggestions are separate durable sidecars linked by ids; neither duplicates the other's payload unnecessarily.
- Sidecar persistence is enabled by default when Copilot is enabled, but never stores secrets/tokens/raw prompts.

## Scope
**May modify:**
- `domains.yaml`
- `packages/org-suggestions/`
- `packages/org-comments/` public APIs/link rendering needed for suggestions
- `packages/org-copilot/` durable session/schema/orchestration
- relevant tests under those packages
- interactive module wiring for updated commands/keybindings

**Must not modify:**
- upstream `gptel`
- private setup/secrets
- publishing backends except to fix breakages from public comment API changes
- unrelated Org authoring workflows

## Verification Plan
| Criterion | Method | Automated? |
|---|---|---|
| AC-1, AC-10 | End-to-end fake Copilot response fixture writes three sidecars | Yes |
| AC-2 | Section accept-after-edit integration test | Yes |
| AC-3, AC-4 | Follow-up refinement request/install test | Yes |
| AC-5, AC-6 | Suggestion lifecycle unit/integration tests | Yes |
| AC-7 | Reload/restore fixture test | Yes |
| AC-8, AC-11 | Model parser validation tests | Yes |
| AC-9 | Review-comment persistence test | Yes |
| AC-12–AC-14 | Temp sidecar archive/erase command tests | Yes |
| AC-15 | Architecture lint or focused grep/test asserting ownership boundaries | Yes |

## Implementation Slices
1. `org-suggestions` package scaffold and sidecar round-trip for one `section-replace` candidate.
2. `org-comments` public link/API slice and compact linked-suggestion panel indicator.
3. Copilot new model schema installs durable comments/suggestions from fake responses.
4. Copilot sidecar/session persistence and restore.
5. Remove/replace Copilot in-memory comment store and route artifact UI through comments/suggestions.
6. Clear/archive/erase session semantics.

## References
- `packages/org-suggestions/SPEC.md`
- `packages/org-comments/SPEC.md`
- `packages/org-copilot/SPEC.md`
- `packages/org-copilot/persistent-suggestions.plan.md`
