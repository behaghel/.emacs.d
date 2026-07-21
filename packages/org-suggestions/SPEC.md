# Spec: Org Suggestions

Status: MVP implemented for sidecar round-trip, candidate accept, live diff, stale detection, alternatives, session-local undo, provider-session archive/delete, and Copilot integration. Remaining known gap: explicit unknown-newer-schema fail-closed behavior (AC-12).

## Problem
Org authors need executable edit proposals that are more precise than comments and more semantic than raw diffs. Suggestions may be produced by AI, humans, or future tools, and must support review, preview, acceptance, staleness detection, revision, and durable storage without forcing edit semantics into the generic comments system.

## Context
`org-suggestions` is a reusable package under `packages/org-suggestions/`. It is a companion extension to `org-comments`: comments provide anchored review/discussion records, while suggestions provide executable edit artifacts. `org-copilot` is one provider of suggestions, but suggestion identity, storage, lifecycle, and apply semantics are provider-neutral. The implementation should reuse existing anchoring/recovery helpers where possible instead of duplicating algorithms; extraction to a shared anchor helper package is allowed if it reduces duplication.

## Decisions
| Decision | Choice | Rationale |
|---|---|---|
| Package boundary | New `org-suggestions` package | Executable edit semantics such as section replacement are out of scope for `org-comments` and not AI-specific. |
| Relationship to comments | `org-suggestions` depends on/extends `org-comments`; `org-comments` remains mostly suggestion-agnostic | Keeps comments as anchored discussion substrate while allowing linked suggestion threads. |
| Storage | Source-adjacent Org sidecar `SOURCE.suggestions.org` | Human-readable, local to the document, consistent with `SOURCE.comments.org`. |
| Canonical representation | Structured patch-like data, not unified diff text | Enables Org-aware semantics and robust section replacement; diff/patch views are derived. |
| Hierarchy | Thread → candidate → hunks | Thread is proposed edit intent/changeset; candidate is an alternative/revision; hunk is an executable operation. |
| Root-level suggestions | Not supported | Every candidate belongs to a thread to avoid dual schema shapes. |
| Provider ids | Provider-controlled canonical ids, unique per source sidecar | Allows Copilot labels like `ai-1`; fallback generator may use `sug-N`. |
| MVP hunk kinds | `replace`, `insert`, `section-replace` | Covers current UX without document replacement/delete complexity. |
| Section replacement | Replacement body only, excluding section heading | Preserves section identity and avoids duplicated headings. |
| Acceptance | Candidate-level all-or-nothing in MVP | A candidate is a coherent patch; partial apply can change meaning. |
| Staleness | Candidate becomes `STALE` if any hunk cannot resolve/apply | All-or-nothing acceptance requires candidate-level conflict reporting. |
| Original text storage | Store original target text for `replace`; store anchor text for `insert`; do not store full original section body for `section-replace` | Avoids sidecar bloat while preserving required anchors and diagnostics. |
| Undo | Session-local generic undo in MVP; no durable rollback guarantee | Preserves existing immediate UX without bloating sidecars. |
| Thread status | Derived from child candidates, not independently authoritative | Avoids inconsistent persisted lifecycle. |
| Session cleanup | Provider/session archive and delete helpers | Lets orchestrators such as Copilot clean current-session artifacts without owning suggestion internals. |
| Section resolution | Targeted scans by path/title | Avoids unnecessary full-section materialization on large documents. |

## Sidecar Shape
For `draft.org`, the default sidecar is `draft.suggestions.org`.

```org
#+title: Suggestions for draft.org
#+source: draft.org
#+org_suggestions_schema_version: 1
#+todo: ACTIVE ACCEPTED SUPERSEDED DISMISSED STALE | ARCHIVED

* Thread ai-thread-1
:PROPERTIES:
:ORG_SUGGESTIONS_THREAD_ID: ai-thread-1
:ORG_SUGGESTIONS_PROVIDER: org-copilot
:ORG_SUGGESTIONS_SESSION_ID: session-1
:ORG_SUGGESTIONS_SUMMARY: Rewrite intro and add transition
:ORG_SUGGESTIONS_COMMENT_ID: cmt-1
:END:

** ACTIVE ai-1
:PROPERTIES:
:ORG_SUGGESTIONS_ID: ai-1
:ORG_SUGGESTIONS_PARENT_ID:
:ORG_SUGGESTIONS_ALTERNATIVE_GROUP_ID: alt-1
:ORG_SUGGESTIONS_CREATED_BY: org-copilot
:ORG_SUGGESTIONS_CREATED_BY_MESSAGE_ID: msg-2
:END:
Optional short candidate label or rationale.

*** Hunk h1
:PROPERTIES:
:ORG_SUGGESTIONS_HUNK_ID: h1
:ORG_SUGGESTIONS_HUNK_KIND: section-replace
:ORG_SUGGESTIONS_PRIMARY: t
:ORG_SUGGESTIONS_SECTION_TITLE: Intro
:ORG_SUGGESTIONS_SECTION_PATH: Doc/Intro
:END:

**** Replacement
#+begin_src org
Replacement Intro body.
#+end_src

*** Hunk h2
:PROPERTIES:
:ORG_SUGGESTIONS_HUNK_ID: h2
:ORG_SUGGESTIONS_HUNK_KIND: insert
:ORG_SUGGESTIONS_ANCHOR_TEXT: Existing transition anchor.
:ORG_SUGGESTIONS_PLACEMENT: after
:END:

**** Replacement
#+begin_src org
New transition paragraph.
#+end_src
```

For `replace` hunks, use child blocks:

```org
*** Hunk h1
:PROPERTIES:
:ORG_SUGGESTIONS_HUNK_ID: h1
:ORG_SUGGESTIONS_HUNK_KIND: replace
:END:

**** Original
#+begin_src org
Old paragraph.
#+end_src

**** Replacement
#+begin_src org
New paragraph.
#+end_src
```

## Acceptance Criteria
- [x] AC-1: Given an Org source file, when `org-suggestions` creates a thread with one active candidate and one `section-replace` hunk, then `SOURCE.suggestions.org` is written atomically in the documented Org shape.
- [x] AC-2: Given a persisted suggestions sidecar, when `org-suggestions` loads it, then it restores threads, candidates, hunks, provider ids, lifecycle statuses, and comment/session links without opening UI windows.
- [x] AC-3: Given an active candidate with a `section-replace` hunk, when accepted after the section body changed, then the current live section body is replaced and the candidate becomes `ACCEPTED` as long as the section resolves uniquely.
- [x] AC-4: Given an active candidate with a `replace` hunk, when the original target text cannot be resolved or recovered, then no source text changes and the candidate becomes `STALE` with a hunk-level stale reason.
- [x] AC-5: Given an active candidate with an `insert` hunk, when the anchor text resolves and placement is `before` or `after`, then the replacement body is inserted at the resolved anchor and the candidate becomes `ACCEPTED`.
- [x] AC-6: Given a multi-hunk candidate where any hunk cannot resolve, when accepted, then no hunks apply and the candidate becomes `STALE`.
- [x] AC-7: Given alternative active candidates in the same alternative group, when one is accepted, then only that candidate becomes `ACCEPTED` and the other active alternatives become `SUPERSEDED`.
- [x] AC-8: Given an accepted candidate `ai-1`, when a provider creates revision `ai-1.1`, then `ai-1` remains `ACCEPTED`, `ai-1.1` is `ACTIVE`, and both remain under the same thread.
- [ ] AC-9: Given accepted candidate `ai-1` and later accepted revision `ai-1.1`, then `ai-1` remains `ACCEPTED` and records `SUPERSEDED_BY: ai-1.1` metadata rather than changing status to `SUPERSEDED`.
- [x] AC-10: Given an accepted candidate in the current Emacs session, when undo is invoked before the source changed incompatibly, then the source reverts as an all-or-nothing operation and candidate status is restored appropriately.
- [x] AC-11: Given a candidate preview request, when the source changed since creation, then the generated diff compares the candidate hunks against the live source, not stale captured section text.
- [ ] AC-12: Given unknown newer `org_suggestions_schema_version`, when loading, then the package warns and skips restore/mutation rather than corrupting data.

## Invariants
- No suggestion accept path may mutate the source unless all hunks in the candidate can be applied safely.
- `section-replace` never uses original section body mismatch as a stale condition; unresolved/non-unique section identity is the stale condition.
- Numeric line/range metadata is only a hint; text/section anchors are canonical.
- Sidecar writes use temporary-file then rename semantics.
- Suggestions sidecars never store credentials, OAuth tokens, request headers, or raw prompts.
- Avoid duplicate anchoring algorithms; reuse or extract helpers when two packages need the same behavior.
- Provider/session archive and delete helpers operate only on matching top-level thread metadata.

## Scope
**May modify:**
- `packages/org-suggestions/`
- focused reusable anchor helpers in `packages/org-comments/` if needed
- tests under `packages/org-suggestions/test/`

**Must not modify:**
- LLM transport/provider code except through later Copilot integration slices
- publishing backends
- user private configuration

## Verification Plan
| Criterion | Method | Automated? |
|---|---|---|
| AC-1, AC-2 | Sidecar fixture round-trip ERT tests | Yes |
| AC-3–AC-6 | Source-buffer accept/stale ERT tests per hunk kind | Yes |
| AC-7–AC-9 | Lifecycle transition ERT tests | Yes |
| AC-10 | Session-local undo ERT test | Yes |
| AC-11 | Diff preview generation ERT test | Yes |
| AC-12 | Schema-version fixture test | Yes |

## References
- `packages/org-copilot/spec-persistent-suggestions.md`
- `packages/org-comments/SPEC.md`
- `packages/org-copilot/SPEC.md`
