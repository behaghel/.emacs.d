---
domain: authoring.suggestions
status: draft
last-reviewed: 2026-07-21
---

# Org Suggestions

Executable, durable edit suggestions for Org files.

`org-suggestions` stores patch-like edit artifacts in source-adjacent Org sidecars. It is provider-neutral: suggestions may come from Org Copilot today and from other tools later.

For `draft.org`, suggestions live in `draft.suggestions.org`.

## Concepts

- **Thread** — one edit intent/changeset.
- **Candidate** — one alternative or revision of that changeset.
- **Hunk** — one executable edit operation.

Supported MVP hunk kinds:

- `section-replace` — replace the live body of a uniquely resolved section; preserve the heading.
- `replace` — replace exact original text.
- `insert` — insert text before/after an exact anchor.

Candidate acceptance is all-or-nothing. If any hunk cannot resolve, no source text is changed and the candidate becomes stale.

## Sidecar shape

```org
#+title: Suggestions for draft.org
#+source: draft.org
#+org_suggestions_schema_version: 1
#+todo: ACTIVE ACCEPTED SUPERSEDED DISMISSED STALE | ARCHIVED

* Thread ai-thread-1
:PROPERTIES:
:ORG_SUGGESTIONS_THREAD_ID: ai-thread-1
:ORG_SUGGESTIONS_PROVIDER: org-copilot
:ORG_SUGGESTIONS_SESSION_ID: default
:ORG_SUGGESTIONS_SUMMARY: Rewrite intro
:ORG_SUGGESTIONS_COMMENT_ID: cmt-1
:END:

** ACTIVE ai-1
:PROPERTIES:
:ORG_SUGGESTIONS_ID: ai-1
:ORG_SUGGESTIONS_PARENT_ID:
:ORG_SUGGESTIONS_ALTERNATIVE_GROUP_ID: alt-1
:END:

*** Hunk h1
:PROPERTIES:
:ORG_SUGGESTIONS_HUNK_ID: h1
:ORG_SUGGESTIONS_HUNK_KIND: section-replace
:ORG_SUGGESTIONS_PRIMARY: t
:ORG_SUGGESTIONS_SECTION_TITLE: Intro
:END:

**** Replacement
#+begin_src org
Replacement body only.
#+end_src
```

## Public helpers

- `org-suggestions-sidecar-path`
- `org-suggestions-write-sidecar`
- `org-suggestions-load-sidecar`
- `org-suggestions-find-candidate`
- `org-suggestions-accept-candidate`
- `org-suggestions-accept-candidate-id`
- `org-suggestions-undo-accepted-candidate`
- `org-suggestions-candidate-diff`
- `org-suggestions-archive-provider-session-threads`
- `org-suggestions-delete-provider-session-threads`

## Safety rules

- Source files change only through explicit accept commands.
- `section-replace` compares against the live section body; old section body text is not a stale condition.
- `replace` and `insert` require resolvable text anchors.
- Sidecar writes are atomic temp-file + rename writes.
- Undo is session-local in the MVP.

See `SPEC.md` for the implementation contract.
