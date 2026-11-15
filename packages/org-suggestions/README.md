---
domain: authoring/suggestions
status: draft
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

- `section-replace` — replace a uniquely resolved section body only when it still matches the proposal's source baseline; preserve its heading and metadata.
- `replace` — replace exact original text.
- `insert` — insert text before/after an exact anchor.

Candidate acceptance is all-or-nothing. If any hunk cannot resolve, no source text is changed and the candidate becomes stale. If an error interrupts source editing, all edits are rolled back; candidate and alternative statuses and session-local rollback history remain unchanged. Only successful acceptance records rollback data, marks the candidate accepted, and supersedes its active alternatives.

## Standalone Contract

The package requires only Emacs and Org. Creating, persisting, reloading,
previewing, accepting, and undoing a candidate does not require comments,
Copilot, account configuration, or network access.

Preview resolves every hunk against the live source and shows the text removed
and added for each supported hunk kind. Conflicting section proposals show the
request-time baseline and current-to-proposed comparison. Missing or ambiguous
targets report a stale-target error rather than an empty preview. Preview does not modify source
text, persisted sidecars, or candidate lifecycle state.

Acceptance by candidate ID persists its lifecycle change to the sidecar but
changes source text in the buffer only; saving the source is a caller decision.
The low-level undo helper updates the buffer and candidate record. Callers
persist that record explicitly when using it outside a comment integration.

The [executable standalone walkthrough](examples/standalone.org) demonstrates
these public operations without an external provider.

## Sidecar shape

Original and replacement bodies preserve their final-newline state, blank lines,
trailing horizontal whitespace, and Unicode text across write/load cycles.
Section hunks retain their relevant request-time body in an `Original` block,
with outline path/title and optional `ORG_SUGGESTIONS_SECTION_ID` or
`ORG_SUGGESTIONS_SECTION_CUSTOM_ID`. Stable IDs take precedence over path/title;
without IDs, unique path matching and then unique-title recovery locate moved
sections. A duplicate path is ambiguous, not permission to choose the first.
Heading metadata is not part of the replaced body.

`ORG_SUGGESTIONS_ESCAPED: t` marks Org-escaped block contents, preserving nested
headings and block delimiters. Unmarked existing blocks remain literal.
Optional hunk properties `ORG_SUGGESTIONS_ORIGINAL_NO_FINAL_NEWLINE` and
`ORG_SUGGESTIONS_REPLACEMENT_NO_FINAL_NEWLINE`, when `t`, distinguish a synthetic
source-block delimiter newline from authored text. Readers remove only that
synthetic newline. Unmarked blocks retain their literal, newline-terminated
contents; loading does not infer inline text or rewrite the sidecar.

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
:ORG_SUGGESTIONS_ESCAPED: t
:END:

**** Original
#+begin_src org
Original body.
#+end_src

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
- `org-suggestions-candidate-with-source-context`
- `org-suggestions-reconcile-candidate`
- `org-suggestions-finish-reconciliation`
- `org-suggestions-archive-provider-session-threads`
- `org-suggestions-delete-provider-session-threads`

## Safety rules

- Source files change only through explicit accept commands.
- `section-replace` requires an exact, known source-body baseline at every acceptance, even after preview or reconciliation. Missing baseline evidence is a conflict, including on restored proposals. Empty baseline text is valid evidence.
- Unrelated edits outside the target body do not invalidate it. Missing/ambiguous targets, changed relevant text and overlapping hunks prevent the whole candidate from applying.
- `replace` and `insert` require resolvable text anchors.
- Sidecar writes are atomic temp-file + rename writes.
- Undo is session-local in the MVP and reverses all candidate hunks together, including length-changing edits. If accepted text has changed incompatibly or an edit fails, undo leaves source text, candidate status, and rollback history unchanged.

## Source context and reconciliation

A producer binds a candidate to the immutable Org text used for its request:

```elisp
(org-suggestions-candidate-with-source-context candidate request-source-text)
```

The returned copy contains only the relevant section baselines and identities,
not the entire request. Binding never uses a model-supplied original section body
or newly read callback-time text as evidence. Manual section hunks can supply an
exact `:original` baseline directly. Text replacement and insertion still use
unique exact text anchors.

For a conflicting persisted candidate, run
`M-x org-suggestions-reconcile-candidate` in the source buffer and select its ID.
The comparison buffer protects Baseline and Current source and leaves each
Replacement field editable. `C-c C-c` saves the reviewed proposal with the source
baseline captured when comparison opened; it does not change or save the source.
Killing the comparison buffer cancels. Accept separately through the normal
candidate/comment action. Changes made while comparing, or after saving, are
checked again at acceptance. Undo restores the complete pre-acceptance text,
including the human edits used for reconciliation.

Missing or ambiguous targets must regain a unique identity before reconciliation.
A concurrently changed stored candidate requires reopening the comparison.

See `SPEC.md` for the implementation contract.
