# Native quote emphasis smoke check

Non-normative manual acceptance procedure. Offline fixture results do not establish live Google Docs acceptance.

## Preconditions

- Explicitly install the reviewed SDK/configuration revisions before this check. Review acceptance does not authorize installation automatically.
- Use a new disposable Org file and a new disposable Google Doc, never a product document.
- Keep automatic push disabled and use the intended account explicitly.

## Source

```org
#+BEGIN_QUOTE
Plain /emphasized 😀/ text.

second paragraph with *bold* and [[https://example.org][a link]].
#+END_QUOTE

#+BEGIN_QUOTE
A separate quote.
#+END_QUOTE
```

## Checks

1. Run `M-x org-google-docs-debug-pipeline` before creating the document. Inspect quote marker metadata and final italic range requests; do not expose authentication records.
2. Create the document with italic quote styling enabled. Plain quote text must appear italic; authored emphasis must appear upright. Bold, links, paragraph boundaries and separate quote boundaries must survive.
3. Push the linked document unchanged twice. Both pushes must be no-ops. Change emphasis in Org, push, and verify the displayed inversion.
4. In Google Docs, toggle an emphasized span from upright to italic. Pull: the corresponding Org emphasis must disappear. Push unchanged: no immediate diff.
5. Change the local quote style before another pull. The existing document's stored applied base must still control decoding. Restore the intended style, deliberately republish/restyle, then repeat the no-op checks.
6. Repeat create/push/edit/pull with non-italic quote styling. Ordinary text must remain upright and authored emphasis italic.

A legacy marked quote without an applied-style snapshot must stop pull with an actionable provenance diagnostic, not overwrite source using a guessed interpretation. Republish only from the trusted, authoritative Org source to establish that snapshot; do not override the guard using a potentially lossy pulled copy.

When behavior is surprising, use the debug pipeline with its prefix argument to inspect returned JSON, IR and requests. Record the reviewed revisions, account selection (not credentials), observed results and any failures. Live acceptance remains pending until these checks pass.
