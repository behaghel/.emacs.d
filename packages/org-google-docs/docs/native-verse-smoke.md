# Native verse fidelity smoke check

Non-normative manual acceptance procedure. Offline tests do not establish browser acceptance.

## Preconditions

- Use reviewed SDK/configuration sources in isolated batch Emacs with the already
  installed authenticated transport, or explicitly install reviewed revisions.
  Acceptance does not authorize installation or a production SDK reload.
- Use only a new disposable Org source and Google Doc, with the account selected
  explicitly. Keep automatic push disabled; never use a product document.
- Keep request plans and returned snapshots immutable. Do not retry mutations
  automatically or include authentication records in traces.

## Source

```org
#+BEGIN_VERSE
  First *bold* line 😀.
second /emphasized/ line with [[https://example.org][a link]].

    A stanza with ~literal  spaces~.
  final line.
#+END_VERSE

#+BEGIN_VERSE
Separate poem.
#+END_VERSE

After both verses 😀.
```

## Checks

1. Run `M-x org-google-docs-debug-pipeline` before mutation. Verify distinct verse
   groups, rich runs and UTF-16 request boundaries. Check that unsupported source
   content blocks publication rather than becoming literal or flattened prose.
2. Create the document. In the browser and exported PDF, inspect hard lines,
   indentation, one blank stanza line, compact consecutive lines, emphasis,
   links, literal double spaces and separation between the two verses.
3. Push unchanged twice: no requests. Change indentation, emphasis and text in
   both verses, insert a hard line and edit the following paragraph. Push, fetch
   independently and pull; preserve all content and repeat without requests.
4. Insert a line inside the verse in Google Docs, then change a word in the
   browser. Pull and inspect the Org wrappers, literal spacing, stanza lines and
   rich text. Push the pulled source unchanged: no requests and no hidden stale
   text or line-count snapshot restoring the previous content.
5. Check empty verses, a verse with one authored blank line, adjacent verses and
   leading/trailing stanza gaps independently. Do not confuse the empty-block
   placeholder with an authored line.

Unsupported native objects, partial/overlapping semantic ranges or missing
structural versions must stop import with an actionable diagnostic. Nested
source verses and unsupported dates, notes, status or inline-source content must
stop before remote mutation. Never bypass a guard using a potentially lossy
pulled copy; recover only from trusted authoritative source.

For surprising behavior, inspect the debug pipeline's returned JSON, IR and
requests with its prefix argument. Record revisions, account selection (not
credentials), observed results and failures in the review. Browser acceptance
remains a separate human checkpoint.
