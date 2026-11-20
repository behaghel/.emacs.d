# Native centered prose smoke check

Non-normative manual acceptance procedure. Offline tests do not establish live browser acceptance.

## Preconditions

- Use reviewed sources in isolated batch Emacs with already installed authenticated
  transport, or explicitly install reviewed revisions. Never reload the production
  SDK as a side effect of testing. Acceptance does not authorize installation.
- Use only disposable synthetic Org and Google Docs documents, selecting the account
  explicitly. Keep auto-push disabled; retain immutable plans and snapshots and
  disable automatic mutation retries. Never print credentials or raw auth errors.
- Share the matching Org source alongside the browser link for visual comparison.

## Source

```org
Before centered prose 😀.

#+BEGIN_CENTER
First *bold* centered paragraph 😀.

Second /italic/ paragraph with [[https://example.org][a link]].
#+END_CENTER

#+BEGIN_CENTER
Separate centered block.
#+END_CENTER

#+BEGIN_CENTER
#+END_CENTER

After centered prose 😀.
```

## Checks

1. Run `M-x org-google-docs-debug-pipeline` before mutation. Inspect grouped source
   paragraphs, distinct structural markers and UTF-16 request bounds.
2. Create the disposable Doc. Inspect both browser and PDF: rich prose is centered,
   paragraphs have visible separation, distinct blocks stay distinct and following
   prose remains left-aligned. The empty center retains structural identity.
3. Pull and push unchanged twice: zero requests. Add a centered paragraph, change
   emphasis/text in both populated blocks and edit following prose; push, fetch
   independently and pull without splitting the first block into multiple wrappers.
4. Insert a native paragraph inside the first center and edit a word in the browser.
   Pull and inspect matching Org. Preserve all paragraphs, emphasis, links and
   following text; unchanged repeated push must be a no-op.
5. Unsupported native objects, partial/overlapping ranges, missing version metadata
   or uncentered paragraphs inside a marked center must stop import rather than
   guessing the block's meaning. Unsupported nested source blocks, images or
   semantic objects stop before remote mutation.

Unmarked native centered paragraphs retain single-block Org rendering, without
inferring shared identity from adjacency. For surprising behavior, inspect the
pipeline with its prefix argument; record reviewed revisions and observed results
in the review, not credentials. Browser acceptance remains a human checkpoint.
