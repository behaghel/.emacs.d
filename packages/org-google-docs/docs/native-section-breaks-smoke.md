# Section-break fidelity smoke check

Non-normative manual acceptance procedure. Offline fixtures do not establish live browser acceptance.

## Preconditions

- Use reviewed sources in isolated batch Emacs through existing authenticated
  transport, or explicitly install reviewed revisions. Do not reload the
  production SDK as a side effect of testing or acceptance.
- Use only disposable synthetic Org and Google Docs documents and select the
  account explicitly. Disable automatic push and mutation retries. Preserve
  immutable sent plans and returned snapshots; never expose credentials.
- Share the matching Org source with the browser link before visual review.

## Source

```org
Before *bold* prose 😀.

-----
Middle /italic/ prose with [[https://example.org][a link]].

-----

-----
After final prose 😀.

-----
```

## Checks

1. Run `M-x org-google-docs-debug-pipeline` before mutation. Inspect independently
   identified section breaks, logical rule styles and UTF-16 bounds. Confirm the
   published representation is a marked separator, not a native insertion request.
2. Create the disposable Doc. Inspect browser and PDF: all four lines are visible,
   adjacent lines remain separate and the terminal line survives. Verify width,
   color and outside spacing, with no inherited borders or tiny following text.
3. Pull and push unchanged twice: zero requests. Edit surrounding emphasis/text,
   add and remove breaks, then push and independently fetch. Preserve following
   Unicode text, rich formatting and all remaining section-break identities.
4. Inspect actual native rich runs, not only normalized IR or an empty diff.
   In the owned synthetic Doc, seed a whitespace-only URL beside an authored
   link, then change a word before both spans (including a UTF-16 surrogate pair).
   Verify the unwanted URL is absent from returned `textStyle.link` fields,
   authored link labels/targets and emphasis remain intact, and a repeat is empty.
5. Edit a word in ordinary prose in the browser. Pull and inspect matching Org;
   repeated planning from the pulled source must produce zero requests.
6. A marked separator containing authored text, invalid provenance, bullets or
   overlapping/partial annotations must stop import rather than discard content.
   Arbitrary unmarked paragraph borders must not be inferred as section breaks.

Native `horizontalRule` objects are a distinct supported read representation, not
what publication inserts. Keep native-object fixture coverage separate from the
live published-line result; do not claim browser-native rule coverage without
actually creating one through Google Docs. Unsupported mixed native objects and
text stop with actionable diagnostics.

For unexpected results, inspect returned JSON, IR and requests using the debug
pipeline's prefix argument. Record reviewed revisions and observed acceptance in
the review; visual artifacts remain runtime files, not tracked golden images.
