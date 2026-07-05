# Google Docs Source Block Smoke Tests

These manual checks cover semantic source block preservation in the local `gdocs` source-block marker seam.

## Reload after code changes

Evaluate from the linked Org buffer:

```elisp
(progn
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-convert.el")
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-diff.el")
  (load-file "/Users/hubertbehaghel/ws/gdocs/gdocs-sync.el")
  (load-file "/Users/hubertbehaghel/ws/.emacs.d-org-googledoc/packages/org-google-docs/org-google-docs.el")
  (setq-local gdocs-sync--push-in-progress nil
              gdocs-sync--push-queued nil)
  (message "Reloaded Google Docs source block support"))
```

## Source block round-trip

Org source:

```org
Before.

#+begin_src emacs-lisp
(message "hello")
(+ 1 2)
#+end_src

After.
```

Expected Google Doc v1 behavior:

- the code text is visible in the Google Doc;
- code text uses monospace text styling;
- the source block language is stored in a semantic marker named like `gdocs-org-marker:src-block:<id>:emacs-lisp`;
- native Google Docs code block building-block visuals are not required for v1.

Expected Org source after pull:

```org
Before.

#+BEGIN_SRC emacs-lisp
(message "hello")
(+ 1 2)
#+END_SRC

After.
```

Manual result: pending.

## Known limitations

- Syntax highlighting is styling and is deferred.
- Native Google Docs code block building blocks are deferred until the public API exposes a reliable creation/update seam.
- If the semantic marker is deleted in Google Docs, pulled monospace paragraphs may fall back to an example block rather than an Org source block with language.
