# Native callout smoke checklist

## Status

Automated fixture support is implemented; live smoke is pending.

## Canonical Org syntax

```org
#+ATTR_CALLOUT: :type warning :title "Heads up"
#+BEGIN_CALLOUT
Body with *bold*, /italic/, and ~code~.
#+END_CALLOUT
```

Supported types:

- `info`
- `note`
- `warning`
- `tip`
- `important`

Missing `:type` defaults to `info`.

## Expected Google Docs rendering

A callout renders as:

```text
Warning — Heads up
Body with bold, italic, and code.
```

The label paragraph is generated chrome. It is marked separately from body paragraphs and is dropped from the Org body on pull. Body paragraphs and simple list items remain editable remote content and should pull back into the Org callout body.

## Live checklist

- [ ] Create or push an Org buffer containing info, warning, and titled callouts.
- [ ] Confirm the generated label paragraph is visible.
- [ ] Confirm callout body text and simple unordered list items remain editable ordinary Docs text.
- [ ] Edit body text remotely, pull, and confirm the Org callout body updates.
- [ ] Confirm the generated label does not appear in the pulled Org body.
- [ ] Run `M-x org-google-docs-push-restyle-current` after a style tweak and confirm callouts restyle with source/quote blocks.
- [ ] Confirm unsupported callout types fail before remote mutation.

## Design notes

- The visible label is derived from marker metadata and ignored on pull in v1.
- Label markers and body markers both store type/title metadata.
- Label-only callouts preserve an empty callout; body-only callouts preserve the wrapper and recreate the label on next push.
- Per-type logical styles are present from v1 so the branding pass can tune colors/spacing later without changing semantics.
- Lists inside callout bodies are preserved as body paragraph elements with list metadata, including unordered, ordered, and nested list levels. This is intentionally built on the same paragraph/list IR foundations as top-level lists rather than a callout-specific list model.
- Callouts intentionally do not use the quote indentation style. They use full-width paragraph shading, 6pt paragraph border padding for breathing room inside the shaded block, a small gap below the generated label, and an unmarked empty separator paragraph after each callout because Google Docs paragraph shading can visually merge adjacent shaded paragraphs despite spacing properties.
- The generated separator paragraph is semantic chrome. Pull conversion includes one immediately following empty paragraph in the owning callout block range so normal and restyle pushes replace the whole generated shape instead of accumulating blank lines.
