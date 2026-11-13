---
domain: authoring/marginalia
status: draft
---

# cp-org-marginalia

`cp-org-marginalia` is a Context Panels provider for native Org footnotes and
sidenotes. Its collector and layout helpers remain usable without activating
any panels.

## Boundary

The package owns native Org footnote collection, `HUB_NOTE_*` parsing, layout,
context-panel provider integration, marginalia row rendering, and jump-to-source
reference behavior.

It does not own Org comments, sidecars, compose flows, filters, help UI,
Confluence publishing, personal keybindings, or edit/delete/toggle actions.

## Activation

The package declares Emacs 29.1+, Org 9.6+, and context-panels 0.1.0+.
Loading a library does not enable its buffer-local mode or modify source text.
Local display and reference navigation require no comments package, private
configuration, accounts, or network access.

```elisp
(require 'cp-org-marginalia-context-panel)
(add-hook 'org-mode-hook #'cp-org-marginalia-context-panel-mode)
```

## Native Notes and Metadata

Named references use `[fn:label]` with separate `[fn:label] Body` definitions.
Definitions may be indented, span multiple lines, or use a colon separator before
a body or property drawer. Records follow reference order, not definition order.
Anonymous and inline-definition footnotes are outside this collector's supported
syntax.

An ordinary named footnote uses `cp-org-marginalia-default-kind` (`sidenote` by
default). Optional `HUB_NOTE_KIND` values `sidenote` and `footnote` select a kind;
missing or unknown values use that default. `HUB_NOTE_STATUS`, `HUB_NOTE_SOURCE`,
and `HUB_NOTE_REMOTE_ID` are metadata, not rendered body text. These authored
property names are independent of the package's Lisp namespace.

Layout is deterministic and keeps source order, displacing later notes downward
when rendered rows would overlap. Display and navigation do not write source
content or create sidecars.

## Context Panels Integration

With sufficient frame width, the panel opens to the right of its displayed
source window. When enabled alongside `org-comments`, both providers share the
same `context-panels` side panel. Rows are ordered by viewport/source position;
provider priority breaks ties. `RET` uses `context-panels-jump-at-point`, so
marginalia rows jump to their Org footnote reference while comment rows use the
comments provider action. Marginalia rows bind both terminal `RET` and graphical
Return events locally, without replacing the panel's major-mode map or other
providers' bindings. The stable provider ID is `marginalia`; library,
command, customization, face, and row-property symbols use `cp-org-marginalia`.

Usage and standalone verification are described in [README.org](README.org).
