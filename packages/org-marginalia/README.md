# org-marginalia

`org-marginalia` is a reusable Org package for native footnote/sidenote marginalia.

## Boundary

The package owns native Org footnote collection, `HUB_NOTE_*` parsing, layout,
context-panel provider integration, marginalia row rendering, and jump-to-source
reference behavior.

It does not own Org comments, sidecars, compose flows, filters, help UI,
Confluence publishing, personal keybindings, or edit/delete/toggle actions.

## Activation

```elisp
(add-hook 'org-mode-hook #'org-marginalia-context-panel-mode)
```

When enabled alongside `org-comments`, both providers share the same
`org-context-panel` side panel. Rows are ordered by viewport/source position;
provider priority breaks ties. `RET` uses `org-context-panel-jump-at-point`, so
marginalia rows jump to their Org footnote reference while comment rows use the
comments provider action.
