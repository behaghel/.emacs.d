# Org Export Authoring Guide

## Available LaTeX classes

Discovered from `etc/latex/*.cls`:

| Class | File | Use case |
|-------|------|----------|
| `hub-article` | `etc/latex/hub-article.cls` | Personal articles, notes, essays |
| `veriff` | `etc/latex/veriff.cls` | Professional Veriff documents |

Set with `#+LATEX_CLASS: hub-article` in the file header.  
The auto-insert template (`template.org`) prompts for class and variant.

### Veriff variants

When using `veriff`, add `#+LATEX_VARIANT:` to select:

| Variant | Purpose |
|---------|---------|
| `refresh-overdrive` (default) | Standard layout |
| `dark-campaign` | Dark theme |
| `gallery-white` | White gallery layout |

## Structure templates

Type `<` followed by the shortcut, then `TAB` to expand:

| Shortcut | Expands to | Description |
|----------|-----------|-------------|
| `<co` | `#+ATTR_CALLOUT` + `#+begin_callout` / `#+end_callout` | Semantic callout block with prompted type/title |
| `<ci` | `#+begin_info` / `#+end_info` | Info callout (teal rule) |
| `<cw` | `#+begin_warning` / `#+end_warning` | Warning callout (rust rule) |
| `<ca` | `#+begin_authorsnote` / `#+end_authorsnote` | Author's note (green rule) |
| `<sf` | `#+begin_standfirst` / `#+end_standfirst` | Standfirst (large upright) |
| `<c` | `#+begin_comment` / `#+end_comment` | Comment block |
| `<C` | `#+begin_center` / `#+end_center` | Centered block |
| `<gr` | `#+begin_graph` / `#+end_graph` | Graph/metrics block |
| `<im` | optional `#+CAPTION` + image link | Shared-exporter image link; leave caption empty to remove the caption line |
| `<fn` | footnote reference + bottom definition | Inline shortcut; works anywhere in text, jumps to the footnote body, then returns after the body is filled |

### Callout titles

Use one semantic callout attribute for every exporter:

```org
#+ATTR_CALLOUT: :type warning :title "Review before deploying"
#+begin_callout
This step requires a second pair of eyes.
#+end_callout
```

Confluence exports `:type` to the corresponding panel macro and `:title` to the panel title. XeLaTeX exports `:title` to the callout environment title.

## Confluence export

Use the normal Org export dispatcher with `C-c C-e C`:

| Key | Action |
|-----|--------|
| `C` | Publish/update the current buffer or selected subtree |
| `X` | Export to a temporary Confluence XHTML buffer |
| `x` | Export to a `.xhtml` file |

For whole-buffer updates, set `#+CONFLUENCE_PAGE_ID:`. For subtree updates, put `CONFLUENCE_PAGE_ID` in the subtree property drawer and use Org's subtree export option from the dispatcher.

### Standfirst

Use for the opening paragraph of an article — rendered at `\large` (upright):

```org
#+begin_standfirst
This is the article's lead paragraph, visually distinct from body text.
#+end_standfirst
```

## Drop caps

Select a word in visual mode and press `,x d` (leader → Text/Transform → Drop cap).  
Transforms `Hello` into `\HubArticleDropCap{H}{ello}`.

The drop cap sinks 2 lines with the rest rendered in small caps.

## Metadata keywords

| Keyword | Purpose |
|---------|---------|
| `#+EXPORT_EYEBROW:` | Eyebrow text above the title (small caps) |
| `#+EXPORT_FOOTER_NOTE:` | Footer note at the end of the document |
| `#+EXPORT_CODE_THEME:` | Code theme for veriff class: `light` or `dark` |

## Page numbering

Page 1 has no number. Subsequent pages show a right-aligned number in `HubArticleRule` color (`#CBD8D2`), positioned in the right margin.
