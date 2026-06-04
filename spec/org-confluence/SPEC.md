# Org → Confluence Export & Publish System

## Context

The Org export layer in this repo (`modules/org/export-latex.el`, providing `org/export-latex`) handles Org → LaTeX → PDF for the `veriff` and `hub-article` families. This spec adds a parallel path: Org → Confluence Storage Format (XHTML) → publish to Confluence Cloud via the `cfl` CLI.

The motivating document is `~/Documents/org/veriff-bloom.org`, a strategic Veriff document originally authored in Confluence then migrated to Org for offline editing. The immediate need is to push the edited Org version back to the existing Confluence page for team review. More broadly, this enables an Org-native authoring workflow for Confluence-published documents.

## Target Architecture

Three files, each with a distinct responsibility:

| File | Responsibility | Layer |
| --- | --- | --- |
| `lisp/hub-confluence-api.el` | Low-level `cfl` shell wrappers: page create/update, attachment upload, config lookup | Always-on library |
| `modules/org/export-confluence/export.el` | Pure Org → Confluence Storage Format (XHTML) translation backend (`org-export-define-backend 'confluence`) | Always-on module (batch-safe) |
| `modules/org/export-confluence/commands.el` | User-facing commands (`hub/confluence-publish`, `hub/confluence-publish-dwim`) that compose the API layer and the export backend | Always-on module (batch-safe) |
| `modules/org/export-confluence/api.el` | `cfl` shell wrappers specific to the confluence publish flow | Always-on module (batch-safe) |

### Layering Rules

- `hub-confluence-api.el` must not depend on Org or the export system. It receives pre-formatted XHTML and shell arguments.
- `export.el` (providing `org/export-confluence`) must not shell out. It receives an Org document and returns an XHTML string.
- `api.el` (providing `org/export-confluence-api`) wraps `cfl` shell calls.
- `commands.el` (providing `org/export-confluence-commands`) composes the two: export buffer to XHTML, then dispatch via `cfl`.
- The user interacts only with the commands namespace.

## Accepted Decisions (from grilling)

### Scope

- **Primary direction:** Org → Confluence (push). Confluence → Org (pull) is deferred to a future iteration.
- **Content format:** Confluence Storage Format (XHTML), sent via `cfl page edit --storage` or `cfl page create --storage`.
- **Page identity:** Document-level `#+CONFLUENCE_PAGE_ID: <id>` keyword for existing pages. Optional `#+CONFLUENCE_SPACE: <key>` for new page creation.
- **Update semantics:** Full replacement. Confluence versions the page automatically on each update. No append/prepend mode.
- **Table mapping:** Simple `<table>` with `<th>` for header rows, `<td>` for body rows. No branded Confluence styling or custom column widths.

### Callout Mapping

- `#+begin_callout` / `#+end_callout` → Confluence panel macros.
- Optional type via `#+begin_callout info|note|warning|tip|important`. Default: `info`.
- Panel title via `#+ATTR_CONFLUENCE: :panel-title "Title"` (or fallback to `#+ATTR_LATEX: :options [Title]` for backward compatibility with existing documents).

### `cfl` Integration

- Configurable via `defcustom` defaulting to `"cfl"`.
- Reuses `cfl`'s auth config (`~/.config/cfl/config.yml`) — the user runs `cfl init` independently.
- For future comment API access, read the cfl config file to extract the API token and cloud ID, then call the Confluence REST API directly via `url-retrieve` or `curl`.

### Image Handling (deferred to Iteration 3)

- Images uploaded as Confluence page attachments via `cfl attachment upload`.
- Image references rewritten from local paths to `ri:attachment` storage format.
- Not yet implemented in iterations 1–2.

## Implementation Iterations

### Iteration 1 — Publish Backbone (happy path)

**Elements translated:**
- Metadata: `#+TITLE` → page title, `#+AUTHOR` → stored but not rendered in Confluence
- Headings (H1–H4): `<h1>` through `<h4>`
- Paragraphs: `<p>`
- Bold: `<strong>`
- Italic: `<em>`
- Inline code: `<code>`
- External links (`[[https://...][text]]`): `<a href="...">`
- Bullet lists: `<ul><li>`
- Ordered lists: `<ol><li>`
- Horizontal rules: `<hr/>`

**Commands delivered:**
- `hub/confluence-publish` — export current buffer and update existing page (requires `#+CONFLUENCE_PAGE_ID`)
- `hub/confluence-publish-dwim` — if `#+CONFLUENCE_PAGE_ID` present, update; if `#+CONFLUENCE_SPACE` present but no page ID, prompt for title and create new

**API layer functions:**
- `hub/confluence--page-update (page-id xhtml)` — calls `cfl page edit <id> --storage`
- `hub/confluence--page-create (space title xhtml &optional parent-id)` — calls `cfl page create`
- `hub/confluence--page-id-from-buffer` — reads `#+CONFLUENCE_PAGE_ID`
- `hub/confluence--space-from-buffer` — reads `#+CONFLUENCE_SPACE`

**Tests:**
- Export backend: one Org buffer with headings, bold, italic, links, lists → assert XHTML output matches expected structure
- API layer: verify argument construction (command string) without executing `cfl`

### Iteration 2 — Rich Content

**Elements added:**
- Tables: `<table><thead><tr><th>`, `<tbody><tr><td>`
- Blockquotes (`#+begin_quote`): `<blockquote>`
- Callout blocks (`#+begin_callout`): Confluence `<ac:structured-macro>` with `name="info|note|warning|tip|important"`
- Code blocks (`#+begin_src`): `<ac:structured-macro name="code"` with language attribute
- Strikethrough: `<strike>` or `<span style="text-decoration: line-through;">`
- Underline: `<u>`

**Tests:**
- Export one Org buffer with tables, blockquotes, callouts, code blocks → assert XHTML
- Verify callout type dispatch (info default, explicit types respected)
- Verify callout title extraction from `#+ATTR_CONFLUENCE` and `#+ATTR_LATEX`

### Iteration 3 — Images and Attachments

**Elements added:**
- Image links (`[[./img/foo.png]]`): upload as attachment then reference via `<ri:attachment ri:filename="foo.png">`
- `cfl attachment upload <page-id> <file>` dispatch

**API layer additions:**
- `hub/confluence--attachment-upload (page-id filepath)` — calls `cfl attachment upload`
- `hub/confluence--attachment-url (page-id filename)` — constructs the public attachment URL
- Image path resolution (local path relative to `.org` file → absolute path)

**Tests:**
- Attachment upload command construction
- Image path resolution: relative paths, absolute paths, missing files

### Iteration 4 — Polish & Pull

**Additions:**
- Subtree export (publish a single heading as its own Confluence page, linked from the parent)
- Confluence → Org import (`hub/confluence-pull`): fetch page content via `cfl page view` and convert from XHTML to Org
- Footnote handling in Confluence Storage Format
- Definition lists (tagged Org items) → Confluence definition list macro
- Better error handling: `cfl` non-zero exit, auth failures, missing page ID

## File Headers & Naming

```elisp
;;; hub-confluence-api.el --- Low-level Confluence Cloud CLI wrappers -*- lexical-binding: t; -*-
(provide 'hub-confluence-api)

;;; export.el --- Org -> Confluence Storage Format (XHTML) export backend -*- lexical-binding: t; -*-
;; Located in modules/org/export-confluence/
(provide 'org/export-confluence)

;;; commands.el --- Org Confluence publish commands -*- lexical-binding: t; -*-
;; Located in modules/org/export-confluence/
(provide 'org/export-confluence-commands)

;;; api.el --- cfl shell wrappers for confluence export -*- lexical-binding: t; -*-
;; Located in modules/org/export-confluence/
(provide 'org/export-confluence-api)
```

## Cross-Links

- Existing LaTeX export system: `modules/org/export-latex.el` (provides `org/export-latex`)
- Existing export tests: `test/org-export-test.el`
- Existing test (full latex PDF): `test/org-latex-pdf-export-test.el`
- Module architecture: `modules/org/` is on `load-path` unconditionally (like `modules/lang/`), unlike `modules/interactive/` which is gated behind interactive mode
- Test helpers: `test/test-helpers.el`
- `cfl` CLI docs: `cfl page edit --help`, `cfl page create --help`, `cfl attachment upload --help`
- Confluence Cloud REST API: `/rest/api/content/{id}/comment` endpoints (for future comment support)

## Confluence Comment Support (Exploratory)

`cfl` does not support comments, but the Confluence Cloud REST API does:

| Endpoint | Method | Purpose |
| --- | --- | --- |
| `/rest/api/content/{pageId}/comment` | GET | List comments on a page |
| `/rest/api/content` (type=comment, container={pageId}) | POST | Create a comment |
| `/rest/api/content/{commentId}` | PUT | Update a comment |
| `/rest/api/content/{commentId}` | DELETE | Delete a comment |

The `cfl` auth config file (`~/.config/cfl/config.yml`) contains the API token, cloud ID, and user email. A future enhancement could parse this file and use `url-retrieve` or `curl` to access the comment endpoints directly, reusing the same credentials.

Possible future commands:
- `hub/confluence-comment-list` — display page comments in an Emacs buffer
- `hub/confluence-comment-create` — add a comment from the minibuffer or from an Org entry
- `hub/confluence-comment-pull` — fetch comments and insert as Org annotations

This is deferred — no iteration plan until the core publish path is stable.

## Non-Goals

- This spec does not cover Confluence → Org import (pull) beyond identifying it as a deferred iteration.
- This spec does not cover ADF (Atlassian Document Format) output. Storage Format XHTML is the target.
- This spec does not cover bidirectional sync or conflict resolution.
- This spec does not cover Confluence page moves, re-parenting, or space management.
- This spec does not require a separate `devenv.nix` change in this repo — `cfl` is expected to be on PATH from the Veriff workspace devenv or equivalent.

## Acceptance Signals

- A user can run `hub/confluence-publish` from any Org buffer with `#+CONFLUENCE_PAGE_ID` and see the content live on Confluence within seconds.
- The Confluence page renders headings, paragraphs, bold, italic, links, and lists correctly after Iteration 1.
- Tables, callouts, blockquotes, and code blocks are added by Iteration 2 without breaking Iteration 1 output.
- Images appear inline by Iteration 3.
- A reader can trace every decision in this spec back to a specific question answered during the grilling session.
