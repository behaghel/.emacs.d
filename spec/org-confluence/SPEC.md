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
- **Content format:** Confluence Storage Format (XHTML), written to a temporary `.xhtml` file and sent via `cfl page edit --file <file> --storage` or `cfl page create --file <file> --storage`.
- **Page identity:** Document-level `#+CONFLUENCE_PAGE_ID: <id>` keyword for existing pages. Optional `#+CONFLUENCE_SPACE: <key>` for new page creation.
- **Update semantics:** Full replacement. Confluence versions the page automatically on each update. No append/prepend mode.
- **Table mapping:** Simple `<table>` with `<th>` for header rows, `<td>` for body rows. No branded Confluence styling or custom column widths.

### Callout Mapping

- `#+begin_callout` / `#+end_callout` → Confluence panel macros.
- Optional type via `#+ATTR_CALLOUT: :type info|note|warning|tip|important`. Default: `info`.
- Panel title via `#+ATTR_CALLOUT: :title "Title"`.

### `cfl` Integration

- Configurable via `defcustom` defaulting to `"cfl"`.
- Reuses `cfl`'s auth config (`~/.config/cfl/config.yml`) — the user runs `cfl init` independently.
- For future comment API access, read the cfl config file to extract the API token and cloud ID, then call the Confluence REST API directly via `url-retrieve` or `curl`.

### Image Handling (Iteration 3)

Canonical image authoring uses native Org image links and standard Org captions:

```org
#+CAPTION: Architecture overview
#+NAME: fig:architecture-overview
[[./img/architecture-overview.png]]
```

Decisions:

- Plain standalone local image links (`[[./img/foo.png]]`) are embedded as Confluence images.
- Described local image links (`[[./img/foo.png][text]]`) remain normal links.
- Remote image URLs remain normal links; the exporter does not fetch or embed remote assets.
- Non-image local file links remain normal links; generic attachments are out of scope for Iteration 3.
- Supported image extensions: `.png`, `.jpg`, `.jpeg`, `.gif`, `.webp`, `.svg`.
- Relative paths resolve from the Org buffer file directory; unsaved buffers with relative image paths are hard errors.
- Absolute local paths are allowed for upload, but Confluence output does not expose the local path.
- Missing image files are hard errors before upload or page edit.
- Attachment filenames are content-hashed (`stem-<sha256-prefix>.ext`) during publish so existing Confluence attachments with the same source basename do not block re-publish.
- Duplicate source basenames are allowed when file contents differ because the hashed attachment filenames differ.
- `#+CAPTION` is rendered visibly as a simple italic paragraph after the image and also populates `ac:alt`.
- Images include numeric `ac:width` from `org-confluence-image-max-width` plus `ac:style="max-width: 100%; height: auto;"` so large images do not exceed the Confluence page width even when Confluence ignores CSS style.
- `#+NAME` is accepted for source-document identity but is not exported visibly.
- Width/alignment metadata is out of scope for Iteration 3; introduce a shared semantic `#+ATTR_IMAGE` only when needed.
- Documents with embedded images require `#+CONFLUENCE_PAGE_ID` in Iteration 3. Create-flow image support is deferred.
- Every publish uploads every referenced local image under its content-hashed attachment filename before editing the page.
- If a content-hashed attachment already exists, the duplicate-upload error is treated as already uploaded and publishing continues.
- Any attachment upload failure aborts before page edit.
- Temporary XHTML files are cleaned up on success and failure.
- Authoring UX includes `<im` / `M-x hub/org-insert-image-template`, inserting a caption plus image link template without `#+NAME`.

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
- Verify callout title extraction from `#+ATTR_CALLOUT`

### Iteration 3 — Images and Attachments

**Elements added:**
- Standalone plain local image links (`[[./img/foo.png]]`): upload as attachment then reference via Confluence image storage XHTML.
- Captions (`#+CAPTION`) rendered visibly and used as image alt text.
- `cfl attachment upload <page-id> <file>` dispatch before page edit.
- `<im` and `M-x hub/org-insert-image-template` authoring helper.

**Storage mapping:**

```xml
<ac:image ac:alt="Architecture overview"><ri:attachment ri:filename="architecture-overview.png"/></ac:image>
<p><em>Architecture overview</em></p>
```

Without a caption, omit `ac:alt` and the caption paragraph.

**API/publish layer additions:**
- `hub/confluence-api--attachment-upload-command (page-id filepath)` — builds `cfl attachment upload` command.
- Image asset collection from the Org AST for exported standalone local image links.
- Image path resolution: relative paths from `.org` file directory, absolute paths allowed.
- Preflight validation for missing files and duplicate basenames.
- Existing-page image publishing only; image documents without `#+CONFLUENCE_PAGE_ID` fail before create flow.

**Tests:**
- Attachment upload command construction.
- Image path resolution: relative paths, absolute paths, missing files, unsaved relative buffer.
- Duplicate basename detection.
- Plain standalone local image link exports to Confluence attachment image markup.
- Caption exports as `ac:alt` plus visible italic caption.
- Described local image links, remote image URLs, and non-image file links remain normal links.
- Publish command uploads all referenced images before page edit and cleans temporary XHTML on errors.
- `<im` authoring shortcut and `hub/org-insert-image-template` insertion behavior.

### Iteration 4 — Polish & Pull

**Iteration 4a export polish additions:**
- Footnote references (`[fn:1]`) export as superscript Confluence anchor links; definitions include Confluence anchor macros as targets and a clickable `↩` backlink.
- Definition lists (`- Term :: Definition`) export as Confluence-safe unordered lists with bold inline term labels.
- Publish diagnostics distinguish expected attachment reuse from blocking command failures.

**Iteration 4b Org export dispatch additions:**
- The Confluence backend appears in the normal Org export dispatcher (`C-c C-e C`).
- Dispatcher actions support publishing/updating, exporting to a temporary XHTML buffer, and exporting to a `.xhtml` file.
- Subtree publishing uses normal Org subtree export semantics and can read `CONFLUENCE_PAGE_ID` from the subtree property drawer.
- Subtree publishing only uploads image assets referenced by the selected subtree.

**Iteration 4c pull/import additions:**
- `hub/confluence-pull` fetches a page with `cfl page view <id> --raw --content-only` and opens the converted content in a new Org buffer.
- Import starts conservatively with common storage XHTML: headings, paragraphs, inline emphasis/code/links, simple ordered/unordered lists, nested lists, tables, and Confluence status chips.
- Pull defaults to the current buffer's `#+CONFLUENCE_PAGE_ID` when present and otherwise prompts for a page ID.

**Future additions:**
- Richer Confluence → Org conversion for tables, macros, images, footnotes, and nested lists.
- Better error handling: auth failures, missing page ID, and richer `cfl` failure categorization.

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
