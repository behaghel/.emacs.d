# Org → Confluence Export & Publish System

## Context

The Org export layer in this repo (`modules/org/export-latex.el`, providing `org/export-latex`) handles Org → LaTeX → PDF for the `veriff` and `hub-article` families. This spec adds a parallel path: Org → Confluence Storage Format (XHTML) → publish to Confluence Cloud via the `cfl` CLI.

The motivating document is `~/Documents/org/veriff-bloom.org`, a strategic Veriff document originally authored in Confluence then migrated to Org for offline editing. The immediate need is to push the edited Org version back to the existing Confluence page for team review. More broadly, this enables an Org-native authoring workflow for Confluence-published documents.

## Target Architecture

The current package lives under `packages/org-confluence/` with canonical
`org-confluence-*` modules:

| File | Responsibility | Layer |
| --- | --- | --- |
| `org-confluence.el` | Package facade, minor mode, dispatch, and integration entrypoint | Package entrypoint |
| `org-confluence-export.el` | Pure Org → Confluence Storage Format (XHTML) export backend | Export backend |
| `org-confluence-import.el` | Confluence Storage XHTML → Org import and page pull | Import/pull |
| `org-confluence-publish.el` | Publish/create orchestration, asset upload, and inline-comment preflight | Publish |
| `org-confluence-sync.el` | Page sync and page/comment sync orchestration | Sync |
| `org-confluence-api.el` | `cfl`/REST command construction, config lookup, metadata helpers | API boundary |
| `org-confluence-comments-*.el` | Confluence-backed `org-comments` import/open/push/sync/backend adapters | Comment integration |
| `org-confluence-sync-status-*.el` | Sync status cache/collect/render/display/actions/source marker | Status UI |

Former short wrapper files such as `api.el`, `commands.el`, and `export.el` are
not part of the target architecture.

### Layering Rules

- `org-confluence-export.el` must not shell out. It receives an Org document and returns XHTML.
- `org-confluence-api.el` owns Confluence CLI/REST command construction and configuration lookup.
- `org-confluence-publish.el` composes export, API, asset upload, and inline-comment preservation.
- `org-confluence-sync.el` owns page sync safety decisions and coordinates page/comment sync.
- `org-confluence-comments-*` modules integrate with `org-comments` without depending on personal modules.
- The user interacts with the `org-confluence-*` command namespace.

## Accepted Decisions (from grilling)

### Scope

- **Primary direction:** Org → Confluence (push). Confluence → Org (pull) is deferred to a future iteration.
- **Content format:** Confluence Storage Format (XHTML), written to a temporary `.xhtml` file and sent via `cfl page edit --file <file> --storage` or `cfl page create --file <file> --storage`.
- **Page identity:** Document-level `#+CONFLUENCE_PAGE_ID: <id>` keyword for existing pages. Optional `#+CONFLUENCE_SPACE: <key>` for new page creation, falling back to `org-confluence-api-default-space` when configured.
- **Update semantics:** Full replacement. Confluence versions the page automatically on each update. No append/prepend mode.
- **Publish safety:** When a buffer records `#+CONFLUENCE_PAGE_VERSION`, normal publish checks the remote page version before uploading assets or editing the page. If Confluence changed since the last recorded sync, publish refuses and directs the user to run page sync or explicitly force publish. Successful whole-buffer publish records sync metadata, including `#+CONFLUENCE_LOCAL_ORG_HASH`, so a subsequent pull can prove the local file is safe to refresh.
- **Table mapping:** Simple `<table>` with `<th>` for header rows, `<td>` for body rows. No branded Confluence styling or custom column widths.
- **Date mapping:** Org active and inactive date timestamps export to Confluence native `<time datetime="YYYY-MM-DD" />` elements. Confluence time elements import as inactive Org dates (`[YYYY-MM-DD]`) to preserve the calendar date without implying scheduled Org semantics.

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
- `org-confluence-publish` — export current buffer and update existing page (requires `#+CONFLUENCE_PAGE_ID`); refuses when recorded sync metadata shows the remote page changed since the last pull
- `org-confluence-publish-dwim` — if `#+CONFLUENCE_PAGE_ID` present, update; otherwise create using `#+CONFLUENCE_SPACE` or `org-confluence-api-default-space`, then insert created page metadata into the buffer for future publishes

**API layer functions:**
- `org-confluence-api--page-update-command (page-id &optional file-path)` — builds `cfl page edit <id> --storage`
- `org-confluence-api--page-create-command (space title &optional file-path parent-id)` — builds `cfl page create`
- `org-confluence-api--page-id-from-buffer` — reads `#+CONFLUENCE_PAGE_ID`
- `org-confluence-api--space-from-buffer` — reads `#+CONFLUENCE_SPACE`

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
- `org-confluence-api--attachment-upload-command (page-id filepath)` — builds `cfl attachment upload` command.
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
- `org-confluence-pull` fetches a page with `cfl page view <id> --raw --content-only` and opens the converted content in a new Org buffer.
- Import starts conservatively with common storage XHTML: headings, paragraphs, inline emphasis/code/links, simple ordered/unordered lists, nested lists, tables, Confluence status chips, Confluence emoji fallbacks, and panel-like macros as semantic callouts.
- Pull defaults to the current buffer's `#+CONFLUENCE_PAGE_ID` when present and otherwise prompts for a page ID.
- Interactive `org-confluence-pull` includes comments when called with a prefix argument.
- Sync status exposes direct content actions separately from comment actions: `y` syncs page content, `f` fetches page content, `p` publishes, `Y` syncs page plus comments, `F` fetches page plus comments, and `i` imports comments only.
- `org-confluence-pull-to-file` accepts `:include-comments t`; when set, page body import writes only the main Org file and remote Confluence comments are imported into the conventionally adjacent `.comments.org` sidecar. The return plist includes `:comments-file`, `:comments-count`, `:footer-comments-count`, and `:inline-comments-count`.

**Current acceptance status:**
- Manual DWIM publish/create/open acceptance is confirmed on real Confluence pages.
- Image create flow, metadata insertion, default personal space, page opening, duplicate image references, and soft-wrap paragraph normalization are accepted for current needs.

**Future additions:**
- Add round-trip tests that prove supported storage lands on its feet across pull → publish/export → pull/import normalization. These should compare canonical Org forms rather than byte-identical source, because import/export may normalize harmless whitespace and metadata ordering.
- Extend Confluence → Org conversion when real pages expose gaps, especially image import, footnote import, unknown macro preservation, and complex table cells containing block content.
- For Confluence-only or complex constructs that cannot be represented cleanly in native Org, preserve raw storage in `#+begin_confluence_markup` / `#+end_confluence_markup` blocks instead of flattening content.
- Better error handling: auth failures, missing page ID, and richer `cfl` failure categorization.

## File Headers & Naming

Canonical package files use the `org-confluence-*` namespace and provide the
matching feature.  Examples:

```elisp
;;; org-confluence-export.el --- Org to Confluence Storage Format backend -*- lexical-binding: t; -*-
(provide 'org-confluence-export)

;;; org-confluence-api.el --- cfl shell wrappers for Confluence export -*- lexical-binding: t; -*-
(provide 'org-confluence-api)

;;; org-confluence-publish.el --- Publish Org buffers to Confluence -*- lexical-binding: t; -*-
(provide 'org-confluence-publish)
```

## Cross-Links

- Existing LaTeX export system: `modules/org/export-latex.el` (provides `org/export-latex`)
- Existing export tests: `test/org-export-test.el`
- Existing test (full latex PDF): `test/org-latex-pdf-export-test.el`
- Module architecture: `modules/org/` is on `load-path` unconditionally (like `modules/lang/`), unlike `modules/interactive/` which is gated behind interactive mode
- Test helpers: `test/test-helpers.el`
- `cfl` CLI docs: `cfl page edit --help`, `cfl page create --help`, `cfl attachment upload --help`
- Confluence Cloud REST API v2 comment endpoints (for future comment support)

## Confluence Comment Support (Exploratory)

`cfl` does not support comments, but the Confluence Cloud REST API v2 does. Comments are external review resources, not primary page body content, so the local representation should be a colocated plain-Org sidecar named like `article.comments.org` for `article.org`.

| Endpoint | Method | Purpose |
| --- | --- | --- |
| `/wiki/api/v2/pages/{pageId}/footer-comments` | GET | List page/footer comments |
| `/wiki/api/v2/pages/{pageId}/inline-comments` | GET | List region-targeted inline comments |
| `/wiki/api/v2/footer-comments` | POST | Create a page/footer comment |
| `/wiki/api/v2/inline-comments` | POST | Create a region-targeted inline comment |
| `/wiki/api/v2/inline-comments/{commentId}` | PUT/DELETE | Update or delete an inline comment |

Read requests should include `body-format=storage` or `body-format=atlas_doc_format` when comment bodies are needed. Inline comments carry target metadata such as original selection text and marker references; locally that belongs in `HUB_COMMENT_*` properties in the sidecar file, not in `HUB_NOTE_KIND` footnotes.

The `cfl` auth config file (`~/.config/cfl/config.yml`) contains the API token, cloud ID, and user email. A future enhancement could parse this file and use `url-retrieve` or `curl` to access the comment endpoints directly, reusing the same credentials.

Possible future commands:
- `org-confluence-comments-list` — display page comments in an Emacs buffer
- `org-confluence-comments-push-current` — push the current sidecar Org comment to Confluence
- `org-confluence-comments-import` — fetch comments into the colocated `*.comments.org` sidecar

This is deferred — no iteration plan until the core publish path is stable.

## Non-Goals

- This spec does not require byte-identical bidirectional sync. Round-trip fidelity means supported content returns to an equivalent canonical Org representation and unsupported content is preserved rather than silently flattened.
- This spec does not cover ADF (Atlassian Document Format) output. Storage Format XHTML is the target.
- This spec does not cover bidirectional sync or conflict resolution.
- This spec does not cover Confluence page moves, re-parenting, or space management.
- This spec does not require a separate `devenv.nix` change in this repo — `cfl` is expected to be on PATH from the Veriff workspace devenv or equivalent.

## Acceptance Signals

- A user can run `org-confluence-publish` from any Org buffer with `#+CONFLUENCE_PAGE_ID` and see the content live on Confluence within seconds.
- The Confluence page renders headings, paragraphs, bold, italic, links, and lists correctly after Iteration 1.
- Tables, callouts, blockquotes, and code blocks are added by Iteration 2 without breaking Iteration 1 output.
- Images appear inline by Iteration 3.
- A reader can trace every decision in this spec back to a specific question answered during the grilling session.
