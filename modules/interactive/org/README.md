---
domain: authoring
status: draft
last-reviewed: 2026-06-08
---

# Knowledge and Writing

## Ubiquitous Language

- **Semantic layer**: Author-facing Org contract describing meaning independently from output styling.
- **Class family**: A related set of LaTeX/PDF export classes and variants.
- **Specimen**: Tracked Org input used to verify export behavior.
- **Authoring shortcut**: Interactive helper that inserts or transforms writing markup.
- **Publishing workflow**: Export or synchronization path from Org content to an external medium.

## Invariants

- Author-facing semantics must stay separate from class-specific visual styling.
- Export behavior should be testable from tracked specimens and textual assertions before relying on visual inspection.
- Writing helpers should not make batch loads depend on interactive-only packages unless guarded.
- Machine-specific paths belong in private overrides or defcustoms, not hard-coded shared behavior.
- Generated PDFs, TeX files, screenshots, and visual diff artifacts belong under runtime output locations, not tracked golden files.
- Marginalia authoring uses native Org footnotes as the canonical source; the panel is a read-only projection and must jump back to footnote definitions for edits.
- Marginalia stacking preserves source order and may push later notes downward when anchors are close.

## Marginalia Contract

- Ordinary Org footnotes in article-oriented authoring buffers default to `sidenote` marginalia.
- Optional footnote definition properties use repo-owned `HUB_NOTE_*` keys; `HUB_NOTE_KIND: footnote` forces a traditional bottom footnote while ordinary footnotes remain sidenotes.
- Review comments are not marginalia footnote kinds; local comments live in colocated sidecar Org files named like `article.comments.org` using compact `OPEN TODO | RESOLVED` Org TODO states.
- Region comments require an active region, keep source Org clean, and render in the context panel when their stored offsets still match the selected text.
- Comment overlays are enabled for Org buffers by `hub/org-comment-overlays-mode`, while `]c` and `[c` navigate to next and previous sidecar comments and open the context panel.
- Org uses `,c` as its context prefix: `,cc` creates sidecar comments from visual selections; `,cA` reanchors a stale comment to the visual selection; `,cm` opens the context panel; `,cj` jumps to the active sidecar heading; `,ce` edits the active sidecar comment body narrowed to its subtree; `,cO`, `,cT`, `,cR`, and `,cs` update the active comment status.
- Stale comments whose source anchor no longer matches are shown as unanchored warning cards in the context panel instead of disappearing silently; they do not receive source overlays.
- The interactive context panel is explicitly opened or toggled with a buffer-local mode; it follows the selected Org buffer while visible and closes when selection moves to a non-Org buffer.
- Opening the context panel docks visually filled prose toward the panel and renders compact icon/status-chip cards; when point is inside a comment target, the panel focuses that comment so it can be read in full.
- Inline authoring shortcuts are `<fn` for the default note/sidenote, `<ft` for a forced traditional footnote, and `<ff` for a colon-separated forced traditional footnote.

## Context Panel User Manual

The Org context panel is a right-side read-only surface for authoring context that
should remain visible near the text it annotates.  It currently shows native Org
marginalia footnotes and sidecar review comments.

### Opening, focus, and lifecycle

- `,cm` opens or refreshes the context panel for the current Org buffer.
- `,cM` toggles `hub/org-context-panel-mode`, which refreshes after source-buffer
  commands.
- The panel follows the selected Org buffer while visible.
- The panel closes automatically when selection moves to a non-Org buffer.
- `q` closes the panel when point is inside it.
- `M-c`, `M-t`, `M-s`, and `M-r` move focus left/up/down/right consistently, so
  `M-r` moves from the source window to the panel and `M-c` returns left.
- The panel preserves its point across refreshes and focus changes where possible.

### Reading items

- `✣` marks a normal sidenote/marginalia item.
- `†` marks a forced traditional footnote (`HUB_NOTE_KIND: footnote`).
- `💬` marks an anchored sidecar review comment.
- `⚠` marks a stale sidecar comment whose stored source anchor no longer matches.
- Comment status chips use `OPEN`, `TODO`, and `RESOLVED` from the sidecar Org
  heading TODO keyword.
- Overview cards are intentionally compact.  When source point is inside a
  comment target, the panel focuses that comment and shows the full wrapped body.

### Navigation and actions inside the panel

Normal-state bindings inside the panel:

| Key | Action |
| --- | --- |
| `RET` | Jump to the item's primary target.  For anchored comments this jumps to the source region; for stale comments this jumps to the sidecar heading; for marginalia this jumps to the footnote definition. |
| `e` | Edit the backing sidecar entry for comment cards, narrowed to the comment body.  Marginalia has no sidecar entry and reports an error. |
| `]c` | Move to the next context item in the panel, wrapping at the end. |
| `[c` | Move to the previous context item in the panel, wrapping at the beginning. |
| `q` | Close the panel. |

### Source-buffer comment workflow

- In visual state, `,cc` creates a sidecar comment for the selected region and
  opens the sidecar body for editing.
- In visual state, `,cA` reanchors a stale comment to the selected region.  If
  exactly one stale comment exists it is selected automatically; if multiple
  stale comments exist, a completion picker shows status, target text, and the
  beginning of the comment body.
- In normal state, `RET` on an anchored commented region jumps to the related
  card in the context panel; outside comments it falls back to Evil's normal RET
  behavior.
- `]c` / `[c` in the source buffer navigate anchored comments and open/refresh
  the panel.
- `,cj` jumps from the active commented region to its sidecar heading.
- `,ce` edits the active sidecar comment body narrowed to its subtree.
- `,cO`, `,cT`, `,cR`, and `,cs` mark the active comment `OPEN`, `TODO`,
  `RESOLVED`, or cycle status.

### Marginalia authoring

Native Org footnotes are the canonical source for authorial marginalia:

- `<fn` inserts a default note/sidenote.
- `<ft` inserts a traditional footnote with `HUB_NOTE_KIND: footnote` metadata.
- `<ff` inserts a colon-separated traditional footnote definition:

```org
[fn:x]:
:PROPERTIES:
:HUB_NOTE_KIND: footnote
:END:
Body.
```

Accepted `HUB_NOTE_KIND` values are `sidenote` and `footnote`.  Missing or
unknown values fall back to the configured default, currently `sidenote`.

## Confluence Comment Sync Plan

The local sidecar comment system is the planned bridge to Confluence comments.
The source Org file remains clean; sidecar entries become the local, inspectable
representation for remote review threads.

### Current local model

- Source `article.org` maps mechanically to `article.comments.org`.
- Sidecars are plain Org files with `#+todo: OPEN TODO | RESOLVED`.
- One sidecar heading represents one region-targeted comment.
- Human workflow status is the Org heading keyword:
  - `OPEN`: active discussion.
  - `TODO`: requires author/user action.
  - `RESOLVED`: closed.
- Anchor/sync health is derived metadata, not a TODO keyword.  A stale comment is
  one whose stored source anchor no longer validates.
- Compact properties currently include target offsets, line/column hints, target
  text, and target hash.

### Confluence mapping direction

- Confluence footer comments map to page-level sidecar entries without a source
  region, or to a future explicit page-comment section.
- Confluence inline comments map to region-targeted sidecar entries.
- Local sidecar comments should carry remote identifiers once synced, for
  example future `HUB_COMMENT_REMOTE_ID`, `HUB_COMMENT_SOURCE`, and sync/version
  properties.
- Remote comment body should be normalized to Org where supported.  Unsupported
  Confluence storage/ADF fragments should be preserved rather than silently
  flattened.

### API foundation

`cfl` does not support comments directly, but its config can provide the Cloud
credentials needed for Confluence REST API v2 calls.  The relevant endpoint
families are:

- `GET /wiki/api/v2/pages/{pageId}/footer-comments`
- `GET /wiki/api/v2/pages/{pageId}/inline-comments`
- `POST /wiki/api/v2/footer-comments`
- `POST /wiki/api/v2/inline-comments`
- `PUT /wiki/api/v2/inline-comments/{comment-id}`
- `DELETE /wiki/api/v2/inline-comments/{comment-id}`

Read requests should ask for comment bodies using `body-format=storage` or
`body-format=atlas_doc_format`.

### Proposed implementation slices

1. **Read-only API/config slice**
   - Parse the `cfl` config for cloud ID/base URL, email, and API token.
   - Add low-level authenticated request helpers.
   - Add tests with mocked process/url calls; no network in tests.

2. **Pull/list slice**
   - Given `#+CONFLUENCE_PAGE_ID`, fetch footer and inline comments.
   - Render a diagnostic buffer first, before writing sidecars.
   - Preserve raw remote IDs and body format metadata.

3. **Sidecar import slice**
   - Merge fetched remote comments into `*.comments.org`.
   - Preserve existing local edits and statuses where possible.
   - Mark missing/changed anchors as stale rather than deleting local entries.

4. **Push/create slice**
   - Push selected local sidecar comments to Confluence.
   - Store returned remote IDs in sidecar properties.
   - Start with footer comments if inline anchoring requires additional remote
     target metadata.

5. **Inline sync slice**
   - Map local region anchors to Confluence inline comment targets.
   - Handle reanchoring and unresolved remote anchor failures explicitly.
   - Keep human workflow status separate from sync/anchor health.

6. **Conflict and round-trip hardening**
   - Detect divergent local/remote edits.
   - Add explicit conflict status/properties without overloading `OPEN/TODO`.
   - Add round-trip tests for supported comment body markup.

## Integration Notes

This domain consumes shared prose/key helpers and conforms to the interactive command surface.  Export-specific specs may live beside the export code or in the existing spec tree when they describe broader authoring contracts.
