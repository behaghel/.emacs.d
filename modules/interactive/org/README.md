---
domain: authoring/org
status: draft
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
- Writing helpers should not make batch loads or isolated authoring tests depend on optional interactive packages unless guarded.
- Machine-specific paths belong in private overrides or defcustoms, not hard-coded shared behavior.
- Generated PDFs, TeX files, screenshots, and visual diff artifacts belong under runtime output locations, not tracked golden files.
- Marginalia authoring uses native Org footnotes as the canonical source; the panel is a read-only projection and must jump back to footnote definitions for edits.
- Marginalia stacking preserves source order and may push later notes downward when anchors are close.

## Marginalia Contract

- Ordinary Org footnotes in article-oriented authoring buffers default to `sidenote` marginalia.
- Optional footnote definition properties use repo-owned `HUB_NOTE_*` keys; `HUB_NOTE_KIND: footnote` forces a traditional bottom footnote while ordinary footnotes remain sidenotes.
- Review comments are not marginalia footnote kinds; local comments live in colocated sidecar Org files named like `article.comments.org` using compact `OPEN TODO | RESOLVED` Org TODO states.
- Region comments require an active region, keep source Org clean, and render in the context panel when their stored offsets still match the selected text.
- Comment overlays are enabled for Org buffers by `org-comments-mode`, while `]c` and `[c` navigate to next and previous visible context-panel items, including comments and AI review comments.
- Org uses `,c` as its context prefix: normal-state `,cc` toggles the context panel, while visual-state `,cc` creates sidecar comments from visual selections; `,cf` creates a page/footer sidecar comment; `,cr` creates a local reply under the active remote-linked comment; `,cA` reanchors a stale comment to the visual selection; `,cC` opens the sidecar comments file when it exists; `,cO` opens the current page in Confluence; `,cl` opens the active remote-linked comment in Confluence; `,cj` jumps to the active sidecar heading; `,ce` edits the active sidecar comment body narrowed to its subtree; `,cx` deletes the active source or sidecar comment after confirmation; `,cmo`, `,cmt`, and `,cmr` update the active comment status.
- Stale comments whose source anchor no longer matches are shown as unanchored warning cards in the context panel instead of disappearing silently; they do not receive source overlays.
- Page/footer comments are shown as a display-only `[N PAGE comments]` marker below leading Org metadata and can be read in a bottom page-comments window without modifying the source file.
- The interactive context panel is explicitly opened or toggled with a buffer-local mode; it follows the selected Org buffer while visible and closes when selection moves to a non-Org buffer.
- Opening the context panel docks visually filled prose toward the panel and renders compact icon/status-chip cards; when point is inside a comment target, the panel focuses that comment so it can be read in full.
- Inline authoring shortcuts are `<fn` for the default note/sidenote, `<ft` for a forced traditional footnote, and `<ff` for a colon-separated forced traditional footnote.

## Context Panel User Manual

The Org context panel is a right-side read-only surface for authoring context that
should remain visible near the text it annotates.  It currently shows native Org
marginalia footnotes and sidecar review comments.

### Opening, focus, and lifecycle

- Normal-state `,cc` opens or refreshes the context panel for the current Org buffer.
- `,cM` toggles `hub/context-panels-mode`, which refreshes after source-buffer
  commands.
- The panel follows the selected Org buffer while visible.
- The panel closes automatically when selection moves to a non-Org buffer.
- `q` closes the panel when point is inside it.
- `M-c`, `M-t`, `M-s`, and `M-r` move focus left/down/up/right consistently, so
  `M-t` moves from the source window down into the page-context panel and `M-s` moves back up.
- The panel preserves its point across refreshes and focus changes where possible.

### Reading items

- `✣` marks a normal sidenote/marginalia item.
- `†` marks a forced traditional footnote (`HUB_NOTE_KIND: footnote`).
- `💬` marks an anchored sidecar review comment.
- `👆` marks a page-level comment card.
- `⚠` marks a stale sidecar comment whose stored source anchor no longer matches.
- Comment status chips use `OPEN`, `TODO`, and `RESOLVED` from the sidecar Org
  heading TODO keyword.  Comment cards end their top line with an emoji-only sync
  badge: `✍️` for unpublished drafts, `🔗` for remote-linked comments, `⚠` for
  missing or dangling remote comments, and `❓` for unconfirmed inline anchors.
- Overview cards are intentionally compact.  When source point is inside a
  comment target, the panel focuses that comment and shows the full wrapped body.
  Focused side-panel rows show the full thread in place: root body first, then
  wrapped `↳` replies with the same badge, author/date, and sync-state layout for
  Confluence, Google Docs, and local sidecar comments.

### Navigation and actions inside the panel

Normal-state bindings inside the panel:

| Key | Action |
| --- | --- |
| `RET` | Jump to the item's primary target.  For anchored comments this jumps to the source region; for stale comments this jumps to the sidecar heading; for marginalia this jumps to the footnote definition. |
| `e` | Edit the backing sidecar entry for comment cards, narrowed to the comment body.  Marginalia has no sidecar entry and reports an error. |
| `p` | Open the bottom page-context panel for the source buffer. |
| `o` | Open the current remote-linked comment in Confluence. |
| `+` | Create a local reply under the current remote-linked comment and jump to its body. |
| `mo` / `mt` / `mr` | Mark the backing sidecar comment `OPEN`, `TODO`, or `RESOLVED`. |
| `C-c C-c` | Push the current draft comment or reply to Confluence without visiting the sidecar. |
| `x` | Delete the backing sidecar comment after confirmation.  Marginalia has no sidecar entry and reports an error. |
| `zz` | Reset composable context filters.  The default shows normal, resolved, and remote-missing/deleted comments. |
| `za` | Toggle actionable-only filtering. |
| `zm` | Toggle current-user-only filtering. |
| `zd` | Toggle draft/local-edit-only filtering. |
| `zr` | Toggle showing resolved comments. |
| `zx` | Toggle showing remote-missing/deleted comments. |
| `z?` | Show active filter status. |
| `?` | Toggle a small help window below the panel. |

When filters differ from defaults, panels show a compact header with active filters, item counts, and the `zz` reset hint. If a reply matches a filter but its root does not, the root thread remains visible as conversation context while non-matching replies are hidden.
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
- A display-only `[N PAGE comments]` marker below leading Org metadata opens the bottom page-context panel with `RET` or mouse-1. `]c` and `[c` include the marker as a keyboard navigation stop.
- `,cf` or `M-x hub/org-page-comment-create` creates a local page/footer sidecar comment and jumps to its body for editing.
- `,cO` or `M-x org-confluence-open-page` opens the current page in Confluence.
- `,cl` or `M-x org-confluence-comments-open-current` opens the current remote-linked sidecar/source comment in Confluence using `focusedCommentId`.
- `,cr` or `M-x hub/org-comment-reply-create` creates a local reply child heading under the active remote-linked comment and jumps to its body for editing; push it afterwards with `C-c C-c` in the comments sidecar or `M-x org-confluence-comments-push-current`.
- `C-c C-c` in `*.comments.org` sidecars pushes the current local footer, inline, or reply comment to Confluence.
- `,cP` or `M-x hub/org-page-comments-open` opens the bottom page-context panel explicitly, using the same card renderer and actions as the main context panel.
- `,cC` opens the current Org file's sidecar comments file when it exists; when no sidecar exists it reports that in the minibuffer and leaves the source buffer unchanged.
- Sidecar headings are readable summaries like `* OPEN Alice · “selected target” — Comment body preview` for anchored/inline comments and `* OPEN Page · Alice — Comment body preview` for page comments; IDs and sync metadata stay in properties.
- Confluence reply conversations are stored as nested child headings under the root comment, for example `** Reply · Alice · 2026-06-10 14:31 — Reply preview`; root headings show a derived `[N replies]` marker immediately after the TODO status.
- `M-x org-comments-refresh-sidecar-headings-command` recomputes existing sidecar headings from properties, body text, and the Confluence people directory while preserving TODO states and body/properties.
- `M-x org-comments-compact-sidecar-metadata` removes obsolete or derivable sidecar properties such as old target hashes, duplicate parent IDs, default storage body format, raw remote target JSON, explicit remote-present state, and duplicate local author/date fields on remote-linked comments.
- `M-x org-comments-anchor-imported-inline-comments` tries to anchor imported Confluence inline comments by exact normalized target-text matching; unique matches get normal anchor metadata, while missing or ambiguous matches are recorded with `ORG_COMMENTS_ANCHOR_STATE`.
- `,cj` jumps from the active commented region to its sidecar heading.
- `,ce` edits the active sidecar comment body narrowed to its subtree.
- `,cx` deletes the active source comment, or the current sidecar comment heading when visiting a `.comments.org` file, after confirmation.
- `,cmo`, `,cmt`, and `,cmr` mark the active source or sidecar comment `OPEN`,
  `TODO`, or `RESOLVED`.
- `org-comment:` links are Org-native links to sidecar comments through their source file, for example `[[org-comment:article.org::local-20260617T230746-56518c][Comment]]`.  Opening a footer comment link opens the page-context panel and selects the row; opening an anchored inline or reply link opens the source context panel and selects the root/reply row; stale, unanchored, or otherwise unrenderable comments fall back to the sidecar heading.

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
