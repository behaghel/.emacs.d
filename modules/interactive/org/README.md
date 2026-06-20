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
- Org uses `,c` as its context prefix: normal-state `,cc` opens the context panel, while visual-state `,cc` creates sidecar comments from visual selections; `,cf` creates a page/footer sidecar comment; `,cr` creates a local reply under the active remote-linked comment; `,cA` reanchors a stale comment to the visual selection; `,cC` opens the sidecar comments file when it exists; `,cO` opens the current page in Confluence; `,cl` opens the active remote-linked comment in Confluence; `,cj` jumps to the active sidecar heading; `,ce` edits the active sidecar comment body narrowed to its subtree; `,cx` deletes the active source or sidecar comment after confirmation; `,cmo`, `,cmt`, and `,cmr` update the active comment status.
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
- `,cM` toggles `hub/org-context-panel-mode`, which refreshes after source-buffer
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
- `,cO` or `M-x hub/confluence-open-page` opens the current page in Confluence.
- `,cl` or `M-x hub/confluence-comment-open-current` opens the current remote-linked sidecar/source comment in Confluence using `focusedCommentId`.
- `,cr` or `M-x hub/org-comment-reply-create` creates a local reply child heading under the active remote-linked comment and jumps to its body for editing; push it afterwards with `C-c C-c` in the comments sidecar or `M-x hub/confluence-comment-push-current`.
- `C-c C-c` in `*.comments.org` sidecars pushes the current local footer, inline, or reply comment to Confluence.
- `,cP` or `M-x hub/org-page-comments-open` opens the bottom page-context panel explicitly, using the same card renderer and actions as the main context panel.
- `,cC` opens the current Org file's sidecar comments file when it exists; when no sidecar exists it reports that in the minibuffer and leaves the source buffer unchanged.
- Sidecar headings are readable summaries like `* OPEN Alice · “selected target” — Comment body preview` for anchored/inline comments and `* OPEN Page · Alice — Comment body preview` for page comments; IDs and sync metadata stay in properties.
- Confluence reply conversations are stored as nested child headings under the root comment, for example `** Reply · Alice · 2026-06-10 14:31 — Reply preview`; root headings show a derived `[N replies]` marker immediately after the TODO status.
- `M-x hub/org-comment-refresh-sidecar-headings` recomputes existing sidecar headings from properties, body text, and the Confluence people directory while preserving TODO states and body/properties.
- `M-x hub/org-comment-compact-sidecar-metadata` removes obsolete or derivable sidecar properties such as old target hashes, duplicate parent IDs, default storage body format, raw remote target JSON, explicit remote-present state, and duplicate local author/date fields on remote-linked comments.
- `M-x hub/org-comment-anchor-imported-inline-comments` tries to anchor imported Confluence inline comments by exact normalized target-text matching; unique matches get normal anchor metadata, while missing or ambiguous matches are recorded with `HUB_COMMENT_ANCHOR_STATE`.
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
- Compact properties currently include local author/creation metadata for local comments, target offsets, line/column hints, target text, remote IDs, source system, sync kind, remote audit fields, and only non-default sync health metadata.

### Confluence mapping direction

- Confluence footer comments map to page-level sidecar entries without a source
  region, or to a future explicit page-comment section.
- Confluence inline comments map to region-targeted sidecar entries.
- Local sidecar comments carry canonical `HUB_COMMENT_AUTHOR` and `HUB_COMMENT_CREATED_AT` metadata. Remote comments preserve remote identity and audit fields without duplicating them into canonical local author fields, for example `HUB_COMMENT_REMOTE_ID`, `HUB_COMMENT_SOURCE`, `HUB_COMMENT_REMOTE_AUTHOR_ID`, `HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME`, `HUB_COMMENT_REMOTE_CREATED_AT`, and future sync/version properties.
- Confluence people mappings live in plain Org `confluence-people.org` files. Lookup prefers a local file next to the source document, then the global file under `org-directory`; imports conservatively cache encountered account IDs and any display names Confluence already provides.
- Mark the current user in a people entry with `HUB_PERSON_ME: t`; context/page-context cards highlight that author name using the current-author face. Local entries without the marker do not suppress a global marker for the same account.
- `M-x hub/confluence-people-mark-current-user` fetches the authenticated Confluence user, caches it in the global people file, and marks it with `HUB_PERSON_ME: t` without overwriting manual display names.
- `M-x hub/confluence-people-resolve` explicitly resolves unresolved cached account IDs through Confluence's bulk user lookup API and updates local/global people files without overwriting manual display names.
- Remote comment body should be normalized to Org where supported.  Unsupported
  Confluence storage/ADF fragments should be preserved rather than silently
  flattened.

### Page sync

- `M-x hub/confluence-sync-current` synchronizes the current Org page and sidecar comments: page content first, then remote comment import, then local draft/local-edit comment push.
- `M-x hub/confluence-sync-page-current` synchronizes only the current Org buffer's main page content.
- Sync metadata is stored as top-level Org keywords: `CONFLUENCE_PAGE_VERSION`, `CONFLUENCE_PAGE_STORAGE_HASH`, `CONFLUENCE_LOCAL_ORG_HASH`, and `CONFLUENCE_PAGE_LAST_SYNCED_AT`.
- The local Org hash excludes `CONFLUENCE_*` metadata lines, so updating sync metadata does not make the document dirty for the next sync.
- If the remote version changed and the local body did not, sync fast-forwards by importing remote storage to Org and replacing the local body while preserving leading keywords.
- If the remote version is unchanged and local content changed, sync publishes local Org and refreshes sync metadata from Confluence.  This uses the same inline-comment preflight guard as `hub/confluence-publish`, so active anchored Confluence inline comments can block a sync push before the page body is updated.
- If both local and remote changed, sync leaves the source unchanged and opens `*Org Confluence Sync Conflict*` with local and remote sections for manual resolution.

### Safe Confluence publishing

- `M-x hub/confluence-publish` checks the live Confluence inline comments for every page it would update, including recursive subpages, before uploading page storage.
- The preflight imports/updates inline comment sidecars first, then opens `*Org Confluence Publish Preflight*` when it finds active anchored inline comments, dangling comments, or remote-missing comments relevant to the publish.
- Active anchored inline comments block publishing because replacing Confluence storage can orphan their markers and show them remotely as deleted-content comments.
- Dangling or `HUB_COMMENT_REMOTE_STATE: missing` inline comments are reported as non-blocking because they are already detached remotely; publishing cannot make their anchor state worse.
- Report entries use clickable `org-comment:` links.  `TAB` moves to the next report link; use `C-c C-o` or `M-x org-open-at-point` to open the link if Evil normal-state `RET` is not configured to follow Org links.
- `C-u M-x hub/confluence-publish` and `M-x hub/confluence-publish-force` still run the preflight/import/report path, then continue despite blockers.  Use force only when accepting possible inline-anchor loss.

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
   - `M-x hub/confluence-comment-import` imports footer and inline comments; imported inline comments remain unanchored and use the existing stale-comment UX until manually reanchored.

4. **Push/create slice**
   - Push selected local sidecar comments to Confluence.
   - Store returned remote IDs in sidecar properties.
   - Start with footer/page comments for page-level local comments, then inline comments for anchored region comments.
   - Treat Confluence `resolutionStatus` as the remote workflow source of truth after a comment becomes remote-linked.

5. **Inline sync slice**
   - Map local region anchors to Confluence inline comment targets.
   - Handle reanchoring and unresolved remote anchor failures explicitly.
   - Keep human workflow status separate from sync/anchor health.

6. **Conflict and round-trip hardening**
   - Detect divergent local/remote edits.
   - Add explicit conflict status/properties without overloading `OPEN/TODO`.
   - Add round-trip tests for supported comment body markup.

## Spec: Confluence Comment Remote Reconciliation

### Problem

Repeated Confluence comment imports should be trustworthy.  A sidecar comment that
was once linked to Confluence must not silently disappear locally when the remote
comment is deleted, hidden, or temporarily absent.  At the same time, remote
resolution status should drive local workflow state for remote-linked comments
without destroying the local `TODO` "I am working on this" marker except when the
remote thread is resolved.

### Context

- Source Org files stay clean; all comment sync state lives in `*.comments.org`
  sidecars and display-only UI projections.
- Sidecar root headings use `OPEN TODO | RESOLVED`; replies are nested child
  headings without TODO keywords.
- A comment becomes Confluence-controlled when it has `HUB_COMMENT_REMOTE_ID`,
  regardless of whether it was imported from Confluence or created locally first
  and later pushed/synced.
- Existing Confluence imports are scoped by comment kind: footer/page comments
  and inline comments can be fetched independently, while broad import fetches
  both.
- Existing context panel cards are intentionally compact and already use `⚠` for
  stale/unanchored comments.

### Decisions

| Decision | Choice | Rationale |
| --- | --- | --- |
| Remote control signal | Presence of `HUB_COMMENT_REMOTE_ID` | Covers imported comments and local comments that later gained a remote identity. |
| Remote state metadata | Omit `HUB_COMMENT_REMOTE_STATE` for present comments; use `HUB_COMMENT_REMOTE_STATE: missing`, `HUB_COMMENT_REMOTE_LAST_SEEN_AT`, and `HUB_COMMENT_REMOTE_MISSING_AT` only when needed | Keeps sidecars compact while preserving missing-sync health without overloading workflow status. |
| Missing timestamp semantics | `HUB_COMMENT_REMOTE_MISSING_AT` records the first missing time and is not overwritten by later missing imports | Preserves "missing since" history. |
| Present timestamp semantics | `HUB_COMMENT_REMOTE_LAST_SEEN_AT` updates whenever the remote comment is seen | Enables diagnostics and later stale-sync checks. |
| Missing reconciliation scope | Only reconcile kinds that were completely and successfully fetched | Prevents partial fetches, network errors, or one-kind imports from producing false missing markers. |
| Reply reconciliation | Track present/missing for replies only when that parent thread's children were successfully fetched | Keeps conversation history auditable without treating failed child fetches as deletions. |
| Root workflow derivation | Root TODO keyword is derived only from known Confluence `resolutionStatus` or explicit boolean `resolved`, with the `TODO` exception below | Remote-linked root comment status reflects Confluence resolution source of truth; Confluence `status` such as `current` is content lifecycle metadata, not resolution. |
| Local `TODO` exception | Local `TODO` remains `TODO` when remote is open/unresolved; local `TODO` becomes `RESOLVED` when remote is resolved and is reported | Preserves local active-work intent unless Confluence confirms closure. |
| Reopening rule | Local `RESOLVED` becomes `OPEN` when remote is open/unresolved | A closed local thread must reopen when Confluence reopens or remains open. |
| Unknown remote resolution | Preserve local TODO keyword and store no misleading resolution status | Confluence payload shape may vary; unknown is not a status change.  A payload with only `status` and no `resolutionStatus` is unknown for workflow purposes. |
| Remote missing workflow | Preserve current local TODO keyword when a remote-linked comment is missing | Missing/deleted/permission-filtered is not the same as resolved. |
| Local-only comments | Comments without `HUB_COMMENT_REMOTE_ID` are untouched by remote reconciliation | Local authoring comments must not be affected by Confluence imports. |
| Context panel overview marker | For remote-missing comments, show only a warning sign and strike the status token | Space is limited; detailed text belongs in focused view. |
| Context panel focused marker | Show explicit `⚠ remote missing since <date>` in detail/focused view | The full state should be discoverable when reading the card. |
| Sidecar metadata UX | Property drawers in comments sidecars fold by default and are refolded after sync/update commands; obsolete derivable metadata can be removed with `hub/org-comment-compact-sidecar-metadata` | Remote metadata is verbose and should not dominate sidecar reading. |
| Run reporting | Omit zero-count report lines; show a diagnostic buffer only for TODO-impacting events | Keeps routine imports concise while surfacing important workflow changes. |

### Acceptance Criteria

- [ ] AC-1: Given a remote-linked root comment is seen during a complete import
      for its kind, when reconciliation runs, then the sidecar has an updated
      `HUB_COMMENT_REMOTE_LAST_SEEN_AT`, no `HUB_COMMENT_REMOTE_STATE`, and no
      `HUB_COMMENT_REMOTE_MISSING_AT`.
- [ ] AC-2: Given a remote-linked root comment is absent from a complete import
      for its kind, when reconciliation runs, then the sidecar is kept, its TODO
      keyword is unchanged, `HUB_COMMENT_REMOTE_STATE: missing` is stored, and
      `HUB_COMMENT_REMOTE_MISSING_AT` records the first missing timestamp.
- [ ] AC-3: Given a previously missing remote-linked comment reappears, when it
      is imported successfully, then `HUB_COMMENT_REMOTE_STATE` and
      `HUB_COMMENT_REMOTE_MISSING_AT` are removed, and the run report includes a
      non-zero "present again" count.
- [ ] AC-4: Given an import command fetches only footer comments, when
      reconciliation runs, then inline comments are not marked missing; the same
      applies symmetrically for inline-only imports.
- [ ] AC-5: Given a reply under a remote-linked root is absent from a complete
      successful child fetch for that root, when reconciliation runs, then that
      reply child heading is marked remote-missing and kept without a TODO
      keyword.
- [ ] AC-6: Given a child/reply fetch fails or is skipped for a parent, when
      reconciliation runs, then existing replies under that parent are not marked
      missing.
- [ ] AC-7: Given a remote-linked root comment has known remote resolved status,
      when sync runs, then local `OPEN` or `TODO` becomes `RESOLVED`, and any
      local `TODO` changed this way is included in the final diagnostic report.
- [ ] AC-8: Given a remote-linked root comment has known remote open/unresolved
      status, when sync runs, then local `OPEN` remains `OPEN`, local `TODO`
      remains `TODO`, and local `RESOLVED` becomes `OPEN`.
- [ ] AC-9: Given remote resolution status is absent or unrecognized in the
      Confluence payload, when sync runs, then the local TODO keyword is
      preserved and no misleading remote resolution status is stored.
- [ ] AC-10: Given a local-only comment has no `HUB_COMMENT_REMOTE_ID`, when any
      Confluence import/reconciliation runs, then the comment's TODO keyword and
      properties remain untouched by remote present/missing/status logic.
- [ ] AC-11: Given a remote-missing comment appears in the context panel overview,
      when rendered, then it shows a warning sign and a struck-through status
      token without the words "remote missing".
- [ ] AC-12: Given a remote-missing comment is focused in the context panel, when
      rendered, then it shows explicit text `remote missing since <date>`.
- [ ] AC-13: Given a comments sidecar is opened or updated by comment sync/update
      commands, when displayed, then `:PROPERTIES:` drawers are folded by default
      while headings and bodies remain readable.
- [ ] AC-14: Given an import produces zero for a report category, when the final
      report is shown, then that category is omitted; if TODO-impacting events
      occurred, a diagnostic buffer lists the affected remote IDs/headings.

### Invariants

- Source Org files must not be modified by comment sync, missing detection, or
  context-panel display.
- Remote sync health (`present`, `missing`, anchor state, last seen) remains
  metadata, not a new TODO keyword.
- Replies remain child headings without TODO keywords; root comments control
  workflow state.
- Tests must use mocked Confluence payloads and must not perform network calls.
- Unknown Confluence payload fields must fail safe by preserving local workflow
  state.

### Scope

**May modify:**

- `lisp/hub-org-comments.el`
- `modules/interactive/org/comments.el`
- `modules/interactive/org/context-panel.el`
- `modules/interactive/org/confluence.el`
- `packages/org-confluence/org-confluence-api.el`
- `packages/org-confluence/org-confluence-commands.el`
- `test/authoring/org/org-comments-test.el`
- `test/authoring/org/org-context-panel-test.el`
- `test/publishing/confluence/org-confluence-api-test.el`
- `modules/interactive/org/README.md`

**Must not modify:**

- Source `.org` documents being commented on.
- `private/setup.el` or generated agent configuration.
- Confluence write-back semantics for resolving/reopening comments remotely.
- Ambiguous anchor completion/preview behavior beyond preserving compatibility.

### Verification Plan

| Criterion | Method | Automated? |
| --- | --- | --- |
| AC-1, AC-2, AC-3 | ERT tests for sidecar reconciliation over mocked remote ID sets and timestamps | Yes |
| AC-4 | ERT tests importing only footer or only inline comments with opposite-kind remote-linked entries present | Yes |
| AC-5, AC-6 | ERT tests for reply child reconciliation with successful vs failed child fetch metadata | Yes |
| AC-7, AC-8, AC-9, AC-10 | ERT tests for the remote resolution to local TODO mapping matrix | Yes |
| AC-11, AC-12 | Context panel rendering tests for overview and focused remote-missing cards | Yes |
| AC-13 | ERT or batch UI-oriented test confirming comments sidecar property drawers are hidden after open/update helpers | Yes |
| AC-14 | ERT tests for import report formatting and TODO-impact diagnostic buffer creation | Yes |
| All touched Elisp | `devenv -q shell -- ./scripts/elisp-format`, `./scripts/elisp-parse`, selected ERT suites | Yes |

### References

- `lisp/hub-org-comments.el`
- `modules/interactive/org/comments.el`
- `modules/interactive/org/context-panel.el`
- `modules/interactive/org/confluence.el`
- `packages/org-confluence/org-confluence-api.el`
- `packages/org-confluence/org-confluence-commands.el`
- `test/authoring/org/org-comments-test.el`
- `test/authoring/org/org-context-panel-test.el`
- `test/publishing/confluence/org-confluence-api-test.el`

## Spec: Org Sidecar Comments Push to Confluence

### Problem

Authors can now import and reconcile Confluence comments into sidecar files, but
local comments still cannot become Confluence comments.  The next workflow needs
to push selected local sidecar threads to Confluence while keeping source Org
files clean, preserving local sidecar history, and turning successfully pushed
comments into normal remote-linked comments governed by the existing sync model.

### Current Readiness

- REST configuration and authenticated request helpers exist.
- Footer and inline remote comments can be listed, imported, reconciled, and
  diagnosed without network calls in tests.
- Sidecar comments have compact metadata and folded drawers.
- Remote-linked comments are identified by `HUB_COMMENT_REMOTE_ID` and preserve
  `HUB_COMMENT_SOURCE` so future sources such as Google Docs can coexist.
- Confluence `resolutionStatus`, not Confluence content `status`, controls local
  workflow derivation for remote-linked root comments.

### Open Ambiguities To Grill Before Implementation

- Which local comments are eligible for push: only current comment, all local
  comments, only `OPEN`/`TODO`, page comments only first, or anchored inline too?
- What exactly represents a local page/footer comment before it has a remote ID?
- How should inline Confluence target creation work from local Org offsets and
  target text, and what should happen when Confluence rejects the target?
- Should local comment bodies be converted from Org to Confluence storage XHTML
  for create, or sent as plain paragraphs in the first slice?
- After push succeeds, should local status remain as-is until the next import, or
  should push immediately stamp remote status metadata and derive local TODO?
- What confirmation/reporting UX should protect against accidentally pushing many
  local comments?
- Should replies ever be pushed in this phase, or are root comments only in
  scope?

### Initial Recommended Plan

1. **Footer/page create MVP**
   - Push one selected local page/footer sidecar comment to Confluence from inside
     the `.comments.org` sidecar via `M-x hub/confluence-comment-push-current`.
   - A local page/footer comment is explicitly marked with
     `HUB_COMMENT_SYNC_KIND: footer` before it has a remote ID; absence of target
     metadata is not inferred as push intent.
   - Store `HUB_COMMENT_REMOTE_ID`, `HUB_COMMENT_SOURCE: confluence`,
     `HUB_COMMENT_SYNC_KIND: footer`, remote audit fields returned by the API,
     and `HUB_COMMENT_REMOTE_LAST_SEEN_AT`.
   - Convert the sidecar Org body to conservative Confluence storage XHTML using
     escaped plain paragraphs only.
   - Leave source Org unchanged and preserve the sidecar body, canonical local
     author/creation metadata, and current `OPEN`/`TODO` keyword.

2. **Push reporting and idempotency**
   - Refuse to push comments that already have `HUB_COMMENT_REMOTE_ID` unless an
     explicit update command exists later.
   - Report created, skipped, and failed comments without zero-count noise.

3. **Inline create MVP**
   - Push one selected anchored local region comment as a Confluence inline
     comment using existing target metadata.
   - New local region comments are explicitly marked with
     `HUB_COMMENT_SYNC_KIND: inline`; older target-bearing comments without a
     sync kind may still be treated as inline for push compatibility.
   - Create payloads use Confluence REST v2 `inlineCommentProperties` with
     `textSelection`, `textSelectionMatchCount`, and `textSelectionMatchIndex`.
   - Inline push requires a valid local anchor; stale anchors are refused before
     any network call.
   - Store local target match diagnostics with `HUB_COMMENT_TARGET_MATCH_COUNT`
     and `HUB_COMMENT_TARGET_MATCH_INDEX`.
   - If Confluence accepts the create response but does not return inline anchor
     confirmation properties, store `HUB_COMMENT_REMOTE_ANCHOR_STATE:
     unconfirmed`; if it reports `resolutionStatus: dangling`, store
     `HUB_COMMENT_REMOTE_ANCHOR_STATE: dangling`.
   - If Confluence rejects the inline target, keep the local comment unlinked and
     report the failure without data loss.

4. **Batch push triage**
   - Add completion/confirmation for pushing multiple eligible local comments.
   - Prefer explicit user selection over broad automatic push.

5. **Round-trip verification**
   - After push, a subsequent import should see the remote comment as present,
     avoid duplication, and reconcile resolution status according to the existing
     rules.

### Non-goals For The First Push Work

- Running the push command from the source Org buffer.
- Pushing local comments that are already remote-linked or locally `RESOLVED`.
- Pushing replies or creating remote reply threads.
- Updating existing remote comment bodies.
- Resolving or reopening Confluence comments from local status changes.
- Solving advanced marker-ref based inline targeting.
- Supporting Google Docs push; `HUB_COMMENT_SOURCE` is preserved only to keep the
  model source-extensible.

### Future Architecture Note

`lisp/hub-org-comments.el` currently acts as the generic Org sidecar comment
model while Confluence-specific code lives in `packages/org-confluence/`.  The
model should remain source-agnostic (`HUB_COMMENT_SOURCE` distinguishes remote
adapters).  A future iteration should extract or move the generic sidecar comment
model under `packages/`, likely as an `org-comments` or `org-sidecar-comments`
package.  This refactor is deliberately out of scope for the first Confluence
push slice.

## Integration Notes

This domain consumes shared prose/key helpers and conforms to the interactive command surface.  Export-specific specs may live beside the export code or in the existing spec tree when they describe broader authoring contracts.
