---
domain: personal-apps/apps
status: approved
---

# Music presentation and access

The native music interface combines a compact Hi-Fi console with a keyboard
cockpit. It presents the existing [EMMS playback workflow](README.md#docked-player)
and [Spotify library](README.md#spotify-library); it is not another playback engine
or playlist authority.

## Access and focus

| Binding | Action |
|---------|--------|
| `'mm` | Open or focus the docked player |
| `'ml` | Open or focus Spotify Liked Songs |
| `'m+` | Open the current editable EMMS queue |
| `'m/` | Search music |
| `'m?` | Show the music action palette |

These entry points are available without first loading the player. Repeated
invocation reuses the corresponding view and preserves its browsing position and
filter when its data remains valid. Opening a view never starts playback, changes
the queue or enables spectrum capture. Existing player and prefixed transport
bindings retain their meanings. In the player, `l` opens or focuses Liked Songs
with the same behavior as `'ml`; `?` opens the action palette.

Music views use `m` for Player, `l` for Liked Songs, `+` for Queue, `/` for Search,
`?` for Actions and `q` for Close. Liked Songs uses `g+` for Load more and `gr` for
Refresh. Query typing remains ordinary minibuffer input.

## Shared panel chrome

Context-panel providers share native tabs and a fixed bottom footer. Tabs expose
all available provider views, not just music. `C-Tab` and `C-S-Tab` move to the
next and previous view; `C-c v` chooses a view by name through native completion.
These bindings are scoped to managed panel windows and do not replace bindings
in ordinary copies of adopted buffers. The Queue tab adopts the actual editable
EMMS playlist without clearing, copying, reordering or selecting a track. Switching views preserves their
browsing state and never starts playback or capture. Painting the tabs does not
initialize a provider, fetch data or authorize an account.

The footer stays at the panel's bottom edge independently of content length,
scrolling or sorting. Each provider supplies its relevant key hints and optional
metadata. Music supplies whole-queue count/duration or loaded-library coverage;
the framework owns the shared layout. Metadata is right-aligned when it fits and
wraps onto another footer line otherwise. The footer retains the player's
background-only surface, accent keys, neutral labels, theme-derived primary
emphasis, 16-pixel outer separation and 8-pixel padding.

## Hi-Fi console

- The current track has a clear visual hierarchy: title, artist, album, source,
  playback state and time. Source attribution is distinguishable from connection
  status. Browsing selection and the playing occurrence are visually distinct.
- Progress, queue duration and upcoming entries reflect EMMS state. Missing
  metadata and partial duration totals remain explicit; the view does not invent
  artwork, waveforms or playback progress. Upcoming track titles are distinct from
  their quieter artist/source lines. Service links remain on individual entries;
  Spotify content shares a section-level brand mark rather than repeating one
  beside each artist. The smaller whole-queue count and duration share the transport
  footer, right-aligned when space permits and on their own footer line otherwise.
- Artwork remains intact with source attribution and service links where required.
  A deliberate placeholder represents absent artwork. Controls do not cover art.
- Typography, spacing, progress and spectrum share a visual vocabulary derived from
  Emacs faces. Music-specific styling does not replace the editor's global theme or
  fonts. The current title is large, the artist is normal-sized, and album, source
  and secondary details are smaller. Playback state and actionable warnings remain
  prominent. Text remains navigable; SVG supplements rather than replaces buffer text.
- Waiting-for-audio, playing, paused, seeking, stopped, empty and reconnect-needed states pair an
  icon with meaningful text, not colour alone. Unavailable font glyphs use an
  ASCII marker while retaining the full label. Library authorization and Spotify audio readiness are separate
  states. Healthy receiver readiness does not add a player badge; the position reads
  "Track N of M". Missing browser audio adds "Enable audio in Chrome" or
  "Reconnect Spotify audio" beside that position rather than replacing it. Readiness means the receiver can accept commands, not
  that sound is audible. Recovery actions preserve the queue and reuse the existing
  connection commands without exposing credentials or raw service responses.
- While a browser seek awaits acknowledgement, progress uses only the receiver's
  last confirmed position; absent confirmation remains explicitly unknown.
  The time caption remains stable without transient confirmation annotations.
  The transport icon and Pause/Resume action retain the actual EMMS playing or
  paused state even while the displayed text is Seeking. A later control's acknowledgement covers preceding serial controls;
  an older acknowledgement does not clear a newer pending state.
- The spectrum retains its [explicit-consent lifecycle](README.md#spectrum-analyzer)
  with "Spectrum off" while inactive and "Spectrum · all computer audio" while active.
  The footer owns the keyboard hint rather than duplicating it beside the spectrum.
  Redrawing visible information does not move point,
  steal focus, reopen dismissed views or perform network work repeatedly.
- Narrow layouts remain usable without horizontal scrolling, reducing secondary
  content before hiding essential track, state and control information.

## Shared music rows

Queue occurrences and retained Liked Songs metadata project into read-only rows
with title, artist, album, duration and provenance. Display labels are not action
identity. Queue rows retain their exact playlist occurrence, including duplicate
track objects; library rows retain their original loaded record and generation.
A stale source is rejected before mutation. Formatting never constructs a playable
track, accesses credentials or fetches metadata.

Liked Songs and Vertico search use the same native column schema: playback marker,
title, artist, album, right-aligned time, and provenance. Source flags show
`Queue #N` for a queue-only occurrence, `Queue #N ♥` for an occurrence also saved
in loaded Liked Songs, and `♥` for a library-only result. Only queue positions have
ranks. The heart has readable saved-status help and a `Liked` fallback when the
font cannot display it; it never denotes playback or browsing selection.
Column widths adapt to the
available view width; long values are ellipsized rather than pushing later fields
out of alignment. Search still matches full retained metadata and acts on retained
identities, never the truncated display text. Native reverse ordering is preserved.

Music roles use the active Modus palette directly, including palette overrides:
title is `blue` and semibold, artist is `cyan`, album is `magenta`, time is `yellow`,
and provenance/separators use `fg-dim`. Enabling a theme updates these local faces
without fetching or changing the global theme faces. Generic themed faces are the
fallback when no Modus theme is active. Green remains a transport/activity accent. Fixed-pitch
fields share character metrics. These are local music faces, not global font/theme
replacements. Refreshing the listing updates already loaded rows without fetching,
reopening, moving the browsing row or authorizing. Native selection backgrounds
and completion-match highlighting remain owned by the frontend; playback markers
remain independent of field colours and browsing selection.

Playing and paused markers describe transport state, not the highlighted browsing
candidate. A queue marker identifies only the active occurrence; a library marker
identifies the matching Spotify URI. Waiting for audio is distinct from playing.
Seeking retains the underlying playing or paused marker. Stopped rows have no
playing marker. Library status redraws preserve focus and browsing position and
never authorize, fetch, reopen a view or start playback.

## Keyboard cockpit

- Search combines the current queue and already loaded Liked Songs through native
  source-grouped completion. It does not load library configuration or fetch a page
  merely to discover candidates; an empty queue does not prevent searching a loaded
  library. Unavailable library records remain identifiable and reject playback or
  enqueue before any queue mutation.
- Search states its source coverage and loaded portion. Moving among candidates or
  inspecting their metadata does not play or enqueue them. Cancelling restores the
  prior editing context without changing playback.
- Search input accepts ordinary text and spaces. Transport keys do not intercept
  minibuffer typing. Playback and enqueue actions are explicit and distinguishable.
- Search combines a loaded Liked record with matching queue occurrences by exact
  Spotify track URI, never by title. Each queued occurrence stays distinct and
  carries Queue + Liked provenance; the redundant library-only hit is absent.
  Different recordings and local files remain distinct. Loaded coverage counts
  retained library records, including those represented by combined results.
- Playing a queue or combined candidate selects that existing occurrence without
  appending. Playing a library-only candidate uses the existing append-and-play
  behavior. Enqueueing a library-only or combined candidate appends exactly one
  copy without interruption. A queue-only candidate does not gain a library action.
  Each action validates the retained source it acts on: the exact queue occurrence
  for Play, or the loaded library record and generation for append.
- The existing Embark action menu exposes Play (`p`) and Append (`a`) for music
  search results. These actions use the same retained row identities and source
  validation as RET and M-RET, never parsing a label or looking up a replacement
  by title or URI. Opening, inspecting or cancelling the menu is passive. A target
  whose identity has been stripped or whose acted-on source is stale is rejected
  before queue mutation or playback interruption.
- The palette exposes navigation, transport, spectrum and connection actions with
  discoverable keys and current availability. It does not create a second set of
  playback semantics. Closing it returns to the previous context.
- Liked Songs retains its existing read-only library, pagination and queue-action
  semantics. The music interface does not imply write access to Spotify or a
  complete Spotify catalogue search.
