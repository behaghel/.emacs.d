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
| `'mq` | Open the current editable EMMS queue |
| `'m/` | Search music |
| `'m?` | Show the music action palette |

These entry points are available without first loading the player. Repeated
invocation reuses the corresponding view and preserves its browsing position and
filter when its data remains valid. Opening a view never starts playback, changes
the queue or enables spectrum capture. Existing player and prefixed transport
bindings retain their meanings. In the player, `l` opens or focuses Liked Songs
with the same behavior as `'ml`; `?` opens the action palette.

## Hi-Fi console

- The current track has a clear visual hierarchy: title, artist, album, source,
  playback state and time. Source attribution is distinguishable from connection
  status. Browsing selection and the playing occurrence are visually distinct.
- Progress, queue duration and upcoming entries reflect EMMS state. Missing
  metadata and partial duration totals remain explicit; the view does not invent
  artwork, waveforms or playback progress.
- Artwork remains intact with source attribution and service links where required.
  A deliberate placeholder represents absent artwork. Controls do not cover art.
- Typography, spacing, progress and spectrum share a visual vocabulary derived from
  Emacs faces. Music-specific styling does not replace the editor's global theme or
  fonts. Text remains navigable; SVG supplements rather than replaces buffer text.
- Waiting-for-audio, playing, paused, seeking, stopped, empty and reconnect-needed states pair an
  icon with meaningful text, not colour alone. Unavailable font glyphs use an
  ASCII marker while retaining the full label. Library authorization and Spotify audio readiness are separate
  states. Recovery actions preserve the queue and reuse the existing connection
  commands without exposing credentials or raw service responses.
- While a browser seek awaits acknowledgement, progress uses only the receiver's
  last confirmed position; absent confirmation remains explicitly unknown.
  The time caption remains stable without transient confirmation annotations.
  The transport icon and Pause/Resume action retain the actual EMMS playing or
  paused state even while the displayed text is Seeking. A later control's acknowledgement covers preceding serial controls;
  an older acknowledgement does not clear a newer pending state.
- The spectrum retains its [explicit-consent lifecycle](README.md#spectrum-analyzer)
  and is labelled as system mix. Redrawing visible information does not move point,
  steal focus, reopen dismissed views or perform network work repeatedly.
- Narrow layouts remain usable without horizontal scrolling, reducing secondary
  content before hiding essential track, state and control information.

## Keyboard cockpit

- Search states its source coverage and loaded portion. Moving among candidates or
  inspecting their metadata does not play or enqueue them. Cancelling restores the
  prior editing context without changing playback.
- Search input accepts ordinary text and spaces. Transport keys do not intercept
  minibuffer typing. Playback and enqueue actions are explicit and distinguishable.
- Playing a queue candidate selects that existing occurrence; playing a library
  candidate uses the existing append-and-play behavior. Enqueueing a library
  candidate uses the existing append-without-interruption behavior.
- The palette exposes navigation, transport, spectrum and connection actions with
  discoverable keys and current availability. It does not create a second set of
  playback semantics. Closing it returns to the previous context.
- Liked Songs retains its existing read-only library, pagination and queue-action
  semantics. The music interface does not imply write access to Spotify or a
  complete Spotify catalogue search.
