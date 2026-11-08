---
domain: personal-apps/apps
status: draft
---

# Personal Apps

## Ubiquitous Language

- **App module**: Self-contained interactive workflow that is not part of foundational editing, writing, email, or development behavior.
- **Feed workflow**: Reading and managing subscriptions from inside Emacs.
- **Video workflow**: Media-oriented command flow integrated into the editor.

## Invariants

- App modules should not impose startup cost unless their UI is explicitly part of startup.
- App-specific keybindings should fit the established leader/localleader model.
- External services or media tools must be optional or clearly declared in the development/runtime environment.
- App modules should keep their state under managed runtime locations rather than the repo root.

## Audio Playback

- Org file links for MP3, WAV/WAVE, OGG/OGA, Opus, FLAC, M4A, AAC,
  AIFF/AIF, and WMA open in EMMS. Other file-opening rules remain unchanged.
- Opening an audio link loads EMMS on demand and passes the file path directly,
  including paths containing spaces.
- Playback commands seek backward ten seconds and forward thirty seconds in
  the current EMMS track. Playback and seeking depend on the active external
  player's format and seek support.

### Docked player

- `'mm` opens and focuses the EMMS player in the shared `context-panels` bottom
  slot. The player is docked: source-specific views temporarily replace it, and
  it returns when their source is no longer visible. Closing it never stops audio.
- The player shows playback state, the current track, elapsed/total track time,
  and a live progress bar, the EMMS playlist's total duration and upcoming tracks.
  Spotify and other media share that playlist. Missing durations remain explicitly
  unknown, including in playlist totals.
- The player is a read-only projection; rendering never modifies the EMMS queue.
  Progress updates do not select windows, move document point, or reopen hidden views.
- Normal-state player keys are `SPC` (play/pause), `c` (back ten seconds), `r`
  (forward thirty seconds), `t` (next track), `s` (previous track), and `q` (close).
  Outside the player the same playback actions use the `'m` prefix. `'mp` also
  toggles playback for compatibility.
- The integration requires dock-enabled `context-panels`; an older framework
  produces an actionable diagnostic instead of silently changing the layout.

### Music presentation

The [music presentation and access contract](music-ui.md) governs the native Hi-Fi
console, keyboard cockpit and consistent music entry points.

- `'m/` and the palette's `/` search the current EMMS queue using native completion.
  Candidates expose title, artist, album, duration and the numbered queue occurrence;
  the prompt states the queue-only coverage. Browsing and cancellation do not alter
  playback, focus, queue selection or capture consent.
- `RET` plays the selected existing occurrence without appending or rebuilding the
  queue, including when the same track occurs more than once. Queue changes during
  completion or a Stop acknowledgement reject obsolete candidates with an actionable
  retry message rather than choosing a different occurrence.

### Artwork loading

- Covers use retained Spotify album image locations, metadata already present in
  the loaded Liked Songs view, or known cover files beside a local track. Resolving
  a cover never requests Spotify metadata, reads authorization, or rewrites EMMS.
- Public image requests omit cookies and credentials, reject redirects and
  authentication challenges, and have bounded download time, bytes and cache size.
  Covers are cached only in memory, never in a persistent image collection.
- Painting reads cached data. Track changes and hiding or closing the view retire
  pending work; late replies cannot replace another track or reopen the player.
  Failed downloads retain a placeholder rather than retrying on clock ticks.
  `M-x hub/music-console-retry-artwork` explicitly retries the visible cover.

### Spectrum analyzer

- The analyzer is opt-in: `v` in the player or `'mv` outside toggles it. Opening,
  restoring, or reopening the player does not start audio capture.
- CAVA analyzes the mixed system output, including other audible applications,
  never the microphone. On macOS it uses an explicit CoreAudio system-output
  tap; missing support or permission fails without input-device fallback.
- Graphical Emacs displays frequency amplitudes as a native SVG spectrum in the
  same bottom view. Animation does not change playback, editing focus, or the queue.
- Capture and its animation stop when disabled, hidden, closed, or killed.
  Reopening the view requires explicit re-enabling. No audio or spectrum frames
  are recorded or stored; only a temporary, private CAVA configuration is written
  under `var/` and removed when capture stops.
- Default CAVA discovery uses the executable search path, then the configuration's
  existing `.devenv/profile/bin/cava`. GUI Emacs does not need a development-shell
  PATH. Explicit executable overrides remain authoritative; discovery never
  launches a shell, installs tools, or enables capture.
- Missing CAVA, unavailable SVG display, process failure, or malformed output
  leaves playback usable and reports how to recover. macOS audio-capture
  permission is controlled by the user in System Settings. CoreAudio taps require
  the lower **System Audio Recording Only** list under **Privacy & Security →
  Screen & System Audio Recording**. The upper screen-recording grant does not
  satisfy CAVA's audio-only permission check. The authorized app is Emacs or its
  responsible launching terminal; microphone access is not required.

## Spotify Library

- Opening Liked Songs shows a read-only library in the shared bottom slot.
  It does not start playback, change the EMMS queue, or start audio capture.
- Tracks show title, artist, album, and duration. Unavailable tracks and missing
  metadata remain explicit. The view distinguishes loaded rows from the total
  library size and loads additional pages only on request.
- Browser authorization uses PKCE, state validation, and a loopback-only callback.
  Library browsing requests only `user-library-read`; no client secret is used.
- The client ID is read from `pass` at runtime. Refresh credentials are encrypted
  in `pass`, associated with the client ID; access tokens remain in memory.
  Credentials, authorization codes, and tokens are not logged or written into
  repository state. Saved authorization supports reopening without signing in.
- Cancellation, expired authorization, network failures, and rate limits provide
  actionable recovery without changing playback or losing already loaded rows.
  Async responses do not steal focus, reopen closed views, or replace newer data.

### Spotify playback

- `a` appends a playable Liked Songs row to the current EMMS playlist without
  interrupting playback. `RET` appends and starts that occurrence, preserving the
  rest of the queue. Sorting and pagination do not change the selected track.
  Spotify URI metadata comes from the retained Spotify response; constructing
  those tracks excludes EMMS's generic file-info initializer. Local media retain
  their normal metadata initialization.
- EMMS is the sole playlist authority. Spotify URI tracks use a dedicated receiver;
  other media retain the configured free-software players. A running browser and
  its licensed media runtime are allowed. Neither the Spotify desktop app nor a
  desktop-app fallback is used.
- EMMS owns next, previous, reordering and repeat behavior across mixed media.
  The browser receiver requests one URI only. Unexpected tracks, queued successors,
  shuffle or receiver repetition stop playback instead of owning a second queue.
  Genuine completion advances EMMS once; failures, cancellation and stale events
  never skip a track or advance a newer occurrence, including repeated URIs.
  Failure diagnostics identify the playback stage, HTTP status or a known receiver
  error kind. A recognized audio-key refusal is distinguished without retaining
  its raw message. Diagnostics never retain raw responses, event messages or credentials.
- Pause, resume and fixed-offset seeking use the current EMMS backend. Backend
  progress and duration feed the normal EMMS player as integer seconds, truncating
  receiver milliseconds at that boundary; hiding that view neither
  stops music nor prevents natural advancement. Spectrum capture remains opt-in.
- An explicit Spotify Play request prepares the Chrome receiver when needed,
  without requiring a separate connection command. Opening the player or library
  remains passive. The requested occurrence waits for explicit browser audio
  activation; stopping, changing selection or editing the queue cancels that
  waiting request. Audio activation never revives a cancelled request.
- `M-x hub/spotify-browser-connect` reuses a live receiver, including one awaiting
  audio activation, without opening another tab or rereading credentials. If no
  live receiver exists it opens Chrome for authorization and audio activation.
  `M-x hub/spotify-browser-disconnect` releases only the owned receiver. Playback
  refresh authorization is encrypted in its own `pass` entry, separate from the
  read-only library grant and associated with the public client ID. Access tokens
  remain in memory and renew automatically; the receiver has no fixed hourly
  session cutoff. Reconnecting reuses saved authorization until it expires or is
  revoked, while a new browser receiver still requires explicit audio activation.
  Unavailable credentials or failed renewal never advance the queue or expose
  credential payloads. A prefix argument to connect requests fresh authorization.
  Obsolete receiver tabs are visibly inactive with disabled actions; ordinary
  receiver retirement is not presented as an SDK failure.
- The loopback HTTP and WebSocket channels require a random session capability.
  The receiver owns one Spotify occurrence at a time, not a playlist. Commands and
  callbacks carry occurrence and control identities; older acknowledgements and
  completion candidates cannot affect a newer occurrence or pending pause/seek.
- Completion requires advancing playback followed by a recent near-end observation,
  a matching paused reset to zero, matching playback history, no queued successor,
  and confirmation from the SDK. Clocks or arbitrary pauses alone never advance EMMS.
  Acknowledged stopping precedes another backend; an unconfirmed stop blocks the
  transition. Cancelling an unstarted load disconnects its SDK instance and requires
  explicit audio activation again. Lost authorization, page closure and playback
  errors stop the current entry rather than skip it. Credentials and raw SDK
  messages are never logged.

### Browser SDK compatibility check

- The SDK check opens Chrome for a single selected track independently of EMMS.
  It does not read, select, clear or advance the EMMS playlist or start capture.
- Browser playback uses explicit PKCE authorization for SDK streaming, basic
  profile/email and playback control. It uses the existing public client ID, not
  the client secret, and does not overwrite saved library authorization.
- Access credentials and PKCE material are held in memory. Refresh credentials
  are not persisted by the compatibility check. Its loopback session expires after
  ten minutes; private API requests require an unguessable session capability.
  Spotify credentials never appear in page URLs, logs or generated files.
- An explicit browser click activates audio and requests the selected track on
  the SDK's own device. A preview pauses after fifteen seconds. Safe SDK event
  names and HTTP statuses identify errors; raw service messages are not displayed.
- With a prefix argument, the check starts normal playback first. Only a separate
  click after the listener hears music seeks to the final eight seconds; initial
  SDK metadata does not establish audible playback. A bounded, memory-only trace
  contains positions, pause flags, track-match booleans and known SDK error names.
  The check stops on a subsequent pause, reset, unexpected track or safety timeout,
  including when the listener does not initiate the seek.
  The trace contains no track identifiers, credentials or raw SDK payloads. Observed
  transitions are not treated as authoritative completion or used to advance EMMS.

## Integration Notes

These workflows conform to the interactive experience domain.  They should remain isolated enough that failures or missing external tools do not degrade core editing, writing, or startup behavior.
