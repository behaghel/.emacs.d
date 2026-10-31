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
  a live progress bar, the playlist's total duration, and upcoming tracks.
  Missing durations remain explicitly unknown, including in playlist totals.
- The player is a read-only projection; rendering never modifies the EMMS queue.
  Progress updates do not select windows, move document point, or reopen hidden views.
- Normal-state player keys are `SPC` (play/pause), `c` (back ten seconds), `r`
  (forward thirty seconds), `t` (next track), `s` (previous track), and `q` (close).
  Outside the player the same playback actions use the `'m` prefix. `'mp` also
  toggles playback for compatibility.
- The integration requires dock-enabled `context-panels`; an older framework
  produces an actionable diagnostic instead of silently changing the layout.

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
- Missing CAVA, unavailable SVG display, process failure, or malformed output
  leaves playback usable and reports how to recover. macOS audio-capture
  permission is controlled by the user in System Settings.

## Integration Notes

These workflows conform to the interactive experience domain.  They should remain isolated enough that failures or missing external tools do not degrade core editing, writing, or startup behavior.
