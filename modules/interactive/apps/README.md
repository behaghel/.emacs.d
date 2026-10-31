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

## Integration Notes

These workflows conform to the interactive experience domain.  They should remain isolated enough that failures or missing external tools do not degrade core editing, writing, or startup behavior.
