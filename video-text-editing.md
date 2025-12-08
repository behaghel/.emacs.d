# Text-Driven Video Editing Concept

This document outlines a potential workflow for editing spoken-word videos by manipulating text instead of cutting timelines manually. It summarises the file formats we expect to generate, how Emacs can be used to rearrange content, and the inputs a future `video-text-edit` CLI would consume.

## 1. Goals (Updated)

- Support **multi-source projects** where several raw clips can be edited and compiled into a single deliverable.
- Provide transcripts with **per-word timestamps** that survive round-tripping through Emacs or other editors.
- Choose a storage format that is **tool-agnostic**, portable, and easy to transform into FFmpeg filter chains (JSON-based rather than Org/Markdown).
- Enable advanced metadata (b-roll, speakers, tags) while keeping the base format readable by external tooling.

## 2. Proposed Transcript Manifest (TJM)

We introduce a JSON-based "Textual Join Manifest" (`.tjm.json`) inspired by subtitle exchange formats and the structures `subed` already understands. The manifest is an object with two sections:

```jsonc
{
  "version": 1,
  "sources": [
    { "id": "clipA", "file": "raw/DJI_20251003135305_0326_D.MP4" },
    { "id": "clipB", "file": "raw/DJI_20251003142011_0327_D.MP4" }
  ],
  "segments": [
    {
      "id": "seg-001",
      "source": "clipA",
      "start": 0.000,
      "end": 6.210,
      "speaker": "Hubert",
      "text": "This week we launched feature X.",
      "words": [
        { "start": 0.000, "end": 0.450, "token": "This" },
        { "start": 0.450, "end": 0.720, "token": "week" },
        { "start": 0.720, "end": 1.120, "token": "we" },
        { "start": 1.120, "end": 2.420, "token": "launched" },
        ...
      ],
      "broll": null,
      "tags": ["keep"],
      "notes": "First announcement"
    },
    {
      "id": "marker-001",
      "kind": "marker",
      "title": "Launch Recap",
      "source": "clipA",
      "start": 6.210
    },
    {
      "id": "seg-002",
      "source": "clipA",
      "start": 6.210,
      "end": 12.418,
      "text": "It went smoothly.",
      "words": [...],
      "broll": {
        "file": "broll/shipping-animation.mp4",
        "mode": "overlay",             // overlay | replace | picture-in-picture
        "audio": "source",              // source | mute | broll
        "still": true,                   // optional flag: treat file as still image
        "continue": true                 // optional flag: continue overlay from previous segment
      }
    },
    {
      "id": "seg-003",
      "source": "clipB",
      "start": 4.010,
      "end": 8.300,
      "text": "Customer feedback has been excellent.",
      "words": [...]
    }
  ]
}
```

Characteristics:

- **`sources`**: list of clips referenced by the project. A single TJM may reference any number of files; segments can jump between sources and the exported ordering follows the `segments` array.
- **`segments`**: each entry maps to a timeline unit. Media segments include `source`/`start`/`end`, `speaker`, text, per-word timing, tags, notes, and optional `broll`. Marker segments set `"kind": "marker"`, provide a `title`, and may carry optional `source`/`start` hints so they behave as section headings without affecting playback.
- **`words`**: array capturing per-token timings; mandatory for editing fidelity. Editors can delete words by removing entries or altering their `token`. The segment’s `start`/`end` timestamps should collapse to the first/last remaining word so playback and exports honour the edit. The `text` field is derived from `words` but stored explicitly for convenience.
- **`broll`**: optional override describing how to replace or overlay the video for the segment (keep source audio by default). When `continue` is present and truthy, adjacent segments with the same clip/mode are treated as a single continuous overlay by the exporter.
- Future extensions can add `transitions`, `emphasis`, or `linkage` fields without breaking consumers if we keep the manifest versioned.

## 3. Editing Workflow in Emacs

We plan to ship a custom major mode (building on ideas from `subed`) that understands TJM files:

- Presents each `segment` as a top-level heading with commands to expand/hide the per-word list. Navigation keys jump to next/previous segment or word.
- Playback integration: using `subed` APIs or `mpv` via D-Bus, pressing `RET` plays the active segment; `M-p`/`M-n` jump between words.
- Editing actions:
  - Delete segment/word: remove the JSON object; mode keeps IDs stable or regenerates them on save.
  - Insert segment: split an existing segment by editing `words` and adjusting `start`/`end`.
  - Merge adjacent segments: with point on the first segment, invoke `C-c m` (`J` in Evil) to span overlays like b-roll across the combined text.
  - Insert marker segments: `M-j` (or `C-c C-m`) prompts for a title, optional `source`, and optional `start` so you can outline sections without altering media order.
  - Set b-roll metadata: choose files (relative paths welcome), switch overlay mode, flag still images (renderer synthesises a looping frame), and enter optional `start_offset`/`duration` values (seconds or `HH:MM:SS.mmm`) to begin partway through the clip without importing its audio.
  - Toggle b-roll continuity: `; B` flips the `continue` flag so an overlay can span multiple consecutive segments.
  - Reorder: drag segments around; the new ordering directly controls the final video timeline.
  - Tagging: quick keybinds to toggle `tags` (e.g., `C-c t b` to toggle `broll`).
  - Compile: `C-c C-c` runs `video-text-edit` on the open manifest, producing `<parent-directory>.mp4` with subtitles and short-gap preservation while streaming output in a small compilation buffer below.
  - When triggered on a section marker, compilation renders only the following segments up to the next marker (or file end) into `<parent-directory>-<section-slug>.mp4`.
- Saving writes canonical, pretty-printed JSON so external tools can consume it. We may accompany the JSON with a minimal `.vtt` or `.srt` render for review players, but the JSON remains the source of truth.

### Emacs Major Mode Requirements

The TJM major mode should cover:

- **Parsing/Serialization**
  - Read TJM JSON into Emacs data structures on visit, preserving ordering and unknown fields (for forward compatibility).
  - Re-emit JSON with stable formatting (sorted object keys except for `segments`, which must maintain user order).
  - Detect merge conflicts when simultaneously editing with other tools (e.g., guard on `version` field).

- **Buffer Presentation**
  - Display each segment as a heading line showing speaker, relative start time, and snippet of text.
  - Render `kind: "marker"` entries as coloured section headings with their title in-flow.
  - Inline per-word overlays showing timestamps when toggled (similar to `subed-toggle-timestamps`).
  - Colour-code tags (e.g., `broll`, `todo`) and highlight missing metadata or gaps between segments.

- **Navigation & Playback**
  - Commands for next/previous segment (`n`/`p`), next/previous word (`M-n`/`M-p`), and jump to timestamp (`g` with prompt).
  - Pluggable playback backend (mpv/ffplay) with configurable latency compensation; show currently playing word.
  - Optional auto-scroll while playing segments.

- **Editing Primitives**
  - Deleting a segment removes its node and closes timing gaps by simply omitting it in the array.
  - Splitting and merging segments should adjust `start`/`end` and regenerate word lists.
  - Moving segments between sources (changing `source` id) with interactive completion.
  - Inline editing of `broll` metadata with schema-aware prompts (mode ensures only valid keys/values are added).
  - Compiling via `C-c C-c` should integrate with Emacs’ Compile framework, defaulting the command to `video-text-edit <file> --output <parent>.mp4 --subtitles --preserve-short-gaps 1.5` and displaying the results in a bottom window (~12 lines high).
  - Commands to normalise timings (e.g., `video-tjm-normalize` to rebuild `text` from `words` or ensure monotonic timestamps).

- **Validation & Tooling**
  - On save, run validators: check that `start < end`, no negative times, `words` array fits within bounds, `broll` assets exist (optional configurable check).
  - Provide interactive commands to preview b-roll (open overlay file) or mark segments needing b-roll.
  - Support inserting new segments from clipboard text by invoking alignment tooling (`video-transcribe` on selection) directly within Emacs.

- **Extensibility Hooks**
  - Allow user-defined elisp hooks before/after saving or playback (e.g., to update outlines, push changes to git).
  - Provide APIs for scripts to read the in-memory AST (enabling other Emacs packages to reuse the data).

## 4. `video-text-edit` Input Contract

`video-text-edit transcript.tjm.json --output edits/final.mp4` will:

1. Validate all sources exist and pre-load durations.
2. Walk the `segments` array **in file order** (post-edit). Deleted segments simply disappear; reordering is achieved by rearranging array entries.
3. For each segment, emit FFmpeg filter instructions:
   - `trim`/`aselect` on the source clip.
   - Optional overlay commands if `broll` is provided (`overlay`, `replace`, `pip` modes map to different filter graphs).
   - Silence or crossfade bridging when gaps exist (if we add `transitions` metadata later).
4. Concatenate the processed streams into the requested output.
5. Produce an updated manifest with the effective segments (for auditing) and an optional WebVTT/SRT export for review tools.

Because the manifest references multiple sources, the CLI can also emit **edit decision lists** (EDL/XML) as a debugging output so other NLEs can ingest the same cut.

## 5. Relationship to Existing Formats

- `subed` already manipulates SRT/WebVTT/ASS, which are line-based. Our TJM can act as a superset: we can export to WebVTT for compatibility, but TJM keeps per-word data and richer metadata. The Emacs mode can display TJM while using `subed` playback utilities.
- `aeneas` outputs JSON with precise alignment; we can convert that JSON to TJM directly.
- Some tools (e.g., Gentle, WhisperX) also emit word-level JSON—again convertible into TJM without precision loss.

## 6. B-roll Overlays

The `broll` object lets editors specify replacement visuals on a per-segment basis. Syntax sketch:

```json
"broll": {
  "file": "broll/team-celebration.mp4",
  "mode": "replace",
  "audio": "source",        // keep original narration
  "start_offset": 1.2,        // optional offset into the b-roll clip
  "duration": 5.0,            // optional explicit duration
  "position": {"x": 0.05, "y": 0.05, "width": 0.3}, // for picture-in-picture
  "still": true,             // renderer should synthesise a video from a still image
  "continue": true          // continue overlay from previous segment (no restart)
}
```

The CLI can interpret these to build the appropriate FFmpeg filter graph (overlay filters, scaling, muting, etc.). `start_offset`/`duration` accept either numeric seconds or `HH:MM:SS.mmm` strings; `continue` signals that consecutive segments with the same overlay should render as a single uninterrupted clip. When `still` is present, downstream tooling loops a static frame (e.g., via `-loop 1` in FFmpeg) so JPEGs/PNGs can serve as b-roll. Tags (`tags: ["broll"]`) can drive conditional behaviour inside Emacs (e.g., highlight segments needing overlay footage).

## 7. Next Steps

1. Prototype a TJM exporter (`video-transcribe`) that ingests multiple clips and writes one combined manifest plus per-source audio proxies if needed.
2. Draft the Emacs major mode: focus on navigation, JSON editing helpers, and playback integration (possibly leveraging `subed` functions for timing display).
3. Implement `video-text-edit` to consume TJM and render the edited video, honouring multi-source timelines and b-roll instructions.
4. Add conversion utilities: TJM ↔ WebVTT/ASS for interoperability with other tooling.

This revision keeps the workflow Emacs-friendly while adopting a neutral, explicit JSON structure that external video tooling can understand and that supports per-word editing, multiple source clips, and b-roll metadata.
