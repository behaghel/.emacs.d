---
domain: publishing/google-docs
status: draft
---

# Google Docs semantic coverage

This provider profile applies the [authoring contract](../../../modules/org/typographic-semantics.md).
The [adapter boundary](../README.md#document-synchronization) separates publishing,
transport, personal presentation and review comments. Support means semantic
preservation within the stated boundaries, not arbitrary Org support or native
Google Docs visual parity.

## Supported profile

| Construct | Representation and boundary |
| --- | --- |
| Title, headings, paragraphs | Document title, heading hierarchy and prose flow. Uniform heading bold is presentation; contrasting authored emphasis is content. |
| Inline emphasis, code, verbatim, external links | Rich runs preserve authored meaning, literal text, targets and labels. Linked whitespace remains linked across native style fragments. |
| Ordered, unordered, description and task lists | Native lists plus explicit continuation/task provenance. Description terms preserve visible `term :: definition` meaning; generated term bold does not add Org emphasis, while authored bold remains distinct. Ambiguous native ownership stops import. |
| Tables | Grid, supported rich cell text and header/body distinction. Unsupported cell semantics stop conversion. |
| Source and example blocks | Marked literal blocks preserve text, hard lines, spacing and source language. Syntax highlighting and native code-building-block appearance are not promised. |
| Quotes | Marked paragraph groups preserve boundaries, rich content and emphasis independently of the applied quote typography. Attribution metadata is not part of this supported mapping. |
| Callouts | Type, optional title, prose and supported flat lists. Generated label/padding are presentation, not authored content. Nested blocks/lists are rejected where the representation cannot preserve them. |
| Centered prose | Marked paragraph groups preserve empty, adjacent and multi-paragraph blocks. Soft wraps remain prose. |
| Verse | Marked rich hard lines preserve indentation, trailing spaces and stanza gaps. Unsupported semantic objects and nesting stop conversion. |
| Section breaks | Independently marked rule paragraphs and outside separators preserve untitled transitions. Arbitrary borders are not inferred as section breaks. Native horizontal-rule objects have an explicit read representation. |
| Named footnotes | Supported rich single-paragraph notes use native references and segments. Unsupported definitions, missing references and marginalia stop preflight. Footnote-only edits update segments rather than recreate host paragraphs. |
| Standalone images and captions | Readable supported assets become inline images; explicit caption provenance preserves association. Unsupported assets fail preflight. Source link and native object identity/geometry remain distinct. |

## Explicit degradations

- **Dates:** supported single UTC day values use native date elements. Both source
  activity kinds carry provenance; an unmarked remote date imports inactive so it
  cannot silently create an agenda obligation. Richer source timestamps retain
  their complete literal Org value instead of becoming lossy day chips. Native
  time-bearing, malformed or non-UTC date values stop import rather than being
  guessed or truncated.
- **Person links:** preserve the person URI and display label as an explicit
  hyperlink. Native people chips are not claimed; unsupported native objects stop
  import rather than disappearing.
- **Status labels:** supported static labels preserve value, display text,
  semantic colour and source protocol. Uniform small-cap rendering retains
  source spelling through inline provenance, without discarding native label
  edits. Labels exceeding reversible provenance limits stop preflight. They do
  not imply native dropdown definitions, option sets or workflow behaviour.
- **Described image links:** remain links, not image uploads. A local file target
  is not a portable embedded image or a publicly reachable asset.
- **Notes:** ordinary and forced-bottom notes use Google Docs bottom footnotes;
  renderer-specific margin placement is not represented. Repeated references may
  become independent native segments, a degradation reported by footnote preflight;
  shared-native-note identity is not promised.

## Rejected source semantics

Before uploads or document creation, publishing rejects source fields without an
implemented reversible mapping:

- `#+ATTR_QUOTE:` attribution; authors can instead use explicit body prose.
- `#+SUBTITLE:`, `#+AUTHOR:`, `#+DATE:`, `#+LANGUAGE:`,
  `#+EXPORT_EYEBROW:` and `#+EXPORT_FOOTER_NOTE:` metadata. Required information
  belongs in explicit body content until a mapping exists; it is not silently
  discarded or treated as styling.
- Standfirst, metric, graph and other unsupported special blocks. Literal Org
  delimiters are not a semantic publication substitute.
- Unsupported footnotes, marginalia and assets, as defined by their preflight
  modules.

Preflight inspects the complete source even when narrowed. Source/footnote checks
precede uploads; image-bearing sources also pass SDK conversion before upload.
Source validity is checked again before body writing. Native union members without
a decoder block import with a diagnostic naming the unsupported object kind.

## Safe synchronization

Automatic text merge is limited to unstructured paragraphs without atomic native
runs. Structured IR, including future structured kinds, requires conflict review
when both local and remote content changed. Native dates are not plain-text merge
inputs merely because their visible label resembles text.

Pull integrity checks protect block identity/shape. Centered paragraph counts and
verse line/whitespace geometry participate alongside existing lists, notes and
blocks. Word edits with stable geometry remain pullable. Structural edits can
require the explicitly confirmed replace-local recovery path; that path preserves
metadata and document identity and backs up local content before replacement.

## Coverage reporting

The reusable support classifier consumes plain audit data, not personal authoring
modules. Every populated contract counter has an explicit supported, degraded or
unsupported disposition. Undeclared counters remain visible as unclassified and
unsupported; absence from the table is not evidence of support.

Native people chips, dropdown workflows, syntax colouring and additional layout
renderers are not prerequisites for this bounded profile. They are distinct
capabilities and cannot be inferred from readable output, a styling approximation
or an empty normalized diff.
