---
domain: authoring/comments
status: draft
---

# Org Comments

## Boundary

`org-comments` owns the provider-neutral comment model, Org sidecar persistence, source anchors, overlays, commands, compose flows, and context-panel integration. Personal activation and Evil/Bépo bindings remain in `modules/interactive/org/`; remote provider APIs remain in their publishing adapters.

The local Org sidecar backend is the default. Remote providers register capabilities and operations through the public backend protocol rather than depending on storage internals.

## Comment Model

- Source Org buffers remain free of comment records.
- A source document maps to a colocated `.comments.org` sidecar.
- Root comments carry workflow state; replies remain child records within the root thread.
- Inline comments retain target text and anchor metadata; page comments have no source range.
- Failed or ambiguous anchors remain visible as stale items instead of disappearing.
- Neutral collaboration fields represent local and remote providers without embedding provider-specific payloads in the core model.

## Backend Contract

Backends declare capabilities for listing, creating, replying, editing, deleting, changing status, opening remote items, pushing, pulling, and synchronizing. Generic commands check those capabilities and report actionable errors for unsupported operations.

Backend detection uses registered detectors and the current source buffer. The package does not assume Confluence, Google Docs, credentials, or network availability.

## User Interface Contract

- `org-comments-mode` provides the generic buffer-local comments experience.
- Context-panel rows expose the same author-facing command language across providers while capability limits remain explicit.
- Package keybindings are Org-native and Emacs-native; the package does not install Evil, leader, or Bépo bindings.
- Generic panels preserve source focus, jump to source or sidecar targets, and render stale or remote-missing state visibly.
- Opening the side panel preserves the comments package's source highlight ranges and faces; it does not add a whole-line source highlight. Panel rows may highlight the focused comment independently.
- Author/date metadata is typographically quieter than headers and bodies while retaining stable author colours. Provider-specific status, badges, and actions remain intact when rows share framework spacing and wrapping.
- Declarative mixed-panel rows start with the comment icon and quoted target. The default `OPEN` state is implicit; `TODO` uses an amber diamond (`◇`) and `RESOLVED` a quiet green check (`✓`) immediately before the target. Faces are theme-derived, and marker help names the state in plain English. Stored workflow values remain unchanged.
- Thread state belongs to the root conversation, not message authors. Each message has its own author/date and body; replies retain their identities and action context. Shared row composition does not flatten reply metadata or change compact-overview/focused-thread behavior.

## Invariants

- Package code has neutral defaults and no dependency on private `hub-*` libraries.
- Local comment creation, sidecar persistence, reopening, and panel navigation work with plain Emacs, Org, and the declared `context-panels` dependency; personal configuration and remote providers are not required.
- Personal preferences, secrets, account defaults, and provider credentials stay outside the package.
- Sidecar updates preserve user-authored bodies, notes, replies, and workflow state unless an explicit operation changes them.
- Remote metadata enters the core model only through neutral fields and public backend operations.
- Unsupported backend capabilities fail visibly and never silently discard comment state.
- Tests for provider-neutral behavior use local fixtures or fake backends and require no network access.

## Related Contracts

- UX parity details: [`docs/ux-parity-audit.md`](docs/ux-parity-audit.md)
- Delivery history: [`refactor.plan.md`](refactor.plan.md)
