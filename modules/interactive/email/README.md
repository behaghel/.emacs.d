---
domain: email-productivity
status: draft
last-reviewed: 2026-06-08
---

# Email Productivity

## Ubiquitous Language

- **Context**: Mail account or identity selected for reading and composing.
- **Dashboard sidebar**: Email-oriented Org/sidebar UI shown alongside mail workflows.
- **Noise rule**: Local rule for reducing low-value mail visibility.
- **Headers view**: mu4e listing buffer used for search and triage.
- **Compose flow**: Message creation/reply path, including Org-based HTML mail behavior.

## Invariants

- mu4e may be provided by the external environment and should not be installed by `straight.el`.
- Email behavior must tolerate CI or machines where `mu`/mu4e are absent via explicit stubs or guarded loading.
- Mail account details and machine-specific values must not be committed as shared configuration.
- Email keybindings must conform to the established Evil and Bépo-aware interaction model.
- Dashboard/sidebar behavior is user-facing and must not disappear as a side effect of performance work.

## Integration Notes

Email conforms to the interactive experience domain and consumes shared helpers.  Runtime code may need to locate external mu4e paths, but that probing should be explicit and measured when performance is under review.
