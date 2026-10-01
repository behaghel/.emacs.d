---
domain: personal-apps/ai
status: draft
---

# AI Workflows

## Ubiquitous Language

- **AI workflow**: Standalone assistant-style command flow integrated into Emacs.
- **Assistant integration**: Package or command surface that delegates work to an external AI service.

## Invariants

- AI workflows are personal apps unless a narrower authoring or development contract emerges.
- API tokens and provider-specific secrets must remain outside tracked configuration.
- Optional AI integrations should not break ordinary startup when unavailable.
