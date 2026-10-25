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
- Org Copilot activation is [explicit and buffer-local](../../../../packages/org-copilot/README.md#activation); loading Org or gptel does not enable it or configure its adapters.
- Reloading AI configuration preserves Copilot activation state and does not
  duplicate activation hooks or review advice.
- If the selected Copilot adapter cannot load on activation, configuration
  identifies the missing dependency and explains how to retry. Offline comments
  and panels remain usable without configuring another provider.
- Making the adapter available does not configure it automatically or activate
  other Org buffers. Disabling and re-enabling `org-copilot-mode` explicitly
  retries setup using the selected authentication route.
