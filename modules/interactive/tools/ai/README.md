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
- When subscription OAuth is selected, Copilot Babel generation configures a
  ChatGPT subscription backend at request time, including on cold startup.
  Login and generation reuse the registered subscription backend without
  replacing its authenticated state. Setup failure or a model absent from the
  adapter catalogue stops generation before a request; an API-key backend or
  another model is never substituted. Catalogue membership is not proof of
  account-specific availability: the provider accepts or rejects the model.
- OAuth cache files are readable and writable only by their owner. Subscription
  setup secures an existing cache without reading its contents, and token writes
  preserve private permissions. A valid cached login is restored without another
  browser sign-in.
- Babel generation does not enable Copilot mode or change the source buffer's
  editing context. Passive loading does not configure its request route.
- Reloading AI configuration preserves Copilot activation state and does not
  duplicate activation hooks or review advice.
- If the selected Copilot adapter cannot load on activation, configuration
  identifies the missing dependency and explains how to retry. Offline comments
  and panels remain usable without configuring another provider.
- Making the adapter available does not configure it automatically or activate
  other Org buffers. Disabling and re-enabling `org-copilot-mode` explicitly
  retries setup using the selected authentication route.
