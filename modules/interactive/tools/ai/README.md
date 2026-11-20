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
- Explicit Copilot activation selects Herbert by default; `hub/org-copilot-provider`
  can select independent gptel instead. The personal Herbert project directory
  supplies its client library and quiet devenv launcher; `hub/herbert-home`
  explicitly selects the live runtime. Passive Org editing neither enables
  Copilot nor starts a connection. An already selected buffer-local Herbert
  connection keeps its runtime and launcher across mode reactivation. Missing
  client/environment dependencies are visible and never select gptel as a fallback.
- Native history remains bound to its recorded runtime. A review fixture's
  conversation is not silently attached to production or replayed as new turns;
  its original history and document artifacts remain intact.
- When subscription OAuth is selected, ordinary gptel, gptel-backed Copilot and
  Babel reuse the same consumer-configured subscription backend and model.
  Activating another adapter does not recreate that backend or replace its
  authenticated state. Missing model selection, outdated adapter setup or a
  model absent from the installed adapter's catalogue stops before a request,
  preventing gptel from silently choosing another model or billing route.
  Catalogue membership is not proof of account-specific availability: the
  provider accepts or rejects the exact requested model. Babel configures this
  route at request time, including on cold startup, without activating Copilot.
- OAuth cache files are readable and writable only by their owner. Subscription
  setup secures an existing cache without reading its contents, and token writes
  preserve private permissions. A valid cached login is restored without another
  browser sign-in.
- Babel generation does not enable Copilot mode or change the source buffer's
  editing context. Passive loading does not configure its request route.
- Personal Copilot chat buffers enter Evil insert state when Evil is available;
  the reusable package does not choose a modal editing state.
- Reloading AI configuration preserves Copilot activation state and does not
  duplicate activation hooks or review advice. Review uses the selected adapter;
  an unavailable native review action does not silently switch to gptel.
- If the selected Copilot adapter cannot load on activation, configuration
  identifies the missing dependency and explains how to retry. Offline comments
  and panels remain usable without configuring another provider.
- Making the adapter available does not configure it automatically or activate
  other Org buffers. Disabling and re-enabling `org-copilot-mode` explicitly
  retries setup using the selected authentication route.
