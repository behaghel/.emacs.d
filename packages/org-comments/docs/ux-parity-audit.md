---
domain: authoring.comments
status: draft
last-reviewed: 2026-07-04
wireframes:
  - packages/org-comments/docs/wireframes/provider-parity-panel.svg
---

# UX Parity Audit: Confluence and Google Docs Comments

## Story

As an Org author collaborating with reviewers in more than one remote system, I want Confluence and Google Docs comments to look, feel, and behave like the same Org comments workflow, so that I can move between providers without relearning navigation, panel actions, feedback, or visual state cues.

## Scope

This audit covers the user-facing experience of comment collaboration through `org-comments`, `org-confluence`, and `org-google-docs`:

- command discovery and naming;
- source-buffer overlays, page markers, and context-panel behavior;
- comments panel look'n'feel, row grammar, badges, filters, and help;
- sidecar heading/readability conventions;
- import, pull, push, sync, status, and unsupported-action feedback;
- docs and keybinding discoverability.

This audit does **not** require Google Docs to implement capabilities its public APIs do not support. In particular, local creation of new native anchored Google Docs root comments remains intentionally unsupported.

## Visual Contract

Primary wireframe: [`provider-parity-panel.svg`](wireframes/provider-parity-panel.svg).

The shared look'n'feel contract is:

1. Source buffers remain clean Org text; visible comment affordances are overlays, highlighted anchors, and display-only page markers.
2. The right comments panel is the canonical review surface for all providers.
3. The detail view/panel contract is provider-neutral: overview rows stay compact, the focused/current thread expands into detail, and any bottom/detail panel uses the same full-thread rendering rules regardless of provider.
4. Replies are displayed as a conversation, not as provider-specific artifacts: root context stays visible, replies are indented with `↳`, each reply carries its own sync badge/author/date/body, and local pending replies and remote replies use the same ordering and spacing for Confluence and Google Docs.
5. Cards use the same row grammar across providers:
   - icon: `💬` anchored/inline, `👆` page-level, `⚠` stale/missing;
   - status chip: `OPEN`, `TODO`, `RESOLVED`;
   - target preview in curly quotes when anchored;
   - sync badge: `✍️` local draft/edit, `🔗` remote-linked, `⚠` missing/dangling, `❓` unconfirmed;
   - author/date metadata line;
   - body preview in overview, full body plus replies when focused.
6. The panel action vocabulary is provider-neutral: `RET`, `e`, `r`, `O`, `U`, `D`, `S`, `m`, `z`, `?`, `q`.
7. Provider-specific limitations appear as action feedback, not as different UI structure.
8. Reports use the same sentence grammar and count vocabulary, with provider name as the only visible prefix.

## Audit Matrix

| UX area | Confluence current state | Google Docs current state | Parity verdict | Required spec |
|---|---|---|---|---|
| Source activation | `org-confluence-mode`, linked page metadata, page status marker. | `org-google-docs-mode`, upstream `gdocs` metadata/property detection. | Partial | Google Docs should have an equally visible linked/unlinked affordance or status entry, even if body status delegates upstream. |
| Package dispatch key | `C-c C-x C` opens a transient/fallback dispatch. | `C-c C-x G` opens completing-read dispatch. | Partial | Both dispatches should have comparable grouping, labels, ordering, and fallback quality. Exact UI widget may differ only if the same choices are obvious. |
| Dispatch contents | Status, sync, publish, pull, open, descendant. | Doctor, create, push, pull, sync, open, status, import comments, authenticate. | Partial | Align labels around shared verbs: Status, Sync, Publish/Create, Pull, Open, Comments, Account/Doctor. Provider-only actions stay visible but grouped. |
| Generic comment commands | DWIM from source, sidecar, and panel rows. | Same generic `org-comments` commands for imported Google threads. | Good | Keep generic commands provider-neutral and capability-gated. |
| Panel mode/keymap | Uses `org-comments-panel-mode` and generic actions. | Uses same panel for imported comments/replies. | Good | `org-comments-panel-mode` remains canonical; no provider-specific panel modes. |
| Panel row visual grammar | Icons, status chips, badges, replies, filters. | Same renderer after import. | Good | Preserve shared renderer; provider metadata must normalize into the same row vocabulary. |
| Reply display | Nested Confluence replies render under their root thread with shared reply summaries/detail rows. | Imported and local Google replies render under imported root threads through the same renderer. | Needs explicit verification | Reply order, indentation, badges, metadata, pending/remote distinction, and collapsed-vs-expanded behavior must match exactly. |
| Detail view/panel | Focused panel rows and page/detail surfaces show the full root body plus replies. | Google rows use the same focused/detail renderer when normalized. | Needs explicit verification | The detail panel must not become provider-specific; both providers should use one full-thread rendering contract. |
| Filter UX | `z` filters, actionable/draft/current-user/resolved/missing toggles. | Same panel filter layer when Google comments are in sidecar. | Good with caveat | Google records need enough normalized metadata for current-user/actionable filters to be meaningful. |
| Open remote | Focused Confluence URL. | Google Doc URL with `disco` comment focus where possible. | Good | Failure messages must explain when provider/browser cannot focus exact thread. |
| Reply workflow | Local reply then push to Confluence. | Local reply then push to Drive replies API. | Good | Reports and sidecar headings should use same reply wording. |
| Resolve workflow | Local/remote status through Confluence where supported. | Resolve imported roots via Drive reply action; reopen unsupported. | Partial but acceptable | UI must advertise only supported status transitions and explain `OPEN`/`TODO` are local when backend cannot reopen. |
| New root comment | Local footer/inline comments can push to Confluence. | Unsupported for native anchored Google Docs comments. | Intentional gap | UX must make this a clear provider limitation, not a broken command. No unanchored Drive-comment substitute. |
| Import/pull feedback | Generic import summary plus Confluence detail buffer for TODO-impacting events. | Generic import summary. | Good | Count labels and preservation language should stay identical where possible. |
| Sync feedback | Generic sync summary; page conflicts remain Confluence-specific. | Generic sync summary for comment actions; body sync delegates upstream. | Partial | Google Docs should distinguish body sync vs comments sync in labels and messages. |
| Status dashboard | Rich Confluence sync status buffer/actions. | Basic `gdocs-status`/doctor/import summaries. | Gap | Either document provider capability difference or add a Google Docs status view that follows the same visual/action grammar. |
| Sidecar headings | Root headings with reply counts, author resolution, readable previews. | Imported roots/replies use shared title helpers where implemented. | Mostly good | Audit sample sidecars for identical readability: root vs reply, local draft vs remote-linked, missing/resolved. |
| Help/docs | Confluence has richer manual/status/action docs. | Google Docs README documents current workflow and limitations. | Partial | Add a provider-neutral “remote comments UX” page or extend generated docs with provider capability notes. |
| Look'n'feel | Mature context-panel manual defines icons/chips/badges. | Shares renderer but docs do not yet present visual parity explicitly. | Partial | Promote the visual contract above into user docs and generated help text where useful. |

## Acceptance Criteria

- [ ] UX-1: Given a Confluence-linked or Google Docs-linked Org buffer, when the comments panel opens, then cards use the same icons, status chips, sync badges, target previews, metadata lines, body preview rules, reply indentation, and filter header grammar.
- [ ] UX-1a: Given a root comment has replies, when it is shown in overview, focused detail, or a bottom/detail panel, then Confluence and Google Docs replies use the same ordering, indentation, sync badges, author/date metadata, body wrapping, and pending-vs-remote visual treatment.
- [ ] UX-2: Given point is in a source buffer, sidecar heading, or panel row, when the user invokes generic comment commands (`open remote`, `reply`, `push`, `pull`, `sync`, `mark status`, `edit`, `delete`), then the command uses the same DWIM behavior and either completes or explains provider capability limits.
- [ ] UX-3: Given a provider does not support an operation, when that action is requested, then the user sees an actionable `user-error` that names the provider, the unsupported capability, and the supported alternative.
- [ ] UX-4: Given a dispatch command for either provider, when the menu is shown, then shared verbs use shared labels/order and provider-specific verbs are grouped without hiding core comment actions.
- [ ] UX-5: Given import, push, pull, or sync changes comments, when feedback is shown, then the summary uses the generic report grammar and count vocabulary across providers.
- [ ] UX-6: Given a user reads docs or `?` help from the comments panel, when they compare Confluence and Google Docs workflows, then they can predict which actions exist, which are provider-limited, and which key to press next.
- [ ] UX-7: Given Google Docs root comment creation is unsupported, when the user attempts to push a local root through the Google backend, then the UX explicitly says native anchored Google Docs root comment creation is unavailable and does not suggest unanchored Drive comments or inline note substitutes.
- [ ] UX-8: Given visual review of the wireframe, when implementation screenshots or Emacs buffers are compared manually, then the layout, row grammar, badges, and action bar match the visual contract within normal Emacs font/theme variance.

## Look'n'Feel Checklist

Before declaring UX parity done, inspect both providers with at least one source file containing:

- one anchored remote-linked `OPEN` comment;
- one local draft reply (`✍️`);
- one resolved remote-linked comment;
- one remote-missing or stale comment (`⚠`);
- one page/footer comment where the provider supports it.

For each fixture, verify:

- [ ] source overlays/highlights do not modify source text;
- [ ] page comment marker location and wording are stable;
- [ ] panel width, card spacing, wrapping, and metadata order match the wireframe;
- [ ] status chips are uppercase and visually consistent;
- [ ] badges are emoji-only and explained in help/docs;
- [ ] focused/current comment expands body and replies while overview comments stay compact;
- [ ] reply rendering is identical in overview, focused row detail, and any bottom/detail panel;
- [ ] the detail panel displays the same full-thread hierarchy as the right panel's focused detail state;
- [ ] filters show active counts and reset hints consistently;
- [ ] unsupported provider actions do not leave stale UI state after errors;
- [ ] reports fit minibuffer/status-buffer usage without provider-specific jargon leaks.

## BDD Scenarios

```gherkin
@domain:authoring.comments @wireframe:packages/org-comments/docs/wireframes/provider-parity-panel.svg
Feature: Provider-neutral remote comments UX parity
  As an Org author collaborating through Confluence and Google Docs
  I want remote comment workflows to share one visual and command language
  So that provider differences are understandable capability limits, not UX surprises

  Scenario: Panel cards use the same visual grammar for both providers
    Given an Org source has imported Confluence and Google Docs remote comments
    When I open the comments panel
    Then each comment row uses the shared icon, status chip, target preview, sync badge, metadata, body, and reply layout
    And provider-specific remote metadata does not change the card structure

  Scenario: Reply threads use one detail rendering model
    Given a Confluence thread and a Google Docs thread each contain remote replies and one pending local reply
    When I view them in overview, focused detail, and the detail panel
    Then replies appear in the same order with the same indentation, badges, author/date metadata, and body wrapping
    And the detail panel shows the same full-thread hierarchy as the focused side-panel detail view

  Scenario: Generic actions work from a panel row
    Given the comments panel is focused on a remote-linked comment row
    When I invoke reply, open remote, push, pull, sync, edit, or status actions
    Then the same key or command works for both providers when the backend advertises the capability
    And unsupported actions fail with a provider-specific explanation and a supported next step

  Scenario: Dispatch menus share command vocabulary
    Given I am in a Confluence-linked or Google Docs-linked Org buffer
    When I open the provider dispatch menu
    Then shared actions use matching labels and ordering
    And provider-only actions are grouped under clearly named sections

  Scenario: Reports use one grammar
    Given a provider imports, pushes, resolves, or marks missing comments
    When the command completes
    Then the minibuffer or report buffer summarizes actions with shared count labels
    And provider names appear only as prefixes or necessary context
```

## Recommended Implementation Slices

1. **Dispatch parity audit/fix**: align Confluence and Google Docs dispatch labels/order/grouping and add tests that assert the shared action labels.
2. **Capability failure copy pass**: collect `user-error` strings for unsupported Google/Confluence comment operations and normalize them against UX-3/UX-7.
3. **Reply/detail rendering parity**: add fixtures for Confluence and Google Docs threads with mixed remote and pending local replies, then assert the side panel focused state and detail panel use the same full-thread renderer.
4. **Panel visual fixture**: add an ERT-rendered fixture or manual fixture command that produces the look'n'feel checklist states for both providers.
5. **Help/docs pass**: update `org-comments-help-text`, generated docs, and provider READMEs with the visual contract, badge legend, reply display rules, detail-panel behavior, and provider capability table.
6. **Google Docs status affordance**: decide whether to add a lightweight Google status view comparable in look'n'feel to the Confluence status dashboard, or explicitly document body/status dashboard as upstream `gdocs` territory.

## Open Questions

1. Should Google Docs use a transient dispatch like Confluence when `transient` is available, or is completing-read acceptable if labels/order/grouping match?
2. Should unsupported provider actions be visible in menus as disabled/explaining actions, or hidden unless invoked through generic commands?
3. Do we want a provider-neutral status dashboard in `org-comments`, with Confluence and Google supplying provider detail rows, or should status dashboards remain provider-specific?
4. Should the comments panel header include provider/source context when a buffer is linked to a remote backend?
5. Should the detail panel be implemented as a strict reuse of the focused side-panel renderer, or as a separate view with golden tests proving equivalent reply/thread layout?
