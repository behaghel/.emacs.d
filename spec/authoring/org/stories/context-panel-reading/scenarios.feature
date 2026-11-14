@story:context-panel-reading @domain:authoring/org @priority:must @status:ready
Feature: Read nearby notes and comments without visual crowding
  As an Org author reviewing a document
  I want clearly separated contextual items that reopen consistently
  So that I can read and navigate without deciphering a dense text block

  Context:
    The restart-check.org fixture contains two adjacent footnote references,
    one sidenote definition, one traditional footnote definition, and one local
    review comment. The initial live panel was crowded and clipped its comment
    body. Reopening through the generic panel command omitted marginalia.
    The draft shows the content area only; existing editor chrome is unchanged.
    Colours illustrate existing theme roles rather than prescribing a theme.

  Out of scope:
    Native crash diagnosis or proof that screenshot drag-over is safe; global
    theme changes; new remote operations; new comment actions; separate provider
    sections; broader harmonization of provider-specific metadata, status labels,
    icons, or rendering contracts; a new framework provider-discovery mechanism;
    changes to existing
    compact-overview versus focused-thread expansion semantics.

  References:
    modules/interactive/org/README.md
    packages/cp-org-marginalia/README.md
    spec/personas.md#org-author

  Background:
    Given I am an Org author using my normal configured Emacs session
    And restart-check.org has a sidenote, a traditional footnote, and a local comment
    And the frame has room for the existing right-side panel

  # AC-1: Reopening needs no fixture helper or manual provider activation.
  @wireframe:context-panel-reading.svg @happy-path
  Scenario Outline: Reopen the source through an ordinary panel entry point
    Given I have closed and reopened restart-check.org without running a fixture helper
    When I open its context panel using <entry>
    Then I see the local comment and both native notes in source order
    And no separate marginalia-mode command is needed
    And the source remains unmodified
    Examples:
      | entry                    |
      | normal-state ,cc         |
      | M-x context-panels-open  |

  # AC-2: Hierarchy and wrapping distinguish items without a provider regrouping.
  @wireframe:context-panel-reading.svg
  Scenario: Read the mixed panel at its normal narrow width
    When I open the context panel at its configured width
    Then each item has a clear icon and header identifying its kind and label or status
    And comment author and date are visually quieter than the header and body
    And bodies are indented two character columns below their headers
    And one blank text line separates adjacent items
    And body lines wrap inside the panel instead of clipping at its right edge
    And Org emphasis and list structure remain readable
    And HUB_NOTE properties do not appear in note bodies
    And item spacing may push later items downward without changing source order

  # AC-3: Reading improvements preserve both Return events and provider actions.
  @wireframe:context-panel-reading.svg
  Scenario Outline: Navigate from an item without editing the source
    Given the mixed context panel is visible
    When I place point in the <item> body and press <key>
    Then source point moves to the <target>
    And the source text and local comment are unchanged
    And comments retain their existing edit and reply actions and modal bindings
    Examples:
      | item        | key     | target                     |
      | sidenote    | Return  | sidenote reference         |
      | sidenote    | C-m     | sidenote reference         |
      | footnote    | Return  | traditional note reference |
      | footnote    | C-m     | traditional note reference |
      | comment     | Return  | reviewed source paragraph  |
      | comment     | C-m     | reviewed source paragraph  |
