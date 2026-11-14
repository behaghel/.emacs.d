@story:context-provider-harmonization @domain:authoring/org @priority:must @status:draft
Feature: Read and navigate different context providers as one panel
  As an Org author reviewing a document
  I want consistent contextual rows with actions appropriate to each item
  So that I can review notes and comments without learning a different panel for each

  Context:
    The accepted context-panel-reading story supplies ordinary reopening, source
    order, spacing, narrow-window wrapping, Org formatting and Return navigation.
    This story changes the shared row anatomy and makes panel navigation and help
    independent of the comments provider. Provider declarations and the reusable
    row-presentation primitive are reviewed alongside this graphical story; their
    API contracts are tested separately, not expressed as graphical scenarios.
    Wireframe colours illustrate existing theme roles, not a new theme.

  Out of scope:
    Copilot, suggestions and other provider migrations; remote operations; new
    note editing or comment workflows; changes to authored records or sidecars;
    new provider discovery, registration DSLs or generated minor modes; global
    key remapping or changes to Evil normal-state and Bepo movement policy;
    bottom-view/footer redesign; native drag-over crash certification.

  References:
    spec/personas.md#org-author
    spec/authoring/org/stories/context-panel-reading/scenarios.feature
    modules/interactive/org/README.md
    packages/cp-org-marginalia/README.md
    packages/org-comments/README.md

  Background:
    Given I am an Org author using my normal configured Emacs session
    And a disposable Org file has one local comment, one sidenote and one footnote
    And those are the only two context providers participating in this fixture
    And its existing narrow right-side context panel is visible

  # AC-1: A common row anatomy preserves meaningful differences.
  @wireframe:context-provider-harmonization.svg @happy-path
  Scenario: Read kinds first and details second
    When I read the mixed context panel
    Then the headers begin with "Comment", "Sidenote" and "Footnote" respectively
    And the comment header retains its "OPEN" workflow state
    And the note headers retain their "side" and "foot" identifiers
    And the comment target preview appears below its header
    And author and date appear below that preview in quieter text
    And each body follows its available details without empty placeholder lines
    And the comment keeps its local-origin icon and stable author colour

  # AC-2: Panel movement follows rendered rows, without jumping into the source.
  @wireframe:context-provider-harmonization.svg
  Scenario Outline: Move between kinds using the existing panel navigation keys
    Given point is on the <from> row
    When I press <key>
    Then point moves to the <to> row in the panel
    And the panel remains selected
    And source point and source scrolling do not move
    Examples:
      | from     | key | to       |
      | comment  | ]c  | sidenote |
      | sidenote | ]c  | footnote |
      | footnote | ]c  | comment  |
      | footnote | [c  | sidenote |
      | sidenote | [c  | comment  |
      | comment  | [c  | footnote |

  # AC-3: Discovery describes the current item, not the panel's legacy major mode.
  @wireframe:context-provider-harmonization-help.svg
  Scenario Outline: Ask for help on a native note
    Given point is on the <kind> row
    When I request item help using the existing help entry point
    Then help identifies the selected <kind> and its source-reference jump
    And help does not advertise comment reply, status, delete or push actions
    And help describes effective bindings or command names rather than invented shortcuts
    And the item is not edited or remotely synchronized
    Examples:
      | kind     |
      | sidenote |
      | footnote |

  # AC-4: Unsupported operations cannot fall through to a neighbouring comment.
  @wireframe:context-provider-harmonization.svg @edge-case
  Scenario Outline: Request a comment operation while reading a note
    Given point is on the <kind> row beside the local comment
    When I explicitly invoke the comment editing command
    Then I receive an actionable message that this item is not a comment
    And no comment editor or remote operation starts
    And neither the source nor the local comment changes
    Examples:
      | kind     |
      | sidenote |
      | footnote |

  # AC-5: Common row composition does not flatten rich comment content.
  @wireframe:context-provider-harmonization-focused.svg
  Scenario: Focus an existing local thread without losing its semantics
    Given the local comment has a reply
    When I focus that comment using the existing comment focus workflow
    Then the full root body and reply remain readable within the comment row
    And reply identity and existing comment actions remain associated with that reply
    And author metadata, status, warnings and badges are not silently discarded
    And native notes remain notes rather than comment records
