@story:context-provider-harmonization @domain:authoring/org @priority:must @status:in-progress
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

  # AC-1: Icons identify kinds without repeating labels or exposing internal IDs.
  @wireframe:context-provider-harmonization.svg @happy-path
  Scenario: Read the note or conversation immediately beside its icon
    When I read the mixed context panel
    Then each native note starts with its icon and actual body on the same line
    And no separate "Sidenote", "Footnote" or internal note ID label is shown
    And wrapped note lines align beneath the body rather than beneath its icon
    And the comment starts with its icon and quoted source target on one line
    And no separate "Comment" label is shown
    And author and date appear underneath the target in quieter text
    And the actual comment message appears below its author and date
    And the comment keeps its local-origin icon and stable author colour

  # AC-6: Thread state belongs beside the target, never beside message authors.
  @wireframe:context-provider-harmonization.svg
  @wireframe:context-provider-harmonization-todo.svg
  @wireframe:context-provider-harmonization-resolved.svg
  Scenario Outline: Recognize a thread state without a state label or extra line
    Given the local comment has stored state <state>
    When I read its first line
    Then I see <marker> between the comment icon and quoted target
    And non-default markers have explanatory help in plain English
    And state is not attached to author/date or repeated for replies
    And no uppercase state text or dedicated state line is added
    And the stored workflow state remains unchanged
    Examples:
      | state    | marker                         |
      | OPEN     | no state marker                |
      | TODO     | an amber diamond meaning To do |
      | RESOLVED | a quiet green check meaning Resolved |

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
    Given the local comment has two replies
    When I focus that comment using the existing comment focus workflow
    Then the quoted target and thread state appear only once above the conversation
    And the full root body and both replies remain readable within the comment row
    And each message has its own author and date above its body
    And reply identity and existing comment actions remain associated with each reply
    And author metadata, status, warnings and badges are not silently discarded
    And native notes remain notes rather than comment records
