@story:music-keyboard-cockpit @domain:personal-apps/apps @priority:must @status:in-progress
Feature: Music navigation at the speed of the editor
  As an Emacs listener switching between writing and choosing music
  I want stable entry points, explicit search actions and a discoverable command palette
  So that listening never requires remembering implementation commands

  Context:
    The established music prefix is "'m". The player and Liked Songs share the
    existing context-panels bottom slot, and EMMS owns the current playlist.
    The initial search covers the active queue and already loaded Liked Songs;
    its source coverage is visible instead of pretending to search all Spotify.

  Out of scope:
    Remote catalogue search, a new local-media index, saved Spotify playlists,
    changes to authorization scopes and playback on search preview.

  References:
    modules/interactive/apps/music-ui.md

  Background:
    Given I am an Emacs listener with music commands not necessarily loaded yet

  @wireframe:music-cockpit-liked-songs.svg
  Scenario: Reach Liked Songs through one stable shortcut
    When I press "'ml" from my document
    Then Liked Songs opens or receives focus in the shared bottom slot
    And invoking the same shortcut again preserves my existing browsing position and filter
    And browsing does not alter playback, the queue or capture consent
    And I retain the existing append and append-and-play library actions

  @wireframe:music-cockpit-liked-songs.svg
  Scenario: Reach Liked Songs directly from the player
    Given the player has focus
    When I press "l"
    Then Liked Songs opens or receives focus in the shared bottom slot
    And it behaves like the global "'ml" shortcut without changing playback or capture

  @wireframe:music-cockpit-search.svg @wireframe:music-cockpit-search-queue.svg
  Scenario: Search without playing the selection merely by moving over it
    When I press "'m/" and type a query
    Then I see candidates with clear Queue or Liked Songs provenance
    And the prompt identifies the loaded source coverage
    And candidate movement and previews do not start or enqueue music
    And ordinary letters and spaces remain search input, not transport commands
    When I cancel the search
    Then I return to my prior editing context with playback unchanged

  @wireframe:music-cockpit-search.svg @wireframe:music-cockpit-search-queue.svg
  Scenario Outline: Explicitly act on a search candidate
    Given I have selected a <source> candidate
    When I invoke <action>
    Then <result>
    And unrelated queue entries remain unchanged
    Examples:
      | source       | action        | result                                      |
      | Liked Songs  | RET           | that track is appended and played           |
      | Liked Songs  | M-RET         | that track is appended without interruption |
      | Queue        | RET           | that existing occurrence is selected and played |

  @wireframe:music-cockpit-search-queue.svg
  Scenario: Reject an obsolete queue occurrence without interrupting playback
    Given queue search is open while music is playing
    When I edit or replace the queue before accepting a candidate
    And I press "RET" on that stale candidate
    Then I am asked to search again
    And no different occurrence is played or appended
    And the existing playback is not stopped

  @wireframe:music-cockpit-palette-access.svg
  Scenario: Discover music actions without remembering M-x commands
    When I press "'m?" or request help in the player
    Then I see navigation, transport, spectrum and connection actions grouped by purpose
    And I see their keys and current availability
    And state labels pair a readable icon with text
    And library authorization and audio activation are separate actions
    When I dismiss the palette
    Then I return to my previous context without changing playback

  @wireframe:music-cockpit-palette-access.svg
  Scenario: Open the existing editable queue rather than a shadow playlist
    When I press "'mq"
    Then the current EMMS queue receives focus
    And queue editing and ordering remain owned by EMMS
