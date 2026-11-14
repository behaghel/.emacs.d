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

  @wireframe:music-cockpit-liked-songs.svg @wireframe:music-hifi-console-playing.svg
  Scenario Outline: Keep controls and metadata below scrolling content
    Given I am viewing <view> in the shared bottom panel
    When I scroll its content or resize the panel
    Then its key-reminder strip remains at the panel's bottom edge
    And its optional metadata is readable without replacing the key hints
    And metadata moves to its own footer line when space is limited
    And rendering does not change selection, playback, authorization or capture
    When I close the panel
    Then both content and footer close without disturbing my document
    Examples:
      | view        |
      | Player      |
      | Liked Songs |

  @wireframe:music-cockpit-liked-songs.svg @wireframe:music-hifi-console-playing.svg
  Scenario: Switch all available providers through one native bar
    Given music and non-music providers offer bottom views for my document
    When I open a bottom view
    Then the native bar lists all available provider views with a clear selected tab
    And drawing it does not initialize, fetch, play or enable capture
    When I press "C-Tab" or "C-S-Tab"
    Then the next or previous provider view receives focus
    And returning to a view preserves its browsing position and scrolling
    When I press "C-c v" and cancel the named chooser
    Then the current view, focus, queue and playback remain unchanged
    And an ordinary copy of the same buffer retains its own navigation bindings

  @wireframe:music-cockpit-liked-songs.svg
  Scenario: Use the same music navigation in every music view
    Given a music view has focus
    Then "m" opens Player and "l" opens Liked Songs
    And "+" opens the existing EMMS queue and "/" opens Search
    And "?" opens Actions and "q" closes the panel
    And Liked Songs uses "g+" to load more and "gr" to refresh
    And minibuffer text is not intercepted by music navigation

  @wireframe:music-cockpit-liked-songs.svg @wireframe:music-cockpit-search.svg
  Scenario: Read the same music hierarchy in Liked Songs and native search
    Given Liked Songs is already loaded and the queue contains music
    When I browse Liked Songs and search the same track through Vertico
    Then the title has the same primary accent and emphasis in both listings
    And artist, album and duration each have their own distinct colour in both listings
    And separators and provenance remain quieter without becoming playback markers
    And native selection and query matching remain visible without removing the hierarchy
    And rendering changes no source records, queue order, playback or authorization

  @wireframe:music-cockpit-liked-songs.svg @wireframe:music-cockpit-search.svg
  @wireframe:music-cockpit-queue.svg
  Scenario: Share listing columns without replacing the editable EMMS queue
    Given the actual EMMS queue contains a local file and two deliberate copies of a saved Spotify track
    When I open its Queue tab
    Then the queue uses the same title, artist, album, time, source flags and palette as Liked Songs and search
    And its playing or paused marker identifies the exact active occurrence independently of browsing
    And styling and resizing preserve playlist text, character revisions, point, selection and undo history
    When I delete, move or yank a track with native EMMS commands
    Then the underlying queue changes exactly once and the displayed ranks follow its order
    And playing the second duplicate selects that existing occurrence without appending
    And removing the display styling leaves the native playlist text intact

  @wireframe:music-cockpit-liked-songs.svg @wireframe:music-cockpit-search.svg
  Scenario: Distinguish queue position from saved status visually
    Given a track is queued at position 18 and is absent from loaded Liked Songs
    Then its source flag reads "Queue #18"
    When the same track is also present in loaded Liked Songs
    Then its source flag reads "Queue #18 ♥"
    And a library-only result shows "♥" without a library rank
    And the heart means saved, never playing or selected
    And unavailable heart glyphs fall back to readable "Liked" text
    And Play and Append retain their existing exact source identities

  @wireframe:music-cockpit-liked-songs.svg @wireframe:music-cockpit-search.svg
  Scenario: Follow the active theme palette without changing global faces
    Given Liked Songs and music search share retained metadata
    When I enable a light or dark Modus theme with palette overrides
    Then title uses blue, artist cyan, album magenta and time yellow from that palette
    And provenance and separators use the palette's dim foreground
    And title retains stronger weight and every field retains the same character metrics
    And global faces, retained identities and playback remain unchanged

  @wireframe:music-cockpit-liked-songs.svg @wireframe:music-cockpit-search.svg
  Scenario: Keep real columns aligned across listings and narrow views
    Given a retained track has a long title, a long album and multilingual metadata
    When I browse Liked Songs and search it in the configured reverse Vertico view
    Then both listings place marker, title, artist, album, time and provenance in the same order
    And shared widths keep later columns aligned instead of following the title length
    And time is right-aligned and long values are ellipsized within their columns
    And resizing preserves the browsing row and all action identities
    And search still finds metadata beyond the displayed truncation
    And RET, M-RET and Embark keep their existing exact-occurrence semantics

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

  @wireframe:music-cockpit-search.svg @wireframe:music-cockpit-search-queue.svg
  Scenario: Combine overlapping sources without losing deliberate queue duplicates
    Given the same Spotify URI occurs at queue positions 1 and 3 and in loaded Liked Songs
    And another loaded recording has the same title but a different Spotify URI
    When I search music
    Then I see both exact queue occurrences with their queue positions and heart flags
    And there is no third library-only hit for their shared URI
    And the different recording stays separately searchable
    And loaded-library coverage still includes the combined record
    When I press "RET" on the combined occurrence at position 3
    Then that existing occurrence is selected and played without appending

  @wireframe:music-cockpit-search.svg
  Scenario: Explicitly append from a combined result without interrupting playback
    Given a search result represents an existing queue occurrence and a loaded Liked record
    When I press "M-RET" on that result
    Then exactly one copy of its retained Liked record is appended
    And current playback and the selected queue occurrence are unchanged
    When the loaded record is refreshed, removed or closed before append
    Then the stale append action is rejected before mutation or interruption

  @wireframe:music-cockpit-search.svg @wireframe:music-cockpit-search-queue.svg
  Scenario Outline: Use native contextual actions on the retained search result
    Given a <source> music search result is selected
    When I open the existing Embark menu with "C-."
    Then the menu offers Play and Append without fetching or changing playback
    When I choose <action>
    Then <result>
    And the same retained identity and stale-source guards apply as RET and M-RET
    Examples:
      | source        | action | result                                        |
      | Queue         | p      | that exact existing occurrence plays          |
      | Liked Songs   | p      | that retained library track appends and plays |
      | Queue + Liked | a      | one retained copy appends without interruption |

  @wireframe:music-cockpit-search.svg
  Scenario: Inspect or cancel contextual music actions without side effects
    Given I am browsing music search while a track is playing
    When I open the Embark menu and cancel it without choosing an action
    Then no track is appended, selected, stopped or started
    And a target without its retained identity cannot invoke a music action

  @wireframe:music-cockpit-search-queue.svg
  Scenario: Reject an obsolete queue occurrence without interrupting playback
    Given queue search is open while music is playing
    When I edit or replace the queue before accepting a candidate
    And I press "RET" on that stale candidate
    Then I am asked to search again
    And no different occurrence is played or appended
    And the existing playback is not stopped

  @wireframe:music-cockpit-search.svg
  Scenario: Search the retained library without fetching or needing a queue
    Given some Liked Songs are loaded and the current queue is empty
    When I search music
    Then the loaded songs remain searchable with honest partial-library coverage
    And no library page, authorization or playback preview is requested
    When the loaded library is refreshed or closed before I accept a candidate
    Then that obsolete candidate cannot append or play a track

  @wireframe:music-cockpit-search.svg @wireframe:music-cockpit-liked-songs.svg
  Scenario: Distinguish actual playback from a highlighted candidate
    Given the same Spotify track occurs twice in the queue and is in Liked Songs
    And only the second queue occurrence is playing
    When I browse another candidate
    Then only the second queue occurrence has the queue playback marker
    And the matching library song has the same actual playback state
    And pausing changes the markers without moving my browsing position
    And seeking retains the underlying playing or paused marker
    And stopping removes the markers without fetching or reopening the library

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
    When I press "'m+"
    Then the current EMMS queue receives focus
    And queue editing and ordering remain owned by EMMS
