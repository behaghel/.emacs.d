@story:emms-player @domain:personal-apps/apps @priority:must @status:done
Feature: Docked EMMS player
  As an Emacs user listening while editing
  I want a compact keyboard-controlled player in my shared bottom panel
  So that I can see and control playback without disrupting my documents

  Context:
    The player shares context-panels' bottom slot with source-specific views.
    The playback queue remains owned by EMMS. Playback survives panel dismissal.
    Executable scenarios are in test/personal-apps/emms-player-test.el.

  Out of scope:
    Spectrum visualization, Spotify, queue editing, and replacing the playback backend.

  # AC-1
  @wireframe:emms-player-playing.svg
  Scenario: Open the player while listening
    Given a playlist containing tracks lasting 180 and 240 seconds
    And the first track is playing at 60 seconds
    When I press "'mm" in normal state
    Then the player is focused in the shared bottom slot
    And I see the current track highlighted, elapsed time, and a progress bar
    And I see "Playlist: 7:00" and the next track
    And the original EMMS playlist is unchanged

  # AC-2
  @wireframe:emms-player-playing.svg
  Scenario Outline: Control playback using Bepo keys
    Given the player is open
    When I press "<local>" in the player or "<global>" in a document
    Then playback performs "<action>"
    Examples:
      | local | global | action           |
      | SPC   | 'm SPC | toggle play/pause |
      | c     | 'mc    | seek -10 seconds |
      | r     | 'mr    | seek 30 seconds  |
      | t     | 'mt    | next track       |
      | s     | 'ms    | previous track   |

  # AC-3
  @wireframe:emms-player-playing.svg
  Scenario: Source views temporarily replace the player
    Given the docked player is visible
    When I open another bottom view and leave its source
    Then the player returns without changing playback
    When I dismiss the player
    Then it stays hidden and playback continues
    And global playback controls remain available

  # AC-4
  @wireframe:emms-player-empty.svg
  Scenario: Open an empty playlist
    Given there are no tracks
    When I open the player
    Then I see "No tracks. Use M-x emms-play-file to load audio."
    And no playback process is started

  # AC-5
  @wireframe:emms-player-unknown.svg
  Scenario: Display incomplete metadata honestly
    Given the current track has no known duration
    When I open the player
    Then the current track's duration and progress are shown as unknown
    And the playlist total identifies how many durations are unknown

  # AC-6
  @wireframe:emms-player-playing.svg
  Scenario: Progress updates without stealing focus or reopening the panel
    Given I am editing a document while the player is visible
    When playback time advances
    Then the progress display updates without moving my document point
    When I switch to another bottom view
    Then progress updates do not replace that view
