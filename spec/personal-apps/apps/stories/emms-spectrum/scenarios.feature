@story:emms-spectrum @domain:personal-apps/apps @priority:must @status:done
Feature: Opt-in system-output spectrum in the EMMS player
  As an Emacs user listening while editing
  I want a pixel-rendered spectrum inside my docked player
  So that I can enjoy a retro visualizer without leaving Emacs

  Context:
    CAVA analyzes the system output mix, including other audible applications.
    It never uses the microphone and does not record or store audio or spectra.
    Executable scenarios live in test/personal-apps/emms-spectrum-test.el.

  Out of scope:
    Spotify control, player-specific audio capture, microphone input, and recording.

  # AC-1
  @wireframe:emms-player-playing.svg
  Scenario: Opening the player does not start capture
    When I open or reopen the player
    Then no analyzer process is started
    And the player offers the "v spectrum" control

  # AC-2
  @wireframe:emms-spectrum-active.svg
  Scenario: Explicitly enable the analyzer
    Given the player is visible in graphical Emacs
    When I press "v" in the player or "'mv" outside it
    Then the player shows that it is analyzing system output
    And a native SVG spectrum appears inside the existing bottom panel
    And bar heights update from CAVA without changing playback or editing focus

  # AC-3
  @wireframe:emms-player-playing.svg
  Scenario Outline: Stop capture when it is no longer visible or requested
    Given the analyzer is active
    When I <action>
    Then the analyzer process and animation timer are stopped
    And temporary analyzer files are removed
    And playback continues
    And restoring the player does not restart capture
    Examples:
      | action                         |
      | toggle the analyzer off        |
      | close the player               |
      | switch to another bottom view  |
      | kill the player buffer         |

  # AC-4
  @wireframe:emms-spectrum-unavailable.svg
  Scenario Outline: Capture failure does not break playback
    Given <failure>
    When I enable the analyzer
    Then I see an actionable diagnostic
    And capture is stopped without microphone fallback
    And playback and its controls remain available
    Examples:
      | failure                              |
      | CAVA is missing                      |
      | SVG display is unavailable           |
      | macOS system-audio permission is denied |
      | CAVA exits or sends malformed frames |

  # AC-5
  @wireframe:emms-spectrum-active.svg
  Scenario: Stream chunks do not determine display frames
    Given CAVA sends several frames in fragmented process output
    When the analyzer updates
    Then it displays the newest complete spectrum frame
    And pending data and diagnostics remain bounded
    And no frame data is written to disk
