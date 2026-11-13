@story:spotify-sdk-probe @domain:personal-apps/apps @priority:must @status:done
Feature: Check official browser playback independently of EMMS
  As the owner of a Spotify Premium account
  I want to hear a selected track through the official SDK in Chrome
  So that I can establish receiver compatibility before changing my music workflow

  Context:
    The Spotify website plays successfully in Chrome. The desktop app is closed.
    This isolated compatibility check does not use or modify the EMMS playlist.
    Authorization uses the existing public client ID, never its client secret.

  Out of scope:
    EMMS control, mixed-queue advancement, persistent browser authorization,
    spectrum capture, and playback through the Spotify desktop app.

  @wireframe:spotify-sdk-probe.svg @wireframe:spotify-sdk-probe-ready.svg
  Scenario: Explicitly authorize the SDK player
    Given I have started a compatibility check for a Spotify track
    Then I see the additional permissions before authorizing
    And playback is disabled
    When I authorize in Spotify and return to the check
    Then I see that the SDK player is ready
    And my saved library authorization is not overwritten

  @wireframe:spotify-sdk-probe-ready.svg @wireframe:spotify-sdk-probe-playing.svg
  Scenario: Hear a bounded preview through the SDK device
    Given I have authorized and the SDK player is ready
    When I click "Play 15 seconds"
    Then the selected track plays through that SDK device
    And I see the track name and Spotify link
    And playback pauses automatically after fifteen seconds
    And the EMMS playlist is unchanged

  @wireframe:spotify-sdk-ending-ready.svg @wireframe:spotify-sdk-ending-listening.svg @wireframe:spotify-sdk-ending-result.svg
  Scenario: Observe the SDK transition at the end without inferring completion
    Given I started the check with a prefix argument and authorized the SDK player
    When I click "Start ending check"
    Then the selected track starts normally without an automatic seek
    And I am asked to confirm audible playback before seeking
    When I hear music and click "Seek final 8 seconds"
    Then the selected track is sought near its end
    And I see a bounded trace of positions, pause flags and track-match booleans
    When a subsequent pause, reset or different track is observed
    Then the owned SDK player stops and disconnects
    And I can copy the safe trace without credentials or track identifiers
    And no completion inference or EMMS playlist advancement occurs

  @wireframe:spotify-sdk-ending-result.svg
  Scenario: Cancel or time out an ending check safely
    Given an ending check is running
    When I click "Pause" or its safety timeout expires
    Then playback stops and the safe trace identifies cancellation or timeout
    And callbacks from the cancelled check cannot restart playback or alter the trace

  @wireframe:spotify-sdk-probe-error.svg
  Scenario Outline: Fail with a safe diagnostic
    Given the check encounters <failure>
    Then I see a short diagnostic without credentials or raw service messages
    And an active ending check includes the safe SDK error name in its trace
    And the check does not fall back to the desktop app
    Examples:
      | failure                 |
      | authorization denial    |
      | SDK account rejection   |
      | a playback HTTP failure |
      | an expired local session |
