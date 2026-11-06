@story:spotify-player @domain:personal-apps/apps @priority:must @status:in-progress
Feature: One mixed EMMS playlist with official browser Spotify playback
  As an Emacs listener browsing Liked Songs
  I want Spotify and local music in one editable EMMS playlist
  So that I can listen without the Spotify desktop app or a second queue

  Context:
    EMMS owns ordering, repeat, and transitions. The official SDK supplies Spotify
    audio in Chrome after separate explicit authorization and an audio-activation click.

  Out of scope:
    Remote playlist editing and Spotify desktop control or fallback.

  # AC-1
  @wireframe:spotify-library.svg @wireframe:spotify-player.svg
  Scenario: Enqueue and play without clearing the playlist
    Given I have a local track in my EMMS playlist
    When I press a on a sorted Liked Songs row
    Then that Spotify track is appended without interrupting playback
    When I press RET on another liked song
    Then that occurrence is appended and played through EMMS
    And all previous queue entries remain

  @wireframe:spotify-browser-activating.svg @wireframe:spotify-browser-ready.svg
  Scenario: First Spotify Play prepares audio without a manual connection command
    Given no browser receiver is connected
    When I explicitly play a Spotify occurrence in EMMS
    Then the receiver opens and I see that audio activation is required
    And the player shows that it is waiting for audio rather than playing
    When I click Enable audio
    Then the requested occurrence starts without another Play command

  @wireframe:spotify-browser-activating.svg @wireframe:spotify-browser-ready.svg
  Scenario Outline: Cancel a waiting occurrence before enabling audio
    Given my requested Spotify occurrence is waiting for browser audio activation
    When I <change>
    And I later click Enable audio
    Then the cancelled occurrence does not start
    And unrelated queue entries remain unchanged by the cancelled request
    Examples:
      | change                           |
      | stop playback                    |
      | select another occurrence        |
      | edit the queue                   |
      | select a different playlist      |

  @wireframe:spotify-browser-ready.svg
  Scenario: Connecting again reuses the live receiver
    Given the current receiver is live or awaiting audio activation
    When I request connection again without forcing authorization
    Then no new receiver tab or credential read is needed
    And the current playback occurrence is unchanged

  @wireframe:spotify-browser-inactive.svg
  Scenario: Distinguish an obsolete receiver tab from the current one
    Given I have a receiver tab open
    When its Emacs connection closes
    Then its tab title and status say Inactive
    And its audio and authorization actions are disabled
    And ordinary retirement is not labelled as an SDK error

  # AC-2
  @wireframe:spotify-player.svg
  Scenario: Naturally advance across media backends
    Given Spotify desktop is closed and the browser receiver is connected and activated
    And my EMMS playlist contains Spotify, local file, and Spotify tracks
    When each track finishes naturally
    Then EMMS advances exactly once and chooses the appropriate backend
    And I can pause and seek using the same player controls
    And no Spotify-internal next or previous command is sent

  # AC-3
  @wireframe:spotify-player.svg
  Scenario: Close the panel or cancel an old occurrence
    Given a Spotify track is playing through EMMS
    When I close the player panel
    Then playback and natural advancement continue without capture
    When I skip to another occurrence of the same Spotify URI
    Then completion from the old occurrence cannot advance the new one

  # AC-4
  @wireframe:spotify-player-error.svg
  Scenario: Fail without silently skipping or opening the desktop app
    Given the receiver cannot play a requested Spotify track
    When playback fails or its event connection closes
    Then EMMS stops at that entry with an actionable diagnostic
    And no other track or desktop app is started

  # AC-5
  @wireframe:spotify-browser-ready.svg @wireframe:spotify-browser-error.svg
  Scenario: Authorize and activate a private browser receiver
    When I explicitly connect the browser receiver
    Then I can authorize Spotify and enable audio in Chrome
    And no track starts until EMMS selects it
    And playback refresh authorization is remembered encrypted without replacing my saved library grant
    And access tokens remain in memory
    And disconnect stops the owned SDK instance without advancing EMMS

  @wireframe:spotify-browser-ready.svg
  Scenario: Reconnect without repeating Spotify authorization
    Given I have previously authorized playback and its grant is still valid
    When I reconnect the browser receiver after restarting Emacs
    Then my encrypted playback grant is reused without another OAuth prompt
    And I still enable audio explicitly before EMMS can start a Spotify track

  @wireframe:spotify-browser-ready.svg
  Scenario: Renew an access token without disconnecting the receiver
    Given Spotify is playing through the activated receiver
    When its short-lived access token needs renewal
    Then playback authorization renews without another consent prompt
    And the current occurrence and EMMS queue are unchanged
    And the receiver does not close because an hour has elapsed

  @wireframe:spotify-player-error.svg
  Scenario: Refuse overlap when stopping cannot be confirmed
    Given a Spotify occurrence is playing
    When I skip to a local track and the receiver does not confirm stopping
    Then no local player starts
    And I see an actionable stop-unconfirmed diagnostic
