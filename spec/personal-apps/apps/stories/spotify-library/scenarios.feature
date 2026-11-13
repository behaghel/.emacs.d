@story:spotify-library @domain:personal-apps/apps @priority:must @status:done
Feature: Browse Spotify Liked Songs
  As an Emacs listener with Spotify Premium
  I want to browse my Liked Songs without leaving the editor
  So that my Spotify library is accessible alongside local music

  Out of scope:
    Playback, queue changes, playlist browsing, and library mutations.

  # AC-1
  @wireframe:spotify-library-loading.svg @wireframe:spotify-library.svg
  Scenario: Authorize and browse
    Given my public client ID is stored in pass
    When I open Liked Songs and authorize in my browser
    Then I see track titles, artists, albums, and durations in the shared bottom slot
    And playback and audio capture remain unchanged
    And my refresh credential is stored encrypted in pass

  # AC-2
  @wireframe:spotify-library.svg
  Scenario: Reopen using saved authorization
    Given I have authorized library access
    When I reopen Liked Songs in a fresh session
    Then my saved authorization is refreshed without opening a login browser
    And no token appears in messages or repository files

  # AC-3
  @wireframe:spotify-library.svg
  Scenario: Load another page while editing
    Given I see a partially loaded library
    When I request more songs and return to my document
    Then the next page is appended without moving my editing point or focus
    And a closed library view is not reopened by the response

  # AC-4
  @wireframe:spotify-library-empty.svg @wireframe:spotify-library-error.svg
  Scenario Outline: Empty or interrupted library access
    Given Spotify returns <response>
    When I open or refresh Liked Songs
    Then I see <feedback>
    And existing playback remains unchanged
    Examples:
      | response             | feedback                              |
      | an empty library     | No Liked Songs yet                     |
      | denied authorization | instructions to reconnect             |
      | a network failure    | instructions to retry                 |
      | a rate limit         | how long to wait before retrying      |

  # AC-5
  @wireframe:spotify-library-error.svg
  Scenario: Reject a mismatched browser callback
    Given Spotify authorization is pending
    When an unrelated browser callback reaches the loopback listener
    Then authorization does not complete
    And I can still complete the original authorization
