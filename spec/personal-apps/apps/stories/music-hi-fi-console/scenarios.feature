@story:music-hi-fi-console @domain:personal-apps/apps @priority:must @status:in-progress
Feature: A native Hi-Fi console that belongs in the editor
  As an Emacs listener working while a mixed playlist plays
  I want a composed, readable player with a living spectrum
  So that music feels considered without competing with my work

  Context:
    The docked player and browser-backed EMMS playback already work.
    This story changes presentation, not playback authority or capture consent.
    The visual draft uses fictional metadata and original fixture artwork.

  Out of scope:
    Immersive Listening Room, WebKit presentation, a new playback engine,
    crossfading, a second playlist, remote Spotify writes and global theme changes.

  References:
    modules/interactive/apps/music-ui.md
    https://github.com/behaghel/.emacs.d/issues/30

  Background:
    Given I am an Emacs listener using the current mixed EMMS playlist

  @wireframe:music-hifi-console-playing.svg
  Scenario: Focus the console without interrupting work or enabling capture
    Given I have already explicitly enabled the visible spectrum
    When I press "'mm"
    Then the shared bottom player receives focus without replacing my document
    And I see clear track identity, source, progress, queue duration and upcoming tracks
    And the artwork, controls and system-mix spectrum share a coherent visual hierarchy
    And upcoming entries have clear title and artist rows with quiet, service-linked attribution
    And the smaller queue count and duration share the transport footer without crowding its controls
    And normal connection readiness adds no badge or redundant keyboard hint
    And rendering does not move document point or change the playlist

  @wireframe:music-hifi-console-paused.svg @wireframe:music-hifi-console-seeking.svg
  Scenario Outline: State feedback is explicit rather than merely decorative
    Given the spectrum is off
    When the current playback state becomes <state>
    Then I see <feedback> accompanied by its state icon
    And I can distinguish the state without relying on colour alone
    And the existing transport commands retain their meanings
    Examples:
      | state                   | feedback                              |
      | paused                  | a Paused label and a Resume control   |
      | awaiting a seek result  | a Seeking label, not invented progress |

  @wireframe:music-hifi-console-empty.svg
  Scenario: Start from an empty playlist without a dead-end screen
    Given my EMMS playlist is empty
    When I open the console
    Then I see actions for Liked Songs and the current queue
    And missing artwork and time are deliberate placeholders
    And the spectrum remains off until I explicitly enable it
    And no network request, audio playback or capture starts merely from opening the console

  @wireframe:music-hifi-console-reconnect.svg
  Scenario: Recover audio without confusing library access with playback readiness
    Given Liked Songs remains available but Spotify audio needs reconnection
    When I inspect the console
    Then the library and audio readiness states are shown separately
    And I see a Reconnect audio action instead of a raw service error
    And using recovery preserves my existing queue

  @wireframe:music-hifi-console-playing.svg
  Scenario: Artwork arrives without interrupting the editor
    Given the current track has a cover in its retained metadata or local album folder
    When the cover becomes available
    Then it fits intact beside the native track text with source attribution where required
    And its arrival preserves document point, player reading position and the EMMS queue
    And clock redraws reuse the cover without new downloads or authorization

  @wireframe:music-hifi-console-playing.svg
  Scenario Outline: Late artwork cannot restore an obsolete view
    Given the current cover is still loading
    When I <action>
    Then the previous cover cannot update an obsolete view
    And a late response cannot replace the new track's artwork or reopen the console
    And artwork cancellation does not stop or advance playback
    Examples:
      | action               |
      | select another track |
      | dismiss the console  |

  @wireframe:music-hifi-console-playing.svg
  Scenario: Dismiss the console without ending the listening session
    Given music is playing with the spectrum explicitly enabled
    When I close the console
    Then music continues under EMMS control
    And capture and animation stop
    And reopening the console does not silently re-enable capture
