---
domain: configuration-system/secrets-private
status: draft
---

# Secrets and Private Setup

## Ubiquitous Language

- **Secret store**: External credential backend such as pass, GPG, YubiKey, SSH agent, or auth-source.
- **Private setup**: Gitignored machine-specific configuration loaded before modules consume private values.
- **Loopback pinentry**: Emacs-mediated pinentry mode used so GPG prompts work inside Emacs workflows.

## Invariants

- Tracked configuration may define integration points for secret stores, but must not contain secrets.
- Machine-specific paths, work-only integrations, and sensitive local values belong in `private/setup.el`.
- Secret-store integration should be available before interactive modules or mail/workflow modules request credentials.
- On macOS, GPG requests use external pinentry (`ask`), keeping card insertion and PIN prompts outside the Emacs minibuffer. The configured GPG agent chooses the native pinentry program.
- Linux and other non-macOS platforms retain Emacs-mediated loopback pinentry, including terminal-only workflows.
- Required authentication infrastructure should fail visibly unless the integration is explicitly optional and guarded.
