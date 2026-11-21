# Scoped package installation

Run the command through the task's development environment. The target is an
explicit installed configuration, **not implicitly the invoking worktree**:

```sh
as <task-slug> run ./scripts/package-install --config /path/to/installed/config --package gdocs
```

Default output is a JSON dry-run plan: selected package, committed revision,
current checkout revision and existing dependency closure. It reads Git and cache
data without loading Straight or personal initialization. No source checkout,
package build, cache write, fetch or clone occurs in dry-run mode.

Review that revision, then explicitly apply it when installation is authorized:

```sh
as <task-slug> run ./scripts/package-install --config /path/to/installed/config \
  --package gdocs --apply --expect FULL_REVISION_FROM_DRY_RUN
```

Do not run another package-manager operation concurrently. The installer uses a
local lock for its own invocations; an existing lock is a diagnostic, not permission
to remove it. Do not retry an interrupted apply without inspecting the target.

## Preconditions and scope

- The configuration is a clean Git checkout with a committed full package pin.
- The package repository is clean and the pinned commit already exists locally.
  Obtain missing source through a separately reviewed operation; this command
  never fetches or clones.
- Existing dependency recipes, builds and autoload caches are available. Cached
  dependency checkouts match their committed pins. Emacs built-ins do not require
  artificial package caches or checkouts.
- The supported Straight cache format is `:tanat`, with the exact running Emacs
  version and symlink-based builds. Other formats/layouts fail rather than
  initiating a broad rebuild.
- Select an ordinary cached Git package whose main library has the package's name.
  The package manager itself and repositories shared by several cached packages
  are not supported by this single-package command.

Apply checks out the selected commit detached, uses the installed Straight
implementation to rebuild only that package, and loads its compiled main library
in another fresh batch Emacs. It never loads personal initialization or replaces
live definitions. Compilation is synchronous byte compilation; background native
compilation is disabled. Building/loading reviewed package code is not an OS
sandbox: use trusted source and the existing reviewed manager.

Before publishing the cache, the installer checks unrelated build/autoload entries
and recipe-lookup metadata, preserves the original eager-check list, and rejects
concurrent cache changes. Only the selected package's entries may change. Success
returns `"mode":"installed"`, the exact revision, compiled library path,
`"unrelated-cache-changes":0` and `"restart-required":true`.

## Failure and activation

A preflight rejection leaves the installation unchanged. Failure after checkout
or compilation may leave the **selected** source/build changed; the command reports
failure, does not roll back implicitly, and does not publish an unaudited cache.
Inspect the error and selected checkout/build before deciding how to recover.

Successful disk installation is not live activation. Restart Emacs normally, then
verify that definitions resolve to the managed compiled build rather than a local
development override. Do not synchronize Google Docs from an old process while a
new gdocs library installation is awaiting restart.

## Isolated regression checks

```sh
as <task-slug> run ./scripts/elisp-ert test/configuration-system/package-install-test.el
```

`STRAIGHT_BUILD_PATH` locates existing dependencies and the sibling public
`repos/straight.el/straight.el` source for real-manager fixture tests. These tests
create disposable configurations and Git repositories; they never apply to the
installed configuration. Missing manager source is reported as a skip, not a
successful real-manager installation test.
