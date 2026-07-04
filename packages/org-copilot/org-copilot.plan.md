---
domain: authoring.copilot
status: draft
last-reviewed: 2026-07-02
---

# Org Copilot MVP TDD Plan

This plan implements `packages/org-copilot/README.md` through vertical slices.

## Iteration 1: Loadable package and in-memory AI comments

- Slice goal: A user can load `org-copilot`, add normalized AI comments to the current Org buffer session, inspect them through package APIs, and clear the session.
- User interaction path: load package in an Org buffer, call a test helper/API equivalent of receiving a review response, then clear the session.
- Tests first:
  - `org-copilot-core-loads-without-gptel`
  - `org-copilot-session-stores-and-lists-comments`
  - `org-copilot-clear-session-removes-comments`
- Expected red signal: `require` fails because files/features do not exist.
- Minimal green target: `org-copilot.el`, `org-copilot-model.el`, and `org-copilot-session.el` with plist validation, per-buffer state, add/list/clear helpers, and no hard `gptel` dependency.
- Feedback checkpoint: package loads and can hold ephemeral AI comments without UI.

## Iteration 2: Context-panel provider renders AI comments

- Slice goal: AI comments appear as AI-labeled rows in the right context panel.
- User interaction path: add AI comments to an Org buffer, enable/open `org-copilot`, and see rows in the context panel.
- Tests first:
  - `org-copilot-context-panel-registers-provider`
  - `org-copilot-context-panel-collects-visible-items`
  - `org-copilot-context-panel-renders-ai-label-and-status`
- Expected red signal: no context-panel provider or renderer exists.
- Minimal green target: `org-copilot-context-panel.el`, `org-copilot-open`, provider registration, side-item collection, and simple row rendering.
- Feedback checkpoint: the side panel has visible AI comment rows backed by session state.

## Iteration 3: Diff preview for suggestions

- Slice goal: A user can view a suggested replacement as a dedicated read-only diff buffer.
- User interaction path: place point on an AI comment with a suggestion and run `org-copilot-view-diff-at-point`.
- Tests first:
  - `org-copilot-diff-buffer-is-read-only`
  - `org-copilot-diff-buffer-shows-old-and-new-text`
  - `org-copilot-view-diff-errors-without-suggestion`
- Expected red signal: diff command and buffer mode do not exist.
- Minimal green target: `org-copilot-diff.el`, diff buffer rendering, metadata linking to source/comment.
- Feedback checkpoint: diff visualization exists but does not yet apply changes.

## Iteration 4: Safe accept and dismiss actions

- Slice goal: A user can accept a valid suggestion, stale invalid suggestions, and dismiss comments.
- User interaction path: run accept/dismiss from a panel row or diff buffer.
- Tests first:
  - `org-copilot-accept-replaces-valid-target-text`
  - `org-copilot-accept-marks-comment-accepted`
  - `org-copilot-accept-refuses-and-stales-on-target-mismatch`
  - `org-copilot-dismiss-removes-comment`
- Expected red signal: commands are missing or do not mutate state/source.
- Minimal green target: safe source validation, source replacement, status update, dismissal.
- Feedback checkpoint: author-controlled source mutation works with stale protection.

## Iteration 5: LLM adapter protocol and fake review command path

- Slice goal: `org-copilot-review-dwim` can request a review through an adapter function and turn normalized results into visible AI comments.
- User interaction path: configure a fake adapter, run `org-copilot-review-dwim` on a region/subtree, then see comments.
- Tests first:
  - `org-copilot-review-dwim-prefers-region`
  - `org-copilot-review-dwim-falls-back-to-subtree`
  - `org-copilot-review-dwim-errors-without-adapter`
  - `org-copilot-review-dwim-installs-normalized-comments`
- Expected red signal: review command and adapter protocol do not exist.
- Minimal green target: `org-copilot-llm.el`, request plist construction, adapter variable, normalized response installation.
- Feedback checkpoint: end-to-end fake LLM review creates panel comments without network access.

## Iteration 6: Bottom chat view skeleton

- Slice goal: A user can open a source-buffer-scoped bottom chat, optionally focused around an AI comment.
- User interaction path: run `org-copilot-chat` from source or comment row.
- Tests first:
  - `org-copilot-chat-opens-bottom-view-for-source`
  - `org-copilot-chat-focuses-current-comment`
  - `org-copilot-chat-keeps-one-session-per-source-buffer`
- Expected red signal: chat command/view does not exist.
- Minimal green target: context-panel bottom view descriptor and a narrow chat buffer mode with focus metadata. Real streaming can remain adapter-backed later.
- Feedback checkpoint: bottom panel exists and preserves source/comment focus.

## Iteration 7: Optional gptel adapter

- Slice goal: Users with `gptel` installed can use it as the first real LLM backend.
- User interaction path: load `org-copilot-gptel`, configure gptel, run review command.
- Tests first:
  - `org-copilot-gptel-loads-optionally`
  - parser tests for strict JSON normalization
  - prompt/request construction tests
- Expected red signal: adapter file does not exist.
- Minimal green target: optional `(require 'gptel nil 'noerror)`, prompt builder, JSON parser/validator, clear missing-gptel error.
- Feedback checkpoint: package boundary remains optional while enabling real model use.

## Backlog / next design slices

These items need a fresh grill/design pass before implementation where noted.

### Section and full-document suggestions

- Status: deferred; grill again before implementation.
- Goal: allow full-document and section chat responses to include suggestions.
- Full-document suggestions:
  - no side-panel row for unanchored full-document suggestions;
  - preview-only in a left-side suggestion buffer;
  - no accept/apply until explicitly designed.
- Section suggestions:
  - response may include `heading_line`, `section_title`, and/or `section_path`;
  - if in section context and no explicit anchor is returned, current section is the implicit anchor;
  - replacement target is section body only, preserving the heading;
  - normalized accept inserts one trailing newline;
  - anchored section suggestions should become normal AI comment rows with accept/dismiss/undo.
- Preview UX:
  - read-only `org-mode` preview buffer;
  - displayed on the left so source can remain next to the right side panel;
  - section previews should use Org narrowing semantics for comparison.

### Chat scrolling UX

- Status: deferred; grill before implementation.
- Problems observed:
  - navigating long chat history is tedious;
  - prompt and newest response can be hard to keep in view.
- Design questions to resolve:
  - when to auto-scroll to prompt vs preserve reading position;
  - whether prompt should stay visible independently of transcript scrolling;
  - commands/keys for jump-to-latest, jump-to-prompt, and scroll transcript;
  - behavior after async model responses arrive.

### Secret memoization

- Status: deferred.
- Problem: `auth-source-pass`/YubiKey prompts on every model contact are too cumbersome.
- Goal: memoize the model API secret after first unlock.
- Design questions to resolve:
  - cache lifetime: Emacs session, timeout, or manual reset;
  - command to clear cached secret;
  - whether memoization belongs in generic AI config or Org Copilot adapter setup.

### Auto-enable Org Copilot mode

- Status: deferred.
- Problem: `org-copilot-mode` currently has to be enabled manually per Org buffer.
- Candidate behavior:
  - add `org-copilot-mode` to `org-mode-hook`, or
  - provide a user option controlling auto-enable.
- Needs review of startup cost and whether every Org buffer should get Copilot context-panel integration.
