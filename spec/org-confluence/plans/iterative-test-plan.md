# Org → Confluence — Iterative TDD Test Plan

Approved vertical-slice execution order for implementing the Confluence export/publish system as defined in [`../SPEC.md`](../SPEC.md).

## Overview

| Iteration | Goal | Key Deliverables |
|-----------|------|------------------|
| **1** | Publish backbone (happy path) | Org → XHTML export (headings, paragraphs, bold, italic, links, lists) + `cfl` page update/create wrappers + `hub/confluence-publish` command |
| **2** | Rich content | Tables, blockquotes, callouts, code blocks, strikethrough, underline |
| **3** | Images | Upload + inline attachment references |
| **4a** | Export polish | Definition lists, footnotes, clearer diagnostics |
| **4b** | Org export dispatch | Subtree/export UX through normal `C-c C-e` flow |
| **4c** | Pull/import | Confluence → Org import and conversion |

## File Structure

All new files are under these directories, which are on `load-path` unconditionally:

- `modules/org/export-confluence/` — the confluence export module
- `lisp/hub-confluence-api.el` — always-on API abstraction
- `test/` — ERT tests

| File | Provides | Responsibility |
|------|----------|----------------|
| `lisp/hub-confluence-api.el` | `hub-confluence-api` | Low-level `cfl` shell wrappers: page create/update, attachment upload, config lookup. Must not depend on Org. |
| `modules/org/export-confluence/export.el` | `org/export-confluence` | Pure Org → Confluence Storage Format (XHTML) backend. Must not shell out. |
| `modules/org/export-confluence/api.el` | `org/export-confluence-api` | `cfl` shell wrappers specific to the confluence publish flow. |
| `modules/org/export-confluence/commands.el` | `org/export-confluence-commands` | User-facing commands (`hub/confluence-publish`, `hub/confluence-publish-dwim`). Composes export + API layer. |

## Iteration 1 — Publish Backbone (Happy Path)

**Slice goal:** A human can write an Org buffer with `#+CONFLUENCE_PAGE_ID: <id>`, run `M-x hub/confluence-publish`, and see content live on Confluence — with headings, bold, italic, links, and lists rendering correctly.

### Sub-Iterations

#### 1a — Export backend core

- **Test file:** `test/org-confluence-export-test.el`
- **Tests to write:**
  - `hub/org-confluence-export-paragraph` — `"Hello world"` → `<p>Hello world</p>`
  - `hub/org-confluence-export-heading-h1` — `"* Title"` → `<h1>Title</h1>`
  - `hub/org-confluence-export-heading-h2` — `"** Subtitle"` → `<h2>Subtitle</h2>`
  - `hub/org-confluence-export-heading-h3` — `"*** Subsub"` → `<h3>Subsub</h3>`
  - `hub/org-confluence-export-heading-h4` — `"**** Deep"` → `<h4>Deep</h4>`
  - `hub/org-confluence-export-mixed` — heading + paragraph → correct order
  - `hub/org-confluence-export-empty` — empty buffer → empty string
- **Red signal:** `ert` fails with "void-function org-confluence-export" or similar
- **Green target:** `org-define-export-backend 'confluence` with minimal translators for paragraph and headline, producing valid XHTML

#### 1b — Inline formatting & links

- **Same test file:** `test/org-confluence-export-test.el`
- **Tests to write:**
  - `hub/org-confluence-export-bold` — `"*bold*"` → `<strong>bold</strong>`
  - `hub/org-confluence-export-italic` — `"/italic/"` → `<em>italic</em>`
  - `hub/org-confluence-export-inline-code` — `"~code~"` → `<code>code</code>`
  - `hub/org-confluence-export-strikethrough` — `"+strike+"` → `<strike>strike</strike>`
  - `hub/org-confluence-export-link-external` — `"[[https://x.com][text]]"` → `<a href=\"https://x.com\">text</a>`
  - `hub/org-confluence-export-link-plain` — `"[[https://x.com]]"` → `<a href=\"https://x.com\">https://x.com</a>`
  - `hub/org-confluence-export-mixed-inline` — bold within link, italic + bold
- **Red signal:** bold/italic/link tests fail — inline markup not translated
- **Green target:** Register `bold`, `italic`, `code`, `link`, `strike-through` translators in the backend

#### 1c — Lists

- **Same test file:** `test/org-confluence-export-test.el`
- **Tests to write:**
  - `hub/org-confluence-export-bullet-list` — `"- item"` → `<ul><li>item</li></ul>`
  - `hub/org-confluence-export-bullet-list-multi` — multiple items
  - `hub/org-confluence-export-ordered-list` — `"1. item"` → `<ol><li>item</li></ol>`
  - `hub/org-confluence-export-ordered-list-multi` — multiple items
  - `hub/org-confluence-export-list-nested` — nested bullet
  - `hub/org-confluence-export-horizontal-rule` — `"-----"` → `<hr/>`
- **Red signal:** list tests fail — no list translators
- **Green target:** Register `plain-list` and `horizontal-rule` translators

#### 1d — `cfl` API wrappers

- **Test file:** `test/org-confluence-api-test.el`
- **Tests to write:**
  - `hub/confluence-api--page-update-command` — returns correct shell command string with `--storage` flag
  - `hub/confluence-api--page-create-command` — returns correct shell command with `--space`, `--title`, `--storage`
  - `hub/confluence-api--page-id-from-buffer` — reads `#+CONFLUENCE_PAGE_ID` from buffer
  - `hub/confluence-api--space-from-buffer` — reads `#+CONFLUENCE_SPACE` from buffer
  - `hub/confluence-api--page-update-command-with-file` — reads XHTML from a temporary `.xhtml` file via `--file`
  - `hub/confluence-api--page-create-command-with-parent` — includes `--parent`
  - `hub/confluence-api--page-create-missing-space` — signals error when no space
  - `hub/confluence-api--page-update-missing-id` — signals error when no page ID
- **Red signal:** ert errors on undefined functions
- **Green target:** `confluence-api.el` with shell-command construction functions. Tests verify command strings without executing them.

#### 1e — Publish command

- **No automated tests** (shells out to `cfl`)
- **Manual verification:**
  1. Open a test Org buffer with `#+CONFLUENCE_PAGE_ID` pointing to a draft Confluence page
  2. Run `M-x hub/confluence-publish`
  3. Verify page content appears on Confluence
  4. Run again — verify content updates (new version)

## Iteration 3 — Images and Attachments

**Slice goal:** A human can write a saved Org buffer with native standalone image links, run `M-x hub/confluence-publish`, and see images plus captions rendered on an existing Confluence page.

**Canonical authoring syntax:**

```org
#+CAPTION: Architecture overview
[[./img/architecture-overview.png]]
```

**Authoring UX:**

- `<im TAB` and `M-x hub/org-insert-image-template` insert:

```org
#+CAPTION: Caption
[[./img/image.png]]
```

- With Yasnippet, caption/path are editable fields.
- Without Yasnippet, prompt for caption and existing file; omit `#+CAPTION` when empty; prefer paths relative to the saved Org buffer file.

### 3a — Image storage export

- **Test file:** `test/org-confluence-export-test.el`
- **Tests to write first:**
  - `hub/org-confluence-export-standalone-image` — `[[./img/foo.png]]` → `<ac:image ac:style="max-width: 100%; height: auto;"><ri:attachment ri:filename="foo.png"/></ac:image>`
  - `hub/org-confluence-export-captioned-image` — `#+CAPTION` adds `ac:alt` and visible italic caption paragraph
  - `hub/org-confluence-export-image-max-width` — image storage includes max-width styling so large images do not exceed page width
  - `hub/org-confluence-export-described-image-link` — `[[./img/foo.png][text]]` remains a normal link
  - `hub/org-confluence-export-remote-image-url` — remote `.png` URL remains a normal link
  - `hub/org-confluence-export-non-image-file-link` — local non-image file remains a normal link
- **Red signal:** image tests fail because current link translator always emits `<a>`.
- **Green target:** standalone plain local image links in exported paragraphs emit Confluence image XHTML and captions; other links stay unchanged.

### 3b — Image asset discovery and validation

- **Test files:** `test/org-confluence-export-test.el`, `test/org-confluence-api-test.el`
- **Tests to write first:**
  - `hub/org-confluence-image-resolves-relative-path` — relative image resolves from `buffer-file-name` directory
  - `hub/org-confluence-image-resolves-absolute-path` — absolute image path is accepted
  - `hub/org-confluence-image-missing-file-errors` — missing image hard-errors
  - `hub/org-confluence-image-unsaved-relative-buffer-errors` — relative image in unsaved buffer hard-errors
  - `hub/org-confluence-image-duplicate-basenames-use-hashed-filenames` — duplicate source basenames get distinct hashed attachment filenames
  - `hub/org-confluence-image-collects-only-exported-standalone-images` — described/remote/non-image links are excluded
- **Red signal:** asset discovery helpers are undefined.
- **Green target:** AST-based exported-image collector returns validated absolute source paths and content-hashed attachment filenames.

### 3c — Attachment upload publish flow

- **Test file:** `test/org-confluence-api-test.el`
- **Tests to write first:**
  - `hub/confluence-api--attachment-upload-command` — builds `cfl attachment upload <page-id> <file>`
  - `hub/confluence-publish-uploads-images-before-page-edit` — shell commands run upload(s), then page edit
  - `hub/confluence-publish-images-require-page-id` — image documents without `#+CONFLUENCE_PAGE_ID` hard-error before create flow
  - `hub/confluence-publish-cleans-temp-xhtml-on-upload-failure` — temp XHTML removed with `unwind-protect`
  - `hub/confluence-publish-continues-when-hashed-attachment-exists` — duplicate hashed attachment upload is treated as already uploaded
- **Red signal:** command builder/upload orchestration absent.
- **Green target:** publish validates and uploads all referenced local images before page edit; any upload failure aborts page edit; temp XHTML cleanup happens on success/failure.

### 3d — Manual acceptance

1. Save an Org buffer containing `#+CONFLUENCE_PAGE_ID`, one local image, and one caption.
2. Run `M-x hub/confluence-publish`.
3. Verify Confluence renders the image and visible caption.
4. Change the local image, publish again, and verify the attachment updates.
5. Verify duplicate basenames and missing files fail before changing the page.

## Executed Status

| Iteration | Status |
|-----------|--------|
| 1a — Export core | Complete |
| 1b — Inline formatting | Complete |
| 1c — Lists | Complete |
| 1d — API wrappers | Complete |
| 1e — Publish command | Complete |
| 2 — Rich content | Complete |
| 3 — Images | Implemented; manual verification pending |
| 4a — Export polish | Complete |
| 4b — Org export dispatch | Not started |
| 4c — Pull/import | Not started |

## Running Tests

```sh
# Run all Confluence export tests
cd ~/.emacs.d && HOME=$PWD emacs --batch -Q -L . -l ert \
  -l test/org-confluence-export-test.el \
  -f ert-run-tests-batch-and-exit

# Run a single test
cd ~/.emacs.d && HOME=$PWD emacs --batch -Q -L . -l ert \
  -l test/org-confluence-export-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote hub/org-confluence-export-paragraph))'

# Run API tests
cd ~/.emacs.d && HOME=$PWD emacs --batch -Q -L . -l ert \
  -l test/org-confluence-api-test.el \
  -f ert-run-tests-batch-and-exit
```

## Key Constraints

- **Every iteration must be end-to-end:** a human can exercise the new behavior (even if automatically verified only at the component level).
- **Tests mock `cfl` by verifying command strings** — never actually shell out in automated tests.
- **Do not implement future iterations** during the current one.
- **Red before green:** tests must fail before any implementation.
- **Spec updates:** if implementation reveals spec gaps, update `SPEC.md` before proceeding.
