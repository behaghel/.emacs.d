# Org → Confluence — Iterative TDD Test Plan

Approved vertical-slice execution order for implementing the Confluence export/publish system as defined in [`../SPEC.md`](../SPEC.md).

## Overview

| Iteration | Goal | Key Deliverables |
|-----------|------|------------------|
| **1** | Publish backbone (happy path) | Org → XHTML export (headings, paragraphs, bold, italic, links, lists) + `cfl` page update/create wrappers + `hub/confluence-publish` command |
| **2** | Rich content | Tables, blockquotes, callouts, code blocks, strikethrough, underline |
| **3** | Images | Upload + inline attachment references |
| **4** | Polish & pull | Subtree export, Confluence → Org import, error handling |

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

## Executed Status

| Iteration | Status |
|-----------|--------|
| 1a — Export core | Complete |
| 1b — Inline formatting | Complete |
| 1c — Lists | Complete |
| 1d — API wrappers | Complete |
| 1e — Publish command | Complete |
| 2 — Rich content | Complete |
| 3 — Images | Not started |
| 4 — Polish & pull | Not started |

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
