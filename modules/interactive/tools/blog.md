---
domain: publishing
status: draft
last-reviewed: 2026-06-10
---

# Blog Publishing

## Ubiquitous Language

- **Publishing workflow**: Export or synchronization path from authored content to an external medium.
- **Static site**: A Hugo project whose Org sources live under a project-local `content-org` tree and export through ox-hugo.
- **Blog post**: Authored content prepared for a static-site publishing destination, conventionally under `content-org/posts`.
- **Page**: Durable site content, conventionally under `content-org/pages`.
- **Project-local configuration**: `.dir-locals.el` variables such as `denote-directory`, `denote-prompts`, and `org-hugo-base-dir` that select the active site without hardcoding one repository.
- **Capture template**: A command surface for creating a publishable post skeleton.
- **Section index**: A Hugo `_index` Org source that defines a list/landing page for a section.

## ox-hugo Practice

- ox-hugo does not own a `hugo new` equivalent; creation is Org-first.
- Posts are Denote files under `content-org/posts` with Hugo metadata.
- Durable pages are plain Org files under `content-org/pages` or `content-org/<section>`.
- Sections are plain Org `_index.org` files under `content-org/<section>`.
- Export remains ox-hugo's job via `org-hugo-export-wim-to-md` or auto-export.

## Invariants

- Blog workflows consume authored content; they should not own general authoring semantics.
- Blog-specific capture or export helpers should remain isolated from unrelated Org authoring behavior.
- Publishing destinations and local paths should be configurable per project, not hardcoded to one personal site.
- Missing optional packages such as Denote or ox-hugo should fail with actionable user errors or skip non-essential behavior.
- Reusable static-site mechanics live in `packages/hb-static-site/`; `modules/interactive/tools/blog.el` is only an interactive compatibility entrypoint.
