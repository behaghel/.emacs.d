---
domain: publishing/blog
status: draft
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
- Relative project settings stay anchored at the site root. Mode activation gives Denote and ox-hugo absolute buffer-local roots; generated page and section headers derive their relative Hugo roots from each file's actual location, including custom source-directory depths.
- Missing optional packages such as Denote or ox-hugo should fail with actionable user errors or skip non-essential behavior.
- Reusable static-site mechanics and native `C-c w` mode bindings live in `packages/hb-static-site/`. Loading the library requires only Emacs and Org built-ins, not personal configuration or optional integrations.
- Personal Evil bindings, which-key labels, prose activation, and historical command aliases live in `modules/interactive/tools/blog.el`.
- Page/section creation and local validation do not require Denote, ox-hugo, or Hugo. Post naming delegates to Denote; export delegates to ox-hugo. Hugo themes own the shortcode implementations referenced by exported content.

## User Documentation

Installation, project settings, commands, and isolated usage are documented in the
[package manual](README.org).
