---
domain: interactive-foundation
status: draft
last-reviewed: 2026-06-10
---

# Navigation

## Ubiquitous Language

- **Project**: A focus context rooted in `project.el`; it is used to find files, buffers, searches, shells, and compilation for one workstream.
- **Perspective**: A named workspace that isolates buffers/windows so multiple projects can remain open while one feels like the active focus.
- **Project bridge**: The integration that maps project roots to perspectives and keeps navigation commands scoped to the current project.
- **Sidebar**: A supporting navigation window, usually Treemacs, that reflects the active project or perspective without stealing focus.

## Invariants

- Project workflow belongs to navigation because it models focus and context, not only coding.
- Perspective/project integration must preserve the illusion that each project is an exclusive workspace even when several projects are open.
- Treemacs alignment should follow the current project or perspective without disrupting the selected editing window.
- Navigation modules may depend on interactive keybinding conventions and must not be loaded in ordinary batch sessions.
