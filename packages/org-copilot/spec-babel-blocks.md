# Spec: Org Copilot Babel Blocks

## Problem
Authors need durable, reviewable AI-maintained document regions that participate in native Org/Babel export workflows without surprising re-generation during export. A `copilot` Babel language should let prompts maintain prose such as executive summaries, or generate another Babel source block such as Mermaid/DOT/Ditaa whose evaluated result is exported.

## Context
Org Babel custom languages are implemented by `org-babel-execute:<language>`. `:exports results` exports evaluated results rather than the source block, and `:results value raw replace` can insert Org markup directly in `#+RESULTS:`. Org Copilot already owns model/backend orchestration and debug traces; interactive chat transcripts and durable suggestion sidecars are separate from Babel block results.

## Decisions
| Decision | Choice | Rationale |
|---|---|---|
| Package shape | New `packages/org-copilot/ob-copilot.el` providing `ob-copilot` | Follows Org Babel language convention and keeps normal chat load path lazy. |
| Execution model | Normal Babel execution writes `#+RESULTS:` | Preserves native `C-c C-c`, Git-visible results, and `:exports results`. |
| Defaults | Backend supplies `:results value raw replace`, `:exports results`, `:context document`, `:output org` when omitted | Makes common authoring syntax concise and safe. |
| Backend reuse | Same Copilot backend family, distinct Babel request kind | Reuses OAuth/model setup without polluting interactive chat transcript. |
| Context default | Current Org document excluding the current Copilot block and its prior result | Avoids recursive prompt/result pollution. |
| Subtree context | Support `:context subtree` | Enables local maintained sections/diagrams. |
| Output contract | Model returns JSON with `output`; malformed/missing output fails closed | Separates model status from exportable result and prevents accidental overwrite. |
| Prose output | `:output org` returns raw Org result text | Lets export include prose/markup, not source. |
| Source output | `:output src:LANG` returns inner language code; backend wraps a nested source block | Enforces structure and avoids model prose around code. |
| Nested headers | Propagate `:file`; set nested `:exports results` | Gives diagram blocks an exportable target without broad unsafe header forwarding. |
| Language safety | `src:LANG` must be Babel-available enough to execute/export | Fail closed for unsupported generated blocks. |
| Freshness | Hash prompt body + relevant headers + schema version; not document context in MVP | Avoids re-evaluating on every document edit while catching prompt/header changes. |
| Freshness storage | Hidden Org comment lines at start of results, e.g. `# copilot-fingerprint: ...` | Metadata remains human-visible in source but is not exported. |
| Export preflight | When loaded, scan Copilot blocks before export; missing/stale results prompt for evaluation and unreviewed export continuation | Protects authors from surprising AI changes while preserving optional export-as-review workflows. |

## Acceptance Criteria
- [ ] AC-1: Given a `copilot` source block without explicit headers, when manually evaluated, then Babel inserts raw replacement results using defaults equivalent to `:results value raw replace`, `:exports results`, `:context document`, and `:output org`.
- [ ] AC-2: Given a `copilot` block in a document, when the backend receives its request, then context excludes the current Copilot block and its prior `#+RESULTS:`.
- [ ] AC-3: Given `:context subtree`, when the backend receives its request, then context is limited to the current subtree excluding the current block/results.
- [ ] AC-4: Given a backend response JSON with `output`, when the block evaluates, then only `output` is inserted as the Babel result and debug records include request and parsed response.
- [ ] AC-5: Given malformed JSON or missing `output`, when the block evaluates, then evaluation errors and any previous result remains unchanged.
- [ ] AC-6: Given `:output src:mermaid :file flow.svg`, when the block evaluates, then the result is a wrapped `mermaid` source block with `:file flow.svg :exports results` and model output as the inner code.
- [ ] AC-7: Given `:output src:LANG` for an unavailable Babel language, when the block evaluates, then it fails before the model call.
- [ ] AC-8: Given existing result metadata whose fingerprint matches the prompt and relevant headers, when export preflight runs, then no model call occurs.
- [ ] AC-9: Given a missing or stale Copilot result during export, when the author declines evaluation, then export aborts.
- [ ] AC-10: Given a missing or stale Copilot result during export, when the author accepts evaluation, then the block evaluates and export asks whether to continue with unreviewed content.

## Invariants
- Normal Copilot chat and review UX must not load or depend on `ob-copilot`.
- Babel execution must not append messages to the interactive chat transcript by default.
- Failed or malformed generation must preserve existing results.
- Normal sidecars must not store raw prompts or credentials; debug buffer may show local ephemeral request/response diagnostics.
- Export must not silently re-run stale Copilot blocks.

## Scope
**May modify:**
- `packages/org-copilot/ob-copilot.el`
- `packages/org-copilot/org-copilot-debug.el`
- `packages/org-copilot/test/ob-copilot-test.el`
- Copilot docs/specs
- later personal Org Babel config under `modules/interactive/org/`

**Must not modify:**
- upstream Org/gptel code
- private setup/secrets
- `org-comments`/`org-suggestions` sidecar schemas for this feature

## Verification Plan
| Criterion | Method | Automated? |
|---|---|---|
| AC-1–AC-4 | ERT with deterministic fake Babel generator | Yes |
| AC-5 | ERT preserving pre-existing result after malformed response | Yes |
| AC-6–AC-7 | ERT for nested source wrapper and unsupported language fail-closed | Yes |
| AC-8–AC-10 | ERT around export preflight with stub prompts/evaluation | Yes |

## References
- Org manual: Exporting Code Blocks, Results of Evaluation, Using Header Arguments, Comment Lines.
- `packages/org-copilot/SPEC.md`
- `packages/org-copilot/org-copilot-debug.el`
