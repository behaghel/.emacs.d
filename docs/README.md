---
domain: configuration-system
status: draft
last-reviewed: 2026-06-08
---

# Architecture Governance

## Ubiquitous Language

- **Performance discipline**: Rule set requiring measurement, UX preservation, and standard mechanisms before custom optimization code.
- **Domain spec**: Colocated README or sibling spec that captures domain language, invariants, and behavior contracts.
- **Architecture rule**: Constraint that protects module boundaries, runtime layers, or user experience consistency.
- **Plan**: Evolving non-normative document that records sequencing, measurements, and open questions.

## Invariants

- Specs and plans must not duplicate the structural ownership encoded in `domains.yaml`.
- Behavioral specs should have observable acceptance criteria and verification methods.
- Architecture guidance should preserve batch safety, predictable module boundaries, and stable interactive UX.
- Performance documentation must distinguish desired behavior from measured facts.
- Docs that govern behavior should point to the relevant code/spec locations rather than restating generated mappings.

## Integration Notes

This domain supplies guidance consumed by quality tooling and implementation work.  When a rule becomes mechanically enforceable, prefer adding a check in the quality system rather than relying only on prose.
