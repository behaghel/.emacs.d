# Google Docs Publishing Tests

Integration tests for the Google Docs publishing domain live here when they span
multiple package/module boundaries. Package-local adapter tests should stay under
`packages/org-google-docs/test/`.

The semantic-classification contract test here loads the repository's real
Org auditor and typographic specimen. Package-local classification tests use
plain audit data, preserving the adapter's independence from that producer.
