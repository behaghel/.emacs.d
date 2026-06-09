# Local packages

This directory contains package-style Emacs Lisp libraries that should remain reusable and extractable.

Guidelines:

- Keep package defaults neutral and broadly reusable.
- Put activation policy and personal workflow configuration in `modules/`.
- Keep secrets and truly uncommittable local values out of this directory.
- Prefer conventional package filenames, autoload cookies, README/manual docs, and focused ERT coverage.
