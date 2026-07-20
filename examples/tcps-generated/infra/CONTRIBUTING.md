# Contributing

Every change must preserve the Japanese canonical domain vocabulary and the distinction between selection, authorization, and actuation.

Required sequence:

1. State the originating abnormality.
2. Name the failed invariant.
3. Add a negative fixture that reproduces the defect.
4. Implement the smallest countermeasure.
5. Add or revise standardized work.
6. Run `python tools/lifecycle.py verify`.
7. Run affected target builds and tests.
8. Attach receipts.

Do not disable tests, weaken refusal types, introduce ambient authority, or convert an infrastructure gap into a successful status.

