# Truth Gate Adapter Contract

Truth Gate separates tool syntax from policy semantics.

1. An adapter converts a tool-specific mutation payload into a `WriteIntent`.
2. Path classification establishes the exact policy subject before content rules run.
3. Policy modules return typed violations; absence of violations yields ADMIT.
4. A refusal removes the mutation edge represented by that intent; it does not imply the repository or task is blocked.
5. Generated projections are fenced from manual writes. Their lawful mutation path is rematerialization from canonical source.
6. Receipts bind subject, base, head, operation, outcome, consequence and replay information.
7. Hook, CI, and service surfaces consume the same decision representation.

This contract makes new agent/tool integrations an adapter problem rather than a policy fork.
