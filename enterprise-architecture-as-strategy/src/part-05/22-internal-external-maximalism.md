# 22. Internal and External Maximalism

The correct boundary principle is:

> Maximal ontological leverage internally. Maximal interoperable leverage externally. Minimal ambiguity and authority leakage across the boundary.

Internal maximalism asks how many internal representations can be eliminated. One service fact can drive code, infrastructure, policy, tests, observability, runbooks, and evidence.

External maximalism asks how many consumer-specific uses a published semantic contract can support. A producer may publish a service ontology, constraints, failure semantics, extension points, and conformance rules. Consumers can then manufacture their own SDKs, deployments, policies, monitoring, and compliance mappings.

The producer does not need to manufacture every consumer artifact. It must publish enough stable meaning for consumers to act lawfully.

The boundary should reduce:

- hidden assumptions;
- ambient privilege;
- private dependency leakage;
- unspecified behavior;
- unstable implementation detail.

It should not reduce semantic utility.

This is a departure from conventional API design. APIs expose operations. External ontology packs expose meanings, relationships, constraints, capabilities, proof obligations, and extension points. The API becomes one projection among many.

External maximalism requires heavier governance:

- distributable identity;
- compatibility law;
- capability declaration;
- bounded writes;
- conformance suite;
- support lifecycle;
- promotion receipt;
- consumer execution receipt.

The architecture must preserve sovereignty. The consumer admits the pack under local policy. The pack cannot inherit hidden producer authority or require access to producer-internal graphs.

This model scales knowledge across organizational boundaries without turning the producer into a centralized artifact factory.
