# 19. Pack and Digital Product Lifecycle

Packs, engines, ontologies, and generated application families should be managed as digital products.

A lifecycle includes:

```text
idea
-> candidate
-> incubating
-> internal
-> promoted
-> supported
-> mature
-> deprecated
-> retired
-> archived
```

The product descriptor should record owner, consumers, support model, roadmap, architecture contract, dependencies, service levels, operating envelope, security posture, cost, risk, evidence, and replacement.

Promotion is a change in standing and jurisdiction. An internal pack becomes distributable only after hidden assumptions are closed or exported, write authority is bounded, compatibility is declared, conformance tests exist, and independent replay succeeds.

Support must be explicit. A published pack without a maintainer, compatibility policy, or deprecation path transfers unmanaged risk to consumers.

Consumer telemetry is also architectural. It should answer which versions are active, which projections are used, where local overlays exist, which deprecated terms remain, and what migration cost is expected. The telemetry must respect consumer sovereignty and privacy; external packs should not assume unrestricted reporting.

IT4IT's digital-product perspective is useful here. Architecture development creates products that continue through build, deploy, operate, support, measure, and retire. The architecture repository should preserve that complete lifecycle.

A generated repository is not complete if its release system, support policy, observability, migration, and retirement remain handcrafted unknowns. Complete substitution requires lifecycle substitution.

This raises the maturity bar. A pack that generates correct source code may be technically impressive while remaining a low-maturity product. Product standing depends on the complete consumer experience.
