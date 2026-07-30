# 17. The Canonical Enterprise Architecture Graph

The canonical enterprise architecture graph is the shared semantic substrate of the ecosystem.

It should represent at least:

- enterprise and boundary;
- principle and requirement;
- capability and value stream;
- product and service;
- organization, owner, and steward;
- information concept and ontology;
- application and component;
- technology and environment;
- pack, query, template, and projection;
- plan, work package, and transition architecture;
- claim, evidence, receipt, and standing;
- risk, exception, migration, deprecation, and retirement.

The graph is federated in acquisition but canonical in identity. Repository-specific facts can be imported through adapters while retaining source provenance. No connector receives authority merely because it can read a system.

The graph should answer traversals such as:

```text
requirement
-> constrains capability
-> realized by service
-> implemented by component
-> hosted in repository
-> manufactured by pack
-> imports ontology
-> deployed to environment
-> observed by metric
-> evidenced by receipt
```

The graph is not a replacement for all source systems. Jira may remain authoritative for ticket workflow. Git remains authoritative for commit history. Cloud APIs remain authoritative for observed resources. The architecture graph admits bounded observations from those systems and relates them.

This distinction supports replay. A receipt can identify which source observation, at which version and time, produced an architecture decision.

The repository should expose multiple projections:

- TOGAF work products;
- ArchiMate exchange and views;
- service catalogues;
- roadmaps;
- dependency diagrams;
- risk reports;
- developer diagnostics;
- machine plans;
- governance packets.

The projections are disposable. The identities and relationships are durable.

The graph becomes strategically valuable when it can calculate impact and manufacture obligations, not merely store inventory.
