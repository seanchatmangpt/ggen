# 7. Business and Information Systems Architecture

Phase B defines the operating model that architecture must enable. Phase C defines the information and application systems that realize it. The phases should be tightly linked but not collapsed.

Business Architecture establishes:

- value streams;
- business capabilities;
- organizational actors;
- decision rights;
- process variation;
- standardization requirements;
- integration requirements;
- customer and partner relationships;
- performance outcomes.

The operating model is the key strategic choice. A high-integration, high-standardization enterprise requires shared data and standardized processes. A low-integration, low-standardization enterprise should not be forced into a universal platform merely because central architecture prefers uniformity.

The architecture graph must therefore represent both common law and permitted local variation.

Information architecture identifies the semantic objects required by the operating model: customer, product, employee, asset, agreement, event, entitlement, risk, location, and many others. Each object needs authority, stewardship, lifecycle, quality policy, and compatibility rules.

Application architecture identifies which services create, transform, distribute, and consume those objects. It models interfaces, interactions, product boundaries, and replacement relationships.

The important relationship is:

```text
business capability
-> requires information capability
-> realized by application service
-> implemented by solution building blocks
-> observed by operating evidence
```

This trace allows architecture to detect orphan states. A business capability with no application realization is aspirational. An application with no capability relationship may be accidental complexity. A shared data object with no steward is ungoverned. A platform service with no consumers is inventory, not leverage.

ggen can project several useful views from the same graph:

- capability map;
- value-stream map;
- information-concept map;
- application cooperation view;
- ownership matrix;
- interface catalogue;
- consumer contract;
- impact graph;
- migration backlog.

These are views, not separate authorities. Their consistency follows from shared identity.

Phase C is complete only when the architecture has defined how information and application services will behave across lifecycle states, not simply how they are arranged in the target diagram.
