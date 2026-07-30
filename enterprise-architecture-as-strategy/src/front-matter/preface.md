# Preface: Strategy Must Survive Contact with Execution

Enterprise architecture is often evaluated by the quality of its descriptions. A good capability map appears coherent. A target-state diagram shows rational boundaries. A roadmap presents a plausible sequence. These artifacts are useful, but they do not answer the decisive question:

> Does the operating enterprise continue to embody the architecture after thousands of local implementation decisions?

The ordinary answer is unknowable. The architecture team publishes intent. Delivery teams translate that intent into software and infrastructure. Security teams translate it again into controls. Operations teams translate it into telemetry and runbooks. Finance translates it into budgets. Product teams translate it into roadmaps. Auditors reconstruct the chain after the fact.

Every group is competent, yet the enterprise accumulates semantic divergence because no common mechanism carries one architectural assertion through all of its consequences.

ggen begins from a different premise. An architectural fact is valuable in proportion to the number of lawful, independently maintained restatements it eliminates.

Suppose the architecture admits that a payment service processes regulated card data, is internet-facing, has a five-minute recovery objective, and is classified as critical. Those assertions should not remain confined to an architecture repository. They should determine encryption policy, data retention, network segmentation, deployment topology, recovery tests, incident severity, approval requirements, monitoring, evidence collection, client behavior, and migration constraints.

That is the central thesis of this book:

> Enterprise architecture becomes strategy when admitted architectural knowledge governs the complete family of operational consequences required to realize the operating model.

The word **admitted** matters. Not every discovered fact has authority. Not every generated artifact has standing. Not every plan may actuate. The Chatman Ecosystem separates observation, admission, derivation, planning, construction, verification, authorization, actuation, and evidence so that automation does not become hidden power.

This separation enables greater ambition, not less. Because authority is bounded, the architecture can be projected widely. Because evidence is explicit, the enterprise can generate more. Because change is modeled as a transition architecture, the organization can move quickly without pretending uncertainty has disappeared.

The book is written backward from a future enterprise in which any proposed change can be traced through capabilities, products, ontologies, packs, repositories, policies, owners, consumers, migration steps, capacity effects, proof obligations, and operating consequences before the change is authorized.

The work is therefore both architectural and constitutional. It asks not only what the enterprise should become, but what evidence and authority are required for the enterprise to truthfully claim that it has become it.
