# Appendix A. Minimum Enterprise Architecture Metamodel

The minimum metamodel includes the following classes.

## Strategy

- Enterprise
- Boundary
- Principle
- Driver
- Outcome
- Requirement
- Constraint
- Risk
- Exception

## Business

- Capability
- ValueStream
- Process
- Actor
- Role
- Product
- Service
- Consumer

## Information

- InformationConcept
- Ontology
- OntologyVersion
- OntologyProfile
- DataProduct
- Policy
- Shape
- Rule

## Application and Technology

- ApplicationService
- Component
- Repository
- Interface
- TechnologyStandard
- Runtime
- Environment
- Deployment

## Manufacturing

- Pack
- ConstituentPack
- DistributionPack
- Query
- Template
- Projection
- Artifact
- Validator

## Transition

- ArchitectureState
- BaselineArchitecture
- TargetArchitecture
- TransitionArchitecture
- WorkPackage
- Plan
- PlanCertificate
- Migration
- Rollback

## Governance and Evidence

- Claim
- Checkpoint
- Standing
- Evidence
- Receipt
- ExecutionGrant
- Observation
- Event
- Deprecation
- Retirement

Key relations include `realizes`, `requires`, `constrains`, `owns`, `consumes`, `imports`, `projects`, `produces`, `validates`, `authorizes`, `evidences`, `replaces`, and `transitionsTo`.

Every relation should identify provenance and effective lifecycle where material.
