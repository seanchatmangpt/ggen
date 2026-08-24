# BCINR evidence-contract authority

This subtree is the authored authority for the first reconstitution slice of BCINR's claim/evidence calculus.

## Ownership

Edit only the ontology, generator configuration, templates, and verifier sources in this subtree. Generated projections are downstream artifacts and must not be hand-edited.

The producing authority is `seanchatmangpt/ggen`. `seanchatmangpt/ggen-legacy` is an independent receiver and verifier. `seanchatmangpt/bcinr` may consume a generated contract only after the exact projection is manufactured and receiver-verified.

## Purpose

The contract separates semantic claims that BCINR currently conflates in prose:

- bounded work;
- target-indexed branchlessness;
- semantic equivalence;
- proof citation versus proof receipt;
- runtime receipt;
- authority fencing;
- scoped standing.

Inspection, citations, source comments, workflow definitions, and digest presence cannot manufacture `ALIVE` standing.

## Manufacture

```bash
cd self-host/bcinr-evidence-contract
ggen sync run --config ggen.toml
```

Run the same projection twice from the same exact ontology and require byte identity before copying it to a receiver.

## Standing ceiling

Until the ggen command above executes against an exact source identity, authored ontology/config/template work is `PARTIAL_ALIVE` at most. Connector publication is not manufacture evidence.
