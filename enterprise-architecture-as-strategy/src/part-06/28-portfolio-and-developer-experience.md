# 28. Portfolio, Cost, and Developer Experience

Enterprise architecture becomes strategy only when it changes resource allocation and daily work.

The portfolio view connects capabilities, products, repositories, packs, consumers, cost, risk, lifecycle, and evidence. It should identify:

- duplicated capabilities;
- high-leverage platforms;
- unsupported critical dependencies;
- products with no consumers;
- consumers trapped on deprecated versions;
- architecture debt;
- migration concentration;
- cost per validated consequence.

Combinatorial Maximalism creates economic questions. Generating more surfaces can reduce semantic maintenance but increase compute, storage, review, and support. The objective is not maximal artifact count. It is maximal net leverage.

Useful measures include:

$$
RestatementElimination =
1 -
\frac{manual\ semantic\ restatements}
{total\ semantic\ uses}
$$

and:

$$
VerifiedLeverage =
\frac{validated\ consequences}
{maintenance\ cost\ of\ source\ knowledge}
$$

Developer experience is the local interface to this portfolio.

`ggen architecture doctor` should answer:

- why an ontology or pack is present;
- which dependency introduced it;
- who owns it;
- what it will generate;
- what it may write;
- what it costs;
- what is deprecated;
- what changed since the last run;
- which evidence is missing;
- how to remediate.

Other commands should include:

```text
ggen architecture inspect
ggen architecture impact
ggen ontology graph
ggen ontology diff
ggen pack promote
ggen plan transition
ggen evidence replay
```

The best architecture control is one that explains itself at the point of work. Developers should not need to interpret a distant policy document to understand a local refusal.

Good DX and strong governance are the same design problem: make lawful action easy, explainable, and fast.
