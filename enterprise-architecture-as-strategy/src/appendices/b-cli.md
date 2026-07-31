# Appendix B. Proposed CLI Surface

```text
ggen architecture init
ggen architecture inspect
ggen architecture doctor
ggen architecture graph
ggen architecture impact
ggen architecture baseline
ggen architecture target
ggen architecture transition
ggen architecture comply
ggen architecture explain
ggen architecture export archimate

ggen ontology discover
ggen ontology register
ggen ontology inspect
ggen ontology graph
ggen ontology validate
ggen ontology compose
ggen ontology diff
ggen ontology impact
ggen ontology benchmark
ggen ontology profile
ggen ontology deprecate
ggen ontology retire
ggen ontology replay

ggen pack inspect
ggen pack graph
ggen pack compose
ggen pack verify
ggen pack benchmark
ggen pack promote
ggen pack deprecate
ggen pack retire
ggen pack replay

ggen plan candidates
ggen plan transition
ggen plan verify
ggen plan explain
ggen plan receipt

ggen evidence inspect
ggen evidence verify
ggen evidence graph
ggen evidence replay
```

Every command should support machine-readable output and typed refusal. Human output should identify identity, source, standing, dominant cause, and remediation.

Dry-run planning is the default for consequential commands. Actuation requires an explicit broker path and execution grant.
