# Erlang Jobs Library Documentation

**Generated from RDF ontology using ggen sync**

This documentation follows the [Diataxis framework](https://diataxis.fr/) for comprehensive, user-friendly technical documentation.

---

## Documentation Quadrants

The Diataxis framework organizes documentation into four distinct types, each serving a specific purpose:

```
                       STUDY                          WORK
                    (learning)                      (doing)
              ┌─────────────────────┬────────────────────────┐
              │                     │                        │
              │    📚 Tutorials     │    🛠️ How-To Guides    │
ACQUISITION   │   (learning-oriented)│   (task-oriented)      │
              │                     │                        │
              ├─────────────────────┼────────────────────────┤
              │                     │                        │
COGNITION     │  📖 Explanation     │    📋 Reference        │
              │ (understanding)     │  (information)         │
              │                     │                        │
              └─────────────────────┴────────────────────────┘
```

---

## 📚 Tutorials (Learning-Oriented)

**Purpose:** Guide newcomers through building their first projects.

**Characteristics:** Step-by-step, hands-on, complete working examples.

1. [Getting Started with ggen for Erlang](tutorials/01-getting-started.md)
   - Install ggen, initialize project, generate first Erlang job queue
   - **Outcome:** Working job queue in 15 minutes

2. [Building Your First Job Queue](tutorials/02-first-job-queue.md)
   - Create RDF specification, generate queue module, run tests
   - **Outcome:** Production-ready job queue with tests

3. [Creating a Supervised Worker Pool](tutorials/03-supervised-worker-pool.md)
   - Design supervision tree, implement workers, handle failures
   - **Outcome:** Fault-tolerant worker pool with OTP supervision

4. [End-to-End: From RDF Spec to Running Erlang App](tutorials/04-rdf-to-running-app.md)
   - Complete workflow: RDF ontology → ggen sync → build → deploy
   - **Outcome:** Deployed Erlang application with monitoring

---

## 🛠️ How-To Guides (Task-Oriented)

**Purpose:** Solve specific problems users encounter.

**Characteristics:** Goal-focused, problem-solving, assumes basic knowledge.

1. [How to Add a Custom Job Backend](howto/01-custom-job-backend.md)
   - Integrate NATS, RabbitMQ, Redis, or custom backend
   - **Outcome:** Job queue using your preferred backend

2. [How to Implement Rate Limiting](howto/02-rate-limiting.md)
   - Token bucket algorithm, leaky bucket, fixed window
   - **Outcome:** Rate-limited job processing

3. [How to Write Benchmarks for Your Jobs](howto/03-benchmarks.md)
   - Measure latency (p50/p95/p99), throughput, memory usage
   - **Outcome:** Performance benchmarks for your job queue

4. [How to Stress Test with PropEr](howto/04-stress-testing.md)
   - Property-based testing for concurrency and fault tolerance
   - **Outcome:** Comprehensive stress tests

5. [How to Customize Generated Templates](howto/05-customize-templates.md)
   - Edit Tera templates, add custom logic, regenerate
   - **Outcome:** Customized code generation for your needs

---

## 📋 Reference (Information-Oriented)

**Purpose:** Provide technical details and API documentation.

**Characteristics:** Precise, complete, searchable, up-to-date.

1. [API Documentation for Generated Modules](reference/01-api-reference.md)
   - Complete API for job_queue, job_worker, job_scheduler
   - **Includes:** Function signatures, types, examples

2. [RDF Ontology Reference](reference/02-rdf-ontology.md)
   - Complete RDF vocabulary for job queue domain
   - **Includes:** Classes, properties, constraints, examples

3. [Template Variable Reference](reference/03-template-variables.md)
   - All available variables in Tera templates
   - **Includes:** Data types, scoping rules, filters

4. [Configuration Options](reference/04-configuration.md)
   - sys.config, vm.args, rebar.config options
   - **Includes:** Defaults, ranges, environment variables

5. [CLI Command Reference](reference/05-cli-reference.md)
   - Complete ggen CLI documentation
   - **Includes:** Commands, flags, examples, outputs

---

## 📖 Explanation (Understanding-Oriented)

**Purpose:** Clarify and illuminate topics in-depth.

**Characteristics:** Big-picture thinking, conceptual, informative.

1. [Why Use RDF for Code Generation](explanation/01-why-rdf.md)
   - Benefits of ontology-driven development
   - **Topics:** Semantic modeling, deterministic generation, knowledge graphs

2. [OTP Design Patterns in the Jobs Library](explanation/02-otp-patterns.md)
   - gen_server, gen_statem, supervision trees, poolboy
   - **Topics:** Fault tolerance, concurrency, message passing

3. [Supervision Tree Architecture](explanation/03-supervision-tree.md)
   - one_for_one, one_for_all, rest_for_one strategies
   - **Topics:** Restart intensities, shutdown sequences, lifecycle

4. [Performance Characteristics and Trade-offs](explanation/04-performance.md)
   - Latency vs throughput, memory vs speed, pull vs push
   - **Topics:** Benchmarks, profiling, optimization strategies

5. [Deterministic Generation Benefits](explanation/05-deterministic-generation.md)
   - Same RDF → same code, every time. Version control, reproducibility
   - **Topics:** Holographic factory metaphor, receipts, audit trails

---

## Quick Start

**New to the Erlang Jobs Library?** Start here:

1. **First-time users:** Begin with [Tutorial 1: Getting Started](tutorials/01-getting-started.md)
2. **Experienced users:** Jump to [How-To Guides](howto/) for specific tasks
3. **API lookups:** Check [Reference](reference/) for technical details
4. **Deep dives:** Read [Explanation](explanation/) for conceptual understanding

---

## Navigation Tips

- **Learning path:** Tutorials → How-To Guides → Explanation → Reference
- **Problem-solving:** Start with How-To Guides, refer to Reference as needed
- **Understanding concepts:** Read Explanation articles
- **Quick lookups:** Use Reference documentation

---

## Generated with ggen

This documentation is **generated from RDF ontology** using `ggen sync`.

**To regenerate this documentation:**

```bash
cd /home/user/ggen/examples/erlang_jobs
ggen sync --audit true
```

**Source:** `.specify/docs/` (RDF ontology for documentation)

**Templates:** `templates/docs/` (Tera templates for markdown generation)

**Output:** `docs/erlang_jobs/` (this documentation)

---

## Documentation Metadata

- **Version:** 1.0.0
- **Generated:** 2026-01-29
- **RDF Source:** `.specify/specs/015-erlang-jobs-example/docs.ttl`
- **Template Version:** 1.0.0
- **Generator:** ggen v6.0.0

---

## Contributing to Documentation

To improve this documentation:

1. **Edit RDF source** in `.specify/specs/015-erlang-jobs-example/docs.ttl`
2. **Edit templates** in `templates/docs/`
3. **Regenerate** with `ggen sync --audit true`
4. **Review changes** in `docs/erlang_jobs/`

**Never edit generated markdown directly** — changes will be overwritten on next `ggen sync`.

---

## License

This documentation is licensed under the same license as the ggen project.

See [LICENSE](../../LICENSE) for details.
