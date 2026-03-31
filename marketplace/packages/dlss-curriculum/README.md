# DLSS Black Belt Curriculum Package

Design for Lean Six Sigma (DLSS) Black Belt certification curriculum following the DMEDI methodology. 53 modules across 5 phases with real-world examples, statistical datasets, exercises, and capstone project.

## Quick Start

```bash
# Generate the curriculum using ggen's pipeline
cd marketplace/packages/dlss-curriculum
ggen sync --verbose

# Preview without writing files
ggen sync --dry-run

# Validate only (no generation)
ggen sync --validate-only
```

## Package Structure

```
dlss-curriculum/
├── ggen.toml              # Package configuration and generation settings
├── package.toml           # Package metadata and dependency info
├── ontology.ttl           # OWL/RDFS ontology (10 classes, 14 properties)
├── instances.ttl          # 53 module instances with full metadata
├── shapes.ttl             # SHACL validation shapes
├── datasets/              # Statistical datasets (7 categories, 20+ files)
│   ├── basic_stats/       # Descriptive statistics datasets
│   ├── control_charts/    # SPC chart datasets (X-bar/R, p, u charts)
│   ├── doe/               # Design of Experiments datasets
│   ├── hypothesis_tests/  # Hypothesis testing datasets
│   ├── process_capability/# Capability analysis datasets
│   ├── regression/        # Regression analysis datasets
│   └── capstone/          # Capstone project datasets
├── queries/               # SPARQL queries (11 files)
│   ├── extract-modules.rq
│   ├── extract-phases.rq
│   ├── extract-instructor-content.rq
│   ├── module-index.rq
│   ├── course-schedule.rq
│   ├── prerequisites.rq
│   ├── learning-objectives.rq
│   ├── exercises.rq
│   ├── examples-by-track.rq
│   ├── capstone-requirements.rq
│   └── assessment-criteria.rq
├── templates/dlss/        # Tera templates (10 files)
│   ├── _curriculum_base.tera   # DMEDI methodology reference
│   ├── syllabus.tera           # Full course syllabus
│   ├── phase.tera              # Phase-level content (per phase)
│   ├── module.tera             # Module content (per module)
│   ├── lesson.tera             # Individual lesson content
│   ├── exercise.tera           # Exercise worksheets
│   ├── instructor-guide.tera   # Instructor timing and notes (per module)
│   ├── student-workbook.tera   # Student progress workbook
│   ├── capstone-template.tera  # Capstone project specification
│   └── assessment-checklist.tera # Quality checkpoint checklist
└── output/                # Generated output directory
    ├── modules/
    ├── phases/
    ├── instructor/
    ├── student/
    ├── exercises/
    ├── assessment/
    └── capstone/
```

## DMEDI Phases Overview

| Phase | Focus | Duration | Modules |
|-------|-------|----------|---------|
| **1. Define** | Charter, MGPP, Risk Management, Communication Plan | Days 1-3 | M1-M10 |
| **2. Measure** | VoC, QFD, Statistics, Minitab, Control Charts, MSA, Capability | Days 4-6 | M11-M21 |
| **3. Explore** | Concept Generation, TRIZ, Hypothesis Testing, ANOVA, Regression, FMEA | Days 7-9 | M22-M35 |
| **4. Develop** | DOE, RSM, Lean Design, DfM, Reliability, Robust Design | Days 10-12 | M36-M49 |
| **5. Implement** | Prototype/Pilot, Process Control, Implementation Planning, Capstone | Days 13-14 | M50-M53 |

## Module Inventory (53 Modules)

### Define Phase (10 modules)

| Code | Module | Difficulty |
|------|--------|-----------|
| M1 | Introduction to DFSS and DMEDI | Introductory |
| M2 | Project Charter Development | Introductory |
| M3 | Management and Planning Product (MGPP) | Intermediate |
| M4 | Risk Management Planning | Intermediate |
| M5 | Communication Plan | Introductory |
| M6 | Team Formation and Roles | Introductory |
| M7 | Voice of the Stakeholder | Intermediate |
| M8 | CTQ Flowdown | Intermediate |
| M9 | Project Planning and Scheduling | Introductory |
| M10 | Define Phase Review | Intermediate |

### Measure Phase (11 modules)

| Code | Module | Difficulty |
|------|--------|-----------|
| M11 | Voice of the Customer (VoC) | Intermediate |
| M12 | Kano Model Analysis | Intermediate |
| M13 | Quality Function Deployment (QFD) | Advanced |
| M14 | Descriptive Statistics with Minitab | Introductory |
| M15 | Probability Distributions | Intermediate |
| M16 | Measurement System Analysis (MSA) | Advanced |
| M17 | Process Capability Analysis | Advanced |
| M18 | Control Chart Fundamentals | Intermediate |
| M19 | Metrics and Scorecards | Intermediate |
| M20 | Data Collection Planning | Introductory |
| M21 | Measure Phase Review | Intermediate |

### Explore Phase (14 modules)

| Code | Module | Difficulty |
|------|--------|-----------|
| M22 | Concept Generation Methods | Intermediate |
| M23 | TRIZ Methodology | Advanced |
| M24 | Pugh Matrix and Concept Selection | Intermediate |
| M25 | Analytic Hierarchy Process (AHP) | Advanced |
| M26 | Monte Carlo Simulation | Advanced |
| M27 | Hypothesis Testing Fundamentals | Intermediate |
| M28 | t-Tests and Paired Comparisons | Intermediate |
| M29 | ANOVA | Advanced |
| M30 | Regression Analysis | Advanced |
| M31 | Multi-Vari Analysis | Advanced |
| M32 | Statistical Tolerance Analysis | Advanced |
| M33 | Design FMEA | Advanced |
| M34 | Reliability Estimation | Advanced |
| M35 | Explore Phase Review | Intermediate |

### Develop Phase (14 modules)

| Code | Module | Difficulty |
|------|--------|-----------|
| M36 | Full-Factorial DOE | Advanced |
| M37 | Fractional Factorial DOE | Advanced |
| M38 | Response Surface Methodology (RSM) | Advanced |
| M39 | Mixture Designs | Advanced |
| M40 | Robust Design - Parameter Design | Advanced |
| M41 | Robust Design - Tolerance Design | Advanced |
| M42 | Lean Design Principles | Intermediate |
| M43 | Design for Manufacturability (DfM) | Advanced |
| M44 | Design for Assembly (DfA) | Intermediate |
| M45 | Reliability Engineering | Advanced |
| M46 | Conjoint Analysis | Advanced |
| M47 | Tolerance Stack-Up Analysis | Advanced |
| M48 | Design Verification Testing | Advanced |
| M49 | Develop Phase Review | Intermediate |

### Implement Phase (4 modules)

| Code | Module | Difficulty |
|------|--------|-----------|
| M50 | Prototype and Pilot Testing | Advanced |
| M51 | Statistical Process Control (SPC) | Advanced |
| M52 | Implementation Planning | Intermediate |
| M53 | DMEDI Capstone Project | Advanced |

## Dataset Inventory (7 Categories)

| Category | Files | Tools | Modules |
|----------|-------|-------|---------|
| **basic_stats** | thermostat_dimensions.csv, hospital_wait_times.csv, generate_basic_stats.R | R, Minitab | M14, M15 |
| **control_charts** | thermostat_xbar_r.csv, hospital_error_rates.csv, defect_counts.csv, control_charts.R | R, Minitab | M18, M51 |
| **doe** | catapult_full_factorial.csv, software_deployment_fractional.csv, response_surface.csv, doe_analysis.R | R, Minitab, JMP | M36, M37, M38 |
| **hypothesis_tests** | supplier_comparison.csv, discharge_time_vs_temperature.csv, battery_before_after.csv, hypothesis_tests.R | R, Minitab | M27, M28, M29 |
| **process_capability** | battery_capacity.csv, loan_processing_time.csv, capability_analysis.R | R, Minitab | M17 |
| **regression** | patient_satisfaction.csv, loan_approval_multi_vari.csv, regression_analysis.R | R, Minitab | M30, M31 |
| **capstone** | (project-specific datasets) | All tools | M53 |

Each dataset category includes a `metadata.json` describing variables, types, and source context.

## Template Inventory (10 Templates)

| Template | Render Mode | SPARQL Query | Output |
|----------|-------------|--------------|--------|
| `_curriculum_base.tera` | Reference | (none) | DMEDI methodology reference |
| `syllabus.tera` | Once | course-schedule.rq | Full course syllabus |
| `phase.tera` | Per phase | extract-phases.rq | Phase overview with module table |
| `module.tera` | Per module | extract-modules.rq | Full module content |
| `lesson.tera` | Per lesson | module-index.rq | Individual lesson |
| `exercise.tera` | Per exercise | exercises.rq | Exercise worksheet |
| `instructor-guide.tera` | Per module | extract-instructor-content.rq | Timing guide and teaching notes |
| `student-workbook.tera` | Once | extract-modules.rq | Progress workbook with checkboxes |
| `capstone-template.tera` | Once | capstone-requirements.rq | Capstone specification |
| `assessment-checklist.tera` | Once | assessment-criteria.rq | Quality checkpoint checklist |

## Generation Pipeline (mu1-mu5)

The curriculum is generated through ggen's five-stage pipeline:

```
mu1 (Load)     -> Load ontology.ttl + instances.ttl into RDF graph
mu2 (Extract)  -> Execute SPARQL queries against the graph
mu3 (Generate) -> Render Tera templates with SPARQL results
mu4 (Validate) -> SHACL validation via shapes.ttl
mu5 (Emit)     -> Write generated files to output/ directory
```

### Template Variables

Templates use ggen's Tera conventions:

- `{{ column_name }}` - Current SPARQL row value by column name
- `{{ sparql_results }}` - Full JSON array of all SPARQL result rows
- Standard Tera filters: `upper`, `lower`, `title`, `length`, `default`
- Loop constructs: `{% for row in sparql_results %}` for iterating over results
- Conditional blocks: `{% if row.phaseName == "Define" %}` for filtering

### Customization Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `course_title` | string | "Design for Lean Six Sigma Black Belt" | Course title |
| `course_duration` | string | "2 weeks" | Course duration |
| `target_audience` | string | "Engineering Professionals" | Target audience |
| `output_format` | string | "markdown" | Output format (markdown, html, pdf) |
| `include_datasets` | boolean | true | Include statistical datasets |
| `include_exercises` | boolean | true | Include exercises with solutions |

### Configuration

```toml
# ggen.toml - Key settings
[package]
name = "dlss-curriculum"
version = "1.0.0"
category = "education"

[variables]
course_title = { type = "string", required = true }
course_duration = { type = "string", default = "2 weeks" }
target_audience = { type = "string", default = "Engineering Professionals" }
```

## Contributing

1. **Ontology changes**: Edit `ontology.ttl` (source of truth). Never edit generated output files.
2. **New modules**: Add instances to `instances.ttl` following the existing pattern.
3. **New datasets**: Add CSV files to `datasets/<category>/` with a `metadata.json`.
4. **New queries**: Add `.rq` files to `queries/` following SPARQL 1.1 conventions.
5. **New templates**: Add `.tera` files to `templates/dlss/` using ggen Tera conventions.
6. **Validation**: Run `ggen validate shapes.ttl` before committing.
7. **Testing**: Run `ggen sync --dry-run` to verify template rendering.

### Template Rules

- Use `{{ column_name }}` for SPARQL row values (not `{{ module.code }}`)
- Use `{% for row in sparql_results %}` for iteration
- NO `{% extends %}` or `{% block %}` (not supported by ggen)
- NO custom filters not registered in ggen
- Use standard Tera filters only: `upper`, `lower`, `title`, `length`, `default`, `date`

## License

MIT License. See [LICENSE](../../LICENSE) for details.
