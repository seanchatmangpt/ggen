# Collaboration Guide for ggen

This guide covers best practices for collaborating on ggen development, especially for academic papers and research publications.

## Overview

ggen uses RDF-driven ontologies to generate code and documentation, including academic papers. This guide ensures collaborators can work effectively while maintaining consistency and quality.

## Collaboration Workflow

### 1. Setting Up Your Development Environment

```bash
# Clone the repository
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen

# Create your feature branch
git checkout -b feature/your-feature-name

# Install dependencies
cargo build

# Run tests to verify setup
cargo test
```

### 2. Branching Strategy

- **main**: Production-ready code
- **develop**: Development branch for next release
- **feature/**: New features or paper collaborations
- **fix/**: Bug fixes
- **docs/**: Documentation updates

**Branch Naming Convention:**
```
feature/graph-universe-thesis-paper
fix/watch-service-race-condition
docs/api-reference-update
```

### 3. Working on Shared Papers

When collaborating on academic papers:

1. **Create a feature branch per paper:**
   ```bash
   git checkout -b docs/paper-graph-universe-2025
   ```

2. **Use RDF ontologies for paper structure:**
   ```ttl
   @prefix paper: <http://ggen.ai/paper#> .

   paper:GraphUniverse2025 a paper:ResearchPaper ;
       paper:title "The Graph Universe: RDF as Single Source of Truth" ;
       paper:authors "Sean Chatman", "Collaborator Name" ;
       paper:venue "IEEE Transactions" ;
       paper:status "draft" .
   ```

3. **Generate paper templates:**
   ```bash
   ggen generate --template templates/academic/ieee-conference.jinja2 \
                 --ontology ontologies/papers/graph-universe.ttl
   ```

### 4. Code Review Process

#### Before Submitting a Pull Request

1. **Update your branch with latest changes:**
   ```bash
   git fetch origin
   git rebase origin/develop
   ```

2. **Run the full test suite:**
   ```bash
   cargo test --all
   cargo clippy --all
   cargo fmt --check --all
   ```

3. **Verify documentation:**
   ```bash
   cargo doc --no-deps --open
   ```

#### Pull Request Guidelines

**PR Title Format:**
```
[AREA] Brief description (fixes #123)

Examples:
- [feat] Add watch mode for continuous regeneration (fixes #45)
- [docs] Update collaboration guide for paper submissions
- [fix] Resolve race condition in MAPE-K loop (fixes #89)
```

**PR Description Template:**
```markdown
## Summary
Brief summary of changes.

## Type of Change
- [ ] New feature
- [ ] Bug fix
- [ ] Documentation update
- [ ] Paper collaboration
- [ ] Performance improvement

## Related Issues
Fixes #123
Related to #456

## Changes Made
1. Change 1
2. Change 2
3. Change 3

## Testing
- [ ] Added tests for new functionality
- [ ] All tests pass locally
- [ ] Verified with provided test steps

## Checklist
- [ ] Code follows style guidelines
- [ ] Self-review completed
- [ ] Comments added for complex logic
- [ ] Documentation updated
- [ ] No breaking changes
```

### 5. Paper Collaboration Specifics

#### Paper Metadata in Ontology

```ttl
@prefix paper: <http://ggen.ai/paper#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .

paper:YourPaper a paper:ResearchPaper ;
    dc:title "Paper Title Here" ;
    dc:creator "Author Name" ;
    paper:venue "Conference or Journal Name" ;
    paper:submissionDeadline "2025-06-15"^^xsd:date ;
    paper:status "draft" ;  # draft, submitted, accepted, published
    paper:collaborators (
        [ paper:name "Collaborator 1" ; paper:role "Co-author" ]
        [ paper:name "Collaborator 2" ; paper:role "Reviewer" ]
    ) ;
    paper:sections (
        paper:Introduction
        paper:RelatedWork
        paper:Methodology
        paper:Results
        paper:Discussion
        paper:Conclusion
    ) .
```

#### Generating Paper with Collaborators

```bash
# Generate paper with all collaborator templates
ggen generate \
    --ontology ontologies/papers/your-paper.ttl \
    --template templates/academic/ieee.jinja2 \
    --output generated/papers/your-paper-final.pdf
```

#### Managing Collaborator Comments

Create a comments ontology file:

```ttl
@prefix comment: <http://ggen.ai/comment#> .

comment:AuthorFeedback1 a comment:Review ;
    comment:author "Collaborator Name" ;
    comment:date "2025-01-15"^^xsd:date ;
    comment:section paper:Introduction ;
    comment:comment "Suggest expanding on background context." ;
    comment:status "pending" .
```

### 6. Commit Guidelines

**Commit Message Format:**
```
[area] Brief description (max 50 chars)

Longer description explaining the change (wrap at 72 chars).
- Point 1
- Point 2
- Point 3

Fixes #123
```

**Example:**
```
[feat] Implement watch service for continuous regeneration

The watch service monitors RDF files and templates for changes,
automatically triggering regeneration of affected artifacts.

Features:
- File system event monitoring with debouncing
- Dependency graph analysis for affected templates
- Async task spawning for non-blocking regeneration
- Configurable watch intervals and batch sizes

Fixes #45
Closes #89
```

### 7. Documentation Standards

#### Code Comments

```rust
/// Process observations from the MAPE-K feedback loop
///
/// This function analyzes telemetry data to identify patterns
/// and guide the autonomous kernel toward optimal decisions.
///
/// # Arguments
/// * `observations` - Collection of telemetry observations
/// * `context` - Runtime context for decision making
///
/// # Returns
/// * `AnalysisResult` with insights and recommendations
pub fn analyze_observations(
    observations: &[Observation],
    context: &RuntimeContext,
) -> Result<AnalysisResult> { }
```

#### Doc Comments for Papers

```rust
/// RDF-driven academic paper generation
///
/// This module generates publication-quality academic papers
/// from RDF ontologies, supporting multiple venues and formats.
///
/// # Supported Formats
/// - IEEE Conference
/// - ACM Conference
/// - NeurIPS
/// - arXiv
/// - PhD Thesis
/// - Bibliography
///
/// # Example
///
/// ```rust,ignore
/// let paper = AcademicPaper::from_ontology(
///     "ontologies/papers/my-paper.ttl"
/// )?;
/// paper.generate_latex(Format::IEEEConference)?;
/// ```
pub mod academic_papers;
```

### 8. Resolving Conflicts

When merge conflicts occur:

1. **Update your branch:**
   ```bash
   git fetch origin
   git rebase origin/develop
   ```

2. **Resolve conflicts using an editor**
   ```bash
   git mergetool  # Use your configured merge tool
   ```

3. **After resolving:**
   ```bash
   git add .
   git rebase --continue
   ```

4. **Force push to your feature branch:**
   ```bash
   git push origin feature/your-feature -f
   ```

### 9. Communication Channels

- **Issues**: Bug reports and feature requests
- **Discussions**: Ideas, questions, and community conversation
- **Pull Requests**: Code review and collaboration
- **Email**: For security issues only

### 10. Meeting Etiquette (for distributed team)

- Come prepared with updates on your work
- Share progress on paper collaborations
- Discuss blockers and ask for help
- Keep meetings to scheduled time
- Record key decisions and action items

## Recognition and Attribution

We recognize all contributors:

- **Code Contributors**: Listed in CONTRIBUTORS.md
- **Paper Co-authors**: Listed in paper metadata ontology
- **Reviewers**: Mentioned in paper acknowledgments
- **Issue Reporters**: Referenced in release notes

## Questions and Support

- **General Questions**: Start a [Discussion](https://github.com/seanchatmangpt/ggen/discussions)
- **Technical Issues**: [Open an Issue](https://github.com/seanchatmangpt/ggen/issues)
- **Direct Contact**: Email `sean@chatmangpt.com`

## Code of Conduct

All collaborators agree to follow our [Code of Conduct](CODE_OF_CONDUCT.md).
