# Diátaxis Best Practices in ggen

[Diátaxis](https://diataxis.fr/) is a systemic framework for technical documentation authoring. It categorizes documentation into four distinct quadrants based on the user's immediate needs: **Tutorials**, **How-To Guides**, **Reference**, and **Explanations**.

As `ggen` scales, structuring its documentation natively within the Diátaxis framework ensures that users can intuitively navigate from absolute beginners to expert integrators without context fatigue.

## The Four Quadrants in ggen

### 1. Tutorials (Learning-Oriented)
**Goal:** Allow the user to learn by doing.
**Location:** `docs/tutorials/`
**Characteristics:**
*   **Focus:** The user's journey.
*   **Tone:** Pedagogical, encouraging, and highly prescriptive.
*   **Structure:** Step-by-step instructions that *must* result in a successful outcome if followed linearly.
*   **ggen Example:** "Building your first RDF template generator from scratch."
*   **Anti-pattern:** Do not explain the underlying graph traversal mechanism deeply here. Just tell them what commands to type to see the result.

### 2. How-To Guides (Problem-Oriented)
**Goal:** Show the user how to solve a specific, real-world problem.
**Location:** `docs/how-to-guides/`
**Characteristics:**
*   **Focus:** The task at hand.
*   **Tone:** Practical and direct.
*   **Structure:** A recipe. Assume the user already knows *how* to use the tool broadly but needs a specific answer now.
*   **ggen Example:** "How to inject an import statement conditionally using `skip_if` frontmatter."
*   **Anti-pattern:** Do not include setup or installation steps. Assume they have `ggen` installed and running.

### 3. Reference (Information-Oriented)
**Goal:** Describe the machinery and architecture accurately and comprehensively.
**Location:** `docs/reference/`
**Characteristics:**
*   **Focus:** The product and its internal mechanics.
*   **Tone:** Dry, objective, austere, and exhaustive.
*   **Structure:** Dictionaries, API schemas, CLI flag lists, and exhaustive property maps.
*   **ggen Example:** "The `Frontmatter` API configuration schema" or "CLI command list for `ggen pack`."
*   **Anti-pattern:** Do not include tutorials or step-by-step guides. Only list what things are and what they do.

### 4. Explanations (Understanding-Oriented)
**Goal:** Explain the *why* behind the architecture.
**Location:** `docs/explanations/`
**Characteristics:**
*   **Focus:** The domain and architectural philosophy.
*   **Tone:** Discursive, historical, and narrative.
*   **Structure:** High-level overviews, architectural deep dives, and "why we built it this way" manifestos.
*   **ggen Example:** "Why ggen uses Tera over Handlebars" or "Understanding the RDF Integration Layer in v2.0."
*   **Anti-pattern:** Do not put code examples that users are meant to copy-paste and run.

---

## Best Practices for Authoring ggen Docs

1. **Avoid Quadrant Bleed:** If you are writing a Tutorial and find yourself explaining the deep architecture of the `PipelineBuilder`, stop. Move that information to an Explanation document and link to it.
2. **Keep the Root Clean:** The root `docs/` folder should contain the `INDEX.md`, high-level manifests, and architectural decisions (ADRs). All user-facing documentation should be strictly bucketed into the four quadrants.
3. **Use Explicit Links:** Liberally cross-link between quadrants. A How-To Guide should link to the Reference manual for specific flag details.
