<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Rust Documentation Website Tools - Complete Survey](#rust-documentation-website-tools---complete-survey)
  - [Overview](#overview)
  - [Official Rust Tools](#official-rust-tools)
    - [1. Rustdoc (cargo doc)](#1-rustdoc-cargo-doc)
    - [2. mdBook](#2-mdbook)
  - [Static Site Generators (Language-Agnostic)](#static-site-generators-language-agnostic)
    - [3. MkDocs](#3-mkdocs)
    - [4. Sphinx](#4-sphinx)
    - [5. Docusaurus](#5-docusaurus)
  - [Hosting Platforms](#hosting-platforms)
    - [6. docs.rs](#6-docsrs)
    - [7. Read the Docs](#7-read-the-docs)
    - [8. GitHub Pages](#8-github-pages)
    - [9. Netlify](#9-netlify)
    - [10. Vercel](#10-vercel)
  - [Specialized Tools](#specialized-tools)
    - [11. ROBODoc](#11-robodoc)
    - [12. Pandoc](#12-pandoc)
  - [Comparison Matrix](#comparison-matrix)
  - [Recommendations by Use Case](#recommendations-by-use-case)
    - [API Documentation Only](#api-documentation-only)
    - [Comprehensive Documentation (API + Guides)](#comprehensive-documentation-api--guides)
    - [Modern Documentation Site](#modern-documentation-site)
    - [Large, Complex Documentation](#large-complex-documentation)
    - [Simple, Fast Setup](#simple-fast-setup)
  - [Integration Examples](#integration-examples)
    - [Rustdoc + mdBook Combined](#rustdoc--mdbook-combined)
    - [GitHub Actions Workflow](#github-actions-workflow)
  - [Conclusion](#conclusion)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Rust Documentation Website Tools - Complete Survey

Comprehensive survey of tools for compiling Rust module documentation into websites.

## Overview

This survey covers all major options for generating documentation websites from Rust projects, including official Rust tools, static site generators, and hosting platforms.

## Official Rust Tools

### 1. Rustdoc (cargo doc)

**Type:** Official Rust documentation tool  
**Language:** Rust (bundled with Rust)  
**Best For:** API documentation from code comments

**Features:**
- Extracts documentation from `///` comments
- Generates HTML documentation
- Integrates with Cargo
- Supports code examples and doctests
- Automatic cross-referencing
- Search functionality

**Usage:**
```bash
# Generate documentation
cargo doc

# Generate and open in browser
cargo doc --open

# Generate only for your crate (no dependencies)
cargo doc --no-deps

# Generate with private items
cargo doc --document-private-items
```

**Output:** Static HTML files in `target/doc/`

**Hosting Options:**
- **docs.rs** - Official Rust documentation hosting (automatic for crates.io)
- **GitHub Pages** - Deploy `target/doc/` to `gh-pages` branch
- **Netlify/Vercel** - Deploy static files
- **Custom hosting** - Any static file server

**Pros:**
- ✅ Official tool, always up-to-date
- ✅ Zero configuration needed
- ✅ Automatic API documentation
- ✅ Supports doctests
- ✅ Free hosting on docs.rs

**Cons:**
- ❌ Only generates API docs, not guides/tutorials
- ❌ Limited customization
- ❌ Markdown-only in comments

**Documentation:** [Rustdoc Book](https://doc.rust-lang.org/rustdoc/)

---

### 2. mdBook

**Type:** Static site generator  
**Language:** Rust  
**Best For:** Comprehensive documentation, tutorials, guides

**Features:**
- Markdown-based content
- Live reloading (`mdbook serve`)
- Search functionality
- Theme customization
- Preprocessors and backends
- Multiple output formats (HTML, EPUB, etc.)

**Usage:**
```bash
# Install
cargo install mdbook

# Initialize new book
mdbook init my-book

# Build
mdbook build

# Serve locally with live reload
mdbook serve

# Watch for changes
mdbook watch
```

**Configuration:** `book.toml`

**Hosting Options:**
- **GitHub Pages** - Built-in support
- **Netlify** - Automatic builds
- **Vercel** - Static site deployment
- **Read the Docs** - Via mdBook plugin
- **Custom hosting** - Deploy `book/` directory

**Pros:**
- ✅ Written in Rust, fast
- ✅ Great for tutorials/guides
- ✅ Extensible (preprocessors, backends)
- ✅ Active development
- ✅ Used by Rust project itself

**Cons:**
- ❌ Separate from code (not auto-generated)
- ❌ Requires manual content creation
- ❌ Learning curve for configuration

**Documentation:** [mdBook Guide](https://rust-lang.github.io/mdBook/)

**Alternatives/Extensions:**
- **mdbook-rust** - Write chapters as Rust source files
- **mdbook-epub** - Generate EPUB output
- **mdbook-linkcheck** - Validate links
- **mdbook-mermaid** - Diagram support

---

## Static Site Generators (Language-Agnostic)

### 3. MkDocs

**Type:** Static site generator  
**Language:** Python  
**Best For:** Project documentation with rich features

**Features:**
- Markdown-based
- Extensive plugin ecosystem
- Multiple themes (Material, ReadTheDocs, etc.)
- Search functionality
- PDF generation
- Multi-language support

**Usage:**
```bash
# Install
pip install mkdocs

# Create new project
mkdocs new my-project

# Serve locally
mkdocs serve

# Build
mkdocs build
```

**Configuration:** `mkdocs.yml`

**Hosting Options:**
- **Read the Docs** - Native support
- **GitHub Pages** - Via GitHub Actions
- **Netlify/Vercel** - Static site deployment
- **MkDocs hosting** - Various options

**Pros:**
- ✅ Rich plugin ecosystem
- ✅ Beautiful themes
- ✅ Active community
- ✅ Good for large documentation

**Cons:**
- ❌ Requires Python
- ❌ Not Rust-native
- ❌ More complex setup

**Documentation:** [MkDocs](https://www.mkdocs.org/)

---

### 4. Sphinx

**Type:** Documentation generator  
**Language:** Python  
**Best For:** Large, complex documentation projects

**Features:**
- reStructuredText (RST) markup
- Multiple output formats (HTML, PDF, LaTeX, etc.)
- Extensive extensions
- Automatic API documentation (via extensions)
- Cross-referencing
- Search functionality

**Usage:**
```bash
# Install
pip install sphinx

# Initialize project
sphinx-quickstart

# Build HTML
make html

# Build PDF
make latexpdf
```

**Configuration:** `conf.py`

**Hosting Options:**
- **Read the Docs** - Native support
- **GitHub Pages** - Via Actions
- **Custom hosting** - Deploy `_build/html/`

**Pros:**
- ✅ Very powerful and extensible
- ✅ Multiple output formats
- ✅ Great for large projects
- ✅ Used by many major projects

**Cons:**
- ❌ Steep learning curve
- ❌ Requires Python
- ❌ RST syntax (not Markdown)
- ❌ Complex configuration

**Documentation:** [Sphinx](https://www.sphinx-doc.org/)

---

### 5. Docusaurus

**Type:** Static site generator  
**Language:** JavaScript/React  
**Best For:** Modern documentation with React components

**Features:**
- Markdown and MDX support
- React components in docs
- Versioning support
- Blog functionality
- Search (Algolia integration)
- Dark mode
- i18n support

**Usage:**
```bash
# Install
npx create-docusaurus@latest my-website classic

# Start dev server
npm start

# Build
npm run build
```

**Configuration:** `docusaurus.config.js`

**Hosting Options:**
- **Vercel** - Native support
- **Netlify** - Easy deployment
- **GitHub Pages** - Via Actions
- **Custom hosting** - Deploy `build/`

**Pros:**
- ✅ Modern, beautiful UI
- ✅ React components
- ✅ Versioning built-in
- ✅ Great for product docs

**Cons:**
- ❌ Requires Node.js
- ❌ More complex than mdBook
- ❌ Overkill for simple docs

**Documentation:** [Docusaurus](https://docusaurus.io/)

---

## Hosting Platforms

### 6. docs.rs

**Type:** Documentation hosting  
**Language:** Rust (official)  
**Best For:** crates.io packages

**Features:**
- Automatic builds from crates.io
- Versioned documentation
- Free hosting
- Automatic updates
- Custom builds via `Cargo.toml` metadata

**Usage:**
```toml
# Cargo.toml
[package.metadata.docs.rs]
features = ["feature1", "feature2"]
rustdoc-args = ["--cfg", "docsrs"]
```

**URL Format:** `https://docs.rs/crate-name/version`

**Pros:**
- ✅ Free and official
- ✅ Automatic for crates.io
- ✅ Versioned
- ✅ No configuration needed

**Cons:**
- ❌ Only for published crates
- ❌ Limited customization
- ❌ Build time limits

**Documentation:** [docs.rs](https://docs.rs/)

---

### 7. Read the Docs

**Type:** Documentation hosting platform  
**Language:** Python (but supports any tool)  
**Best For:** Open-source projects

**Features:**
- Free hosting for open-source
- Automatic builds from Git
- Versioning
- PDF generation
- Multiple formats
- Webhooks

**Supported Tools:**
- Sphinx
- MkDocs
- mdBook (via plugin)
- Any static HTML

**Usage:**
1. Connect GitHub/GitLab repository
2. Configure build settings
3. Automatic builds on push

**Pros:**
- ✅ Free for open-source
- ✅ Automatic builds
- ✅ Versioning
- ✅ Multiple tool support

**Cons:**
- ❌ Requires public repository (for free tier)
- ❌ Build time limits
- ❌ Less control than self-hosting

**Documentation:** [Read the Docs](https://readthedocs.org/)

---

### 8. GitHub Pages

**Type:** Static site hosting  
**Language:** Any  
**Best For:** Simple, free hosting

**Features:**
- Free for public repos
- Automatic HTTPS
- Custom domains
- GitHub Actions integration
- Jekyll support (optional)

**Usage:**
```yaml
# .github/workflows/deploy-docs.yml
name: Deploy Docs
on:
  push:
    branches: [main]
jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Build docs
        run: cargo doc --no-deps
      - name: Deploy
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./target/doc
```

**Pros:**
- ✅ Free
- ✅ Easy setup
- ✅ Custom domains
- ✅ GitHub integration

**Cons:**
- ❌ Public repos only (free)
- ❌ Build limits
- ❌ Manual configuration needed

**Documentation:** [GitHub Pages](https://pages.github.com/)

---

### 9. Netlify

**Type:** Static site hosting  
**Language:** Any  
**Best For:** Modern deployment workflows

**Features:**
- Free tier available
- Automatic builds from Git
- Preview deployments
- Custom domains
- HTTPS automatic
- Form handling
- Serverless functions

**Usage:**
1. Connect repository
2. Configure build command: `cargo doc --no-deps`
3. Set publish directory: `target/doc`
4. Automatic deployments

**Pros:**
- ✅ Easy setup
- ✅ Preview deployments
- ✅ Good free tier
- ✅ Fast CDN

**Cons:**
- ❌ Build time limits
- ❌ Requires Rust in build environment

**Documentation:** [Netlify](https://www.netlify.com/)

---

### 10. Vercel

**Type:** Static site hosting  
**Language:** Any  
**Best For:** Modern web projects

**Features:**
- Free tier
- Automatic deployments
- Preview deployments
- Edge functions
- Analytics
- Custom domains

**Usage:**
Similar to Netlify - connect repo and configure build.

**Pros:**
- ✅ Fast global CDN
- ✅ Great DX
- ✅ Preview deployments
- ✅ Free tier

**Cons:**
- ❌ Build time limits
- ❌ Requires Rust in build

**Documentation:** [Vercel](https://vercel.com/)

---

## Specialized Tools

### 11. ROBODoc

**Type:** Documentation extractor  
**Language:** Multi-language  
**Best For:** Extracting headers from code

**Features:**
- Extracts formatted headers
- Multiple output formats
- Language-agnostic
- HTML, PDF, LaTeX output

**Usage:**
```bash
robodoc --src src/ --doc docs/ --html
```

**Pros:**
- ✅ Works with any language
- ✅ Multiple formats

**Cons:**
- ❌ Less common for Rust
- ❌ Requires specific comment format
- ❌ Limited features

---

### 12. Pandoc

**Type:** Document converter  
**Language:** Haskell  
**Best For:** Converting between formats

**Features:**
- Convert between formats
- Markdown to HTML
- PDF generation
- Multiple input/output formats

**Usage:**
```bash
pandoc input.md -o output.html
```

**Pros:**
- ✅ Universal converter
- ✅ Many formats

**Cons:**
- ❌ Not a documentation generator
- ❌ Manual process
- ❌ No Rust integration

---

## Comparison Matrix

| Tool | Type | Language | Best For | Setup Complexity | Customization |
|------|------|----------|----------|------------------|---------------|
| **rustdoc** | API Docs | Rust | API documentation | ⭐ Easy | ⭐⭐ Limited |
| **mdBook** | Static Site | Rust | Guides/Tutorials | ⭐⭐ Medium | ⭐⭐⭐ Good |
| **MkDocs** | Static Site | Python | Project docs | ⭐⭐ Medium | ⭐⭐⭐⭐ Excellent |
| **Sphinx** | Doc Generator | Python | Large projects | ⭐⭐⭐ Complex | ⭐⭐⭐⭐⭐ Excellent |
| **Docusaurus** | Static Site | JavaScript | Product docs | ⭐⭐⭐ Complex | ⭐⭐⭐⭐ Excellent |
| **docs.rs** | Hosting | Rust | crates.io packages | ⭐ Easy | ⭐ Limited |
| **Read the Docs** | Hosting | Any | Open-source | ⭐⭐ Medium | ⭐⭐⭐ Good |
| **GitHub Pages** | Hosting | Any | Simple hosting | ⭐⭐ Medium | ⭐⭐ Limited |
| **Netlify** | Hosting | Any | Modern deployment | ⭐⭐ Medium | ⭐⭐⭐ Good |
| **Vercel** | Hosting | Any | Modern deployment | ⭐⭐ Medium | ⭐⭐⭐ Good |

## Recommendations by Use Case

### API Documentation Only
**Recommended:** `rustdoc` + `docs.rs` or GitHub Pages
- Generate with `cargo doc`
- Host on docs.rs (if published) or GitHub Pages

### Comprehensive Documentation (API + Guides)
**Recommended:** `rustdoc` + `mdBook`
- Use `rustdoc` for API docs
- Use `mdBook` for guides/tutorials
- Link between them

### Modern Documentation Site
**Recommended:** `Docusaurus` or `MkDocs`
- Rich features
- Modern UI
- Good for product documentation

### Large, Complex Documentation
**Recommended:** `Sphinx` + Read the Docs
- Most powerful
- Best for enterprise documentation
- Multiple output formats

### Simple, Fast Setup
**Recommended:** `mdBook` + GitHub Pages
- Rust-native
- Easy setup
- Good for most projects

## Integration Examples

### Rustdoc + mdBook Combined

```bash
# Generate API docs
cargo doc --no-deps

# Build mdBook
mdbook build

# Combine in deployment
# - API docs: target/doc/
# - Guides: book/
```

### GitHub Actions Workflow

```yaml
name: Deploy Documentation

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      
      - name: Generate API docs
        run: cargo doc --no-deps
      
      - name: Build mdBook
        run: |
          cargo install mdbook
          mdbook build docs/
      
      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: |
            target/doc
            docs/book
```

## Conclusion

For **Rust projects**, the most common combinations are:

1. **Standard:** `rustdoc` (API) + `mdBook` (guides) + GitHub Pages
2. **Published Crates:** `rustdoc` + docs.rs (automatic)
3. **Modern:** `Docusaurus` or `MkDocs` for full-featured sites
4. **Enterprise:** `Sphinx` + Read the Docs for complex needs

**For ggen specifically:**
- Current: Diataxis structure in Markdown (no build tool)
- **Core Team Recommendation:** `mdBook` + `rustdoc` + GitHub Pages (see [Core Team Recommendation](CORE_TEAM_RECOMMENDATION.md))
- Alternative: Keep Markdown, deploy directly to GitHub Pages

## References

- [Rustdoc Book](https://doc.rust-lang.org/rustdoc/)
- [mdBook Guide](https://rust-lang.github.io/mdBook/)
- [MkDocs Documentation](https://www.mkdocs.org/)
- [Sphinx Documentation](https://www.sphinx-doc.org/)
- [Docusaurus Documentation](https://docusaurus.io/)
- [docs.rs](https://docs.rs/)
- [Read the Docs](https://readthedocs.org/)

