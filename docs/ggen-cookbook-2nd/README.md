<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGen Cookbook: A Pattern Language for Autonomic Code Generation (2nd Edition)](#ggen-cookbook-a-pattern-language-for-autonomic-code-generation-2nd-edition)
  - [Building the Book](#building-the-book)
    - [Prerequisites](#prerequisites)
    - [Building](#building)
    - [Testing](#testing)
  - [Structure](#structure)
  - [Contributing](#contributing)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGen Cookbook: A Pattern Language for Autonomic Code Generation (2nd Edition)

**The complete guide to eliminating software's "dark matter" using knowledge-first development and the 80/20 principle.**

This cookbook shows how GGen **inverts the 80/20 equation**:
- **Traditional development**: 80% dark matter, 20% value creation
- **GGen development**: 20% dark matter, 80% value creation

This is the source for the GGen Cookbook 2nd Edition, built with [mdBook](https://rust-lang.github.io/mdBook/).

## 🔥 New in This Edition: The Dark Matter Cookbook

- **[DARK_MATTER_COOKBOOK.md](DARK_MATTER_COOKBOOK.md)** - Complete 80/20 transformation guide
- **Dark Matter Solutions** - Specific patterns for eliminating each category of dark matter
- **Implementation Roadmap** - Step-by-step transformation from traditional to knowledge-first development
- **Quantitative Metrics** - Measure your dark matter elimination success

## Building the Book

### Prerequisites

Install mdBook:

```bash
cargo install mdbook
```

### Building

```bash
# Build the book
mdbook build

# Serve locally with live reload
mdbook serve

# Open at http://localhost:3000
```

### Testing

```bash
# Test all code examples
mdbook test
```

## Structure

The book follows the Alexandrian pattern language approach:

- **Part I**: Foundation (Chapters 1-2)
- **Part II**: Core Engine & CLI (Chapters 3-4)
- **Part III**: Authoring Language (Chapters 5-10)
- **Part IV**: Autonomic System (Chapters 11-13)
- **Part V**: The Ecosystem (Chapters 14-15)
- **Part VI**: Advanced & Enterprise (Chapters 16-17)
- **Appendices**: References (A-C)

## Contributing

To contribute to the book:

1. Edit markdown files in `src/`
2. Update `SUMMARY.md` if adding new chapters
3. Test your changes with `mdbook serve`
4. Submit a pull request

See the main [GGen repository](https://github.com/seanchatmangpt/ggen) for contribution guidelines.

## License

Same as GGen - see LICENSE in the root repository.
