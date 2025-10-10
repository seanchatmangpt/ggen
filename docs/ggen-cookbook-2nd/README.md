# GGen Cookbook: A Pattern Language for Autonomic Code Generation (2nd Edition)

This is the source for the GGen Cookbook 2nd Edition, built with [mdBook](https://rust-lang.github.io/mdBook/).

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
