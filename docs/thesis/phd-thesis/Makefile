# PhD Thesis Build Makefile
# Canonical build: XeLaTeX + BibTeX (3-pass for TOC and cross-references)

LATEX    := xelatex
BIBTEX   := bibtex
MAIN     := thesis
TEX_FILE := $(MAIN).tex
PDF_FILE := $(MAIN).pdf
WORDCOUNT_SCRIPT := scripts/count_words.py

.PHONY: thesis draft clean wordcount view help

## Default target: full three-pass build
thesis: $(PDF_FILE)

$(PDF_FILE): $(TEX_FILE) bib-data/references.bib
	$(LATEX) $(MAIN)
	$(BIBTEX) $(MAIN)
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

## Single-pass draft (faster, citations/TOC may be incomplete)
draft:
	$(LATEX) $(MAIN)

## Remove all build artifacts
clean:
	rm -f $(MAIN).aux $(MAIN).bbl $(MAIN).blg $(MAIN).log \
	       $(MAIN).out $(MAIN).toc $(MAIN).lof $(MAIN).lot \
	       $(MAIN).pdf chapters/*.aux appendices/*.aux

## Word count using the count_words.py script
wordcount:
	@if [ -f "$(WORDCOUNT_SCRIPT)" ]; then \
	    python3 $(WORDCOUNT_SCRIPT); \
	else \
	    texcount -sum -1 chapters/*.tex | awk '{s+=$$1} END {print s " words"}'; \
	fi

## Open the generated PDF
view: $(PDF_FILE)
	@if command -v xdg-open >/dev/null 2>&1; then \
	    xdg-open $(PDF_FILE); \
	elif command -v open >/dev/null 2>&1; then \
	    open $(PDF_FILE); \
	else \
	    echo "Cannot auto-open: open $(PDF_FILE) manually"; \
	fi

help:
	@echo "Thesis build targets:"
	@echo "  make thesis    - Full XeLaTeX build with bibtex (default)"
	@echo "  make draft     - Single-pass draft (fast)"
	@echo "  make clean     - Remove build artifacts"
	@echo "  make wordcount - Count words in chapters"
	@echo "  make view      - Open the generated PDF"
