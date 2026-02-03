PACKAGE  := $(shell sed -n 's/^Package: //p' DESCRIPTION)
VERSION  := $(shell sed -n 's/^Version: //p' DESCRIPTION)
TARBALL  := $(PACKAGE)_$(VERSION).tar.gz

R_SOURCE := $(wildcard R/*.R)
MAN_PAGES := $(wildcard man/*.Rd)
VIGNETTES := $(wildcard vignettes/*.Rmd)
TESTS := $(wildcard tests/testthat/*.R)

.PHONY: all quick document test check build install site sync-bib readme logo clean

all: document test check site install

# Everything except check (for fast iteration)
quick: document test site install

# Regenerate man pages from roxygen comments
document: $(R_SOURCE)
	Rscript -e 'devtools::document()'

# Regenerate hex sticker (run after document since make clean removes man/)
logo: document
	Rscript data-raw/make-hex.R

# Run unit tests
test: $(R_SOURCE) $(TESTS)
	Rscript -e 'devtools::test()'

# Keep vignettes/bibliography.bib in sync with the master copy
sync-bib: src/bibliography.bib
	cp src/bibliography.bib vignettes/bibliography.bib

# Full R CMD check (0 errors, 0 warnings, 0 notes)
check: document sync-bib
	Rscript -e 'devtools::check()'

# Build source tarball
build: document
	Rscript -e 'devtools::build()'

# Install package locally
install: document
	Rscript -e 'devtools::install()'

# Knit README.Rmd to README.md
readme: logo README.Rmd $(R_SOURCE)
	Rscript -e 'devtools::build_readme()'

# Build pkgdown website into docs/
site: document sync-bib readme logo
	Rscript -e 'pkgdown::build_site(lazy = TRUE)'

# Remove generated artifacts
clean:
	rm -rf docs
	rm -rf man
	rm -f $(TARBALL)
	rm -rf $(PACKAGE).Rcheck
	rm -f src/power-rules.aux src/power-rules.bbl src/power-rules.blg
	rm -f src/power-rules.fdb_latexmk src/power-rules.pdf src/power-rules.synctex.gz
	rm -f .DS_Store
