PACKAGE  := $(shell sed -n 's/^Package: //p' DESCRIPTION)
VERSION  := $(shell sed -n 's/^Version: //p' DESCRIPTION)
TARBALL  := $(PACKAGE)_$(VERSION).tar.gz

R_SOURCE := $(wildcard R/*.R)
TESTS := $(wildcard tests/testthat/*.R)

.PHONY: all quick document test check build install site readme logo clean

all: document test check site install

# Everything except check (for fast iteration)
quick: document test site install

# Regenerate man pages from roxygen comments
document: $(R_SOURCE)
	Rscript -e 'devtools::document()'

# Regenerate hex sticker
logo:
	Rscript data-raw/make-hex.R

# Run unit tests
test: $(R_SOURCE) $(TESTS)
	Rscript -e 'devtools::test()'

# Full R CMD check (0 errors, 0 warnings, 0 notes)
check: document
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
site: document readme logo
	Rscript -e 'pkgdown::build_site(lazy = TRUE)'

# Remove build artifacts (man/ is tracked in git, so it stays)
clean:
	rm -rf docs
	rm -f $(TARBALL)
	rm -rf $(PACKAGE).Rcheck
	rm -f .DS_Store
