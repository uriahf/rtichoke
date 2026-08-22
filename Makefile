.PHONY: all test doc format format-check check coverage install clean

all: doc format test check

# Run testthat unit tests
test:
	Rscript -e 'devtools::test()'

# Update roxygen documentation and NAMESPACE
doc:
	Rscript -e 'devtools::document()'

# Format R files using Air
format:
	@if command -v air >/dev/null 2>&1; then \
		air format R/ tests/ vignettes/; \
	else \
		Rscript -e 'styler::style_pkg()'; \
	fi

# Check formatting using Air
format-check:
	@if command -v air >/dev/null 2>&1; then \
		air format --check R/ tests/ vignettes/; \
	else \
		echo "Air is not installed, skipping air --check"; \
	fi

# Run R CMD check
check: doc
	Rscript -e 'devtools::check()'

# Calculate code coverage
coverage:
	Rscript -e 'covr::package_coverage()'

# Install local package
install: doc
	Rscript -e 'devtools::install()'

# Clean build artifacts
clean:
	rm -rf *.tar.gz *.Rcheck
