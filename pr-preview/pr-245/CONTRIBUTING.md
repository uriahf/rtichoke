# Contributing

Install the development dependencies for the R package, then enable the
repository’s pre-commit hooks:

``` sh
python -m pip install pre-commit
pre-commit install
```

The hook formats staged R files with Air before each commit. Its Air
version is pinned to the same version used by GitHub Actions. To check
every tracked file without committing, run:

``` sh
pre-commit run --all-files
```
