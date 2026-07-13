# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

A collection of R packages that implement UNFCCC CDM (Clean Development Mechanism) and Paris Agreement Article 6.4 GHG methodologies. Each methodology is its own installable R package. All packages follow a strict shared template documented in `PACKAGE_DEVELOPMENT_TEMPLATE.md`.

## Package layout

```
CDMSmallScale/   # AMS-* packages (e.g. cdmAmsIa, cdmAmsIIIh)
CDMLargeScale/   # ACM* packages (e.g. cdmAcm0002)
ParisAgreement/  # Article 6.4 packages (e.g. A6_4-METH-001)
tools/           # Shared automation scripts
```

Every package follows the standard R package layout with exactly four source file types under `R/`:
- `*_equations.R` — one function per numbered equation in the methodology
- `*_applicability.R` — helpers that evaluate quantitative applicability conditions
- `*_meta.R` — orchestrator functions that compose the equation-level functions
- `*_simulation.R` — `simulate_*()` functions using `DeclareDesign`

## Common commands

Run tests for a single package (from repo root):
```bash
Rscript -e "testthat::test_local('CDMSmallScale/cdmAmsIa')"
```

Run all large-scale package tests:
```bash
Rscript CDMLargeScale/run_tests.R
```

Document a package (regenerate man/ and NAMESPACE):
```bash
Rscript -e "devtools::document('CDMSmallScale/cdmAmsIa')"
```

Check a package (equivalent to `R CMD check --as-cran`):
```bash
Rscript -e "devtools::check('CDMSmallScale/cdmAmsIa')"
```

Build pkgdown site locally for all packages:
```bash
Rscript tools/pkgdown/preview_all_pkgdown.R
```

Scaffold/refresh `_pkgdown.yml` for all packages (requires `PKGDOWN_GITHUB_ORG` env var):
```bash
PKGDOWN_GITHUB_ORG=IndependentImpact Rscript tools/pkgdown/scaffold_pkgdown_yml.R
```

Trigger manual pkgdown rebuild via GitHub Actions:
```bash
gh workflow run pkgdown-matrix --ref main
```

## Mandatory conventions (from `PACKAGE_DEVELOPMENT_TEMPLATE.md`)

- **Every numbered equation** in the methodology source document maps to a dedicated R function.
- All functions accept tibbles and return tidyverse-compatible structures; must be vectorised and stateless.
- Full Roxygen2 docs required: `@param`, `@return`, `@examples`, `@seealso`. Run `devtools::document()` before committing.
- Every package exposes at least one `simulate_*()` function via `DeclareDesign` (in `Suggests`).
- Tests live in `tests/testthat/` and must cover equation functions (with numerical tolerances), applicability helpers with valid and invalid inputs, and simulation output shape/statistics.
- Each package needs a vignette at `vignettes/<package>-methodology.Rmd` with a fixed seven-section structure (see template §7).
- Package names: lowercase, no hyphens, `cdm` prefix, camelCase (e.g. `cdmAmsIa`, `cdmAcm0002`).
- Default author email for `DESCRIPTION`: `independentimpact.org` domain.

## Key cross-cutting files

| File | Purpose |
|---|---|
| `PACKAGE_DEVELOPMENT_TEMPLATE.md` | Authoritative spec for all packages — read this first |
| `VARIABLE_REGISTRY.md` | Shared symbol/variable definitions per methodology — keep consistent |
| `PROGRESS_TRACKING.md` | Status table for all methodology packages |
| `tools/list_packages.R` | Discovers all packages in repo; used by CI matrix |
| `tools/pkgdown/_pkgdown-template.yml` | Template for generating each package's pkgdown config |

## CI

GitHub Actions runs `tools/pkgdown/build_single_pkgdown.R` for every package on pushes to `main` via a dynamic matrix (`.github/workflows/pkgdown-matrix.yml`). The matrix is built by `tools/list_packages.R` which scans for `DESCRIPTION` files. Sites publish to `gh-pages` at `https://<org>.github.io/GHG_methodologies/sites/<package>/`.
