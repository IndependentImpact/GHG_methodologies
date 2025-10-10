# cdmAmsIc

`cdmAmsIc` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-I.C Thermal energy production with or without electricity**.
The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and meta-calculation
wrappers to reproduce emission reduction estimates for renewable thermal energy systems.

## Installation

```
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIc")
```

## Getting Started

```
library(cdmAmsIc)

applicable <- all(
  check_applicability_thermal_capacity(capacity_mwth = 20),
  check_applicability_renewable_supply(renewable_fraction = 0.85),
  check_applicability_fossil_displacement(fossil_heat_share = 0.7)
)

if (applicable) {
  thermal <- tibble::tibble(facility_id = 1, thermal_energy_mwh = 900)
  baseline <- calculate_baseline_thermal_output(thermal)
  emissions <- calculate_baseline_emissions(baseline, baseline_emission_factor = 0.25)
  project <- calculate_project_emissions(baseline, project_emission_factor = 0.02)
  emission_reductions <- calculate_emission_reductions(emissions, project)
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsIc-methodology.Rmd`.
