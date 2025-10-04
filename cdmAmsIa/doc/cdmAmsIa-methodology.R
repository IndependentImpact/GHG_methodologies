## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----applicability------------------------------------------------------------
library(cdmAmsIa)
check_applicability_installed_capacity(capacity_kw = 1000, renewable_fraction = 1)
check_applicability_distributed_generation(fossil_fraction_baseline = 0.85)

## ----simulation---------------------------------------------------------------
set.seed(123)
example_data <- simulate_ams_ia_dataset(n_users = 6, n_periods = 4, start_year = 2024, start_month = 10)
knitr::kable(
  head(example_data),
  format = "html",
  digits = 2
)

## ----calculations-------------------------------------------------------------
baseline_gen <- calculate_baseline_generation(example_data, group_cols = "user_id")
baseline_emis <- calculate_baseline_emissions(baseline_gen, grid_emission_factor = 0.75)
project_emis <- calculate_project_emissions(baseline_gen)
emission_reductions <- calculate_emission_reductions(baseline_emis, project_emis)
knitr::kable(head(emission_reductions), format = "html", digits = 2)

## ----monitoring---------------------------------------------------------------
period_summary <- aggregate_monitoring_periods(
  generation_data = example_data,
  monitoring_cols = c("monitoring_label"),
  group_cols = "user_id"
)

knitr::kable(head(period_summary), format = "html", digits = 2)

## ----meta---------------------------------------------------------------------
estimate_emission_reductions_ams_ia(
  generation_data = example_data,
  grid_emission_factor = 0.75,
  group_cols = "user_id"
)

## ----reference----------------------------------------------------------------
function_reference <- tibble::tibble(
  Function = c(
    "`calculate_baseline_generation()`",
    "`calculate_baseline_emissions()`",
    "`calculate_project_emissions()`",
    "`calculate_emission_reductions()`",
    "`aggregate_monitoring_periods()`",
    "`estimate_emission_reductions_ams_ia()`",
    "`simulate_ams_ia_dataset()`"
  ),
  Signature = c(
    "`calculate_baseline_generation(generation_data, generation_col = \"generation_kwh\", group_cols = NULL)`",
    "`calculate_baseline_emissions(baseline_generation, grid_emission_factor, output_col = \"baseline_emissions_tco2e\")`",
    "`calculate_project_emissions(baseline_generation, project_emission_factor = 0, output_col = \"project_emissions_tco2e\")`",
    "`calculate_emission_reductions(baseline_emissions, project_emissions, baseline_col = \"baseline_emissions_tco2e\", project_col = \"project_emissions_tco2e\", output_col = \"emission_reductions_tco2e\")`",
    "`aggregate_monitoring_periods(generation_data, monitoring_cols = c(\"year\", \"month\"), group_cols = \"user_id\")`",
    "`estimate_emission_reductions_ams_ia(generation_data, grid_emission_factor, project_emission_factor = 0, group_cols = NULL)`",
    "`simulate_ams_ia_dataset(n_users = 20, n_periods = 12, start_year = 2023, start_month = 1, mean_generation_kwh = 15000, sd_generation_kwh = 2000, grid_emission_factor = 0.75)`"
  ),
  `Equation (LaTeX)` = c(
    "\\(E_{BL,mp} = \\sum_{i} G_{i,mp}\\)",
    "\\(E_{BL,mp}^{CO2} = E_{BL,mp} \\times EF_{grid}\\)",
    "\\(E_{PR,mp}^{CO2} = E_{BL,mp} \\times EF_{PR}\\)",
    "\\(ER_{mp} = E_{BL,mp}^{CO2} - E_{PR,mp}^{CO2}\\)",
    "\\(ER_{mp}^{agg} = \\sum_{i} ER_{i,mp}\\)",
    "\\(ER_{total} = \\sum_{mp} ER_{mp}\\)",
    "\\(G_{i,mp} \\sim \\mathcal{N}(\\mu_{G}/P, \\sigma_{G}/\\sqrt{P})\\)"
  ),
  Purpose = c(
    "Summed baseline generation for each group or monitoring period.",
    "Baseline emissions using the applicable grid factor.",
    "Project emissions (typically zero for AMS-I.A).",
    "Emission reductions for each monitoring period.",
    "Aggregates monitoring data to reporting periods.",
    "Meta-calculation composing the core equations.",
    "DeclareDesign-based simulation with temporal monitoring metadata."
  )
)

knitr::kable(function_reference, format = "html", escape = FALSE)

## ----workflow-overview, echo = FALSE------------------------------------------
workflow_steps <- tibble::tibble(
  Step = c(
    "Input generation data",
    "calculate_baseline_generation",
    "calculate_baseline_emissions",
    "calculate_project_emissions",
    "calculate_emission_reductions",
    "estimate_emission_reductions_ams_ia"
  ),
  Purpose = c(
    "Collect user-level generation and monitoring metadata.",
    "Aggregate electricity delivered to each user or period.",
    "Apply the grid emission factor to baseline generation.",
    "Account for any residual project emissions (often zero).",
    "Compute emission reductions by differencing baselines and project values.",
    "Provide a single wrapper returning reductions and supporting columns."
  )
)

knitr::kable(workflow_steps, format = "html", escape = FALSE)

