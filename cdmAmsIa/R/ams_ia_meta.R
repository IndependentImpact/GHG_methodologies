
#' Estimate emission reductions under AMS-I.A
#'
#' Composes the equation-level functions to compute emission reductions for a dataset describing
#' user-level electricity generation and the grid emission factor.
#'
#' @param generation_data Tibble containing user-level electricity generation in kWh.
#' @param grid_emission_factor Grid emission factor in tCO2e/kWh.
#' @param project_emission_factor Optional project emission factor in tCO2e/kWh.
#' @param group_cols Optional character vector specifying grouping columns in `generation_data`.
#' @return A tibble with baseline generation, baseline emissions, project emissions, and emission reductions.
#' @examples
#' generation <- tibble::tibble(user_id = c("A", "B"), generation_kwh = c(1200, 1500))
#' estimate_emission_reductions_ams_ia(generation, grid_emission_factor = 0.8)
#' @export
estimate_emission_reductions_ams_ia <- function(generation_data,
                                                grid_emission_factor,
                                                project_emission_factor = 0,
                                                group_cols = NULL) {
  baseline_generation <- calculate_baseline_generation(
    generation_data = generation_data,
    group_cols = group_cols
  )

  baseline_emissions <- calculate_baseline_emissions(
    baseline_generation = baseline_generation,
    grid_emission_factor = grid_emission_factor
  )

  project_emissions <- calculate_project_emissions(
    baseline_generation = baseline_generation,
    project_emission_factor = project_emission_factor
  )

  calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions
  )
}
