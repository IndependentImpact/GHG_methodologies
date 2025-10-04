
#' Simulate a dataset compliant with AMS-I.A
#'
#' Generates a tidy dataset representing user-level electricity generation using DeclareDesign.
#' The simulation reflects renewable generation that displaces fossil baseline
#' electricity use.
#'
#' @param n_users Number of end users to simulate.
#' @param mean_generation_kwh Mean annual electricity generation per user in kWh.
#' @param sd_generation_kwh Standard deviation of annual electricity generation.
#' @param grid_emission_factor Grid emission factor in tCO2e/kWh used for simulated emissions.
#' @return A tibble containing simulated user identifiers, generation, and emissions.
#' @examples
#' simulate_ams_ia_dataset(n_users = 5)
#' @importFrom DeclareDesign declare_model
#' @importFrom fabricatr add_level
#' @importFrom stats rnorm
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @export
simulate_ams_ia_dataset <- function(n_users = 20,
                                    mean_generation_kwh = 15000,
                                    sd_generation_kwh = 2000,
                                    grid_emission_factor = 0.75) {
  if (!is.numeric(n_users) || n_users <= 0) {
    stop("`n_users` must be a positive numeric value.", call. = FALSE)
  }

  design <- declare_model(
    users = add_level(
      N = n_users,
      user_id = paste0("user_", seq_len(n_users)),
      generation_kwh = pmax(rnorm(n_users, mean_generation_kwh, sd_generation_kwh), 0),
      grid_emission_factor = grid_emission_factor
    )
  )

  design() |>
    as_tibble() |>
    mutate(
      baseline_generation_kwh = generation_kwh,
      baseline_emissions_tco2e = baseline_generation_kwh * grid_emission_factor,
      project_emissions_tco2e = 0,
      emission_reductions_tco2e = baseline_emissions_tco2e - project_emissions_tco2e
    )
}
