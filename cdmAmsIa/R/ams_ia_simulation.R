
#' Simulate a dataset compliant with AMS-I.A
#'
#' Generates a tidy dataset representing user-level electricity generation using DeclareDesign.
#' The simulation reflects renewable generation that displaces fossil baseline
#' electricity use.
#'
#' @param n_users Number of end users to simulate.
#' @param n_periods Number of monitoring periods (default monthly observations across a year).
#' @param start_year Calendar year assigned to the first monitoring period.
#' @param start_month Calendar month (1-12) assigned to the first monitoring period.
#' @param mean_generation_kwh Mean annual electricity generation per user in kWh.
#' @param sd_generation_kwh Standard deviation of annual electricity generation.
#' @param grid_emission_factor Grid emission factor in tCO2e/kWh used for simulated emissions.
#' @return A tibble containing simulated user identifiers, monitoring period metadata, generation, and emissions.
#' @examples
#' simulate_ams_ia_dataset(n_users = 5)
#' @importFrom DeclareDesign declare_model
#' @importFrom fabricatr add_level
#' @importFrom stats rnorm
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @export
simulate_ams_ia_dataset <- function(n_users = 20,
                                    n_periods = 12,
                                    start_year = 2023,
                                    start_month = 1,
                                    mean_generation_kwh = 15000,
                                    sd_generation_kwh = 2000,
                                    grid_emission_factor = 0.75) {
  if (!is.numeric(n_users) || n_users <= 0) {
    stop("`n_users` must be a positive numeric value.", call. = FALSE)
  }

  if (!is.numeric(n_periods) || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }

  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }

  design <- declare_model(
    users = add_level(
      N = n_users,
      user_id = paste0("user_", seq_len(n_users)),
      grid_emission_factor = grid_emission_factor
    ),
    monitoring_periods = add_level(
      N = n_periods,
      monitoring_period = seq_len(N),
      period_index = seq_len(N),
      year = start_year + ((start_month - 1L + period_index - 1L) %/% 12L),
      month = ((start_month - 1L + period_index - 1L) %% 12L) + 1L,
      day = sample.int(28L, size = N, replace = TRUE),
      monitoring_date = as.Date(sprintf("%04d-%02d-%02d", year, month, day)),
      monitoring_label = sprintf("%04d-%02d", year, month),
      generation_kwh = pmax(
        stats::rnorm(
          n = N,
          mean = mean_generation_kwh / n_periods,
          sd = sd_generation_kwh / sqrt(n_periods)
        ),
        0
      )
    )
  )

  design() |>
    as_tibble() |>
    dplyr::select(-period_index) |>
    mutate(
      baseline_generation_kwh = generation_kwh,
      baseline_emissions_tco2e = baseline_generation_kwh * grid_emission_factor,
      project_emissions_tco2e = 0,
      emission_reductions_tco2e = baseline_emissions_tco2e - project_emissions_tco2e
    )
}
