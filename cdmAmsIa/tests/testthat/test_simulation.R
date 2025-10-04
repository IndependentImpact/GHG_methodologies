
test_that("simulation generates expected columns", {
  set.seed(123)
  data <- simulate_ams_ia_dataset(n_users = 5)
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("user_id", "generation_kwh", "baseline_emissions_tco2e", "emission_reductions_tco2e") %in% names(data)))
  expect_true(all(data$emission_reductions_tco2e >= 0))
})
