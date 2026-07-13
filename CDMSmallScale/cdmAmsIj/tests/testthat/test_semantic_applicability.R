# Helpers -----------------------------------------------------------------

CDM <- "http://independentimpact.org/cdm/"

.make_triples <- function(tech_type, grid_type = NULL) {
  rows <- list(
    data.frame(subject="http://ex.org/proj", predicate="http://www.w3.org/1999/02/22-rdf-syntax-ns#type", object=paste0(CDM,"CDMProjectActivity"), datatype=NA_character_, stringsAsFactors=FALSE),
    data.frame(subject="http://ex.org/proj", predicate="http://w3id.org/aiao#isPerformedWith", object="http://ex.org/tech", datatype=NA_character_, stringsAsFactors=FALSE),
    data.frame(subject="http://ex.org/tech", predicate="http://www.w3.org/1999/02/22-rdf-syntax-ns#type", object=tech_type, datatype=NA_character_, stringsAsFactors=FALSE)
  )
  if (!is.null(grid_type)) {
    rows <- c(rows, list(data.frame(subject="http://ex.org/proj", predicate=paste0(CDM,"hasGridConnectionType"), object=grid_type, datatype=NA_character_, stringsAsFactors=FALSE)))
  }
  do.call(rbind, rows)
}

# Minimal thermal_data for gate tests
.monitoring_data <- function() {
  tibble::tibble(
    site_id              = "A",
    useful_heat_mwh      = 500,
    auxiliary_energy_mwh = 40
  )
}

# check_applicability_solar_technology ------------------------------------

test_that("returns list with conforms, violations, attestation", {
  triples <- .make_triples(paste0(CDM, "SolarThermal"))
  result  <- check_applicability_solar_technology(triples)
  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"))
  expect_type(result$conforms, "logical")
  expect_s3_class(result$violations, "data.frame")
})

test_that("SolarThermal passes check_applicability_solar_technology", {
  triples <- .make_triples(paste0(CDM, "SolarThermal"))
  expect_true(check_applicability_solar_technology(triples)$conforms)
})

test_that("NonRenewableEnergyTechnology fails check_applicability_solar_technology", {
  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_solar_technology(triples)
  expect_false(result$conforms)
  expect_gt(nrow(result$violations), 0L)
})

test_that("attestation records methodology and condition", {
  triples <- .make_triples(paste0(CDM, "SolarThermal"))
  att     <- check_applicability_solar_technology(triples)$attestation
  expect_equal(att$methodology, "AMS-I.J")
  expect_equal(att$condition,   "SolarThermal")
  expect_true(att$prima_facie)
})

test_that("missing fluree_conn with IRI data raises an error", {
  expect_error(
    check_applicability_solar_technology("http://ex.org/proj"),
    regexp = "fluree_conn"
  )
})

# estimate_emission_reductions_ams_ij validate_applicability gate ---------

test_that("validate_applicability = FALSE skips checks regardless of project", {
  expect_no_error(
    estimate_emission_reductions_ams_ij(
      .monitoring_data(),
      baseline_emission_factor = 0.22,
      auxiliary_emission_factor = 0.19
    )
  )
})

test_that("validate_applicability = TRUE with passing triples proceeds", {
  passing <- .make_triples(paste0(CDM, "SolarThermal"))
  result  <- estimate_emission_reductions_ams_ij(
    .monitoring_data(),
    baseline_emission_factor  = 0.22,
    auxiliary_emission_factor = 0.19,
    validate_applicability    = TRUE,
    project_id                = passing
  )
  expect_s3_class(result, "tbl_df")
})

test_that("validate_applicability = TRUE with failing triples stops with informative error", {
  failing <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  expect_error(
    estimate_emission_reductions_ams_ij(
      .monitoring_data(),
      baseline_emission_factor  = 0.22,
      auxiliary_emission_factor = 0.19,
      validate_applicability    = TRUE,
      project_id                = failing
    ),
    regexp = "applicability check failed"
  )
})

test_that("validate_applicability = TRUE without project_id stops immediately", {
  expect_error(
    estimate_emission_reductions_ams_ij(
      .monitoring_data(),
      baseline_emission_factor  = 0.22,
      auxiliary_emission_factor = 0.19,
      validate_applicability    = TRUE
    ),
    regexp = "project_id"
  )
})
