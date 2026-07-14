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

# ---------------------------------------------------------------------------
# Return structure
# ---------------------------------------------------------------------------

test_that("check_applicability_renewable_technology returns conforms, violations, attestation", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "OffGridSystem"))
  result  <- check_applicability_renewable_technology(triples)
  expect_true(is.list(result))
  expect_true("conforms"    %in% names(result))
  expect_true("violations"  %in% names(result))
  expect_true("attestation" %in% names(result))
})

test_that("check_applicability_grid_connection returns conforms, violations, attestation", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "OffGridSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_true(is.list(result))
  expect_true("conforms"    %in% names(result))
  expect_true("violations"  %in% names(result))
  expect_true("attestation" %in% names(result))
})

# ---------------------------------------------------------------------------
# Technology type checks
# ---------------------------------------------------------------------------

test_that("SolarPV (skos:broader RenewableEnergyTechnology) passes technology check", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  result  <- check_applicability_renewable_technology(triples)
  expect_true(result$conforms)
})

test_that("NonRenewableEnergyTechnology fails technology check", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_renewable_technology(triples)
  expect_false(result$conforms)
})

test_that("attestation records methodology as AMS-I.D for technology check", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  result  <- check_applicability_renewable_technology(triples)
  expect_equal(result$attestation$methodology, "AMS-I.D")
  expect_equal(result$attestation$condition, "RenewableEnergyTechnology")
})

# ---------------------------------------------------------------------------
# Grid connection checks
# ---------------------------------------------------------------------------

test_that("OffGridSystem passes grid connection check", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "OffGridSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_true(result$conforms)
})

test_that("MiniGridSystem passes grid connection check", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "MiniGridSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_true(result$conforms)
})

test_that("CaptiveUseSystem passes grid connection check", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "CaptiveUseSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_true(result$conforms)
})

test_that("GridConnectedSystem fails grid connection check (AMS-I.D requires captive mini-grid)", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "GridConnectedSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_false(result$conforms)
})

test_that("missing grid connection type fails grid connection check", {
  skip_if_not_installed("shaclR")
  triples <- .make_triples(paste0(CDM, "SolarPV"), grid_type = NULL)
  result  <- check_applicability_grid_connection(triples)
  expect_false(result$conforms)
})

# ---------------------------------------------------------------------------
# IRI input requires fluree_conn
# ---------------------------------------------------------------------------

test_that("IRI data without fluree_conn raises error", {
  skip_if_not_installed("shaclR")
  expect_error(
    check_applicability_renewable_technology("http://ex.org/proj"),
    "fluree_conn"
  )
})

test_that("IRI data without fluree_conn raises error for grid check", {
  skip_if_not_installed("shaclR")
  expect_error(
    check_applicability_grid_connection("http://ex.org/proj"),
    "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# validate_applicability gate in meta function
# ---------------------------------------------------------------------------

test_that("validate_applicability=FALSE skips checks and proceeds normally", {
  data <- tibble::tibble(grid_id = "A", electricity_mwh = 1000)
  result <- estimate_emission_reductions_ams_id(
    data,
    baseline_emission_factor  = 0.72,
    validate_applicability    = FALSE
  )
  expect_true(is.data.frame(result))
})

test_that("validate_applicability=TRUE without project_id stops immediately", {
  data <- tibble::tibble(grid_id = "A", electricity_mwh = 1000)
  expect_error(
    estimate_emission_reductions_ams_id(
      data,
      baseline_emission_factor = 0.72,
      validate_applicability   = TRUE
    ),
    "project_id"
  )
})

test_that("validate_applicability=TRUE with passing triples proceeds to result", {
  skip_if_not_installed("shaclR")
  data    <- tibble::tibble(grid_id = "A", electricity_mwh = 1000)
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "OffGridSystem"))
  result  <- estimate_emission_reductions_ams_id(
    data,
    baseline_emission_factor = 0.72,
    validate_applicability   = TRUE,
    project_id               = triples
  )
  expect_true(is.data.frame(result))
  expect_true("emission_reductions_tco2e" %in% names(result))
})

test_that("validate_applicability=TRUE with failing triples stops with error", {
  skip_if_not_installed("shaclR")
  data    <- tibble::tibble(grid_id = "A", electricity_mwh = 1000)
  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"), paste0(CDM, "GridConnectedSystem"))
  expect_error(
    estimate_emission_reductions_ams_id(
      data,
      baseline_emission_factor = 0.72,
      validate_applicability   = TRUE,
      project_id               = triples
    ),
    "AMS-I.D applicability check failed"
  )
})
