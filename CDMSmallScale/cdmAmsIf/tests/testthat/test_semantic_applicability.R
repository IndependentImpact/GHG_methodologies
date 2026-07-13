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

test_that("check_applicability_renewable_technology returns list with required slots", {
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  result <- check_applicability_renewable_technology(triples)
  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"))
  expect_type(result$conforms, "logical")
  expect_s3_class(result$violations, "data.frame")
  expect_type(result$attestation, "list")
})

test_that("check_applicability_grid_connection returns list with required slots", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "CaptiveUseSystem"))
  result <- check_applicability_grid_connection(triples)
  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"))
  expect_type(result$conforms, "logical")
  expect_s3_class(result$violations, "data.frame")
})

test_that("attestation records methodology and condition", {
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  result <- check_applicability_renewable_technology(triples)
  expect_equal(result$attestation$methodology, "AMS-I.F")
  expect_equal(result$attestation$condition, "RenewableEnergyTechnology")
})

# ---------------------------------------------------------------------------
# SolarPV (skos:broader RenewableEnergyTechnology) passes technology check
# ---------------------------------------------------------------------------

test_that("SolarPV passes check_applicability_renewable_technology", {
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  result <- check_applicability_renewable_technology(triples)
  expect_true(result$conforms)
  expect_equal(nrow(result$violations), 0L)
})

test_that("WindTurbine passes check_applicability_renewable_technology", {
  triples <- .make_triples(paste0(CDM, "WindTurbine"))
  result <- check_applicability_renewable_technology(triples)
  expect_true(result$conforms)
})

# ---------------------------------------------------------------------------
# NonRenewableEnergyTechnology fails technology check
# ---------------------------------------------------------------------------

test_that("NonRenewableEnergyTechnology fails check_applicability_renewable_technology", {
  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result <- check_applicability_renewable_technology(triples)
  expect_false(result$conforms)
  expect_gt(nrow(result$violations), 0L)
})

# ---------------------------------------------------------------------------
# Missing fluree_conn with IRI data raises error
# ---------------------------------------------------------------------------

test_that("IRI data without fluree_conn raises error", {
  expect_error(
    check_applicability_renewable_technology("http://ex.org/proj"),
    "fluree_conn"
  )
})

test_that("IRI data without fluree_conn raises error for grid check", {
  expect_error(
    check_applicability_grid_connection("http://ex.org/proj"),
    "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# Grid connection checks
# ---------------------------------------------------------------------------

test_that("CaptiveUseSystem passes check_applicability_grid_connection", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "CaptiveUseSystem"))
  result <- check_applicability_grid_connection(triples)
  expect_true(result$conforms)
  expect_equal(nrow(result$violations), 0L)
})

test_that("MiniGridSystem passes check_applicability_grid_connection", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "MiniGridSystem"))
  result <- check_applicability_grid_connection(triples)
  expect_true(result$conforms)
})

test_that("GridConnectedSystem fails check_applicability_grid_connection", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "GridConnectedSystem"))
  result <- check_applicability_grid_connection(triples)
  expect_false(result$conforms)
  expect_gt(nrow(result$violations), 0L)
})

test_that("OffGridSystem fails check_applicability_grid_connection", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "OffGridSystem"))
  result <- check_applicability_grid_connection(triples)
  expect_false(result$conforms)
  expect_gt(nrow(result$violations), 0L)
})

test_that("missing grid connection triple fails check_applicability_grid_connection", {
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  result <- check_applicability_grid_connection(triples)
  expect_false(result$conforms)
})

# ---------------------------------------------------------------------------
# validate_applicability gate in estimate_emission_reductions_ams_if
# ---------------------------------------------------------------------------

.supply_data <- tibble::tibble(grid_id = c("A", "B"), electricity_mwh = c(900, 750))

test_that("validate_applicability=FALSE skips checks", {
  result <- estimate_emission_reductions_ams_if(
    .supply_data,
    baseline_emission_factor = 0.72,
    validate_applicability = FALSE
  )
  expect_s3_class(result, "data.frame")
})

test_that("validate_applicability=TRUE without project_id stops immediately", {
  expect_error(
    estimate_emission_reductions_ams_if(
      .supply_data,
      baseline_emission_factor = 0.72,
      validate_applicability = TRUE
    ),
    "project_id"
  )
})

test_that("validate_applicability=TRUE with passing triples proceeds to result", {
  triples <- .make_triples(
    paste0(CDM, "SolarPV"),
    paste0(CDM, "CaptiveUseSystem")
  )
  result <- estimate_emission_reductions_ams_if(
    .supply_data,
    baseline_emission_factor = 0.72,
    validate_applicability = TRUE,
    project_id = triples
  )
  expect_s3_class(result, "data.frame")
  expect_true("emission_reductions_tco2e" %in% names(result))
})

test_that("validate_applicability=TRUE with failing triples stops with error", {
  triples <- .make_triples(
    paste0(CDM, "NonRenewableEnergyTechnology"),
    paste0(CDM, "GridConnectedSystem")
  )
  expect_error(
    estimate_emission_reductions_ams_if(
      .supply_data,
      baseline_emission_factor = 0.72,
      validate_applicability = TRUE,
      project_id = triples
    ),
    "applicability check failed"
  )
})
