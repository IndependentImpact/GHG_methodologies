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

skip_if_no_shapeR <- function() {
  testthat::skip_if_not_installed("shapeR")
}

skip_if_no_rdflib <- function() {
  testthat::skip_if_not_installed("rdflib")
}

test_that("check_applicability_renewable_technology returns correct structure", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "SolarPV"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()
  shapes <- read_ams_ib_technology_shapes()
  result <- check_applicability_renewable_technology(triples, concept_triples = concept_triples, shapes = shapes)

  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"))
  expect_type(result$conforms, "logical")
  expect_type(result$attestation, "list")
  expect_named(result$attestation, c("project_id", "methodology", "condition", "checked_at", "prima_facie"))
  expect_equal(result$attestation$methodology, "AMS-I.B")
  expect_equal(result$attestation$condition, "RenewableEnergyTechnology")
})

test_that("SolarPV (skos:broader RenewableEnergyTechnology) passes renewable technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "SolarPV"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()
  shapes <- read_ams_ib_technology_shapes()
  result <- check_applicability_renewable_technology(triples, concept_triples = concept_triples, shapes = shapes)

  expect_true(result$conforms)
})

test_that("NonRenewableEnergyTechnology fails renewable technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()
  shapes <- read_ams_ib_technology_shapes()
  result <- check_applicability_renewable_technology(triples, concept_triples = concept_triples, shapes = shapes)

  expect_false(result$conforms)
})

test_that("passing IRI without fluree_conn raises error", {
  skip_if_no_shapeR()

  expect_error(
    check_applicability_renewable_technology("http://ex.org/proj"),
    "fluree_conn"
  )
})

test_that("OffGridSystem passes grid connection check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "SolarPV"), grid_type = paste0(CDM, "OffGridSystem"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()
  shapes <- read_ams_ib_grid_shapes()
  result <- check_applicability_grid_connection(triples, concept_triples = concept_triples, shapes = shapes)

  expect_true(result$conforms)
  expect_equal(result$attestation$condition, "GridConnectionType")
})

test_that("CaptiveUseSystem passes grid connection check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "SolarPV"), grid_type = paste0(CDM, "CaptiveUseSystem"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()
  shapes <- read_ams_ib_grid_shapes()
  result <- check_applicability_grid_connection(triples, concept_triples = concept_triples, shapes = shapes)

  expect_true(result$conforms)
})

test_that("GridConnectedSystem fails grid connection check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "SolarPV"), grid_type = paste0(CDM, "GridConnectedSystem"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()
  shapes <- read_ams_ib_grid_shapes()
  result <- check_applicability_grid_connection(triples, concept_triples = concept_triples, shapes = shapes)

  expect_false(result$conforms)
})

test_that("missing grid connection triple fails grid connection check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "SolarPV"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()
  shapes <- read_ams_ib_grid_shapes()
  result <- check_applicability_grid_connection(triples, concept_triples = concept_triples, shapes = shapes)

  expect_false(result$conforms)
})

test_that("validate_applicability=FALSE skips semantic checks", {
  fuel <- tibble::tibble(
    machine_id = c("pump-1", "pump-1"),
    fuel_consumption = c(200, 180),
    net_calorific_value = c(43, 43)
  )
  result <- estimate_emission_reductions_ams_ib(
    fuel_data = fuel,
    emission_factor = 0.00007,
    group_cols = "machine_id",
    validate_applicability = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_true("emission_reductions_tco2e" %in% names(result))
})

test_that("validate_applicability=TRUE with passing triples proceeds to calculation", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(
    paste0(CDM, "SolarPV"),
    grid_type = paste0(CDM, "OffGridSystem")
  )
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  fuel <- tibble::tibble(
    machine_id = c("pump-1", "pump-1"),
    fuel_consumption = c(200, 180),
    net_calorific_value = c(43, 43)
  )
  result <- estimate_emission_reductions_ams_ib(
    fuel_data = fuel,
    emission_factor = 0.00007,
    group_cols = "machine_id",
    validate_applicability = TRUE,
    project_id = triples,
    concept_triples = concept_triples
  )
  expect_s3_class(result, "data.frame")
  expect_true("emission_reductions_tco2e" %in% names(result))
})

test_that("validate_applicability=TRUE with failing triples stops with error", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  fuel <- tibble::tibble(
    machine_id = c("pump-1"),
    fuel_consumption = c(200),
    net_calorific_value = c(43)
  )
  expect_error(
    estimate_emission_reductions_ams_ib(
      fuel_data = fuel,
      emission_factor = 0.00007,
      group_cols = "machine_id",
      validate_applicability = TRUE,
      project_id = triples,
      concept_triples = concept_triples
    ),
    "AMS-I.B applicability check failed"
  )
})

test_that("validate_applicability=TRUE without project_id stops immediately", {
  fuel <- tibble::tibble(
    machine_id = c("pump-1"),
    fuel_consumption = c(200),
    net_calorific_value = c(43)
  )
  expect_error(
    estimate_emission_reductions_ams_ib(
      fuel_data = fuel,
      emission_factor = 0.00007,
      validate_applicability = TRUE
    ),
    "project_id"
  )
})
