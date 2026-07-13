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

# Minimal monitoring data that satisfies estimate_emission_reductions_ams_ie
.monitoring_data <- function() {
  tibble::tibble(
    biomass_consumption_tonnes = c(10, 8),
    non_renewable_fraction     = c(0.85, 0.80)
  )
}

test_that("check_applicability_renewable_technology returns correct structure", {
  skip_if_not_installed("shaclR")

  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  shapes  <- read_ams_ie_technology_shapes()
  result  <- check_applicability_renewable_technology(triples, shapes = shapes)

  expect_named(result, c("conforms", "violations", "attestation"))
  expect_type(result$conforms, "logical")
  expect_named(result$attestation,
               c("project_id", "methodology", "condition", "checked_at", "prima_facie"))
  expect_equal(result$attestation$methodology, "AMS-I.E")
  expect_equal(result$attestation$condition,   "RenewableEnergyTechnology")
})

test_that("SolarPV (skos:broader RenewableEnergyTechnology) passes renewable technology check", {
  skip_if_not_installed("shaclR")
  skip_if_not_installed("rdflib")

  triples         <- .make_triples(paste0(CDM, "SolarPV"))
  shapes          <- read_ams_ie_technology_shapes()
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  result <- check_applicability_renewable_technology(triples,
                                                     shapes = shapes,
                                                     concept_triples = concept_triples)
  expect_true(result$conforms)
})

test_that("NonRenewableEnergyTechnology fails renewable technology check", {
  skip_if_not_installed("shaclR")

  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  shapes  <- read_ams_ie_technology_shapes()

  result <- check_applicability_renewable_technology(triples, shapes = shapes)
  expect_false(result$conforms)
})

test_that("IRI data without fluree_conn raises error", {
  expect_error(
    check_applicability_renewable_technology("http://ex.org/proj", fluree_conn = NULL),
    "fluree_conn"
  )
})

test_that("validate_applicability = FALSE skips checks", {
  data <- .monitoring_data()
  result <- estimate_emission_reductions_ams_ie(
    data,
    ncv            = 15,
    emission_factor = 0.0001,
    validate_applicability = FALSE
  )
  expect_true("emission_reductions_tco2e" %in% names(result))
})

test_that("validate_applicability = TRUE with passing triples proceeds", {
  skip_if_not_installed("shaclR")
  skip_if_not_installed("rdflib")

  triples         <- .make_triples(paste0(CDM, "SolarPV"))
  shapes          <- read_ams_ie_technology_shapes()
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  data <- .monitoring_data()
  result <- estimate_emission_reductions_ams_ie(
    data,
    ncv             = 15,
    emission_factor = 0.0001,
    validate_applicability = TRUE,
    project_id      = triples,
    concept_triples = concept_triples,
    fluree_conn     = NULL
  )
  expect_true("emission_reductions_tco2e" %in% names(result))
})

test_that("validate_applicability = TRUE with failing triples stops with error", {
  skip_if_not_installed("shaclR")

  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  shapes  <- read_ams_ie_technology_shapes()

  data <- .monitoring_data()
  expect_error(
    estimate_emission_reductions_ams_ie(
      data,
      ncv             = 15,
      emission_factor = 0.0001,
      validate_applicability = TRUE,
      project_id      = triples
    ),
    "AMS-I.E applicability check failed"
  )
})

test_that("validate_applicability = TRUE without project_id stops immediately", {
  data <- .monitoring_data()
  expect_error(
    estimate_emission_reductions_ams_ie(
      data,
      ncv             = 15,
      emission_factor = 0.0001,
      validate_applicability = TRUE,
      project_id      = NULL
    ),
    "project_id.*required"
  )
})
