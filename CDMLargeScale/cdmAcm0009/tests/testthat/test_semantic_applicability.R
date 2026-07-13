CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"
RDF  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

skip_if_no_shaclR <- function() {
  if (!requireNamespace("shaclR", quietly = TRUE)) testthat::skip("shaclR not available")
}
skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) testthat::skip("rdflib not available")
}

# ---------------------------------------------------------------------------
# check_applicability_baseline_fuel
# ---------------------------------------------------------------------------

test_that("Coal typed as intermediate fuel node passes (via SKOS hierarchy)", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  proj      <- paste0(CDM, "project/test-coal-001")
  fuel_node <- paste0(CDM, "fuelnode/test-coal-001")

  triples <- data.frame(
    subject   = c(proj,       proj,                                    fuel_node),
    predicate = c(paste0(RDF, "type"), paste0(CDM, "hasBaselineFuelType"), paste0(RDF, "type")),
    object    = c(paste0(CDM, "CDMProjectActivity"), fuel_node,        paste0(CDM, "Coal")),
    stringsAsFactors = FALSE
  )

  shapes          <- cdmAcm0009::read_acm0009_baseline_fuel_shapes()
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  result <- cdmAcm0009::check_applicability_baseline_fuel(
    data            = triples,
    shapes          = shapes,
    concept_triples = concept_triples
  )

  expect_true(result$conforms)
})

test_that("attestation condition is FossilFuel", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  proj      <- paste0(CDM, "project/test-coal-002")
  fuel_node <- paste0(CDM, "fuelnode/test-coal-002")

  triples <- data.frame(
    subject   = c(proj,       proj,                                    fuel_node),
    predicate = c(paste0(RDF, "type"), paste0(CDM, "hasBaselineFuelType"), paste0(RDF, "type")),
    object    = c(paste0(CDM, "CDMProjectActivity"), fuel_node,        paste0(CDM, "Coal")),
    stringsAsFactors = FALSE
  )

  shapes          <- cdmAcm0009::read_acm0009_baseline_fuel_shapes()
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  result <- cdmAcm0009::check_applicability_baseline_fuel(
    data            = triples,
    shapes          = shapes,
    concept_triples = concept_triples
  )

  expect_equal(result$attestation$condition, "FossilFuel")
})

test_that("RenewableBiomass typed as intermediate fuel node fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  proj      <- paste0(CDM, "project/test-biomass-001")
  fuel_node <- paste0(CDM, "fuelnode/test-biomass-001")

  triples <- data.frame(
    subject   = c(proj,       proj,                                    fuel_node),
    predicate = c(paste0(RDF, "type"), paste0(CDM, "hasBaselineFuelType"), paste0(RDF, "type")),
    object    = c(paste0(CDM, "CDMProjectActivity"), fuel_node,        paste0(CDM, "RenewableBiomass")),
    stringsAsFactors = FALSE
  )

  shapes          <- cdmAcm0009::read_acm0009_baseline_fuel_shapes()
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  result <- cdmAcm0009::check_applicability_baseline_fuel(
    data            = triples,
    shapes          = shapes,
    concept_triples = concept_triples
  )

  expect_false(result$conforms)
})

test_that("Missing hasBaselineFuelType fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  proj <- paste0(CDM, "project/test-nofuel-001")

  triples <- data.frame(
    subject   = proj,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )

  shapes          <- cdmAcm0009::read_acm0009_baseline_fuel_shapes()
  concept_triples <- cdmSemantic::read_cdm_concept_triples()

  result <- cdmAcm0009::check_applicability_baseline_fuel(
    data            = triples,
    shapes          = shapes,
    concept_triples = concept_triples
  )

  expect_false(result$conforms)
})
