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
# Helpers
# ---------------------------------------------------------------------------

make_tech_triples <- function(tech_type_iri) {
  project_iri <- paste0(CDM, "project_test_001")
  tech_node   <- paste0(CDM, "tech_node_001")
  data.frame(
    subject   = c(project_iri,                     project_iri,                              tech_node),
    predicate = c(paste0(RDF, "type"),              paste0(AIAO, "isPerformedWith"),          paste0(RDF, "type")),
    object    = c(paste0(CDM, "CDMProjectActivity"), tech_node,                               tech_type_iri),
    stringsAsFactors = FALSE
  )
}

make_grid_triples <- function(grid_type_iri) {
  project_iri <- paste0(CDM, "project_test_002")
  data.frame(
    subject   = c(project_iri,                      project_iri),
    predicate = c(paste0(RDF, "type"),               paste0(CDM, "hasGridConnectionType")),
    object    = c(paste0(CDM, "CDMProjectActivity"),  grid_type_iri),
    stringsAsFactors = FALSE
  )
}

make_missing_tech_triples <- function() {
  project_iri <- paste0(CDM, "project_test_003")
  data.frame(
    subject   = project_iri,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
}

make_missing_grid_triples <- function() {
  project_iri <- paste0(CDM, "project_test_004")
  data.frame(
    subject   = project_iri,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
}

make_parent_tech_triples <- function() {
  # Use a SKOS broader concept (parent) of RenewableBiomassSystem.
  # Materialisation is upward-only so the parent does not gain the narrower type.
  parent_iri  <- paste0(CDM, "RenewableEnergySystem")
  project_iri <- paste0(CDM, "project_test_005")
  tech_node   <- paste0(CDM, "tech_node_005")
  data.frame(
    subject   = c(project_iri,                      project_iri,                             tech_node),
    predicate = c(paste0(RDF, "type"),               paste0(AIAO, "isPerformedWith"),         paste0(RDF, "type")),
    object    = c(paste0(CDM, "CDMProjectActivity"),  tech_node,                              parent_iri),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type — 7 tests
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shaclR()
  triples <- make_tech_triples(paste0(CDM, "RenewableBiomassSystem"))
  result  <- check_applicability_technology_type(triples)
  expect_true(is.list(result))
  expect_true(all(c("conforms", "violations", "attestation") %in% names(result)))
})

test_that("attestation has correct methodology and condition", {
  skip_if_no_shaclR()
  triples <- make_tech_triples(paste0(CDM, "RenewableBiomassSystem"))
  result  <- check_applicability_technology_type(triples)
  expect_equal(result$attestation$methodology, "ACM0018")
  expect_equal(result$attestation$condition, "RenewableBiomassSystem")
})

test_that("RenewableBiomassSystem typed as intermediate node passes", {
  skip_if_no_shaclR()
  triples <- make_tech_triples(paste0(CDM, "RenewableBiomassSystem"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("parent concept of RenewableBiomassSystem does not pass (materialisation is upward-only)", {
  skip_if_no_shaclR()
  triples <- make_parent_tech_triples()
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("NonRenewableEnergyTechnology typed as intermediate node fails", {
  skip_if_no_shaclR()
  triples <- make_tech_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  triples <- make_missing_tech_triples()
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  expect_error(
    check_applicability_technology_type(paste0(CDM, "project_test_iri")),
    "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_grid_connection — 4 tests
# ---------------------------------------------------------------------------

test_that("GridConnectedSystem as direct value of hasGridConnectionType passes", {
  skip_if_no_shaclR()
  triples <- make_grid_triples(paste0(CDM, "GridConnectedSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_true(result$conforms)
})

test_that("grid attestation condition is GridConnectedSystem", {
  skip_if_no_shaclR()
  triples <- make_grid_triples(paste0(CDM, "GridConnectedSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_equal(result$attestation$condition, "GridConnectedSystem")
})

test_that("OffGridSystem as direct value of hasGridConnectionType fails", {
  skip_if_no_shaclR()
  triples <- make_grid_triples(paste0(CDM, "OffGridSystem"))
  result  <- check_applicability_grid_connection(triples)
  expect_false(result$conforms)
})

test_that("missing hasGridConnectionType fails", {
  skip_if_no_shaclR()
  triples <- make_missing_grid_triples()
  result  <- check_applicability_grid_connection(triples)
  expect_false(result$conforms)
})
