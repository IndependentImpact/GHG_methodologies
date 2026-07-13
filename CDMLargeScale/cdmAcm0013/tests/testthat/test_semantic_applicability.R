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
  project_iri <- paste0(CDM, "project/test-acm0013")
  tech_node   <- paste0(CDM, "tech/test-node")
  tibble::tribble(
    ~subject,      ~predicate,                                ~object,
    project_iri,   paste0(RDF,  "type"),                      paste0(CDM, "CDMProjectActivity"),
    project_iri,   paste0(AIAO, "isPerformedWith"),            tech_node,
    tech_node,     paste0(RDF,  "type"),                       tech_type_iri
  )
}

make_grid_triples <- function(grid_type_iri) {
  project_iri <- paste0(CDM, "project/test-acm0013")
  tibble::tribble(
    ~subject,      ~predicate,                                    ~object,
    project_iri,   paste0(RDF, "type"),                            paste0(CDM, "CDMProjectActivity"),
    project_iri,   paste0(CDM, "hasGridConnectionType"),            grid_type_iri
  )
}

empty_concept_triples <- function() {
  tibble::tibble(subject = character(), predicate = character(), object = character())
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_tech_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples,
                                                 concept_triples = empty_concept_triples())
  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("attestation carries correct methodology and condition", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_tech_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples,
                                                 concept_triples = empty_concept_triples())
  expect_equal(result$attestation$methodology, "ACM0013")
  expect_equal(result$attestation$condition,   "NonRenewableEnergyTechnology")
})

test_that("NonRenewableEnergyTechnology typed intermediate node passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_tech_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples,
                                                 concept_triples = empty_concept_triples())
  expect_true(result$conforms)
})

test_that("SKOS parent of NonRenewableEnergyTechnology does NOT pass (materialisation is upward-only)", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  # A parent concept does not acquire the type of its narrower concept after
  # materialise_skos_hierarchy(); hierarchy propagates upward only.
  parent_iri <- paste0(CDM, "EnergyTechnology")
  concept_triples <- tibble::tribble(
    ~subject,                                           ~predicate,                                           ~object,
    paste0(CDM, "NonRenewableEnergyTechnology"),         "http://www.w3.org/2004/02/skos/core#broader",        parent_iri
  )
  # Use a node typed as the parent — should not satisfy sh:class NonRenewableEnergyTechnology
  triples <- make_tech_triples(parent_iri)
  result  <- check_applicability_technology_type(triples,
                                                 concept_triples = concept_triples)
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology typed intermediate node fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_tech_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples,
                                                 concept_triples = empty_concept_triples())
  expect_false(result$conforms)
})

test_that("Missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  project_iri <- paste0(CDM, "project/test-acm0013")
  triples <- tibble::tribble(
    ~subject,     ~predicate,                   ~object,
    project_iri,  paste0(RDF, "type"),            paste0(CDM, "CDMProjectActivity")
  )
  result <- check_applicability_technology_type(triples,
                                                concept_triples = empty_concept_triples())
  expect_false(result$conforms)
})

test_that("Character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  expect_error(
    check_applicability_technology_type(paste0(CDM, "project/test-acm0013")),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_grid_connection
# ---------------------------------------------------------------------------

test_that("GridConnectedSystem as direct value of hasGridConnectionType passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_grid_triples(paste0(CDM, "GridConnectedSystem"))
  result  <- check_applicability_grid_connection(triples,
                                                 concept_triples = empty_concept_triples())
  expect_true(result$conforms)
})

test_that("grid connection attestation condition is GridConnectedSystem", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_grid_triples(paste0(CDM, "GridConnectedSystem"))
  result  <- check_applicability_grid_connection(triples,
                                                 concept_triples = empty_concept_triples())
  expect_equal(result$attestation$condition, "GridConnectedSystem")
})

test_that("OffGridSystem as direct value fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_grid_triples(paste0(CDM, "OffGridSystem"))
  result  <- check_applicability_grid_connection(triples,
                                                 concept_triples = empty_concept_triples())
  expect_false(result$conforms)
})

test_that("Missing hasGridConnectionType fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  project_iri <- paste0(CDM, "project/test-acm0013")
  triples <- tibble::tribble(
    ~subject,     ~predicate,                  ~object,
    project_iri,  paste0(RDF, "type"),           paste0(CDM, "CDMProjectActivity")
  )
  result <- check_applicability_grid_connection(triples,
                                                concept_triples = empty_concept_triples())
  expect_false(result$conforms)
})
