CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"
RDF  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

skip_if_no_shaclR <- function() {
  if (!requireNamespace("shaclR", quietly = TRUE)) testthat::skip("shaclR not available")
}
skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) testthat::skip("rdflib not available")
}

make_triples <- function(tech_type = paste0(CDM, "N2OAbatementTechnology"),
                         include_tech = TRUE) {
  project_iri <- paste0(CDM, "TestProject001")
  tech_node   <- paste0(CDM, "TestTechNode001")
  rows <- list(
    data.frame(
      subject   = project_iri,
      predicate = paste0(RDF, "type"),
      object    = paste0(CDM, "CDMProjectActivity"),
      stringsAsFactors = FALSE
    )
  )
  if (include_tech) {
    rows <- c(rows, list(
      data.frame(
        subject   = project_iri,
        predicate = paste0(AIAO, "isPerformedWith"),
        object    = tech_node,
        stringsAsFactors = FALSE
      ),
      data.frame(
        subject   = tech_node,
        predicate = paste0(RDF, "type"),
        object    = tech_type,
        stringsAsFactors = FALSE
      )
    ))
  }
  do.call(rbind, rows)
}

make_concept_triples <- function() {
  data.frame(
    subject   = paste0(CDM, "SecondaryN2OCatalyst"),
    predicate = "http://www.w3.org/2004/02/skos/core#broader",
    object    = paste0(CDM, "N2OAbatementTechnology"),
    stringsAsFactors = FALSE
  )
}

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shaclR()
  triples <- make_triples()
  result  <- check_applicability_technology_type(
    triples,
    concept_triples = make_concept_triples()
  )
  expect_true(is.list(result))
  expect_named(result, c("conforms", "violations", "attestation"))
})

test_that("attestation carries methodology and condition", {
  skip_if_no_shaclR()
  triples <- make_triples()
  result  <- check_applicability_technology_type(
    triples,
    concept_triples = make_concept_triples()
  )
  expect_equal(result$attestation$methodology, "ACM0019")
  expect_equal(result$attestation$condition,   "N2OAbatementTechnology")
})

test_that("N2OAbatementTechnology typed intermediate node passes", {
  skip_if_no_shaclR()
  triples <- make_triples(tech_type = paste0(CDM, "N2OAbatementTechnology"))
  result  <- check_applicability_technology_type(
    triples,
    concept_triples = make_concept_triples()
  )
  expect_true(result$conforms)
})

test_that("SKOS parent of N2OAbatementTechnology does not gain narrower types (materialisation is upward-only)", {
  skip_if_no_shaclR()
  parent_concept_triples <- data.frame(
    subject   = paste0(CDM, "N2OAbatementTechnology"),
    predicate = "http://www.w3.org/2004/02/skos/core#broader",
    object    = paste0(CDM, "AbatementTechnology"),
    stringsAsFactors = FALSE
  )
  triples <- make_triples(tech_type = paste0(CDM, "AbatementTechnology"))
  result  <- check_applicability_technology_type(
    triples,
    concept_triples = parent_concept_triples
  )
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology typed intermediate node fails", {
  skip_if_no_shaclR()
  triples <- make_triples(tech_type = paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(
    triples,
    concept_triples = make_concept_triples()
  )
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  triples <- make_triples(include_tech = FALSE)
  result  <- check_applicability_technology_type(
    triples,
    concept_triples = make_concept_triples()
  )
  expect_false(result$conforms)
})

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  expect_error(
    check_applicability_technology_type(
      paste0(CDM, "SomeProjectIRI"),
      fluree_conn     = NULL,
      concept_triples = make_concept_triples()
    ),
    regexp = "fluree_conn"
  )
})
