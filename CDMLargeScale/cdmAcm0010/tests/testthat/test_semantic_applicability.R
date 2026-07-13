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

make_project_triples <- function(tech_type = NULL, waste_type = NULL) {
  project_iri <- paste0(CDM, "project/test001")
  rows <- list(
    data.frame(
      subject   = project_iri,
      predicate = paste0(RDF, "type"),
      object    = paste0(CDM, "CDMProjectActivity"),
      stringsAsFactors = FALSE
    )
  )
  if (!is.null(tech_type)) {
    tech_node <- paste0(CDM, "tech/test001")
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
        object    = paste0(CDM, tech_type),
        stringsAsFactors = FALSE
      )
    ))
  }
  if (!is.null(waste_type)) {
    waste_node <- paste0(CDM, "waste/test001")
    rows <- c(rows, list(
      data.frame(
        subject   = project_iri,
        predicate = paste0(CDM, "hasWasteType"),
        object    = waste_node,
        stringsAsFactors = FALSE
      ),
      data.frame(
        subject   = waste_node,
        predicate = paste0(RDF, "type"),
        object    = paste0(CDM, waste_type),
        stringsAsFactors = FALSE
      )
    ))
  }
  do.call(rbind, rows)
}

make_concept_triples <- function() {
  # Minimal SKOS hierarchy: ManureMethaneCapture is a narrower of
  # AnaerobicDigestionTechnology (a plausible parent concept).
  # AnimalManure is a narrower of OrganicWaste.
  data.frame(
    subject   = c(
      paste0(CDM, "ManureMethaneCapture"),
      paste0(CDM, "AnimalManure")
    ),
    predicate = c(
      "http://www.w3.org/2004/02/skos/core#broader",
      "http://www.w3.org/2004/02/skos/core#broader"
    ),
    object    = c(
      paste0(CDM, "AnaerobicDigestionTechnology"),
      paste0(CDM, "OrganicWaste")
    ),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type — 7 tests
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shaclR()
  triples <- make_project_triples(tech_type = "ManureMethaneCapture")
  shapes  <- read_acm0010_technology_shapes()
  result  <- check_applicability_technology_type(triples,
                                                  concept_triples = make_concept_triples(),
                                                  shapes = shapes)
  expect_true(is.list(result))
  expect_true(all(c("conforms", "violations", "attestation") %in% names(result)))
})

test_that("attestation contains correct methodology and condition", {
  skip_if_no_shaclR()
  triples <- make_project_triples(tech_type = "ManureMethaneCapture")
  shapes  <- read_acm0010_technology_shapes()
  result  <- check_applicability_technology_type(triples,
                                                  concept_triples = make_concept_triples(),
                                                  shapes = shapes)
  expect_equal(result$attestation$methodology, "ACM0010")
  expect_equal(result$attestation$condition, "ManureMethaneCapture")
})

test_that("ManureMethaneCapture typed intermediate node passes", {
  skip_if_no_shaclR()
  triples <- make_project_triples(tech_type = "ManureMethaneCapture")
  shapes  <- read_acm0010_technology_shapes()
  result  <- check_applicability_technology_type(triples,
                                                  concept_triples = make_concept_triples(),
                                                  shapes = shapes)
  expect_true(result$conforms)
})

test_that("SKOS parent concept of ManureMethaneCapture does not pass (upward-only materialisation)", {
  skip_if_no_shaclR()
  # AnaerobicDigestionTechnology is the broader concept; materialisation does
  # not assign ManureMethaneCapture type to parent nodes.
  triples <- make_project_triples(tech_type = "AnaerobicDigestionTechnology")
  shapes  <- read_acm0010_technology_shapes()
  result  <- check_applicability_technology_type(triples,
                                                  concept_triples = make_concept_triples(),
                                                  shapes = shapes)
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology typed intermediate node fails", {
  skip_if_no_shaclR()
  triples <- make_project_triples(tech_type = "RenewableEnergyTechnology")
  shapes  <- read_acm0010_technology_shapes()
  result  <- check_applicability_technology_type(triples,
                                                  concept_triples = make_concept_triples(),
                                                  shapes = shapes)
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  triples <- make_project_triples()  # no tech_type
  shapes  <- read_acm0010_technology_shapes()
  result  <- check_applicability_technology_type(triples,
                                                  concept_triples = make_concept_triples(),
                                                  shapes = shapes)
  expect_false(result$conforms)
})

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  expect_error(
    check_applicability_technology_type("http://independentimpact.org/cdm/project/test001"),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_waste_type — 4 tests
# ---------------------------------------------------------------------------

test_that("AnimalManure typed intermediate waste node passes", {
  skip_if_no_shaclR()
  triples <- make_project_triples(waste_type = "AnimalManure")
  shapes  <- read_acm0010_waste_type_shapes()
  result  <- check_applicability_waste_type(triples,
                                             concept_triples = make_concept_triples(),
                                             shapes = shapes)
  expect_true(result$conforms)
})

test_that("attestation condition is AnimalManure", {
  skip_if_no_shaclR()
  triples <- make_project_triples(waste_type = "AnimalManure")
  shapes  <- read_acm0010_waste_type_shapes()
  result  <- check_applicability_waste_type(triples,
                                             concept_triples = make_concept_triples(),
                                             shapes = shapes)
  expect_equal(result$attestation$condition, "AnimalManure")
})

test_that("MunicipalSolidWaste typed intermediate waste node fails", {
  skip_if_no_shaclR()
  triples <- make_project_triples(waste_type = "MunicipalSolidWaste")
  shapes  <- read_acm0010_waste_type_shapes()
  result  <- check_applicability_waste_type(triples,
                                             concept_triples = make_concept_triples(),
                                             shapes = shapes)
  expect_false(result$conforms)
})

test_that("missing hasWasteType fails", {
  skip_if_no_shaclR()
  triples <- make_project_triples()  # no waste_type
  shapes  <- read_acm0010_waste_type_shapes()
  result  <- check_applicability_waste_type(triples,
                                             concept_triples = make_concept_triples(),
                                             shapes = shapes)
  expect_false(result$conforms)
})
