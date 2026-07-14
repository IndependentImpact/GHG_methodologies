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
# Helper: build a minimal triples data frame for a project activity

make_triples <- function(tech_type = paste0(CDM, "RenewableBiomassSystem"),
                         include_technology = TRUE) {
  project_iri <- paste0(CDM, "TestProject001")
  tech_node   <- paste0(CDM, "TestTechNode001")

  base <- data.frame(
    subject   = project_iri,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    datatype  = NA_character_,
    stringsAsFactors = FALSE
  )

  if (include_technology) {
    link_row <- data.frame(
      subject   = project_iri,
      predicate = paste0(AIAO, "isPerformedWith"),
      object    = tech_node,
      datatype  = NA_character_,
      stringsAsFactors = FALSE
    )
    type_row <- data.frame(
      subject   = tech_node,
      predicate = paste0(RDF, "type"),
      object    = tech_type,
      datatype  = NA_character_,
      stringsAsFactors = FALSE
    )
    rbind(base, link_row, type_row)
  } else {
    base
  }
}

# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- make_triples()
  result  <- check_applicability_technology_type(triples)

  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("attestation has correct methodology and condition", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- make_triples()
  result  <- check_applicability_technology_type(triples)

  expect_equal(result$attestation$methodology, "ACM0006")
  expect_equal(result$attestation$condition, "RenewableBiomassSystem")
})

test_that("RenewableBiomassSystem typed intermediate node passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- make_triples(tech_type = paste0(CDM, "RenewableBiomassSystem"))
  result  <- check_applicability_technology_type(triples)

  expect_true(result$conforms)
})

test_that("SKOS parent concept of RenewableBiomassSystem as intermediate node fails (materialisation is upward-only)", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  # A parent concept does not gain the child's rdf:type via SKOS materialisation.
  # sh:class checks rdf:type, so a parent-typed node should NOT satisfy the shape.
  parent_iri <- paste0(CDM, "EnergyTechnology")
  triples    <- make_triples(tech_type = parent_iri)
  result     <- check_applicability_technology_type(triples)

  expect_false(result$conforms)
})

test_that("NonRenewableEnergyTechnology typed intermediate node fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- make_triples(tech_type = paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)

  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- make_triples(include_technology = FALSE)
  result  <- check_applicability_technology_type(triples)

  expect_false(result$conforms)
})

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  expect_error(
    check_applicability_technology_type(paste0(CDM, "SomeProject"), fluree_conn = NULL),
    regexp = "fluree_conn"
  )
})
