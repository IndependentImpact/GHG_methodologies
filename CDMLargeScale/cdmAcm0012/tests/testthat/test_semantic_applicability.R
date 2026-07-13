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

project_iri  <- paste0(CDM, "TestProject001")
tech_node    <- paste0(CDM, "TestTech001")
parent_node  <- paste0(CDM, "TestParentTech001")
wrong_node   <- paste0(CDM, "TestWrongTech001")

base_triples <- function() {
  tibble::tibble(
    subject   = project_iri,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    datatype  = NA_character_
  )
}

triples_with_tech <- function(tech_type_iri) {
  rbind(
    base_triples(),
    tibble::tibble(
      subject   = project_iri,
      predicate = paste0(AIAO, "isPerformedWith"),
      object    = tech_node,
      datatype  = NA_character_
    ),
    tibble::tibble(
      subject   = tech_node,
      predicate = paste0(RDF, "type"),
      object    = tech_type_iri,
      datatype  = NA_character_
    )
  )
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("returns list with correct names", {
  skip_if_no_shaclR()
  shapes  <- cdmAcm0012::read_acm0012_technology_shapes()
  data    <- triples_with_tech(paste0(CDM, "WasteEnergyRecovery"))
  result  <- cdmAcm0012::check_applicability_technology_type(data, shapes = shapes)
  expect_true(is.list(result))
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("attestation has correct methodology and condition", {
  skip_if_no_shaclR()
  shapes  <- cdmAcm0012::read_acm0012_technology_shapes()
  data    <- triples_with_tech(paste0(CDM, "WasteEnergyRecovery"))
  result  <- cdmAcm0012::check_applicability_technology_type(data, shapes = shapes)
  expect_equal(result$attestation$methodology, "ACM0012")
  expect_equal(result$attestation$condition, "WasteEnergyRecovery")
})

test_that("WasteEnergyRecovery typed as intermediate node passes", {
  skip_if_no_shaclR()
  shapes  <- cdmAcm0012::read_acm0012_technology_shapes()
  data    <- triples_with_tech(paste0(CDM, "WasteEnergyRecovery"))
  result  <- cdmAcm0012::check_applicability_technology_type(data, shapes = shapes)
  expect_true(result$conforms)
})

test_that("SKOS parent concept of WasteEnergyRecovery fails (materialisation is upward-only)", {
  skip_if_no_shaclR()
  # A parent concept does not gain narrower (child) types via materialisation,
  # so a project using only the parent class should not satisfy sh:class WasteEnergyRecovery.
  shapes <- cdmAcm0012::read_acm0012_technology_shapes()
  parent_concept_iri <- paste0(CDM, "EnergyRecoveryTechnology")
  data <- rbind(
    base_triples(),
    tibble::tibble(
      subject   = project_iri,
      predicate = paste0(AIAO, "isPerformedWith"),
      object    = parent_node,
      datatype  = NA_character_
    ),
    tibble::tibble(
      subject   = parent_node,
      predicate = paste0(RDF, "type"),
      object    = parent_concept_iri,
      datatype  = NA_character_
    )
  )
  result <- cdmAcm0012::check_applicability_technology_type(data, shapes = shapes)
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology typed as intermediate node fails", {
  skip_if_no_shaclR()
  shapes <- cdmAcm0012::read_acm0012_technology_shapes()
  data   <- triples_with_tech(paste0(CDM, "RenewableEnergyTechnology"))
  result <- cdmAcm0012::check_applicability_technology_type(data, shapes = shapes)
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  shapes <- cdmAcm0012::read_acm0012_technology_shapes()
  result <- cdmAcm0012::check_applicability_technology_type(base_triples(), shapes = shapes)
  expect_false(result$conforms)
})

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  expect_error(
    cdmAcm0012::check_applicability_technology_type(project_iri, fluree_conn = NULL),
    regexp = "fluree_conn"
  )
})
