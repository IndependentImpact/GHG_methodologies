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
  proj  <- paste0(CDM, "TestProject001")
  rows  <- list(
    data.frame(
      subject   = proj,
      predicate = paste0(RDF, "type"),
      object    = paste0(CDM, "CDMProjectActivity"),
      stringsAsFactors = FALSE
    )
  )
  if (!is.null(tech_type)) {
    tech_node <- paste0(CDM, "TestTechNode001")
    rows <- c(rows, list(
      data.frame(
        subject   = proj,
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
    waste_node <- paste0(CDM, "TestWasteNode001")
    rows <- c(rows, list(
      data.frame(
        subject   = proj,
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

empty_concept_triples <- function() {
  data.frame(
    subject   = character(0),
    predicate = character(0),
    object    = character(0),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

test_that("technology_type: returns list with correct names", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples(tech_type = "LandfillGasCapture")
  shapes  <- cdmAcm0001::read_acm0001_technology_shapes()
  result  <- cdmAcm0001::check_applicability_technology_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_true(is.list(result))
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("technology_type: attestation has correct methodology and condition", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples(tech_type = "LandfillGasCapture")
  shapes  <- cdmAcm0001::read_acm0001_technology_shapes()
  result  <- cdmAcm0001::check_applicability_technology_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_equal(result$attestation$methodology, "ACM0001")
  expect_equal(result$attestation$condition, "LandfillGasCapture")
})

test_that("technology_type: LandfillGasCapture typed as intermediate node passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples(tech_type = "LandfillGasCapture")
  shapes  <- cdmAcm0001::read_acm0001_technology_shapes()
  result  <- cdmAcm0001::check_applicability_technology_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_true(result$conforms)
})

test_that("technology_type: SKOS parent concept typed as intermediate node fails (materialisation is upward-only)", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  # Simulate a parent concept. Without skos:narrower triples in concept_triples,
  # materialise_skos_hierarchy() cannot propagate the subtype downward,
  # so the parent does not gain cdm:LandfillGasCapture membership.
  parent_triples <- make_project_triples(tech_type = "GasManagementSystem")
  shapes <- cdmAcm0001::read_acm0001_technology_shapes()
  result <- cdmAcm0001::check_applicability_technology_type(
    parent_triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_false(result$conforms)
})

test_that("technology_type: RenewableEnergyTechnology typed as intermediate node fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples(tech_type = "RenewableEnergyTechnology")
  shapes  <- cdmAcm0001::read_acm0001_technology_shapes()
  result  <- cdmAcm0001::check_applicability_technology_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_false(result$conforms)
})

test_that("technology_type: missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples()  # no tech_type
  shapes  <- cdmAcm0001::read_acm0001_technology_shapes()
  result  <- cdmAcm0001::check_applicability_technology_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_false(result$conforms)
})

test_that("technology_type: character IRI without fluree_conn raises error", {
  skip_if_no_shaclR()
  expect_error(
    cdmAcm0001::check_applicability_technology_type(
      paste0(CDM, "SomeProject"), fluree_conn = NULL
    ),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_waste_type
# ---------------------------------------------------------------------------

test_that("waste_type: MunicipalSolidWaste typed as intermediate waste node passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples(waste_type = "MunicipalSolidWaste")
  shapes  <- cdmAcm0001::read_acm0001_waste_type_shapes()
  result  <- cdmAcm0001::check_applicability_waste_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_true(result$conforms)
})

test_that("waste_type: attestation$condition == 'MunicipalSolidWaste'", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples(waste_type = "MunicipalSolidWaste")
  shapes  <- cdmAcm0001::read_acm0001_waste_type_shapes()
  result  <- cdmAcm0001::check_applicability_waste_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_equal(result$attestation$condition, "MunicipalSolidWaste")
})

test_that("waste_type: AnimalManure typed as intermediate waste node fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples(waste_type = "AnimalManure")
  shapes  <- cdmAcm0001::read_acm0001_waste_type_shapes()
  result  <- cdmAcm0001::check_applicability_waste_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_false(result$conforms)
})

test_that("waste_type: missing hasWasteType fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_project_triples()  # no waste_type
  shapes  <- cdmAcm0001::read_acm0001_waste_type_shapes()
  result  <- cdmAcm0001::check_applicability_waste_type(
    triples, concept_triples = empty_concept_triples(), shapes = shapes
  )
  expect_false(result$conforms)
})
