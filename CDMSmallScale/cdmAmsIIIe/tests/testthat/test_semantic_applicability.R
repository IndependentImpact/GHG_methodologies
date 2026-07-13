CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"
RDF  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

skip_if_no_shapeR <- function() {
  if (!requireNamespace("shapeR", quietly = TRUE)) testthat::skip("shapeR not available")
}
skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) testthat::skip("rdflib not available")
}

.make_triples <- function(tech_type, waste_type = NULL) {
  project_node <- "http://ex.org/proj"
  tech_node    <- "http://ex.org/tech"
  rows <- list(
    data.frame(subject = project_node, predicate = paste0(RDF, "type"),             object = paste0(CDM, "CDMProjectActivity"), stringsAsFactors = FALSE),
    data.frame(subject = project_node, predicate = paste0(AIAO, "isPerformedWith"), object = tech_node,                         stringsAsFactors = FALSE),
    data.frame(subject = tech_node,    predicate = paste0(RDF, "type"),             object = tech_type,                          stringsAsFactors = FALSE)
  )
  if (!is.null(waste_type)) {
    waste_node <- "http://ex.org/waste"
    rows <- c(rows, list(
      data.frame(subject = project_node, predicate = paste0(CDM, "hasWasteType"), object = waste_node,  stringsAsFactors = FALSE),
      data.frame(subject = waste_node,   predicate = paste0(RDF, "type"),         object = waste_type,  stringsAsFactors = FALSE)
    ))
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AnaerobicDigestion"))
  result  <- check_applicability_technology_type(triples)
  expect_true(is.list(result))
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("check_applicability_technology_type attestation has correct methodology and condition", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AnaerobicDigestion"))
  result  <- check_applicability_technology_type(triples)
  expect_equal(result$attestation$methodology, "AMS-III.E")
  expect_equal(result$attestation$condition,   "AnaerobicDigestion")
})

test_that("AnaerobicDigestion typed as intermediate node passes technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AnaerobicDigestion"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("WasteTreatmentTechnology parent concept fails (shape checks AnaerobicDigestion specifically)", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "WasteTreatmentTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology fails technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("Missing aiao:isPerformedWith triple fails technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  project_node <- "http://ex.org/proj"
  triples <- data.frame(
    subject   = project_node,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
  result <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("Character IRI without fluree_conn raises error mentioning fluree_conn", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  expect_error(
    check_applicability_technology_type("http://ex.org/proj"),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_waste_type
# ---------------------------------------------------------------------------

test_that("AgriculturalResidues typed as intermediate waste node passes waste type check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AnaerobicDigestion"), paste0(CDM, "AgriculturalResidues"))
  result  <- check_applicability_waste_type(triples)
  expect_true(result$conforms)
})

test_that("check_applicability_waste_type attestation condition is OrganicWaste", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AnaerobicDigestion"), paste0(CDM, "AgriculturalResidues"))
  result  <- check_applicability_waste_type(triples)
  expect_equal(result$attestation$condition, "OrganicWaste")
})

test_that("RecyclableMaterial typed as intermediate waste node fails waste type check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AnaerobicDigestion"), paste0(CDM, "RecyclableMaterial"))
  result  <- check_applicability_waste_type(triples)
  expect_false(result$conforms)
})

test_that("Missing hasWasteType triple fails waste type check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AnaerobicDigestion"))
  result  <- check_applicability_waste_type(triples)
  expect_false(result$conforms)
})
