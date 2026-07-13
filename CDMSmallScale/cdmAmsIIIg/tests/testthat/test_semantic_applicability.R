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
    data.frame(subject = tech_node,    predicate = paste0(RDF, "type"),             object = tech_type,                         stringsAsFactors = FALSE)
  )
  if (!is.null(waste_type)) {
    waste_node <- "http://ex.org/waste"
    rows <- c(rows, list(
      data.frame(subject = project_node, predicate = paste0(CDM, "hasWasteType"), object = waste_node, stringsAsFactors = FALSE),
      data.frame(subject = waste_node,   predicate = paste0(RDF, "type"),         object = waste_type, stringsAsFactors = FALSE)
    ))
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns list with expected names", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "LandfillGasCapture"))
  result  <- check_applicability_technology_type(triples)
  expect_named(result, c("conforms", "violations", "attestation"))
})

test_that("attestation has correct methodology and condition", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "LandfillGasCapture"))
  result  <- check_applicability_technology_type(triples)
  expect_equal(result$attestation$methodology, "AMS-III.G")
  expect_equal(result$attestation$condition, "LandfillGasCapture")
})

test_that("LandfillGasCapture typed as intermediate node passes", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "LandfillGasCapture"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("WasteTreatmentTechnology parent concept fails (shape checks LandfillGasCapture specifically)", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "WasteTreatmentTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology typed as intermediate node fails", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith triple fails", {
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

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  expect_error(
    check_applicability_technology_type("http://ex.org/proj"),
    "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_waste_type
# ---------------------------------------------------------------------------

test_that("MunicipalSolidWaste typed as intermediate waste node passes", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "LandfillGasCapture"),
    paste0(CDM, "MunicipalSolidWaste")
  )
  result <- check_applicability_waste_type(triples)
  expect_true(result$conforms)
})

test_that("waste type attestation has correct condition", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "LandfillGasCapture"),
    paste0(CDM, "MunicipalSolidWaste")
  )
  result <- check_applicability_waste_type(triples)
  expect_equal(result$attestation$condition, "MunicipalSolidWaste")
})

test_that("RecyclableMaterial typed as intermediate waste node fails", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "LandfillGasCapture"),
    paste0(CDM, "RecyclableMaterial")
  )
  result <- check_applicability_waste_type(triples)
  expect_false(result$conforms)
})

test_that("missing hasWasteType triple fails", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  project_node <- "http://ex.org/proj"
  triples <- data.frame(
    subject   = project_node,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
  result <- check_applicability_waste_type(triples)
  expect_false(result$conforms)
})
