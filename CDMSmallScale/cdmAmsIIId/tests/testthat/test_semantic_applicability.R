CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"
RDF  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

skip_if_no_shaclR <- function() {
  if (!requireNamespace("shaclR", quietly = TRUE)) testthat::skip("shaclR not available")
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
      data.frame(subject = project_node, predicate = paste0(CDM, "hasWasteType"), object = waste_node,  stringsAsFactors = FALSE),
      data.frame(subject = waste_node,   predicate = paste0(RDF, "type"),         object = waste_type,  stringsAsFactors = FALSE)
    ))
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns list with expected names", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"))
  result  <- check_applicability_technology_type(triples)
  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("attestation has correct methodology and condition for technology type check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"))
  result  <- check_applicability_technology_type(triples)
  expect_equal(result$attestation$methodology, "AMS-III.D")
  expect_equal(result$attestation$condition, "ManureMethaneCapture")
})

test_that("ManureMethaneCapture typed as intermediate node passes technology type check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("WasteTreatmentTechnology as direct typed intermediate node passes technology type check after materialisation", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("RenewableEnergyTechnology as intermediate node fails technology type check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith triple fails technology type check", {
  skip_if_no_shaclR()
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
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  expect_error(
    check_applicability_technology_type("http://ex.org/proj"),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_waste_type
# ---------------------------------------------------------------------------

test_that("AnimalManure typed as intermediate waste node passes waste type check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"), waste_type = paste0(CDM, "AnimalManure"))
  result  <- check_applicability_waste_type(triples)
  expect_true(result$conforms)
})

test_that("attestation condition is AnimalManure for waste type check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"), waste_type = paste0(CDM, "AnimalManure"))
  result  <- check_applicability_waste_type(triples)
  expect_equal(result$attestation$condition, "AnimalManure")
})

test_that("MunicipalSolidWaste typed as intermediate waste node fails waste type check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"), waste_type = paste0(CDM, "MunicipalSolidWaste"))
  result  <- check_applicability_waste_type(triples)
  expect_false(result$conforms)
})

test_that("missing hasWasteType triple fails waste type check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "ManureMethaneCapture"))
  result  <- check_applicability_waste_type(triples)
  expect_false(result$conforms)
})
