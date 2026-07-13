CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"
RDF  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

skip_if_no_shapeR <- function() {
  if (!requireNamespace("shapeR", quietly = TRUE)) testthat::skip("shapeR not available")
}
skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) testthat::skip("rdflib not available")
}

.make_triples <- function(tech_type) {
  project_node <- "http://ex.org/proj"
  tech_node    <- "http://ex.org/tech"
  rows <- list(
    data.frame(subject=project_node, predicate=paste0(RDF,"type"),            object=paste0(CDM,"CDMProjectActivity"), stringsAsFactors=FALSE),
    data.frame(subject=project_node, predicate=paste0(AIAO,"isPerformedWith"),object=tech_node,                        stringsAsFactors=FALSE),
    data.frame(subject=tech_node,    predicate=paste0(RDF,"type"),            object=tech_type,                        stringsAsFactors=FALSE)
  )

  do.call(rbind, rows)
}

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "BiologicalNitrogenFixation"))
  result  <- check_applicability_technology_type(triples)
  expect_true(is.list(result))
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("attestation contains correct methodology and condition", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "BiologicalNitrogenFixation"))
  result  <- check_applicability_technology_type(triples)
  expect_equal(result$attestation$methodology, "AMS-III.A")
  expect_equal(result$attestation$condition,   "BiologicalNitrogenFixation")
})

test_that("BiologicalNitrogenFixation typed node passes validation", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "BiologicalNitrogenFixation"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("AgriculturalMitigationTechnology parent concept fails (shape checks specific subtype)", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "AgriculturalMitigationTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology fails validation", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith triple fails validation", {
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

test_that("character IRI without fluree_conn raises error mentioning fluree_conn", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  expect_error(
    check_applicability_technology_type("http://ex.org/proj"),
    regexp = "fluree_conn"
  )
})
