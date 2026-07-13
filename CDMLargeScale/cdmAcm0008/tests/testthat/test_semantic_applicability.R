CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"
RDF  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

skip_if_no_shaclR <- function() {
  if (!requireNamespace("shaclR", quietly = TRUE)) testthat::skip("shaclR not available")
}
skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) testthat::skip("rdflib not available")
}

make_triples <- function(tech_type_iri = NULL, has_technology = TRUE) {
  project_iri <- paste0(CDM, "project/test001")
  tech_iri    <- paste0(CDM, "tech/test001")

  rows <- list(
    data.frame(
      subject   = project_iri,
      predicate = paste0(RDF, "type"),
      object    = paste0(CDM, "CDMProjectActivity"),
      stringsAsFactors = FALSE
    )
  )

  if (has_technology) {
    rows <- c(rows, list(
      data.frame(
        subject   = project_iri,
        predicate = paste0(AIAO, "isPerformedWith"),
        object    = tech_iri,
        stringsAsFactors = FALSE
      ),
      data.frame(
        subject   = tech_iri,
        predicate = paste0(RDF, "type"),
        object    = tech_type_iri,
        stringsAsFactors = FALSE
      )
    ))
  }

  do.call(rbind, rows)
}

test_that("check_applicability_technology_type returns list with correct names", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_triples(paste0(CDM, "CoalMineMethane"))
  result  <- check_applicability_technology_type(triples)
  expect_true(is.list(result))
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
})

test_that("attestation carries correct methodology and condition", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_triples(paste0(CDM, "CoalMineMethane"))
  result  <- check_applicability_technology_type(triples)
  expect_equal(result$attestation$methodology, "ACM0008")
  expect_equal(result$attestation$condition,   "CoalMineMethane")
})

test_that("CoalMineMethane typed as intermediate node passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_triples(paste0(CDM, "CoalMineMethane"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("SKOS parent of CoalMineMethane does not pass (materialisation is upward-only)", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  # A broader/parent concept does not inherit narrower types after materialisation.
  triples <- make_triples(paste0(CDM, "MethaneCaptureTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("RenewableEnergyTechnology typed as intermediate node fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith fails", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- make_triples(has_technology = FALSE)
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  expect_error(
    check_applicability_technology_type(paste0(CDM, "project/test001")),
    "fluree_conn"
  )
})
