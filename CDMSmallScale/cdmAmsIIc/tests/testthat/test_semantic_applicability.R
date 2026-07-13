CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"

skip_if_no_shapeR <- function() {
  if (!requireNamespace("shapeR", quietly = TRUE)) {
    skip("shapeR not available")
  }
}

skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    skip("rdflib not available")
  }
}

.make_triples <- function(tech_type,
                          facility_type = NULL,
                          fuel_type = NULL) {
  project_iri <- paste0(CDM, "TestProject001")
  tech_iri    <- paste0(CDM, "TestTech001")

  rows <- list(
    data.frame(
      subject   = project_iri,
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object    = paste0(CDM, "CDMProjectActivity"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      subject   = project_iri,
      predicate = paste0(AIAO, "isPerformedWith"),
      object    = tech_iri,
      stringsAsFactors = FALSE
    ),
    data.frame(
      subject   = tech_iri,
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object    = tech_type,
      stringsAsFactors = FALSE
    )
  )

  if (!is.null(facility_type)) {
    rows <- c(rows, list(data.frame(
      subject   = project_iri,
      predicate = paste0(CDM, "hasFacilityType"),
      object    = facility_type,
      stringsAsFactors = FALSE
    )))
  }

  if (!is.null(fuel_type)) {
    rows <- c(rows, list(data.frame(
      subject   = project_iri,
      predicate = paste0(CDM, "hasBaselineFuelType"),
      object    = fuel_type,
      stringsAsFactors = FALSE
    )))
  }

  do.call(rbind, rows)
}

# ---- check_applicability_technology_type ----

test_that("returns correct list structure", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EfficientLightingSystem"))
  result  <- check_applicability_technology_type(triples)

  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
  expect_type(result$conforms, "logical")
  expect_s3_class(result$violations, "data.frame")
  expect_type(result$attestation, "list")
})

test_that("attestation carries correct methodology and condition labels", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EfficientLightingSystem"))
  result  <- check_applicability_technology_type(triples)

  expect_equal(result$attestation$methodology, "AMS-II.C")
  expect_equal(result$attestation$condition,   "EnergyEfficiencyTechnology")
})

test_that("EfficientLightingSystem (SKOS subtype) passes", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EfficientLightingSystem"))
  result  <- check_applicability_technology_type(triples)

  expect_true(result$conforms)
  expect_equal(nrow(result$violations), 0L)
})

test_that("EnergyEfficiencyTechnology top concept itself passes", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)

  expect_true(result$conforms)
  expect_equal(nrow(result$violations), 0L)
})

test_that("RenewableEnergyTechnology (wrong branch) fails", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)

  expect_false(result$conforms)
  expect_gt(nrow(result$violations), 0L)
})

test_that("missing aiao:isPerformedWith triple fails", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  project_iri <- paste0(CDM, "TestProject001")
  triples <- data.frame(
    subject   = project_iri,
    predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
  result <- check_applicability_technology_type(triples)

  expect_false(result$conforms)
  expect_gt(nrow(result$violations), 0L)
})

test_that("character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()

  expect_error(
    check_applicability_technology_type(
      paste0(CDM, "TestProject001"),
      fluree_conn = NULL
    ),
    regexp = "fluree_conn"
  )
})
