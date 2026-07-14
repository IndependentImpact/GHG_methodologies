CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"

# ---------------------------------------------------------------------------
# Skip helpers
# ---------------------------------------------------------------------------

skip_if_no_shaclR <- function() {
  if (!requireNamespace("shaclR", quietly = TRUE)) {
    testthat::skip("shaclR not available")
  }
}

skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    testthat::skip("rdflib not available")
  }
}

# ---------------------------------------------------------------------------
# Triples builder
# ---------------------------------------------------------------------------

.make_triples <- function(tech_type, facility_type = NULL) {
  project_iri <- paste0(CDM, "project/test-001")
  tech_iri    <- paste0(CDM, "tech/test-tech")

  rows <- list(
    data.frame(
      subject   = project_iri,
      predicate = paste0("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
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
      predicate = paste0("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
      object    = tech_type,
      stringsAsFactors = FALSE
    )
  )

  if (!is.null(facility_type)) {
    facility_node <- paste0(CDM, "TestFacility")
    rows <- c(rows, list(
      data.frame(subject = project_iri, predicate = paste0(CDM, "hasFacilityType"), object = facility_node, stringsAsFactors = FALSE),
      data.frame(subject = facility_node, predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", object = facility_type, stringsAsFactors = FALSE)
    ))
  }

  do.call(rbind, rows)
}

.make_triples_no_tech <- function() {
  project_iri <- paste0(CDM, "project/test-001")
  data.frame(
    subject   = project_iri,
    predicate = paste0("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
}

.make_triples_no_facility <- function() {
  .make_triples(tech_type = paste0(CDM, "EnergyEfficiencyTechnology"))
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

testthat::test_that("check_applicability_technology_type returns correct list structure", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)

  testthat::expect_type(result, "list")
  testthat::expect_true("conforms" %in% names(result))
  testthat::expect_true("violations" %in% names(result))
  testthat::expect_true("attestation" %in% names(result))
})

testthat::test_that("attestation has correct methodology and condition for technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)

  testthat::expect_equal(result$attestation$methodology, "AMS-II.H")
  testthat::expect_equal(result$attestation$condition,   "EnergyEfficiencyTechnology")
})

testthat::test_that("EfficientHVACSystem (SKOS subtype) passes technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EfficientHVACSystem"))
  result  <- check_applicability_technology_type(triples)

  testthat::expect_true(result$conforms)
})

testthat::test_that("EnergyEfficiencyTechnology top concept itself passes technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)

  testthat::expect_true(result$conforms)
})

testthat::test_that("RenewableEnergyTechnology fails technology check (wrong branch)", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)

  testthat::expect_false(result$conforms)
})

testthat::test_that("missing aiao:isPerformedWith triple fails technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples_no_tech()
  result  <- check_applicability_technology_type(triples)

  testthat::expect_false(result$conforms)
})

testthat::test_that("passing character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  testthat::expect_error(
    check_applicability_technology_type(paste0(CDM, "project/some-iri")),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_facility_type
# ---------------------------------------------------------------------------

testthat::test_that("CentralizedUtilityFacility passes facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(
    tech_type     = paste0(CDM, "EnergyEfficiencyTechnology"),
    facility_type = paste0(CDM, "CentralizedUtilityFacility")
  )
  result <- check_applicability_facility_type(triples)

  testthat::expect_true(result$conforms)
})

testthat::test_that("attestation condition is CentralizedUtilityFacility for facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(
    tech_type     = paste0(CDM, "EnergyEfficiencyTechnology"),
    facility_type = paste0(CDM, "CentralizedUtilityFacility")
  )
  result <- check_applicability_facility_type(triples)

  testthat::expect_equal(result$attestation$condition, "CentralizedUtilityFacility")
})

testthat::test_that("IndustrialFacility fails facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(
    tech_type     = paste0(CDM, "EnergyEfficiencyTechnology"),
    facility_type = paste0(CDM, "IndustrialFacility")
  )
  result <- check_applicability_facility_type(triples)

  testthat::expect_false(result$conforms)
})

testthat::test_that("missing hasFacilityType triple fails facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples_no_facility()
  result  <- check_applicability_facility_type(triples)

  testthat::expect_false(result$conforms)
})
