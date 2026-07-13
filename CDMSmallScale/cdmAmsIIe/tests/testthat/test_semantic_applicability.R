CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"
RDF  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

PROJECT_IRI <- paste0(CDM, "TestProject001")

skip_if_no_shaclR <- function() {
  testthat::skip_if_not_installed("shaclR")
}

skip_if_no_rdflib <- function() {
  testthat::skip_if_not_installed("rdflib")
}

.make_triples <- function(tech_type, facility_type = NULL, fuel_type = NULL) {
  tech_node <- paste0(CDM, "TestTech001")

  rows <- list(
    data.frame(
      subject   = PROJECT_IRI,
      predicate = paste0(RDF, "type"),
      object    = paste0(CDM, "CDMProjectActivity"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      subject   = PROJECT_IRI,
      predicate = paste0(AIAO, "isPerformedWith"),
      object    = tech_node,
      stringsAsFactors = FALSE
    ),
    data.frame(
      subject   = tech_node,
      predicate = paste0(RDF, "type"),
      object    = tech_type,
      stringsAsFactors = FALSE
    )
  )

  if (!is.null(facility_type)) {
    facility_node <- paste0(CDM, "TestFacility001")
    rows <- c(rows, list(
      data.frame(
        subject   = PROJECT_IRI,
        predicate = paste0(CDM, "hasFacilityType"),
        object    = facility_node,
        stringsAsFactors = FALSE
      ),
      data.frame(
        subject   = facility_node,
        predicate = paste0(RDF, "type"),
        object    = facility_type,
        stringsAsFactors = FALSE
      )
    ))
  }

  if (!is.null(fuel_type)) {
    rows <- c(rows, list(
      data.frame(
        subject   = PROJECT_IRI,
        predicate = paste0(CDM, "hasBaselineFuelType"),
        object    = fuel_type,
        stringsAsFactors = FALSE
      )
    ))
  }

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

testthat::test_that("check_applicability_technology_type returns correct list structure", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientHVACSystem"),
                           facility_type = paste0(CDM, "ResidentialBuilding"))
  result <- check_applicability_technology_type(triples)
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
  testthat::expect_type(result$conforms, "logical")
  testthat::expect_true(is.data.frame(result$violations))
  testthat::expect_type(result$attestation, "list")
})

testthat::test_that("attestation carries correct methodology and condition", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientHVACSystem"))
  result <- check_applicability_technology_type(triples)
  testthat::expect_equal(result$attestation$methodology, "AMS-II.E")
  testthat::expect_equal(result$attestation$condition, "EnergyEfficiencyTechnology")
})

testthat::test_that("EfficientHVACSystem (SKOS subtype) passes technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientHVACSystem"))
  result <- check_applicability_technology_type(triples)
  testthat::expect_true(result$conforms)
})

testthat::test_that("EnergyEfficiencyTechnology top concept itself passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result <- check_applicability_technology_type(triples)
  testthat::expect_true(result$conforms)
})

testthat::test_that("RenewableEnergyTechnology fails technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result <- check_applicability_technology_type(triples)
  testthat::expect_false(result$conforms)
})

testthat::test_that("missing aiao:isPerformedWith triple fails technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  RDF <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  triples <- data.frame(
    subject   = PROJECT_IRI,
    predicate = paste0(RDF, "type"),
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
  result <- check_applicability_technology_type(triples)
  testthat::expect_false(result$conforms)
})

testthat::test_that("character IRI without fluree_conn raises error mentioning fluree_conn", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  testthat::expect_error(
    check_applicability_technology_type(PROJECT_IRI),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_facility_type
# ---------------------------------------------------------------------------

testthat::test_that("ResidentialBuilding passes facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "EfficientHVACSystem"),
    facility_type = paste0(CDM, "ResidentialBuilding")
  )
  result <- check_applicability_facility_type(triples)
  testthat::expect_true(result$conforms)
})

testthat::test_that("attestation$condition is 'Building' for facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "EfficientHVACSystem"),
    facility_type = paste0(CDM, "ResidentialBuilding")
  )
  result <- check_applicability_facility_type(triples)
  testthat::expect_equal(result$attestation$condition, "Building")
})

testthat::test_that("IndustrialFacility fails facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "EfficientHVACSystem"),
    facility_type = paste0(CDM, "IndustrialFacility")
  )
  result <- check_applicability_facility_type(triples)
  testthat::expect_false(result$conforms)
})

testthat::test_that("missing hasFacilityType triple fails facility check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientHVACSystem"))
  result <- check_applicability_facility_type(triples)
  testthat::expect_false(result$conforms)
})
