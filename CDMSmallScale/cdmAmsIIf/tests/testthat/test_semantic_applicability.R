CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"

skip_if_no_shapeR <- function() {
  if (!requireNamespace("shapeR", quietly = TRUE)) {
    skip("shapeR not installed")
  }
}

skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    skip("rdflib not installed")
  }
}

.make_triples <- function(tech_type, facility_type = NULL, fuel_type = NULL) {
  project_iri  <- paste0(CDM, "project_001")
  tech_iri     <- paste0(CDM, "tech_001")

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
      object    = paste0(CDM, tech_type),
      stringsAsFactors = FALSE
    )
  )

  if (!is.null(facility_type)) {
    facility_iri <- paste0(CDM, "facility_001")
    rows <- c(rows, list(
      data.frame(
        subject   = project_iri,
        predicate = paste0(CDM, "hasFacilityType"),
        object    = facility_iri,
        stringsAsFactors = FALSE
      ),
      data.frame(
        subject   = facility_iri,
        predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        object    = paste0(CDM, facility_type),
        stringsAsFactors = FALSE
      )
    ))
  }

  if (!is.null(fuel_type)) {
    rows <- c(rows, list(
      data.frame(
        subject   = project_iri,
        predicate = paste0(CDM, "hasBaselineFuelType"),
        object    = paste0(CDM, fuel_type),
        stringsAsFactors = FALSE
      )
    ))
  }

  do.call(rbind, rows)
}

.make_triples_no_tech <- function() {
  project_iri <- paste0(CDM, "project_001")
  data.frame(
    subject   = project_iri,
    predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
}

.make_triples_no_facility <- function(tech_type) {
  project_iri <- paste0(CDM, "project_001")
  tech_iri    <- paste0(CDM, "tech_001")
  rbind(
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
      object    = paste0(CDM, tech_type),
      stringsAsFactors = FALSE
    )
  )
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns correct list structure", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("EnergyEfficiencyTechnology")
  result <- check_applicability_technology_type(data)
  expect_type(result, "list")
  expect_true(all(c("conforms", "violations", "attestation") %in% names(result)))
})

test_that("attestation fields are correct for technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("EnergyEfficiencyTechnology")
  result <- check_applicability_technology_type(data)
  expect_equal(result$attestation$methodology, "AMS-II.F")
  expect_equal(result$attestation$condition, "EnergyEfficiencyTechnology")
})

test_that("EfficientMotorSystem (SKOS subtype) passes technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("EfficientMotorSystem")
  result <- check_applicability_technology_type(data)
  expect_true(result$conforms)
})

test_that("EnergyEfficiencyTechnology top concept itself passes", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("EnergyEfficiencyTechnology")
  result <- check_applicability_technology_type(data)
  expect_true(result$conforms)
})

test_that("RenewableEnergyTechnology fails technology check (wrong branch)", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("RenewableEnergyTechnology")
  result <- check_applicability_technology_type(data)
  expect_false(result$conforms)
})

test_that("Missing aiao:isPerformedWith triple fails technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples_no_tech()
  result <- check_applicability_technology_type(data)
  expect_false(result$conforms)
})

test_that("Passing a character IRI without fluree_conn raises error", {
  expect_error(
    check_applicability_technology_type("http://example.org/project/1"),
    "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_facility_type
# ---------------------------------------------------------------------------

test_that("AgriculturalFacility passes facility check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("EnergyEfficiencyTechnology", facility_type = "AgriculturalFacility")
  result <- check_applicability_facility_type(data)
  expect_true(result$conforms)
})

test_that("attestation$condition is AgriculturalFacility", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("EnergyEfficiencyTechnology", facility_type = "AgriculturalFacility")
  result <- check_applicability_facility_type(data)
  expect_equal(result$attestation$condition, "AgriculturalFacility")
})

test_that("Building fails facility check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples("EnergyEfficiencyTechnology", facility_type = "Building")
  result <- check_applicability_facility_type(data)
  expect_false(result$conforms)
})

test_that("Missing hasFacilityType triple fails facility check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  data   <- .make_triples_no_facility("EnergyEfficiencyTechnology")
  result <- check_applicability_facility_type(data)
  expect_false(result$conforms)
})
