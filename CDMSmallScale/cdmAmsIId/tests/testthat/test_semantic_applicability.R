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

.make_triples <- function(tech_type, facility_type = NULL, fuel_type = NULL) {
  project_iri <- paste0(CDM, "TestProject")
  tech_iri    <- paste0(CDM, "TestTechnology")

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
    facility_node <- paste0(CDM, "TestFacility")
    rows <- c(rows, list(
      data.frame(subject = project_iri, predicate = paste0(CDM, "hasFacilityType"), object = facility_node, stringsAsFactors = FALSE),
      data.frame(subject = facility_node, predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", object = facility_type, stringsAsFactors = FALSE)
    ))
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

# --- check_applicability_technology_type ---

test_that("check_applicability_technology_type returns correct list structure", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientMotorSystem"))
  result  <- check_applicability_technology_type(triples)
  expect_type(result, "list")
  expect_true(all(c("conforms", "violations", "attestation") %in% names(result)))
})

test_that("check_applicability_technology_type attestation has correct methodology and condition", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientMotorSystem"))
  result  <- check_applicability_technology_type(triples)
  expect_equal(result$attestation$methodology, "AMS-II.D")
  expect_equal(result$attestation$condition, "EnergyEfficiencyTechnology")
})

test_that("EfficientMotorSystem (SKOS subtype) passes technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientMotorSystem"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("EnergyEfficiencyTechnology top concept itself passes technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_true(result$conforms)
})

test_that("RenewableEnergyTechnology fails technology check (wrong branch)", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("Missing aiao:isPerformedWith triple fails technology check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  project_iri <- paste0(CDM, "TestProject")
  triples <- data.frame(
    subject   = project_iri,
    predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    object    = paste0(CDM, "CDMProjectActivity"),
    stringsAsFactors = FALSE
  )
  result <- check_applicability_technology_type(triples)
  expect_false(result$conforms)
})

test_that("Passing a character IRI without fluree_conn raises error matching 'fluree_conn'", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  expect_error(
    check_applicability_technology_type(paste0(CDM, "SomeProject")),
    "fluree_conn"
  )
})

# --- check_applicability_facility_type ---

test_that("IndustrialFacility passes facility check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "EfficientMotorSystem"),
    facility_type = paste0(CDM, "IndustrialFacility")
  )
  result <- check_applicability_facility_type(triples)
  expect_true(result$conforms)
})

test_that("check_applicability_facility_type attestation$condition == 'IndustrialFacility'", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "EfficientMotorSystem"),
    facility_type = paste0(CDM, "IndustrialFacility")
  )
  result <- check_applicability_facility_type(triples)
  expect_equal(result$attestation$condition, "IndustrialFacility")
})

test_that("Building fails facility check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(
    paste0(CDM, "EfficientMotorSystem"),
    facility_type = paste0(CDM, "Building")
  )
  result <- check_applicability_facility_type(triples)
  expect_false(result$conforms)
})

test_that("Missing hasFacilityType triple fails facility check", {
  skip_if_no_shapeR()
  skip_if_no_rdflib()
  triples <- .make_triples(paste0(CDM, "EfficientMotorSystem"))
  result  <- check_applicability_facility_type(triples)
  expect_false(result$conforms)
})
