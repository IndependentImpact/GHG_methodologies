CDM  <- "http://independentimpact.org/cdm/"
AIAO <- "http://w3id.org/aiao#"

skip_if_no_shaclR <- function() {
  if (!requireNamespace("shaclR", quietly = TRUE)) {
    skip("shaclR not installed")
  }
}

skip_if_no_rdflib <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    skip("rdflib not installed")
  }
}

.make_triples <- function(tech_type, fuel_type = NULL) {
  project_iri <- paste0(CDM, "TestProject")
  tech_iri    <- paste0(CDM, "TestTech")

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

  if (!is.null(fuel_type)) {
    fuel_node <- paste0(CDM, "TestFuel")
    rows <- c(rows, list(
      data.frame(subject = project_iri, predicate = paste0(CDM, "hasBaselineFuelType"), object = fuel_node, stringsAsFactors = FALSE),
      data.frame(subject = fuel_node, predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", object = fuel_type, stringsAsFactors = FALSE)
    ))
  }

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# check_applicability_technology_type
# ---------------------------------------------------------------------------

test_that("check_applicability_technology_type returns correct list structure", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)

  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"), ignore.order = TRUE)
  expect_type(result$conforms, "logical")
  expect_s3_class(result$violations, "data.frame")
  expect_type(result$attestation, "list")
})

test_that("attestation carries correct methodology and condition", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)

  expect_equal(result$attestation$methodology, "AMS-II.G")
  expect_equal(result$attestation$condition, "EnergyEfficiencyTechnology")
})

test_that("EfficientHVACSystem (SKOS subtype) passes technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EfficientHVACSystem"))
  result  <- check_applicability_technology_type(triples)

  expect_true(result$conforms)
})

test_that("EnergyEfficiencyTechnology top concept itself passes", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_technology_type(triples)

  expect_true(result$conforms)
})

test_that("RenewableEnergyTechnology (wrong branch) fails technology check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "RenewableEnergyTechnology"))
  result  <- check_applicability_technology_type(triples)

  expect_false(result$conforms)
})

test_that("missing aiao:isPerformedWith triple fails technology check", {
  skip_if_no_shaclR()
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

test_that("passing a character IRI without fluree_conn raises error", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  expect_error(
    check_applicability_technology_type(paste0(CDM, "SomeProject")),
    regexp = "fluree_conn"
  )
})

# ---------------------------------------------------------------------------
# check_applicability_baseline_fuel
# ---------------------------------------------------------------------------

test_that("NonRenewableBiomass passes baseline fuel check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(
    paste0(CDM, "EnergyEfficiencyTechnology"),
    fuel_type = paste0(CDM, "NonRenewableBiomass")
  )
  result <- check_applicability_baseline_fuel(triples)

  expect_true(result$conforms)
})

test_that("attestation condition is NonRenewableBiomass", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(
    paste0(CDM, "EnergyEfficiencyTechnology"),
    fuel_type = paste0(CDM, "NonRenewableBiomass")
  )
  result <- check_applicability_baseline_fuel(triples)

  expect_equal(result$attestation$condition, "NonRenewableBiomass")
})

test_that("RenewableBiomass fails baseline fuel check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(
    paste0(CDM, "EnergyEfficiencyTechnology"),
    fuel_type = paste0(CDM, "RenewableBiomass")
  )
  result <- check_applicability_baseline_fuel(triples)

  expect_false(result$conforms)
})

test_that("missing hasBaselineFuelType triple fails baseline fuel check", {
  skip_if_no_shaclR()
  skip_if_no_rdflib()

  triples <- .make_triples(paste0(CDM, "EnergyEfficiencyTechnology"))
  result  <- check_applicability_baseline_fuel(triples)

  expect_false(result$conforms)
})
