test_that("read_cdm_concept_triples returns expected structure", {
  skip_if_not_installed("rdflib")
  ct <- read_cdm_concept_triples()
  expect_s3_class(ct, "data.frame")
  expect_named(ct, c("subject", "predicate", "object", "datatype"))
  expect_gt(nrow(ct), 0L)
})

test_that("read_cdm_concept_triples includes RenewableEnergyTechnology hierarchy", {
  skip_if_not_installed("rdflib")
  ct  <- read_cdm_concept_triples()
  CDM <- "http://independentimpact.org/cdm/"
  broader <- ct[grepl("skos/core#broader", ct$predicate, fixed = TRUE), ]
  solar_row <- broader[broader$subject == paste0(CDM, "SolarPV"), ]
  expect_equal(solar_row$object, paste0(CDM, "RenewableEnergyTechnology"))
})

test_that("cdm_resolve_triples returns data frame unchanged", {
  df <- data.frame(subject = "a", predicate = "b", object = "c",
                   stringsAsFactors = FALSE)
  result <- cdm_resolve_triples(df, fluree_conn = NULL)
  expect_identical(result, df)
})

test_that("cdm_resolve_triples errors on IRI without fluree_conn", {
  expect_error(
    cdm_resolve_triples("http://ex.org/proj", fluree_conn = NULL),
    regexp = "fluree_conn"
  )
})

test_that("cdm_resolve_triples errors on malformed data frame", {
  df <- data.frame(x = 1, y = 2)
  expect_error(cdm_resolve_triples(df, fluree_conn = NULL), regexp = "missing columns")
})

test_that("cdm_resolve_triples errors on unsupported type", {
  expect_error(cdm_resolve_triples(list(a = 1), fluree_conn = NULL), regexp = "project IRI")
})

test_that("cdm_make_applicability_result conforms=TRUE case", {
  fake_shacl <- list(
    conforms = TRUE,
    results  = data.frame(focusNode = character(), shape = character(),
                          path = character(), component = character(),
                          message = character(), severity = character(),
                          value = character(), scope = character(),
                          stringsAsFactors = FALSE)
  )
  res <- cdm_make_applicability_result(fake_shacl, "http://ex.org/proj", "ACM0002", "Test")
  expect_true(res$conforms)
  expect_equal(nrow(res$violations), 0L)
  expect_equal(res$attestation$methodology, "ACM0002")
  expect_equal(res$attestation$condition, "Test")
  expect_equal(res$attestation$project_id, "http://ex.org/proj")
  expect_true(res$attestation$prima_facie)
})

test_that("cdm_make_applicability_result conforms=FALSE case", {
  fake_shacl <- list(
    conforms = FALSE,
    results  = data.frame(focusNode = "n", shape = "s", path = "p",
                          component = "c", message = "violation",
                          severity = "sh:Violation", value = NA_character_,
                          scope = "property", stringsAsFactors = FALSE)
  )
  res <- cdm_make_applicability_result(fake_shacl, data.frame(), "AMS-I.A", "Grid")
  expect_false(res$conforms)
  expect_equal(nrow(res$violations), 1L)
  expect_true(is.na(res$attestation$project_id))
  expect_false(res$attestation$prima_facie)
})
