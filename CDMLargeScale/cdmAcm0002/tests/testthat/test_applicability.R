# Helpers -----------------------------------------------------------------

.make_triples <- function(tech_type, grid_type = NULL) {
  rows <- list(
    data.frame(
      subject   = "http://ex.org/proj",
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object    = "http://independentimpact.org/cdm/CDMProjectActivity",
      datatype  = NA_character_, stringsAsFactors = FALSE
    ),
    data.frame(
      subject   = "http://ex.org/proj",
      predicate = "http://w3id.org/aiao#isPerformedWith",
      object    = "http://ex.org/tech",
      datatype  = NA_character_, stringsAsFactors = FALSE
    ),
    data.frame(
      subject   = "http://ex.org/tech",
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object    = tech_type,
      datatype  = NA_character_, stringsAsFactors = FALSE
    )
  )
  if (!is.null(grid_type)) {
    rows <- c(rows, list(data.frame(
      subject   = "http://ex.org/proj",
      predicate = "http://independentimpact.org/cdm/hasGridConnectionType",
      object    = grid_type,
      datatype  = NA_character_, stringsAsFactors = FALSE
    )))
  }
  do.call(rbind, rows)
}

CDM <- "http://independentimpact.org/cdm/"

# check_applicability_renewable_technology --------------------------------

test_that("returns list with conforms, violations, attestation", {
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  result  <- check_applicability_renewable_technology(triples)
  expect_type(result, "list")
  expect_named(result, c("conforms", "violations", "attestation"))
  expect_type(result$conforms, "logical")
  expect_s3_class(result$violations, "data.frame")
})

test_that("SolarPV (skos:broader RenewableEnergyTechnology) passes", {
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  expect_true(check_applicability_renewable_technology(triples)$conforms)
})

test_that("all named renewable subtypes pass", {
  subtypes <- c("SolarPV", "SolarThermal", "WindTurbine",
                "SmallHydro", "GeothermalSystem", "RenewableBiomassSystem")
  for (s in subtypes) {
    triples <- .make_triples(paste0(CDM, s))
    result  <- check_applicability_renewable_technology(triples)
    expect_true(result$conforms, info = s)
  }
})

test_that("NonRenewableEnergyTechnology fails", {
  triples <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"))
  result  <- check_applicability_renewable_technology(triples)
  expect_false(result$conforms)
  expect_gt(nrow(result$violations), 0L)
})

test_that("attestation records methodology and condition", {
  triples <- .make_triples(paste0(CDM, "SolarPV"))
  att     <- check_applicability_renewable_technology(triples)$attestation
  expect_equal(att$methodology, "ACM0002")
  expect_equal(att$condition,   "RenewableEnergyTechnology")
  expect_true(att$prima_facie)
})

test_that("missing fluree_conn with IRI data raises an error", {
  expect_error(
    check_applicability_renewable_technology("http://ex.org/proj"),
    regexp = "fluree_conn"
  )
})

# check_applicability_grid_connection -------------------------------------

test_that("GridConnectedSystem passes semantic check", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "GridConnectedSystem"))
  expect_true(check_applicability_grid_connection(triples)$conforms)
})

test_that("OffGridSystem fails semantic check for ACM0002", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "OffGridSystem"))
  expect_false(check_applicability_grid_connection(triples)$conforms)
})

test_that("export_share above threshold passes combined check", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "GridConnectedSystem"))
  expect_true(
    check_applicability_grid_connection(triples, export_share = 0.95)$conforms
  )
})

test_that("export_share below threshold fails combined check", {
  triples <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "GridConnectedSystem"))
  result  <- check_applicability_grid_connection(triples, export_share = 0.5)
  expect_false(result$conforms)
  expect_true(any(grepl("export_share", result$violations$message)))
})

# estimate_emission_reductions_acm0002 validate_applicability gate --------

test_that("validate_applicability = FALSE skips checks regardless of project", {
  monitoring <- data.frame(
    period = 1L, gross_generation_mwh = 100, auxiliary_consumption_mwh = 5,
    combined_margin_ef = 0.8, fossil_fuel_tj = 0, fossil_emission_factor = 0,
    electricity_import_mwh = 0, import_emission_factor = 0, leakage_emissions = 0
  )
  expect_no_error(estimate_emission_reductions_acm0002(monitoring))
})

test_that("validate_applicability = TRUE with passing triples proceeds", {
  monitoring <- data.frame(
    period = 1L, gross_generation_mwh = 100, auxiliary_consumption_mwh = 5,
    combined_margin_ef = 0.8, fossil_fuel_tj = 0, fossil_emission_factor = 0,
    electricity_import_mwh = 0, import_emission_factor = 0, leakage_emissions = 0
  )
  passing <- .make_triples(paste0(CDM, "SolarPV"), paste0(CDM, "GridConnectedSystem"))
  result  <- estimate_emission_reductions_acm0002(
    monitoring,
    validate_applicability = TRUE,
    project_id = passing
  )
  expect_s3_class(result, "tbl_df")
})

test_that("validate_applicability = TRUE with failing triples stops with informative error", {
  monitoring <- data.frame(
    period = 1L, gross_generation_mwh = 100, auxiliary_consumption_mwh = 5,
    combined_margin_ef = 0.8, fossil_fuel_tj = 0, fossil_emission_factor = 0,
    electricity_import_mwh = 0, import_emission_factor = 0, leakage_emissions = 0
  )
  failing <- .make_triples(paste0(CDM, "NonRenewableEnergyTechnology"),
                           paste0(CDM, "GridConnectedSystem"))
  expect_error(
    estimate_emission_reductions_acm0002(
      monitoring,
      validate_applicability = TRUE,
      project_id = failing
    ),
    regexp = "applicability check failed"
  )
})

test_that("validate_applicability = TRUE without project_id stops immediately", {
  monitoring <- data.frame(
    period = 1L, gross_generation_mwh = 100, auxiliary_consumption_mwh = 5,
    combined_margin_ef = 0.8, fossil_fuel_tj = 0, fossil_emission_factor = 0,
    electricity_import_mwh = 0, import_emission_factor = 0, leakage_emissions = 0
  )
  expect_error(
    estimate_emission_reductions_acm0002(monitoring, validate_applicability = TRUE),
    regexp = "project_id"
  )
})
