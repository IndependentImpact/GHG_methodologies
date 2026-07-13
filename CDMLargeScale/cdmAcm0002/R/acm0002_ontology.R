.ACM0002_BASE <- "http://independentimpact.org/methodology/Acm0002/"
.METH_MEASURED_INPUT  <- "http://independentimpact.org/methodology/MeasuredInput"
.METH_EMISSION_FACTOR <- "http://independentimpact.org/methodology/EmissionFactor"
.METH_DERIVED_OUTPUT  <- "http://independentimpact.org/methodology/DerivedOutput"
.METH_REPORTED_OUTPUT <- "http://independentimpact.org/methodology/ReportedOutput"

.QUDT_MWH        <- "http://qudt.org/vocab/unit/MegaW-HR"
.QUDT_TJ         <- "http://qudt.org/vocab/unit/TeraJ"
.QUDT_TCO2E_MWH  <- "http://independentimpact.org/cdm/TonneCO2ePerMegawattHour"
.QUDT_TCO2E_TJ   <- "http://independentimpact.org/cdm/TonneCO2ePerTerajoule"
.QUDT_TCO2E      <- "http://independentimpact.org/cdm/TonneCO2e"

.acm0002_var_iri <- function(symbol) paste0(.ACM0002_BASE, symbol)

#' Variable registry for ACM0002
#'
#' Maps the R column names used by `calculate_*()` functions to their ontology
#' IRIs, variable roles, and QUDT units. This is the machine-readable contract
#' between the monitoring plan and the R methodology functions.
#'
#' @return A [tibble::tibble()] with columns `r_name`, `ontology_iri`, `role`,
#'   `unit_qudt`.
#' @export
variable_registry_acm0002 <- function() {
  tibble::tribble(
    ~r_name,                   ~ontology_iri,                                     ~role,                ~unit_qudt,
    "gross_generation_mwh",    .acm0002_var_iri("GrossGenerationMwh"),             .METH_MEASURED_INPUT, .QUDT_MWH,
    "auxiliary_consumption_mwh", .acm0002_var_iri("AuxiliaryConsumptionMwh"),      .METH_MEASURED_INPUT, .QUDT_MWH,
    "net_electricity_mwh",     .acm0002_var_iri("NetElectricityMwh"),              .METH_DERIVED_OUTPUT, .QUDT_MWH,
    "combined_margin_ef",      .acm0002_var_iri("CombinedMarginEmissionFactor"),   .METH_EMISSION_FACTOR,.QUDT_TCO2E_MWH,
    "fossil_fuel_tj",          .acm0002_var_iri("FossilFuelConsumptionTj"),        .METH_MEASURED_INPUT, .QUDT_TJ,
    "fossil_emission_factor",  .acm0002_var_iri("FossilFuelEmissionFactor"),       .METH_EMISSION_FACTOR,.QUDT_TCO2E_TJ,
    "electricity_import_mwh",  .acm0002_var_iri("ElectricityImportMwh"),           .METH_MEASURED_INPUT, .QUDT_MWH,
    "import_emission_factor",  .acm0002_var_iri("ImportEmissionFactor"),           .METH_EMISSION_FACTOR,.QUDT_TCO2E_MWH,
    "baseline_emissions",      .acm0002_var_iri("BaselineEmissions"),              .METH_DERIVED_OUTPUT, .QUDT_TCO2E,
    "project_emissions",       .acm0002_var_iri("ProjectEmissions"),               .METH_DERIVED_OUTPUT, .QUDT_TCO2E,
    "emission_reductions",     .acm0002_var_iri("EmissionReductions"),             .METH_REPORTED_OUTPUT,.QUDT_TCO2E
  )
}

#' Load ACM0002 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_acm0002_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "acm0002-technology-shapes.ttl",
                package = "cdmAcm0002", mustWork = TRUE)
  )
}

#' Load ACM0002 grid connection applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_acm0002_grid_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "acm0002-grid-shapes.ttl",
                package = "cdmAcm0002", mustWork = TRUE)
  )
}
