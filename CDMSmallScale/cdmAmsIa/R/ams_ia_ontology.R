.AMS_IA_BASE <- "http://independentimpact.org/methodology/AmsIa/"
.METH_MEASURED_INPUT  <- "http://independentimpact.org/methodology/MeasuredInput"
.METH_EMISSION_FACTOR <- "http://independentimpact.org/methodology/EmissionFactor"
.METH_DERIVED_OUTPUT  <- "http://independentimpact.org/methodology/DerivedOutput"
.METH_REPORTED_OUTPUT <- "http://independentimpact.org/methodology/ReportedOutput"

.QUDT_KWH        <- "http://qudt.org/vocab/unit/KiloW-HR"
.QUDT_TCO2E_KWH  <- "http://independentimpact.org/cdm/TonneCO2ePerKilowattHour"
.QUDT_TCO2E      <- "http://independentimpact.org/cdm/TonneCO2e"

.ams_ia_var_iri <- function(symbol) paste0(.AMS_IA_BASE, symbol)

#' Variable registry for AMS-I.A
#'
#' Maps the R column names used by `calculate_*()` functions to their ontology
#' IRIs, variable roles, and QUDT units. This is the machine-readable contract
#' between the monitoring plan and the R methodology functions.
#'
#' @return A [tibble::tibble()] with columns `r_name`, `ontology_iri`, `role`,
#'   `unit_qudt`.
#' @export
variable_registry_ams_ia <- function() {
  tibble::tribble(
    ~r_name,                       ~ontology_iri,                                   ~role,                ~unit_qudt,
    "generation_kwh",              .ams_ia_var_iri("GenerationKwh"),                .METH_MEASURED_INPUT, .QUDT_KWH,
    "grid_emission_factor",        .ams_ia_var_iri("GridEmissionFactor"),            .METH_EMISSION_FACTOR,.QUDT_TCO2E_KWH,
    "project_emission_factor",     .ams_ia_var_iri("ProjectEmissionFactor"),         .METH_EMISSION_FACTOR,.QUDT_TCO2E_KWH,
    "baseline_generation_kwh",     .ams_ia_var_iri("BaselineGenerationKwh"),         .METH_DERIVED_OUTPUT, .QUDT_KWH,
    "baseline_emissions_tco2e",    .ams_ia_var_iri("BaselineEmissions"),             .METH_DERIVED_OUTPUT, .QUDT_TCO2E,
    "project_emissions_tco2e",     .ams_ia_var_iri("ProjectEmissions"),              .METH_DERIVED_OUTPUT, .QUDT_TCO2E,
    "emission_reductions_tco2e",   .ams_ia_var_iri("EmissionReductions"),            .METH_REPORTED_OUTPUT,.QUDT_TCO2E
  )
}

#' Load AMS-I.A technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_ia_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-ia-technology-shapes.ttl",
                package = "cdmAmsIa", mustWork = TRUE)
  )
}

#' Load AMS-I.A grid connection applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_ia_grid_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-ia-grid-shapes.ttl",
                package = "cdmAmsIa", mustWork = TRUE)
  )
}
