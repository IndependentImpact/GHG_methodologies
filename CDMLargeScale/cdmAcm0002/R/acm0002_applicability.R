# Exported applicability functions --------------------------------------------

#' Check ACM0002 renewable technology applicability (semantic)
#'
#' Validates that the project activity implements a renewable energy technology,
#' as required by ACM0002 condition A.4(a). Uses SHACL validation against the
#' CDM concept scheme so any subtype of `cdm:RenewableEnergyTechnology`
#' (SolarPV, WindTurbine, SmallHydro, GeothermalSystem, RenewableBiomassSystem)
#' is accepted automatically.
#'
#' When `data` is a character IRI the project description is fetched from Fluree
#' via `fluree_conn`. When `data` is a triples data frame, validation runs
#' locally — no Fluree connection required (useful for testing).
#'
#' The CDM concept hierarchy is materialised as `rdf:type` triples before SHACL
#' validation; call `shapeR::materialise_skos_hierarchy()` manually if you need
#' to inspect the augmented graph.
#'
#' @param data Either a single character project IRI or a data frame with
#'   columns `subject`, `predicate`, `object` (and optionally `datatype`).
#' @param fluree_conn A connected `FlureeInstance` (novaRush). Required when
#'   `data` is a project IRI.
#' @param concept_triples Optional data frame of CDM concept scheme triples for
#'   SKOS hierarchy materialisation. Defaults to [read_cdm_concept_triples()].
#' @param shapes Optional `sh_shape_graph`. Defaults to
#'   [read_acm0002_technology_shapes()].
#'
#' @return A list with:
#' \describe{
#'   \item{`conforms`}{Logical — `TRUE` if the condition is met.}
#'   \item{`violations`}{Data frame of SHACL violation details (empty when `conforms = TRUE`).}
#'   \item{`attestation`}{List of check metadata (project_id, methodology, condition, checked_at, prima_facie).}
#' }
#' @seealso [check_applicability_grid_connection()], [variable_registry_acm0002()]
#' @export
check_applicability_renewable_technology <- function(data,
                                                     fluree_conn = NULL,
                                                     concept_triples = NULL,
                                                     shapes = NULL) {
  triples         <- cdmSemantic::cdm_resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_acm0002_technology_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) cdmSemantic::read_cdm_concept_triples() else concept_triples

  augmented <- shapeR::materialise_skos_hierarchy(triples, concept_triples)
  result    <- shapeR::validate_shacl(augmented, shapes)
  cdmSemantic::cdm_make_applicability_result(result, data, "ACM0002", "RenewableEnergyTechnology")
}

#' Check ACM0002 grid connection applicability (semantic + optional quantitative)
#'
#' Validates that the project activity is grid-connected, as required by ACM0002
#' condition A.4(b). The semantic check confirms the project declares
#' `cdm:hasGridConnectionType cdm:GridConnectedSystem`. An optional quantitative
#' check on `export_share` can be combined in a single call.
#'
#' @param data Either a single character project IRI or a triples data frame.
#' @param fluree_conn A connected `FlureeInstance`. Required when `data` is a
#'   project IRI.
#' @param concept_triples Optional CDM concept triples data frame.
#' @param shapes Optional `sh_shape_graph`.
#' @param export_share Optional numeric (0–1). When provided, also checks that
#'   the share of net generation exported to the grid meets `minimum_export`.
#' @param minimum_export Numeric threshold for `export_share` (default 0.9).
#'
#' @return A list with `conforms`, `violations`, and `attestation`. When
#'   `export_share` is supplied and fails the threshold, `conforms` is `FALSE`
#'   and the quantitative failure is appended to `violations`.
#' @seealso [check_applicability_renewable_technology()]
#' @export
check_applicability_grid_connection <- function(data,
                                                fluree_conn = NULL,
                                                concept_triples = NULL,
                                                shapes = NULL,
                                                export_share = NULL,
                                                minimum_export = 0.9) {
  triples         <- cdmSemantic::cdm_resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_acm0002_grid_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) cdmSemantic::read_cdm_concept_triples() else concept_triples

  augmented  <- shapeR::materialise_skos_hierarchy(triples, concept_triples)
  result     <- shapeR::validate_shacl(augmented, shapes)
  out        <- cdmSemantic::cdm_make_applicability_result(result, data, "ACM0002", "GridConnectedSystem")

  if (!is.null(export_share)) {
    if (!is.numeric(export_share) || length(export_share) != 1L ||
        export_share < 0 || export_share > 1) {
      stop("`export_share` must be a single numeric value between 0 and 1.",
           call. = FALSE)
    }
    quant_pass <- export_share >= minimum_export
    if (!quant_pass) {
      quant_row <- data.frame(
        focusNode = if (is.character(data)) data else NA_character_,
        shape     = "ExportShareCheck",
        path      = NA_character_,
        component = "QuantitativeThreshold",
        message   = sprintf(
          "ACM0002: export_share %.3f is below minimum %.3f.",
          export_share, minimum_export
        ),
        severity  = "Violation",
        value     = as.character(export_share),
        stringsAsFactors = FALSE
      )
      out$violations <- rbind(out$violations, quant_row)
      out$conforms   <- FALSE
    }
    out$attestation$export_share   <- export_share
    out$attestation$minimum_export <- minimum_export
  }

  out
}
