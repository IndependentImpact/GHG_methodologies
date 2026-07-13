# Private helpers shared by all semantic check_applicability_*() functions -----

# Resolve `data` to a triples data frame.
# `data` is either a project IRI string (fetch from Fluree) or already a
# triples data frame (local / test path).
.resolve_triples <- function(data, fluree_conn) {
  if (!requireNamespace("shapeR", quietly = TRUE)) {
    stop(
      "Package 'shapeR' is required for semantic applicability checks. ",
      "Install it or add it to your library path.",
      call. = FALSE
    )
  }
  if (is.character(data) && length(data) == 1L) {
    if (is.null(fluree_conn)) {
      stop(
        "`fluree_conn` (a connected FlureeInstance) is required when ",
        "`data` is a project IRI.",
        call. = FALSE
      )
    }
    .fetch_project_triples(data, fluree_conn)
  } else if (is.data.frame(data)) {
    missing_cols <- setdiff(c("subject", "predicate", "object"), names(data))
    if (length(missing_cols)) {
      stop(
        "Triples data frame is missing columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    data
  } else {
    stop(
      "`data` must be a single project IRI (character) or a triples data frame.",
      call. = FALSE
    )
  }
}

# Fetch all triples for a project and its directly linked resources from Fluree.
# Returns a data frame with columns subject, predicate, object, datatype.
.fetch_project_triples <- function(project_iri, fluree_conn) {
  sparql <- sprintf(
    "SELECT ?s ?p ?o WHERE {
       { BIND(<%s> AS ?s) ?s ?p ?o . }
       UNION
       { <%s> ?anyProp ?s . FILTER(isIRI(?s)) ?s ?p ?o . }
     }",
    project_iri, project_iri
  )
  result <- fluree_conn$sparql(sparql)$send()
  .sparql_result_to_triples(result)
}

# Convert a SPARQL SELECT result (as parsed by jsonlite::fromJSON) to the
# triples data frame format expected by shapeR::materialise_skos_hierarchy()
# and shapeR::validate_shacl().
.sparql_result_to_triples <- function(result) {
  bindings <- result$results$bindings
  if (is.null(bindings) || (is.data.frame(bindings) && nrow(bindings) == 0L)) {
    return(data.frame(
      subject = character(), predicate = character(),
      object  = character(), datatype  = character(),
      stringsAsFactors = FALSE
    ))
  }
  o_dtype <- if (!is.null(bindings$o$datatype)) {
    bindings$o$datatype
  } else {
    rep(NA_character_, nrow(bindings))
  }
  data.frame(
    subject   = bindings$s$value,
    predicate = bindings$p$value,
    object    = bindings$o$value,
    datatype  = o_dtype,
    stringsAsFactors = FALSE
  )
}

# Build the standardised applicability result list.
.make_applicability_result <- function(shacl_result, data, methodology, condition) {
  list(
    conforms    = shacl_result$conforms,
    violations  = shacl_result$results,
    attestation = list(
      project_id  = if (is.character(data)) data else NA_character_,
      methodology = methodology,
      condition   = condition,
      checked_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      prima_facie = shacl_result$conforms
    )
  )
}

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
  triples         <- .resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_acm0002_technology_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) read_cdm_concept_triples() else concept_triples

  augmented <- shapeR::materialise_skos_hierarchy(triples, concept_triples)
  result    <- shapeR::validate_shacl(augmented, shapes)
  .make_applicability_result(result, data, "ACM0002", "RenewableEnergyTechnology")
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
  triples         <- .resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_acm0002_grid_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) read_cdm_concept_triples() else concept_triples

  augmented  <- shapeR::materialise_skos_hierarchy(triples, concept_triples)
  result     <- shapeR::validate_shacl(augmented, shapes)
  out        <- .make_applicability_result(result, data, "ACM0002", "GridConnectedSystem")

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
