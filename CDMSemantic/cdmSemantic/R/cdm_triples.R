#' Load the CDM concept scheme as a triples data frame
#'
#' Parses the bundled `cdm-concepts.ttl` into a data frame suitable for
#' `shaclR::materialise_skos_hierarchy()`. Call this once and pass the result
#' as `concept_triples` to `cdm_resolve_triples()` or directly to
#' `shaclR::materialise_skos_hierarchy()`.
#'
#' @return Data frame with columns `subject`, `predicate`, `object`, `datatype`.
#' @export
read_cdm_concept_triples <- function() {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    stop("Package 'rdflib' is required to load CDM concept triples.", call. = FALSE)
  }
  path <- system.file("concepts", "cdm-concepts.ttl",
                      package = "cdmSemantic", mustWork = TRUE)
  g <- rdflib::rdf_parse(path, format = "turtle")
  on.exit(rdflib::rdf_free(g))
  result <- rdflib::rdf_query(g, "SELECT ?s ?p ?o WHERE { ?s ?p ?o }")
  colnames(result) <- c("subject", "predicate", "object")
  result$datatype <- NA_character_
  result
}

#' Resolve a project description to a triples data frame
#'
#' Dispatches on the type of `data`:
#' - Character scalar: fetches all triples for the project IRI from Fluree via
#'   a 2-hop SPARQL SELECT query (requires `fluree_conn`).
#' - Data frame: validates that it has `subject`, `predicate`, `object` columns
#'   and returns it unchanged (local / test path).
#'
#' @param data Either a single character project IRI or a data frame with
#'   columns `subject`, `predicate`, `object` (and optionally `datatype`).
#' @param fluree_conn A connected `FlureeInstance` (novaRush). Required when
#'   `data` is a character IRI.
#'
#' @return Data frame with columns `subject`, `predicate`, `object`, `datatype`.
#' @export
cdm_resolve_triples <- function(data, fluree_conn) {
  if (!requireNamespace("shaclR", quietly = TRUE)) {
    stop(
      "Package 'shaclR' is required for semantic applicability checks.",
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
