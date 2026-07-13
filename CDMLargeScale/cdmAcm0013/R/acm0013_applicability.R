#' Check ACM0013 applicability conditions
#'
#' Evaluates whether a monitoring dataset satisfies the core ACM0013
#' applicability conditions: the plant must be a new build, connected to the
#' grid, fuelled by a less carbon-intensive technology than the baseline, and the
#' project must demonstrate that it displaces higher-emitting generation on the
#' grid.
#'
#' @param data A tibble or data frame containing at least the columns
#'   `is_new_plant`, `grid_connected`, `technology_emission_factor_tco2_per_mwh`,
#'   and `baseline_emission_factor_tco2_per_mwh`.
#' @param efficiency_improvement_threshold Minimum percentage improvement in the
#'   emission factor (baseline minus project) required for the methodology to
#'   apply. Defaults to `0.1` (i.e. 10%).
#' @return Logical scalar indicating whether the dataset meets the applicability
#'   criteria.
#' @examples
#' dataset <- simulate_acm0013_dataset(periods = 1, seed = 123)
#' check_applicability_acm0013(dataset)
#' @export
check_applicability_acm0013 <- function(data, efficiency_improvement_threshold = 0.1) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }
  required_cols <- c(
    "is_new_plant", "grid_connected", "technology_emission_factor_tco2_per_mwh",
    "baseline_emission_factor_tco2_per_mwh"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(sprintf(
      "Missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  if (!is.numeric(efficiency_improvement_threshold) || length(efficiency_improvement_threshold) != 1) {
    rlang::abort("`efficiency_improvement_threshold` must be a single numeric value.")
  }
  if (efficiency_improvement_threshold < 0 || efficiency_improvement_threshold >= 1) {
    rlang::abort("`efficiency_improvement_threshold` must be between 0 and 1.")
  }

  if (!is.logical(data$is_new_plant) || !is.logical(data$grid_connected)) {
    rlang::abort("`is_new_plant` and `grid_connected` must be logical vectors.")
  }

  required_flags <- all(data$is_new_plant, na.rm = TRUE) && all(data$grid_connected, na.rm = TRUE)

  emission_factor_check <- mean(data$technology_emission_factor_tco2_per_mwh <
                                  data$baseline_emission_factor_tco2_per_mwh,
                                na.rm = TRUE)

  if (is.nan(emission_factor_check)) {
    emission_factor_check <- 0
  }

  average_improvement <- mean(
    (data$baseline_emission_factor_tco2_per_mwh - data$technology_emission_factor_tco2_per_mwh) /
      data$baseline_emission_factor_tco2_per_mwh,
    na.rm = TRUE
  )

  required_flags && emission_factor_check == 1 && average_improvement >= efficiency_improvement_threshold
}


#' Check ACM0013 technology type applicability (semantic)
#'
#' Validates that the project activity implements the required technology type.
#' Uses SHACL validation with SKOS hierarchy materialisation so any subtype of
#' `cdm:NonRenewableEnergyTechnology` is accepted automatically.
#'
#' @param data Either a single character project IRI or a data frame with
#'   columns `subject`, `predicate`, `object` (and optionally `datatype`).
#' @param fluree_conn A connected `FlureeInstance` (novaRush). Required when
#'   `data` is a project IRI.
#' @param concept_triples Optional CDM concept triples data frame.
#' @param shapes Optional `sh_shape_graph`.
#' @return A list with `conforms`, `violations`, and `attestation`.
#' @export
check_applicability_technology_type <- function(data,
                                                fluree_conn = NULL,
                                                concept_triples = NULL,
                                                shapes = NULL) {
  triples         <- cdmSemantic::cdm_resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_acm0013_technology_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) cdmSemantic::read_cdm_concept_triples() else concept_triples
  augmented <- shaclR::materialise_skos_hierarchy(triples, concept_triples)
  result    <- shaclR::validate_shacl(augmented, shapes)
  cdmSemantic::cdm_make_applicability_result(result, data, "ACM0013", "NonRenewableEnergyTechnology")
}


#' Check ACM0013 grid connection applicability (semantic)
#'
#' Validates that the project plant is grid-connected.
#'
#' @param data Either a single character project IRI or a data frame.
#' @param fluree_conn A connected `FlureeInstance`. Required when `data` is a project IRI.
#' @param concept_triples Optional CDM concept triples data frame.
#' @param shapes Optional `sh_shape_graph`.
#' @return A list with `conforms`, `violations`, and `attestation`.
#' @seealso [check_applicability_technology_type()]
#' @export
check_applicability_grid_connection <- function(data,
                                                fluree_conn = NULL,
                                                concept_triples = NULL,
                                                shapes = NULL) {
  triples         <- cdmSemantic::cdm_resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_acm0013_grid_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) cdmSemantic::read_cdm_concept_triples() else concept_triples
  augmented <- shaclR::materialise_skos_hierarchy(triples, concept_triples)
  result    <- shaclR::validate_shacl(augmented, shapes)
  cdmSemantic::cdm_make_applicability_result(result, data, "ACM0013", "GridConnectedSystem")
}
