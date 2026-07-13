# Semantic applicability functions --------------------------------------------

#' Check AMS-I.A renewable technology applicability (semantic)
#'
#' Validates that the project activity uses a renewable energy technology, as
#' required by AMS-I.A condition 2(a). Accepts any subtype of
#' `cdm:RenewableEnergyTechnology` declared via `aiao:isPerformedWith`.
#'
#' When `data` is a character IRI the project description is fetched from
#' Fluree via `fluree_conn`. When `data` is a triples data frame, validation
#' runs locally — no Fluree connection required.
#'
#' @param data Either a single character project IRI or a data frame with
#'   columns `subject`, `predicate`, `object` (and optionally `datatype`).
#' @param fluree_conn A connected `FlureeInstance` (novaRush). Required when
#'   `data` is a project IRI.
#' @param concept_triples Optional CDM concept triples data frame for SKOS
#'   materialisation. Defaults to [cdmSemantic::read_cdm_concept_triples()].
#' @param shapes Optional `sh_shape_graph`. Defaults to
#'   [read_ams_ia_technology_shapes()].
#'
#' @return A list with `conforms` (logical), `violations` (data frame), and
#'   `attestation` (list of check metadata).
#' @seealso [check_applicability_grid_connection()],
#'   [check_applicability_installed_capacity()]
#' @export
check_applicability_renewable_technology <- function(data,
                                                     fluree_conn = NULL,
                                                     concept_triples = NULL,
                                                     shapes = NULL) {
  triples         <- cdmSemantic::cdm_resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_ams_ia_technology_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) cdmSemantic::read_cdm_concept_triples() else concept_triples

  augmented <- shapeR::materialise_skos_hierarchy(triples, concept_triples)
  result    <- shapeR::validate_shacl(augmented, shapes)
  cdmSemantic::cdm_make_applicability_result(result, data, "AMS-I.A", "RenewableEnergyTechnology")
}

#' Check AMS-I.A grid connection applicability (semantic)
#'
#' Validates that the project activity is user-sited (off-grid, captive use, or
#' mini-grid), as required by AMS-I.A condition 2(b). Grid-connected export
#' projects must use AMS-I.D or ACM0002.
#'
#' @param data Either a single character project IRI or a triples data frame.
#' @param fluree_conn A connected `FlureeInstance`. Required when `data` is a
#'   project IRI.
#' @param concept_triples Optional CDM concept triples data frame.
#' @param shapes Optional `sh_shape_graph`.
#'
#' @return A list with `conforms`, `violations`, and `attestation`.
#' @seealso [check_applicability_renewable_technology()]
#' @export
check_applicability_grid_connection <- function(data,
                                                fluree_conn = NULL,
                                                concept_triples = NULL,
                                                shapes = NULL) {
  triples         <- cdmSemantic::cdm_resolve_triples(data, fluree_conn)
  shapes          <- if (is.null(shapes)) read_ams_ia_grid_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) cdmSemantic::read_cdm_concept_triples() else concept_triples

  augmented <- shapeR::materialise_skos_hierarchy(triples, concept_triples)
  result    <- shapeR::validate_shacl(augmented, shapes)
  cdmSemantic::cdm_make_applicability_result(result, data, "AMS-I.A", "UserSitedConnection")
}

# Quantitative applicability functions ----------------------------------------

#' Check AMS-I.A installed capacity threshold
#'
#' Evaluates the AMS-I.A applicability condition that total installed renewable capacity must
#' remain within the small-scale threshold of 15 MW.
#'
#' @param capacity_kw Total installed capacity in kilowatts (kW).
#' @param renewable_fraction Share of electricity generated from renewable sources (0-1).
#' @param threshold_kw Threshold in kW, defaulting to 15000 (15 MW).
#' @return Logical indicating whether the condition is satisfied.
#' @examples
#' check_applicability_installed_capacity(capacity_kw = 500, renewable_fraction = 1)
#' check_applicability_installed_capacity(capacity_kw = 20000, renewable_fraction = 1)
#' @seealso [check_applicability_distributed_generation()]
#' @export
check_applicability_installed_capacity <- function(capacity_kw,
                                                   renewable_fraction,
                                                   threshold_kw = 15000) {
  if (!is.numeric(capacity_kw) || length(capacity_kw) != 1) {
    stop("`capacity_kw` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(renewable_fraction) || renewable_fraction < 0 || renewable_fraction > 1) {
    stop("`renewable_fraction` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(threshold_kw) || length(threshold_kw) != 1) {
    stop("`threshold_kw` must be a single numeric value.", call. = FALSE)
  }

  capacity_kw <= threshold_kw && renewable_fraction >= 0.95
}

#' Check AMS-I.A distributed generation condition
#'
#' Ensures the electricity generation unit is located at or near the user site and primarily
#' displaces captive fossil fuel generation, approximated by the share of pre-project fossil use.
#'
#' @param fossil_fraction_baseline Share of baseline electricity provided by fossil fuels (0-1).
#' @param minimum_fraction Minimum fossil share that must be displaced to qualify (default 0.5).
#' @return Logical indicating whether the generation primarily displaces fossil fuel use.
#' @examples
#' check_applicability_distributed_generation(fossil_fraction_baseline = 0.9)
#' check_applicability_distributed_generation(fossil_fraction_baseline = 0.3)
#' @export
check_applicability_distributed_generation <- function(fossil_fraction_baseline,
                                                       minimum_fraction = 0.5) {
  if (!is.numeric(fossil_fraction_baseline) || length(fossil_fraction_baseline) != 1) {
    stop("`fossil_fraction_baseline` must be a single numeric value.", call. = FALSE)
  }
  if (fossil_fraction_baseline < 0 || fossil_fraction_baseline > 1) {
    stop("`fossil_fraction_baseline` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(minimum_fraction) || minimum_fraction < 0 || minimum_fraction > 1) {
    stop("`minimum_fraction` must be between 0 and 1.", call. = FALSE)
  }

  fossil_fraction_baseline >= minimum_fraction
}
