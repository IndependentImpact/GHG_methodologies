#' Check ACM0006 biomass feedstock eligibility
#'
#' Validates that the primary fuel used by the project activity is an eligible
#' biomass resource under ACM0006, which covers dedicated energy crops and a
#' wide range of agricultural and forestry residues.
#'
#' @param feedstock Character string describing the dominant biomass resource
#'   (e.g. "agricultural residues", "wood chips", "energy crops").
#' @param allowed Vector of admissible feedstock categories. Defaults to the
#'   common ACM0006 resources.
#' @return Logical indicating whether the feedstock is acceptable.
#' @examples
#' check_applicability_biomass_feedstock("agricultural residues")
#' check_applicability_biomass_feedstock("coal")
#' @export
check_applicability_biomass_feedstock <- function(feedstock,
                                                  allowed = c(
                                                    "agricultural residues",
                                                    "forest residues",
                                                    "wood chips",
                                                    "energy crops",
                                                    "biogas",
                                                    "sewage sludge"
                                                  )) {
  if (!is.character(feedstock) || length(feedstock) != 1) {
    rlang::abort("`feedstock` must be a single character string.")
  }
  if (!is.character(allowed) || length(allowed) == 0) {
    rlang::abort("`allowed` must be a non-empty character vector.")
  }

  tolower(feedstock) %in% tolower(allowed)
}

#' Check ACM0006 biomass dominance condition
#'
#' Ensures that biomass provides the minimum share of useful energy required by
#' ACM0006 (typically at least 90% of the thermal input on an energy basis).
#'
#' @param biomass_fraction Numeric vector giving the biomass contribution to
#'   the total fuel mix on an energy basis (between 0 and 1).
#' @param minimum_fraction Numeric value specifying the minimum share of
#'   biomass required. Defaults to 0.9.
#' @return Logical vector indicating whether the biomass share requirement is
#'   satisfied.
#' @examples
#' check_applicability_biomass_fraction(0.95)
#' check_applicability_biomass_fraction(c(0.85, 0.92), minimum_fraction = 0.9)
#' @export
check_applicability_biomass_fraction <- function(biomass_fraction,
                                                 minimum_fraction = 0.9) {
  if (!is.numeric(biomass_fraction) || any(is.na(biomass_fraction))) {
    rlang::abort("`biomass_fraction` must be a numeric vector without NA values.")
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    rlang::abort("`minimum_fraction` must be a single numeric value.")
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    rlang::abort("`minimum_fraction` must lie between 0 and 1.")
  }
  if (any(biomass_fraction < 0 | biomass_fraction > 1)) {
    rlang::abort("`biomass_fraction` values must lie between 0 and 1.")
  }

  biomass_fraction >= minimum_fraction
}

#' Check ACM0006 technology type applicability (semantic)
#'
#' Validates that the project activity implements the required technology type.
#' Uses SHACL validation with SKOS hierarchy materialisation so any subtype of
#' `cdm:RenewableBiomassSystem` is accepted automatically.
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
  shapes          <- if (is.null(shapes)) read_acm0006_technology_shapes() else shapes
  concept_triples <- if (is.null(concept_triples)) cdmSemantic::read_cdm_concept_triples() else concept_triples
  augmented <- shaclR::materialise_skos_hierarchy(triples, concept_triples)
  result    <- shaclR::validate_shacl(augmented, shapes)
  cdmSemantic::cdm_make_applicability_result(result, data, "ACM0006", "RenewableBiomassSystem")
}
