#' Build a standardised applicability result list
#'
#' Wraps a SHACL validation result and attaches attestation metadata. This is
#' the single return-value constructor used by all `check_applicability_*()`
#' functions across CDM methodology packages.
#'
#' @param shacl_result List returned by `shapeR::validate_shacl()`, containing
#'   `conforms` (logical) and `results` (data frame of violations).
#' @param data The `data` argument that was passed to the calling
#'   `check_applicability_*()` function — used to record the project IRI in
#'   the attestation when `data` is a character IRI.
#' @param methodology Character scalar. Short methodology code, e.g.
#'   `"ACM0002"` or `"AMS-I.A"`.
#' @param condition Character scalar. Name of the applicability condition
#'   being checked, e.g. `"RenewableEnergyTechnology"`.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{`conforms`}{Logical — `TRUE` if the condition is met.}
#'   \item{`violations`}{Data frame of SHACL violation details (empty when
#'     `conforms = TRUE`).}
#'   \item{`attestation`}{Named list recording `project_id`, `methodology`,
#'     `condition`, `checked_at`, and `prima_facie`.}
#' }
#' @export
cdm_make_applicability_result <- function(shacl_result, data, methodology, condition) {
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
