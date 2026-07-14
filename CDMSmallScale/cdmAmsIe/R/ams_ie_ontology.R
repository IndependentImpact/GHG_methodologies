#' Load AMS-I.E technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_ie_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-ie-technology-shapes.ttl",
                package = "cdmAmsIe", mustWork = TRUE)
  )
}

#' @importFrom cdmSemantic read_cdm_concept_triples
#' @export
cdmSemantic::read_cdm_concept_triples

