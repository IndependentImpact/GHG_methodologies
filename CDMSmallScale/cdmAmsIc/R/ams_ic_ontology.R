#' @importFrom cdmSemantic read_cdm_concept_triples
#' @export
cdmSemantic::read_cdm_concept_triples

#' Load AMS-I.C technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_ic_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-ic-technology-shapes.ttl",
                package = "cdmAmsIc", mustWork = TRUE)
  )
}

