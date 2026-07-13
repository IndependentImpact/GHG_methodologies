#' Load AMS-III.A technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiia_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiia-technology-shapes.ttl",
                package = "cdmAmsIIIa", mustWork = TRUE)
  )
}
