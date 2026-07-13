#' Load AMS-I.D technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_id_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-id-technology-shapes.ttl",
                package = "cdmAmsId", mustWork = TRUE)
  )
}

#' Load AMS-I.D grid connection applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_id_grid_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-id-grid-shapes.ttl",
                package = "cdmAmsId", mustWork = TRUE)
  )
}

