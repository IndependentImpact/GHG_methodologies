#' Load AMS-I.B technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_ib_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-ib-technology-shapes.ttl",
                package = "cdmAmsIb", mustWork = TRUE)
  )
}

#' Load AMS-I.B grid connection applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_ib_grid_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-ib-grid-shapes.ttl",
                package = "cdmAmsIb", mustWork = TRUE)
  )
}
