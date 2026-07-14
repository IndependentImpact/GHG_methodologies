#' Load AMS-I.F technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_if_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-if-technology-shapes.ttl",
                package = "cdmAmsIf", mustWork = TRUE)
  )
}

#' Load AMS-I.F grid connection applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_if_grid_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-if-grid-shapes.ttl",
                package = "cdmAmsIf", mustWork = TRUE)
  )
}

