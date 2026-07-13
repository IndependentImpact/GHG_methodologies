#' Load AMS-II.F technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iif_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iif-technology-shapes.ttl",
                package = "cdmAmsIIf", mustWork = TRUE)
  )
}

#' Load AMS-II.F facility type applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iif_facility_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iif-facility-shapes.ttl",
                package = "cdmAmsIIf", mustWork = TRUE)
  )
}
