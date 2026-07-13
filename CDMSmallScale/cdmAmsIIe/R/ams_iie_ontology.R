#' Load AMS-II.E technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iie_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iie-technology-shapes.ttl",
                package = "cdmAmsIIe", mustWork = TRUE)
  )
}

#' Load AMS-II.E facility type applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iie_facility_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iie-facility-shapes.ttl",
                package = "cdmAmsIIe", mustWork = TRUE)
  )
}
