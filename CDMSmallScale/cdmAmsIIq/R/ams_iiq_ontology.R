#' Load AMS-II.Q technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiq_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiq-technology-shapes.ttl",
                package = "cdmAmsIIq", mustWork = TRUE)
  )
}

#' Load AMS-II.Q facility type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiq_facility_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiq-facility-shapes.ttl",
                package = "cdmAmsIIq", mustWork = TRUE)
  )
}
