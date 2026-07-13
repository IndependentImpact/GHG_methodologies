#' Load AMS-II.H technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iih_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iih-technology-shapes.ttl",
                package = "cdmAmsIIh", mustWork = TRUE)
  )
}

#' Load AMS-II.H facility type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iih_facility_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iih-facility-shapes.ttl",
                package = "cdmAmsIIh", mustWork = TRUE)
  )
}
