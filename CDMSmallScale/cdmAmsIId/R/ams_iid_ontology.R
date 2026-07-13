#' Load AMS-II.D technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iid_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iid-technology-shapes.ttl",
                package = "cdmAmsIId", mustWork = TRUE)
  )
}

#' Load AMS-II.D facility type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iid_facility_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iid-facility-shapes.ttl",
                package = "cdmAmsIId", mustWork = TRUE)
  )
}
