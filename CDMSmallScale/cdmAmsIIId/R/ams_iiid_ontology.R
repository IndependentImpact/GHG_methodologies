#' Load AMS-III.D technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiid_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiid-technology-shapes.ttl",
                package = "cdmAmsIIId", mustWork = TRUE)
  )
}

#' Load AMS-III.D waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiid_waste_type_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiid-waste-type-shapes.ttl",
                package = "cdmAmsIIId", mustWork = TRUE)
  )
}
