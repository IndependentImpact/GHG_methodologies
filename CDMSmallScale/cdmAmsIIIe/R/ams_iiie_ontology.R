#' Load AMS-III.E technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiie_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiie-technology-shapes.ttl",
                package = "cdmAmsIIIe", mustWork = TRUE)
  )
}

#' Load AMS-III.E waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiie_waste_type_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiie-waste-type-shapes.ttl",
                package = "cdmAmsIIIe", mustWork = TRUE)
  )
}
