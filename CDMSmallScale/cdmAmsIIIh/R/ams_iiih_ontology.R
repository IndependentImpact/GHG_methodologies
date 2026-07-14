#' Load AMS-III.H technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiih_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiih-technology-shapes.ttl",
                package = "cdmAmsIIIh", mustWork = TRUE)
  )
}

#' Load AMS-III.H waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iiih_waste_type_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iiih-waste-type-shapes.ttl",
                package = "cdmAmsIIIh", mustWork = TRUE)
  )
}
