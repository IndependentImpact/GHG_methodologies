#' Load AMS-III.H technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiih_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiih-technology-shapes.ttl",
                package = "cdmAmsIIIh", mustWork = TRUE)
  )
}

#' Load AMS-III.H waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiih_waste_type_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiih-waste-type-shapes.ttl",
                package = "cdmAmsIIIh", mustWork = TRUE)
  )
}
