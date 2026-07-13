#' Load AMS-III.G technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiig_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiig-technology-shapes.ttl",
                package = "cdmAmsIIIg", mustWork = TRUE)
  )
}

#' Load AMS-III.G waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiig_waste_type_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiig-waste-type-shapes.ttl",
                package = "cdmAmsIIIg", mustWork = TRUE)
  )
}
