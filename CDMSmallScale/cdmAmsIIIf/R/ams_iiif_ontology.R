#' Load AMS-III.F technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiif_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiif-technology-shapes.ttl",
                package = "cdmAmsIIIf", mustWork = TRUE)
  )
}

#' Load AMS-III.F waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiif_waste_type_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiif-waste-type-shapes.ttl",
                package = "cdmAmsIIIf", mustWork = TRUE)
  )
}
