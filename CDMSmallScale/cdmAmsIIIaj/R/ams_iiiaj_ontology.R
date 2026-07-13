#' Load AMS-III.AJ technology applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiiaj_technology_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiiaj-technology-shapes.ttl",
                package = "cdmAmsIIIaj", mustWork = TRUE)
  )
}

#' Load AMS-III.AJ waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shapeR::read_shacl()].
#' @export
read_ams_iiiaj_waste_type_shapes <- function() {
  shapeR::read_shacl(
    system.file("shacl", "ams-iiiaj-waste-type-shapes.ttl",
                package = "cdmAmsIIIaj", mustWork = TRUE)
  )
}
