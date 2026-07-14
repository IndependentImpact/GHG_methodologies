#' Load ACM0010 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0010_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0010-technology-shapes.ttl",
                package = "cdmAcm0010", mustWork = TRUE)
  )
}

#' Load ACM0010 waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0010_waste_type_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0010-waste-type-shapes.ttl",
                package = "cdmAcm0010", mustWork = TRUE)
  )
}
