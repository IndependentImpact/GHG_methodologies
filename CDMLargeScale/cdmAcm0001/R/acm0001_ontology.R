#' Load ACM0001 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0001_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0001-technology-shapes.ttl",
                package = "cdmAcm0001", mustWork = TRUE)
  )
}

#' Load ACM0001 waste type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0001_waste_type_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0001-waste-type-shapes.ttl",
                package = "cdmAcm0001", mustWork = TRUE)
  )
}
