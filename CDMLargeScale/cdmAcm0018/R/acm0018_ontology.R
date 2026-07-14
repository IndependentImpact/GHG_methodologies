#' Load ACM0018 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0018_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0018-technology-shapes.ttl",
                package = "cdmAcm0018", mustWork = TRUE)
  )
}


#' Load ACM0018 grid connection applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0018_grid_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0018-grid-shapes.ttl",
                package = "cdmAcm0018", mustWork = TRUE)
  )
}
