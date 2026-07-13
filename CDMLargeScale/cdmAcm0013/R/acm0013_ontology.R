#' Load ACM0013 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0013_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0013-technology-shapes.ttl",
                package = "cdmAcm0013", mustWork = TRUE)
  )
}


#' Load ACM0013 grid connection applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0013_grid_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0013-grid-shapes.ttl",
                package = "cdmAcm0013", mustWork = TRUE)
  )
}
