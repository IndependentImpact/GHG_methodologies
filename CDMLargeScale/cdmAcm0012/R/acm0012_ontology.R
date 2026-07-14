#' Load ACM0012 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0012_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0012-technology-shapes.ttl",
                package = "cdmAcm0012", mustWork = TRUE)
  )
}
