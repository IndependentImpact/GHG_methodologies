#' Load ACM0019 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0019_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0019-technology-shapes.ttl",
                package = "cdmAcm0019", mustWork = TRUE)
  )
}
