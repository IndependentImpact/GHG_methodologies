#' Load ACM0006 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0006_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0006-technology-shapes.ttl",
                package = "cdmAcm0006", mustWork = TRUE)
  )
}
