#' Load ACM0008 technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0008_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0008-technology-shapes.ttl",
                package = "cdmAcm0008", mustWork = TRUE)
  )
}
