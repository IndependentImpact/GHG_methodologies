#' Load ACM0009 baseline fuel applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_acm0009_baseline_fuel_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "acm0009-baseline-fuel-shapes.ttl",
                package = "cdmAcm0009", mustWork = TRUE)
  )
}
