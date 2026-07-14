#' Load AMS-II.C technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iic_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iic-technology-shapes.ttl",
                package = "cdmAmsIIc", mustWork = TRUE)
  )
}
