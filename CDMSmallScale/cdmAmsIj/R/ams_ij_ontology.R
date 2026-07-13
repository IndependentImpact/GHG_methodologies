#' Load AMS-I.J technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_ij_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-ij-technology-shapes.ttl",
                package = "cdmAmsIj", mustWork = TRUE)
  )
}

