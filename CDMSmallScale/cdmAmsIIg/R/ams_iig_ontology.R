#' Load AMS-II.G technology applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iig_technology_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iig-technology-shapes.ttl",
                package = "cdmAmsIIg", mustWork = TRUE)
  )
}


#' Load AMS-II.G baseline fuel type applicability shape
#' @return A `sh_shape_graph` as returned by [shaclR::read_shacl()].
#' @export
read_ams_iig_baseline_fuel_shapes <- function() {
  shaclR::read_shacl(
    system.file("shacl", "ams-iig-baseline-fuel-shapes.ttl",
                package = "cdmAmsIIg", mustWork = TRUE)
  )
}
