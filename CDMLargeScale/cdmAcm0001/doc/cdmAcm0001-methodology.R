# Generated vignette runner for ACM0001 documentation
tmp <- tempfile(fileext = ".html")
rmarkdown::render(
  input = system.file("doc", "cdmAcm0001-methodology.Rmd", package = "cdmAcm0001"),
  output_file = tmp
)
message("Rendered vignette to ", tmp)
