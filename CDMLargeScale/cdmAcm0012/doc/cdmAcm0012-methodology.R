# Generated vignette runner for ACM0012 documentation
tmp <- tempfile(fileext = ".html")
rmarkdown::render(
  input = system.file("doc", "cdmAcm0012-methodology.Rmd", package = "cdmAcm0012"),
  output_file = tmp
)
message("Rendered vignette to ", tmp)
