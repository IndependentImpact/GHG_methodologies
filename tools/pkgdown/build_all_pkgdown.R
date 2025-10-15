#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("The 'here' package is required. Install it with install.packages('here').", call. = FALSE)
  }
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required. Install it with install.packages('fs').", call. = FALSE)
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("The 'purrr' package is required. Install it with install.packages('purrr').", call. = FALSE)
  }
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("The 'pkgdown' package is required. Install it with install.packages('pkgdown').", call. = FALSE)
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("The 'glue' package is required. Install it with install.packages('glue').", call. = FALSE)
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required. Install it with install.packages('cli').", call. = FALSE)
  }
})

source(fs::path("tools", "list_packages.R"))

root <- here::here()
pkg_dirs <- list_repo_packages(root)

if (length(pkg_dirs) == 0) {
  cli::cli_abort("No packages were discovered in the repository.")
}

purrr::walk(pkg_dirs, function(pkg_dir) {
  abs_pkg_dir <- fs::path(root, pkg_dir)
  pkg_name <- read.dcf(fs::path(abs_pkg_dir, "DESCRIPTION"), "Package")[1, 1]
  cli::cli_inform("Building pkgdown site for {pkg_name} ({pkg_dir})")

  local_site_dir <- fs::path(tempdir(), glue::glue("pkgdown-site-{pkg_name}"))

  if (fs::dir_exists(local_site_dir)) {
    fs::dir_delete(local_site_dir)
  }
  fs::dir_create(local_site_dir)

  pkgdown::build_site(
    pkg = abs_pkg_dir,
    override = list(destination = local_site_dir),
    preview = FALSE
  )

  pkgdown::deploy_to_branch(
    pkg = abs_pkg_dir,
    branch = "gh-pages",
    subdir = fs::path("sites", pkg_name),
    clean = FALSE,
    commit_message = glue::glue("Build pkgdown for {pkg_name}")
  )
})

cli::cli_inform("All pkgdown sites built and deployed.")
