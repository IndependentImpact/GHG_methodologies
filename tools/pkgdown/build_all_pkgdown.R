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
  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    stop("The 'pkgbuild' package is required. Install it with install.packages('pkgbuild').", call. = FALSE)
  }
  if (!requireNamespace("withr", quietly = TRUE)) {
    stop("The 'withr' package is required. Install it with install.packages('withr').", call. = FALSE)
  }
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("The 'gert' package is required. Install it with install.packages('gert').", call. = FALSE)
  }
})

source(fs::path("tools", "list_packages.R"))

root <- here::here()
pkg_dirs <- list_repo_packages(root)

if (length(pkg_dirs) == 0) {
  cli::cli_abort("No packages were discovered in the repository.")
}

pkg_info <- purrr::map(pkg_dirs, function(pkg_dir) {
  abs_pkg_dir <- fs::path(root, pkg_dir)
  pkg_name <- read.dcf(fs::path(abs_pkg_dir, "DESCRIPTION"), "Package")[1, 1]

  list(
    name = pkg_name,
    dir = pkg_dir,
    abs_dir = abs_pkg_dir
  )
})

build_index_html <- function(pkg_info) {
  pkg_names <- purrr::map_chr(pkg_info, "name")
  pkg_info <- pkg_info[order(tolower(pkg_names))]
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  items <- purrr::map_chr(pkg_info, function(info) {
    glue::glue(
      "      <li>\n        <a href=\"sites/{info$name}/\">{info$name}</a>\n        <span class=\"package-path\">{info$dir}</span>\n      </li>"
    )
  })

  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "  <meta charset=\"utf-8\" />",
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />",
    "  <title>GHG Methodologies pkgdown sites</title>",
    "  <style>",
    "    body { font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 0; padding: 2rem; background: #f7f7f7; color: #222; }",
    "    main { max-width: 960px; margin: 0 auto; background: white; padding: 2rem; border-radius: 12px; box-shadow: 0 10px 30px rgba(0, 0, 0, 0.08); }",
    "    h1 { font-size: 2.25rem; margin-bottom: 0.5rem; }",
    "    p.meta { color: #555; margin-top: 0; }",
    "    ul { list-style: none; padding: 0; margin: 2rem 0 0; display: grid; grid-template-columns: repeat(auto-fill, minmax(260px, 1fr)); gap: 1rem; }",
    "    li { background: #fafafa; border: 1px solid #e5e5e5; border-radius: 10px; padding: 1rem 1.25rem; transition: transform 0.2s ease, box-shadow 0.2s ease; }",
    "    li:hover { transform: translateY(-3px); box-shadow: 0 12px 20px rgba(0, 0, 0, 0.08); }",
    "    a { color: #005a9c; font-weight: 600; text-decoration: none; display: block; margin-bottom: 0.5rem; }",
    "    a:hover, a:focus { text-decoration: underline; }",
    "    .package-path { display: inline-block; font-family: 'Fira Code', 'Source Code Pro', monospace; font-size: 0.875rem; color: #555; background: #eef2f7; padding: 0.15rem 0.5rem; border-radius: 6px; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <main>",
    "    <h1>GHG Methodologies pkgdown sites</h1>",
    glue::glue("    <p class=\"meta\">Last updated {now}. Links point to each package's pkgdown site on GitHub Pages.</p>"),
    "    <ul>",
    items,
    "    </ul>",
    "  </main>",
    "</body>",
    "</html>"
  )
}

update_pkgdown_index <- function(root, pkg_info) {
  cli::cli_inform("Updating pkgdown index page")

  clone_dir <- fs::path(tempdir(), "gh-pages-index")

  if (fs::dir_exists(clone_dir)) {
    fs::dir_delete(clone_dir)
  }

  repo <- tryCatch(
    gert::git_clone(root, path = clone_dir, branch = "gh-pages"),
    error = function(err) {
      cli::cli_warn("Skipping index update because the 'gh-pages' branch could not be cloned: {err$message}")
      return(NULL)
    }
  )

  if (is.null(repo)) {
    return(invisible(NULL))
  }

  index_html <- build_index_html(pkg_info)
  index_path <- fs::path(clone_dir, "index.html")
  writeLines(index_html, index_path)

  if (nrow(gert::git_status(repo = clone_dir)) == 0) {
    cli::cli_inform("Index page already up to date")
    return(invisible(NULL))
  }

  gert::git_add(repo = clone_dir, files = "index.html")

  committed <- tryCatch(
    {
      gert::git_commit(repo = clone_dir, message = "Update pkgdown index")
      TRUE
    },
    error = function(err) {
      cli::cli_warn("Unable to commit index update: {err$message}")
      FALSE
    }
  )

  if (!committed) {
    return(invisible(NULL))
  }

  tryCatch(
    gert::git_push(repo = clone_dir),
    error = function(err) {
      cli::cli_warn("Failed to push index update: {err$message}")
    }
  )
}

purrr::walk(pkg_info, function(info) {
  abs_pkg_dir <- info$abs_dir
  pkg_dir <- info$dir
  pkg_name <- info$name
  cli::cli_inform("Building source package for {pkg_name} ({pkg_dir})")

  pkg_tarball <- pkgbuild::build(
    path = abs_pkg_dir,
    dest_path = tempdir(),
    quiet = TRUE
  )

  temp_lib <- fs::path(tempdir(), glue::glue("lib-{pkg_name}"))

  if (fs::dir_exists(temp_lib)) {
    fs::dir_delete(temp_lib)
  }
  fs::dir_create(temp_lib)

  utils::install.packages(
    pkg_tarball,
    repos = NULL,
    type = "source",
    lib = temp_lib,
    quiet = TRUE
  )

  cli::cli_inform("Building pkgdown site for {pkg_name} ({pkg_dir})")

  local_site_dir <- fs::path(tempdir(), glue::glue("pkgdown-site-{pkg_name}"))

  if (fs::dir_exists(local_site_dir)) {
    fs::dir_delete(local_site_dir)
  }
  fs::dir_create(local_site_dir)

  withr::with_libpaths(temp_lib, action = "prefix", {
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

  if (fs::dir_exists(temp_lib)) {
    fs::dir_delete(temp_lib)
  }

  if (fs::file_exists(pkg_tarball)) {
    fs::file_delete(pkg_tarball)
  }
})

update_pkgdown_index(root, pkg_info)

cli::cli_inform("All pkgdown sites built and deployed.")
