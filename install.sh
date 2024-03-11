#! /usr/bin/env Rscript
r_libs_scribe <- Sys.getenv("R_LIBS_SCRIBE", "~/R/scribe-library")
dir.create(r_libs_scribe, recursive = TRUE, showWarnings = FALSE)
devtools::document()
pak::local_install_dev_deps(lib = r_libs_scribe, dependencies = NA)
