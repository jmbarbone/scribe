#!/usr/bin/env Rscript

ca <- commandArgs(TRUE)

lib_scribe <- Sys.getenv("R_LIBS_SCRIBE", "~/R/scribe-library")
lib_scribe <- normalizePath(lib_scribe, "/", FALSE)
dir.create(lib_scribe, recursive = TRUE, showWarnings = FALSE)

op <- options(repos = "https://cloud.r-project.org")

if ("---install" %in% ca) {
  utils::install.packages("scribe", lib = lib_scribe)
  quit("no")
}

if ("---update-dev" %in% ca) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    utils::install.packages("remotes", lib = lib_scribe)
  }
  remotes::install_github("jmbarbone/scribe", lib = lib_scribe)
  quit("no")
}

if (!requireNamespace("scribe", quietly = TRUE, lib.loc = lib_scribe)) {
  stop(
    "The 'scribe' package is not installed. ",
    "Please install it with --install",
    call. = FALSE
  )
}

options(op)
library(scribe, lib.loc = lib_scribe)
ca <- command_args()

ca$add_argument(
  "package",
  info = "Package name"
)
ca$add_argument("...", info = "Arguments to pass to the package function")
args <- ca$parse()
