#!/usr/bin/env Rscript

lib_scribe <- Sys.getenv("R_LIBS_SCRIBE", "~/R/scribe-library")
lib_scribe <- normalizePath(lib_scribe, "/", FALSE)
dir.create(lib_scribe, recursive = TRUE, showWarnings = FALSE)

if (isFALSE(tryCatch(
  identical(find.package("scribe", lib_scribe), lib_scribe),
  packageNotFoundError = function(e) FALSE
))) {
  warning(
    "The 'scribe' package is not installed in",
    " an expected location:\n",
    "R_LIBS_SCRIBE   : ", Sys.getenv("R_LIBS_SCRIBE"), "\n",
    "library searched: ", lib_scribe,
    "\n",
    "Please install in one of these suggested locations.",
    call. = FALSE
  )
  if (!require(
    scribe,
    lib.loc = lib_scribe,
    include.only = "command_args"
  )) {
    stop(
      "The 'scribe' package is not installed. ",
      "Please install with: \n",
      sprintf(
        "Rscript -e \"utils::install.packages('scribe', '%s', '%s')\"",
        lib_scribe,
        getOption("repos")
      ),
      call. = FALSE
    )
  }
} else {
  library(scribe, lib.loc = lib_scribe, include.only = "command_args")
}

ca <- command_args()
ca$add_description(
  "Update the {scribe} package",
  "This script updates the {scribe} package"
)
ca$add_argument(
  "--dev",
  action = "flag",
  info = "Update the {scribe} package from GitHub"
)
args <- ca$parse()

system2(
  system.file("scripts", "scribe.R", package = "scribe"),
  args = if (args$dev) "--update-dev" else "--install",
)
