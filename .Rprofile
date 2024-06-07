
.InstallScribe <- function() {
  devtools::document()
  pak::pkg_install("local::.", Sys.getenv("R_LIBS_SCRIBE"))
}

if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}
