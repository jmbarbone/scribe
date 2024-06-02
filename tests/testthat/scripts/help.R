#!/usr/bin/Rscript --vanilla

if (!require(
  scribe,
  lib.loc = Sys.getenv("R_LIBS_SCRIBE", "~/R/scribe-library")
)) {
  warning("expected {scribe} to be installed at R_LIBS_SCRIBE")
  library(scribe)
}

ca <- command_args()
ca$add_argument("-f", "--foo")
ca$add_argument("-b", "--bar")
# should stop here
ca$parse()
print(ca)
