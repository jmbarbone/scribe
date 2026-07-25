#!/usr/bin/env -S Rscript

options(scribe.lib_check = FALSE)
library(scribe, lib = Sys.getenv("R_LIBS_SCRIBE_TEST"))

ca <- command_args()
ca$add_argument("-f", "--foo")
ca$add_argument("-b", "--bar")
# should stop here
ca$parse()
print(ca)
