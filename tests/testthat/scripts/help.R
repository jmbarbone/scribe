#!/usr/bin/Rscript --vanilla

library(scribe)
ca <- command_args()
ca$add_argument("-f", "--foo")
ca$add_argument("-b", "--bar")
# should stop here
ca$parse()
print(ca)
