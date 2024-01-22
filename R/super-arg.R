# default super helpers.  though these may be the only super helpers.  the
# object is not exported so these should only be used internally.

scribe_help_super <- function() {
  scribeSuperArg$new(
    aliases = "---help",
    action = "flag",
    default = FALSE,
    n = 0,
    info = "prints help information for {scribe} and quietly exits",
    options = list(no = FALSE),
    stop = "hard",
    execute = function(self, ca) {
      if (!self$get_value()) {
        return()
      }

      cat(
        "{scribe} v", format(scribe_version()), "\n",
        "For more information, see https://jmbarbone.github.io/scribe/\n",
        sep = ""
      )
      exit()
    }
  )
}

scribe_version_super <- function() {
  scribeSuperArg$new(
    aliases = "---version",
    action = "flag",
    default = FALSE,
    n = 0,
    info = "prints the version of {scribe} and quietly exits",
    options = list(no = FALSE),
    stop = "hard",
    execute = function(self, ca) {
      if (!isTRUE(self$get_value())) {
        return()
      }

      cat(format(scribe_version()), "\n", sep = "")
      exit()
    }
  )
}

scribe_version <- function() {
  package_version(
    asNamespace("scribe")[[".__NAMESPACE__."]][["spec"]][["version"]]
  )
}
