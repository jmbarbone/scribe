scribe_super_help_arg <- function() {
  new_arg(
    aliases = "---help",
    action = "flag",
    default = FALSE,
    n = 0,
    info = "prints help information for {scribe} and quietly exits",
    options = list(no = FALSE),
    stop = "hard",
    execute = function(self, ca) {
      # TODO include more information
      if (isTRUE(self$get_value())) {
        cat(
          "{scribe} v ", scribe_version,
          "For more information, see https://jmbarbone.github.io/scribe/"
        )
        return(exit())
      }
    }
  )
}

scribe_super_version_arg <- function() {
  new_arg(
    aliases = "---version",
    action = "flag",
    default = FALSE,
    n = 0,
    info = "prints the version of {scribe} and quietly exits",
    options = list(no = FALSE),
    stop = "hard",
    execute = function(self, ca) {
      if (isTRUE(self$get_value())) {
        ca$version()
        return(exit())
      }

      if (isFALSE(self$get_value())) {
        # remove 'version'
        values <- ca$get_values()
        ca$field("values", values[-match("version", names(values))])
      }
    }
  )
}

scribe_version <- function() {
  format(utils::packageVersion("scribe"))
}
