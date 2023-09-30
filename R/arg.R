
#' New command argument
#'
#' Make a new [scribeArg] object
#'
#' @param aliases,action,convert,options,default,info,n,stop,execute See
#'   `$initialize()` in [scribeArg].
#' @examples
#' new_arg()
#' new_arg("values", action = "dots")
#' new_arg(c("-f", "--force"), action = "flag")
#' @returns A [scribeArg] object
#' @family scribe
#' @export
new_arg <- function(
    aliases = "",
    action  = arg_actions(),
    default = NULL,
    convert = scribe_convert(),
    n       = NA_integer_,
    info    = NULL,
    options = list(),
    stop    = c("none", "hard", "soft"),
    execute = invisible
) {
  scribeArg$new(
    aliases = aliases,
    action  = action,
    default = default,
    convert = convert,
    n       = n,
    info    = info,
    options = options,
    stop    = stop,
    execute = execute
  )
}

scribe_help_arg <- function() {
  new_arg(
    aliases = "--help",
    action = "flag",
    default = FALSE,
    n = 0,
    info = "prints this and quietly exits",
    options = list(no = FALSE),
    stop = "hard",
    execute = function(self, ca) {
      if (isTRUE(self$get_value())) {
        ca$help()
        return(exit())
      }

      if (isFALSE(self$get_value())) {
        # remove 'help'
        values <- ca$get_values()
        ca$field("values", values[-match("help", names(values))])
      }
    }
  )
}

scribe_version_arg <- function() {
  new_arg(
    aliases = "--version",
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

# wrappers ----------------------------------------------------------------

arg_initialize <- function( # nolint: cyclocomp_linter.
  self,
  aliases = "",
  action  = arg_actions(),
  default = NULL,
  convert = scribe_convert(),
  n       = NA_integer_,
  info    = NA_character_,
  options = list(),
  stop    = c("none", "hard", "soft"),
  execute = invisible
) {
  action  <- match.arg(action, arg_actions())
  info    <- info    %||% NA_character_
  options <- options %||% list()

  if (action == "default") {
    if (is_arg(default)) {
      action <- default$get_action()
    }

    # consider other sorts of improvements
    if ("no" %in% names(options) || isTRUE(n == 0L)) {
      action <- "flag"
    }

    if ("choices" %in% names(options) || isTRUE(n > 0)) {
      action <- "list"
    }

    if (action == "default") {
      action <- "list"
    }
  }

  switch(
    action,
    flag = {
      convert <- NULL
      options$no <- options$no %||% getOption("scribe.flag.no")

      if (!is_arg(default)) {
        if (is.null(default)) {
          default <- FALSE
        }

        if (!(is.logical(default) && length(default) == 1)) {
          stop(
            "flag must be NULL, TRUE, FALSE, or NA when action=\"flag\"",
            call. = FALSE
          )
        }
      }

      if (is.na(n)) {
        n <- 0L
      }

      if (n != 0L) {
        stop("n must be 0L when action=\"flag\"", call. = FALSE)
      }

      if (isFALSE(options$no) && is.na(default)) {
        warning(
          "default=NA is not a valid default when option no=FALSE",
          "\nusing to FALSE instead",
          call. = FALSE
        )

        default <- FALSE
      }
    },
    list = {
      # don' assume default choices
      options$choices <- options$choices %||% list()

      if (is.na(n)) {
        n <- 1L
      }

      if (n == 0L) {
        stop("n cannot be 0L when action=\"list\"", call. = FALSE)
      }
    },
    dots = {
      if (identical(aliases, "")) {
        aliases <- "..."
      }

      if (is.na(n)) {
        n <- 0L
      }
    }
  )

  if (!is.null(default)) {
    if (!identical(value_convert(default, to = convert), default)) {
      stop("default value doesn't convert to itself", call. = FALSE)
    }
  }

  stopifnot(
    length(aliases) > 0L,
    length(n) == 1L,
    (is_intish(n) & n >= 0L)
  )

  aliases <- unlist(aliases, recursive = TRUE, use.names = FALSE)

  # determine if either an argument or a command
  dashes <- grepl(ARG_PAT, aliases, ignore.case = TRUE)

  if ("..." %in% aliases) {
    action <- "dots"
    n <- NA_integer_
    positional <- NA
  } else {
    dash_args <- all(dashes)
    pos_args <- all(!dashes)

    if (!xor(dash_args, pos_args)) {
      stop("aliases must have all dashes or none (positional)")
    }

    positional <- !dash_args

    if (action == "flag" && isTRUE(options$no)) {
      dash2 <- grep("^--", aliases)
      if (dash2 && length(dash2)) {
        aliases <- c(aliases, paste0("--no-", sub("^--", "", aliases[dash2])))
      } else if (positional) {
        aliases <- c(aliases, paste0("no-", aliases))
      }
    }
  }

  if (is.logical(stop) && length(stop) == 1L) {
    stop <- if (is.na(stop)) {
      "soft"
    } else if (stop) {
      "hard"
    } else {
      "none"
    }
  }

  stop <- match.arg(stop)
  action <- match.arg(action, arg_actions())

  self$field("aliases", aliases)
  self$field("action", action)
  self$field("convert", scribe_convert(convert))
  self$field("options", options)
  self$field("info", as.character(info))
  self$field("n", as.integer(n))
  self$field("positional", as.logical(positional))
  self$field("default", default)
  self$field("resolved", FALSE)
  self$field("value", NULL)
  self$field("stop", stop)
  self$field("execute", execute)
  invisible(self)
}

arg_show <- function(self) {
  value <- self$get_default()
  value <-
    if (is.null(value)) {
      "<null>"
    } else {
      paste(vapply(value, format, NA_character_), collapse = " ")
    }

  aliases <- self$get_aliases()
  aliases <- to_string(aliases)

  print_line(sprintf("Argument [%s] : %s", aliases, value))
  invisible(self)
}

arg_help <- function(self) {
  h <- self$get_help()
  print_lines(sprintf("[%s] %s", h[1], h[2]))
  invisible(self)
}

arg_get_help <- function(self) {
  switch(
    self$get_action(),
    dots = {
      left <- "..."

      right <- paste0(
        to_string(self$get_aliases()[-1L], sep = ", "),
        if (length(self$get_aliases()) > 1) ": ",
        self$info
      )
    },
    flag = {
      left <- to_string(self$get_aliases(), sep = ", ")
      right <- self$info
    },
    list = {
      left <- paste(
        to_string(self$get_aliases(), sep = ", "),
        sprintf("[%s]", if (self$n == 1) "ARG" else  sprintf("..%i", self$n))
      )

      right <- self$info

      if (isTRUE(is.na(right))) {
        right <- ""
      }

      if (!is_empty(self$options$choices)) {
        right <- paste(
          right,
          sprintf("(%s)", to_string(self$options$choices, sep = ", "))
        )
      }
    }
  )

  right <- paste0(right, collapse = "")
  out <- trimws(c(left, right))
  out[is.na(out)] <- ""
  out
}

arg_get_aliases <- function(self) {
  self$aliases
}

arg_get_name <- function(self, clean = TRUE) {
  aliases <- self$get_aliases()

  if (self$action == "flag" && isTRUE(self$options$no)) {
    if (self$positional) {
      aliases <- grep("^no-", aliases, invert = TRUE, value = TRUE)
    } else {
      aliases <- grep("^--no-", aliases, invert = TRUE, value = TRUE)
    }
  }

  n <- length(aliases)

  if (self$get_action() == "dots" && n == 1L) {
    return("...")
  }

  # always choose the last one
  nm <- aliases[n]

  if (clean) {
    nm <- sub("^--?", "", nm)
  }

  nm
}

arg_get_action  <- function(self) {
  self$action %||% character()
}

arg_get_default <- function(self) {
  if (is_arg(self$default)) {
    self$default$get_value()
  } else {
    self$default
  }
}

arg_get_value <- function(self) {
  if (!self$is_resolved()) {
    warning("scribeArg object has not been resolved", call. = FALSE)
  }

  self$value
}

arg_is_resolved <- function(self) {
  isTRUE(self$resolved)
}

# internal ----------------------------------------------------------------

arg_parse_value <- function(self, ca) { # nolint: cyclocomp_linter.
  default <-
    if (is_arg(self$default)) {
      self$default$get_value()
    } else {
      self$default
    }

  if (ca$stop == "soft") {
    value <- default
    self$field("value", value)
    self$field("resolved", TRUE)
    return(value)
  }

  if (ca$stop == "hard") {
    value <- scribe_empty_value()
    self$field("value", value)
    self$field("resolved", TRUE)
    return(value)
  }

  # find alias in working
  alias <- self$get_aliases()

  if (self$action == "dots") {
    m <- 1L
    off <- 0L
  } else if (self$positional) {
    m <- 0L
    off <- 1L
  } else {
    off <- switch(
      self$action,
      flag = 0L,
      list = 1L
    )
    m <- which(match(ca_get_working(ca), alias, 0L) > 0L)
    ok <- which(m > 0L)
    m <- m[ok]
  }

  if (length(m) == 0L) {
    value <- self$get_default()
  } else {
    if (length(m) > 1L) {
      warning(
        sprintf(
          "w Multiple command arguments matched for [%s]",
          to_string(alias)
        ),
        call = FALSE
      )
    }

    switch(
      self$action,
      dots = {
        value <- ca_get_working(ca)
        ca_remove_working(ca, seq_along(value))

        if (!length(value)) {
          value <- self$get_default()
        }
      },
      list = {
        m <- m + seq.int(0L, self$n)
        value <- ca_get_working(ca)[m[-off]]

        if (self$positional && is.na(value)) {
          value <- self$get_default()
        }

        ca_remove_working(ca, m)
      },
      flag = {
        value <- !grepl("^--?no-", ca_get_working(ca)[m + off])
        ca_remove_working(ca, m)
      }
    )

    ca$field("stop", structure(self$stop, arg = self))
    ca$field("stop", structure(self$stop, arg = self))
  }

  if (self$action == "flag") {
    invisible() # do nothing
  } else if (identical(self$convert, value_convert)) {
    value <- self$convert(value, to = default %||% default_convert)
  } else {
    value <- self$convert(value)
  }

  self$field("value", value)
  self$field("resolved", TRUE)
  value
}

# helpers -----------------------------------------------------------------

is_arg <- function(x) {
  methods::is(x, "scribeArg")
}

ARG_PAT <- "^-[a-z]$|^--[a-z]+$|^--[a-z](+[-]?[a-z]+)+$"  # nolint: object_name_linter, line_length_linter.

arg_actions <- function() {
  c("default", "list", "flag", "dots")
}

scribe_empty_value <- function() {
  structure(list(), class = c("scribe_empty_value"))
}
