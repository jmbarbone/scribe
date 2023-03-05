
#' New Arg
#'
#' @description
#' Possible options for each action
#'
#' \describe{
#'   \item{`list`}{`choices` Specifies choices}
#'   \item{`flag`}{`no` Includes `--no-foo` as possible flag value}
#' }
#'
#' @param aliases A list of aliases for the arg
#' @param action An action to perform
#' @param options A list of named options for actions
#' @param convert A conversion specifications, passed to the `to` param in
#'   [value_convert()].
#' @param default Default value
#' @param help Help text for this argument
#' @param n The number of values
#' @returns A `scribeArg` object
#' @noRd
new_arg <- function(
    aliases = "",
    action  = arg_actions(),
    convert = default_convert,
    options = list(),
    default = NULL,
    help    = NULL,
    n       = NA_integer_
) {
  scribeArg$new(
    aliases = aliases,
    action  = action,
    options = options,
    convert = convert,
    default = default,
    help    = help,
    n       = n
  )
}

# ReferenceClass ----------------------------------------------------------

scribeArg <- methods::setRefClass( # nolint: object_name_linter.
  "scribeArg",
  fields       = list(
    aliases    = "character",
    action     = "character",
    options    = "list",
    convert    = "ANY",
    default    = "ANY",
    info       = "character",
    n          = "integer",
    positional = "logical"
  )
)

scribeArg$methods(
  initialize = function(
    aliases  = NULL,
    action   = NULL,
    options  = NULL,
    convert  = NULL,
    default  = NULL,
    help     = NULL,
    n        = NULL
  ) {
    arg_initialize(
      .self,
      aliases = aliases,
      action  = action,
      options = options,
      convert = convert,
      default = default,
      help    = help,
      n       = n
    )
  },

  show = function() {
    arg_show(.self)
  },

  help = function() {
    # should return a matrix
    arg_help(.self)
  },

  get_help = function() {
    arg_get_help(.self)
  },

  get_aliases = function() {
    arg_get_aliases(.self)
  },

  get_name = function(clean = TRUE) {
    arg_get_name(.self, clean = clean)
  },

  get_action = function() {
    arg_get_action(.self)
  },

  parse_value = function(command_arg) {
    arg_parse_value(.self, ca = command_arg)
  },

  get_default = function() {
    arg_get_default(.self)
  }
)

# wrappers ----------------------------------------------------------------

arg_initialize <- function( # nolint: cyclocomp_linter.
    self,
    aliases,
    action = arg_actions(),
    convert = default_convert,
    default = NULL,
    # acceptable values
    options = list(),
    n = NA_integer_,
    help = NA_character_
) {
  action  <- match.arg(action, arg_actions())
  help    <- help    %||% NA_character_
  options <- options %||% list()

  if (action == "default") {
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
      options$no <- options$no %||% TRUE

      if (!(isFALSE(default) | is.null(default))) {
        warning("flag must be NULL or TRUE when action=\"flag\"", call. = FALSE)
      }
      default <- FALSE

      if (is.na(n)) {
        n <- 0L
      }

      if (n != 0L) {
        stop("n must be 0L when action=\"flag\"", call. = FALSE)
      }
    },
    list = {
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

  action <- match.arg(action, arg_actions())

  self$aliases    <- aliases
  self$action     <- action
  self$convert    <- convert
  self$options    <- options
  self$info       <- as.character(help)
  self$n          <- as.integer(n)
  self$positional <- as.logical(positional)
  self$default    <- default
  self
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
  self$default
}

arg_parse_value <- function(self, ca) {
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
    m <- which(match(ca$get_working(), alias, 0L) > 0L)
    ok <- which(m > 0L)
    m <- m[ok]
  }

  if (length(m) == 0L) {
    return(self$get_default())
  }

  if (length(m) > 1L) {
    warning(
      sprintf("w Multiple command args matched for [%s]", to_string(alias)),
      call = FALSE
    )
  }

  switch(
    self$action,
    dots = {
      value <- ca$get_working()
      ca$remove_working(seq_along(value))

      if (!length(value)) {
        value <- self$get_default()
      }
    },
    list = {
      m <- m + seq.int(0L, self$n)
      value <- ca$get_working(m[-off])
      ca$remove_working(m)
    },
    flag = {
      value <- !grepl("^--?no-", ca$get_working()[m + off])
      ca$remove_working(m)
    }
  )

  value <- value_convert(value, to = self$default %||% self$convert)
  value
}

# helpers -----------------------------------------------------------------

is_arg <- function(x) {
  methods::is(x, scribeArg)
}

ARG_PAT <- "^-[a-z]$|^--[a-z]+$|^--[a-z](+[-]?[a-z]+)+$"  # nolint: object_name_linter, line_length_linter.

arg_actions <- function() {
  c("default", "list", "flag", "dots")
}
