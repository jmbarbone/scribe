
# TODO Arg as ReferenceClass, too

#' New Arg
#'
#' @param id Integer
#' @param aliases A list of aliases for the arg
#' @param action An action to perform
#' @param options If not `NULL`,  a `list` of set possible values
#' @param convert A conversion specifications, passed to the `to` param in
#'   [value_convert()].
#' @param default Default value
#' @param help Help text for this argument
#' @param n The number of values
#' @returns A `scribeArg` object
#' @noRd
new_arg <- function(
    aliases = NULL,
    id = NA_integer_,
    action = arg_actions(),
    convert = default_convert,
    options = NULL,
    default = NULL,
    help = NULL,
    n = 0L
) {
  Arg$new(
    aliases = aliases,
    id      = id,
    action  = action,
    options = options,
    convert = convert,
    default = default,
    help    = help,
    n       = n
  )
}

# ReferenceClass ----------------------------------------------------------

Arg <- methods::setRefClass(
  "scribeArg",
  fields = list(
    id = "integer",
    aliases = "character",
    action = "character",
    options = "character",
    convert = "ANY",
    default = "ANY",
    help = "character",
    choices = "list",
    n = "integer",
    values = "list"
  )
)

Arg$methods(
  initialize = function(
    id = NULL,
    aliases = NULL,
    action = NULL,
    options = NULL,
    convert = NULL,
    default = NULL,
    help = NULL,
    n = NULL
  ) {
    # TODO include validation
    arg_initialize(
      .self,
      id      = id,
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

  get_value = function(ca, value = NULL) {
    arg_get_value(.self, ca = ca, value = value)
  },

  get_aliases = function() {
    arg_get_aliases(.self)
  },

  get_name = function() {
    arg_get_name(.self)
  },

  get_action = function() {
    arg_get_action(.self)
  },

  parse_value = function(command_arg) {
    arg_parse_value(.self, ca = command_arg)
  },

  get_default = function() {
    arg_get_default(.self)
  },

  match_cmd = function(commands) {
    arg_match_cmd(.self, commands)
  }
)

# wrappers ----------------------------------------------------------------

arg_initialize <- function(
    self,
    id = NA_integer_,
    aliases,
    action = arg_actions(),
    convert = default_convert,
    default = NULL,
    # acceptable values
    options = NULL,
    n = NA_integer_,
    help = ""
) {
  action  <- match.arg(action, arg_actions())
  options <- options %||% ""
  help    <- help    %||% ""
  n       <- n       %||% 1L

  if (action == "default") {
    # TODO need to determine when actions can be anything other than list
    action <- "list"
  }

  switch(
    action,
    flag = {
      convert <- NULL
      default <- as_bool(default)

      if (is.na(n)) {
        n <- 0L
      }

      if (n != 0L) {
        stop("n must be 0L when action=\"flag\"", call. = FALSE)
      }
    },
    list = {
      if (is.na(n)) {
        n <- 1L
      }

      if (n == 0L) {
        stop("n cannot be 0L when action=\"list\"", call. = FALSE)
      }
    }
  )

  if (!is.null(default)) {
    if (!identical(value_convert(default, to = convert), default)) {
      stop("default value doesn't convert to itself", call. = FALSE)
    }
  }

  stopifnot(
    is_intish(id),
    length(id) == 1L,
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
  } else if (!xor(all(dashes), all(!dashes))) {
    stop("aliases must have all dashes or none")
  }

  action <- match.arg(action, arg_actions())

  self$id      <- as.integer(id)
  self$aliases <- aliases
  self$action  <- action
  self$convert <- convert
  self$options <- options
  self$help    <- help
  self$n       <- n
  self$values  <- list()
  self$default <- default
  self
}

arg_show <- function(self) {
  # TODO add print_scribe.arg
  value <- self$get_default()
  value <-
    if (is.null(value)) {
      "<null>"
    } else {
      paste(vapply(value, format, NA_character_), collapse = " ")
    }

  aliases <- self$get_aliases()
  if (is.null(aliases)) {
    aliases <- "..."
  } else {
    aliases <- to_string(aliases)
  }

  print_line(sprintf("Argument [%s] : %s", aliases, value))
  invisible(self)
}

arg_help <- function(self) {
  print_lines(
    sprintf("[%s]", to_string(attr(x, "aliases")))
  )
}

arg_do_action <- function(self, ca, value) {
  # Pass the scribeCommandArg so we can update/remove
  stopifnot(is_command_args(ca))

  switch(
    match.arg(arg$get_action()),
    ## none ----
    none = value %||% arg$get_default(),

    ## flag ----
    # if present, identify as yes/no.
    # TODO account for --no-option
    flag = {
      found <- match(arg$get_aliases(), "")
    },

    command = {
      match.arg(value, arg$get_options(), several.ok = FALSE)
    }
  )
}

scribe_actions <- function() {
  c("character", "numeric", "bool", "flag")
}

action_validate <- function(action = NULL) {
  mach.arg(action %||% "none", scribe_actions())
}

arg_get_aliases <- function(self) {
  self$aliases
}

arg_get_name <- function(self) {
  aliases <- self$get_aliases()

  if (self$action == "dots") {
    ind <- seq_along(aliases)[-1L]
  } else {
    ind <- grep("^--?", aliases)
  }

  # always choose the last one
  aliases[ind[length(ind)]]
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
  } else {
    off <- 1L
    m <- match(alias, ca$get_working(), 0L)
    ok <- which(m > 0L)
    m <- m[ok]
  }

  if (length(m) == 0L) {
    return(self$get_default())
  }

  if (length(m) > 1L) {
    warning(
      sprintf("w Multiple command args matched for [%s]", to_string(alias)),
      "i Using first found",
      call = FALSE
    )
    m <- m[1L]
  }

  if (self$action == "dots") {
    value <- ca$get_working()
    ca$remove_working(seq_along(value))
  } else {
    # TODO consider if multiple n values should be used?
    m <- m + seq.int(0L, self$n)
    value <- ca$get_working(m[-off])
    ca$remove_working(m)
  }

  value <- value_convert(value, to = self$convert)
  value
}

arg_match_cmd <- function(self, commands, n = 1L) {
  m <- match(self$get_aliases(), commands, 0L)
  m <- m[m > 0L]

  if (!isTRUE(n) && length(m) > n) {
    msg <- sprintf(
      "%s matches %i times but should only match %i time(s)",
      self$get_names(),
      length(m),
      n
    )
    stop(msg, call. = FALSE)
  }

  m
}

# helpers -----------------------------------------------------------------

is_arg <- function(x) {
  identical(class9x)
}

ARG_PAT <- "^-[a-z]$|^--[a-z]+$|^--[a-z](+[-]?[a-z]+)+$"

is_command <- function(x) {
  stopifnot(is.list(x))
  vapply(x, function(i) i$get_action(), NA_character_) == "command"
}

arg_actions <- function() {
  c("default", "list", "flag", "dots")
}
