
# TODO Arg as ReferenceClass, too

#' New Arg
#'
#' @param id Integer
#' @param aliases A list of aliases for the arg
#' @param action An action to perform
#' @param options If not `NULL`,  a `list` of set possible values
#' @param type The type of the value.  Accepts either single characters passed
#'   to `as.vector(mode = type)` or a function to convert values.  When `NULL`:
#'   if `default` is not `NULL`, uses the value of `typeof(default)`, otherwise
#'   uses `"any"`
#' @param default Default value
#' @param help Help text for this argument
#' @param n The number of values
#' @returns A `scribeArg` object
#' @noRd
new_arg <- function(
    aliases = NULL,
    id = NA_integer_,
    action = arg_actions(),
    type = arg_types(),
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
    type    = type,
    default = default,
    help    = help,
    n       = n
  )
}

# ReferenceClass ----------------------------------------------------------

Arg <- methods::setRefClass( # nolint: object_name_linter
  "scribeArg",
  fields = list(
    id = "integer",
    aliases = "character",
    action = "character",
    options = "character",
    type = "ANY",
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
    type = NULL,
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
      type    = type,
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
    type = arg_types(),
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

  if (is.character(type)) {
    type <- match.arg(tolower(type), arg_types())
  } else if (!is.function(type)) {
    stop("type must be a character or a function")
  }

  if (!is.null(default)) {
    default_type <- c(class(default), "default")
    default_type <- match.arg(
      arg = tolower(default_type),
      choices = arg_types(),
      # multiple matches but use the first found
      several.ok = TRUE
    )[1]

    if (!is.function(type)) {
      # if it's a function, then whatever
      if (identical(type, "default")) {
        # if it's "default" we'll update this
        type <- default_type
      } else if (!identical(default_type, type)) {
        # if they don't match, then error
        msg <- "type and default supplied don't appear to be compatable"
        stop(msg, call. = FALSE)
      }
    }
  }

  if (action == "default") {
    # TODO need to determine when actions can be anything other than list
    action <- "list"
  }

  switch(
    action,
    flag = {
      type <- "logical"
      default <- as.logical(default)

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
  self$type    <- type %||% typeof(default) %||% "any"
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
    sprintf("[%s]", to_string(self$get_aliases()))
  )
}

# nolint start
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
# nolint end

scribe_actions <- function() {
  c("character", "numeric", "bool", "flag")
}

action_validate <- function(action = NULL) {
  match.arg(action %||% "none", scribe_actions())
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

  if (is.character(self$type)) {
    value <- switch(
      self$type,
      any = value,
      character = as.character(value),
      logical = as.logical(value),
      integer = as.integer(value),
      numeric = as.numeric(value),
      complex = as.complex(value),
      raw = charToRaw(value),
      stop("something has gone wrong")
    )
  } else if (is.function(self$type)) {
    value <- (self$type)(value)
  }

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
  methods::is(x, Arg)
}

ARG_PAT <- "^-[a-z]$|^--[a-z]+$|^--[a-z](+[-]?[a-z]+)+$"  # nolint: object_name_linter

is_command <- function(x) {
  stopifnot(is.list(x))
  vapply(x, function(i) i$get_action(), NA_character_) == "command"
}

arg_types <- function() {
  c("default", "any", "logical", "integer",
    "numeric", "double", "complex", "character", "raw")
}

arg_actions <- function() {
  c("default", "list", "flag", "dots")
}
