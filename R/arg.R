
# TODO Arg as ReferenceClass, too

new_arg <- function(
    id,
    aliases = NULL,
    action = NULL,
    options = NULL,
    type = NULL,
    default = NULL,
    help = NULL,
    n = 0L
) {
  Arg$new(
    id      = id,
    aliases = aliases,
    action  = action,
    options = options,
    type    = type,
    default = default,
    help    = help,
    n       = n
  )
}

# ReferenceClass ----------------------------------------------------------

Arg <- setRefClass(
  "scribeArg",
  fields = list(
    id = "integer",
    aliases = "character",
    action = "character",
    options = "character",
    type = "character",
    default = "character",
    help = "character",
    choices = "list",
    n = "integer",
    mult = "logical"
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
    arg_help()
  },

  do_action = function(value = NULL) {
    arg_do_action(.self, value = value)
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

  get_default = function() {
    arg_get_default(.self)
  },

  match = function(commands) {
    arg_match(.self)
  }
)

# wrappers ----------------------------------------------------------------

arg_initialize <- function(
    self,
    id,
    aliases,
    action,
    type,
    default,
    options,
    n,
    help
) {
  action  <- action  %||% "none"
  type    <- type    %||% "any"
  default <- default %||% first(options) %||% ""
  options <- options %||% ""
  help    <- help    %||% ""
  n       <- n       %||% 1L

  stopifnot(
    is_intish(id),
    length(id) == 1L,
    length(aliases) > 0L,
    length(n) == 1L,
    isTRUE(n) || (is_intish(n) & n >= 0L)
  )

  if (action == "command") {
    if (any(grepl(ARG_PAT, alises, ignore.case = TRUE))) {
      stop("command must ")
    }
  } else if (is.null(aliases)) {
    aliases <- "..."
  } else {
    aliases <- unlist(aliases, recursive = TRUE, use.names = FALSE)
    # TODO add "strict" requirement?
    stopifnot(
      grepl(ARG_PAT, aliases, ignore.case = TRUE),
      !grepl("[[:space:]]", aliases)
    )
  }

  self$id      <- as.integer(id)
  self$aliases <- aliases
  self$action  <- action
  self$type    <- type %||% typeof(default) %||% "any"
  self$default <- default
  self$help    <- help
  self$n       <- if (isTRUE(n)) 0L else n
  self$mult    <- isTRUE(n)
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

  aliases <- to_string(self$get_aliases())
  print_line(sprintf("Argument [%s] : %s", aliases, value))
  invisible(self)
}

arg_help <- function(self) {
  print_lines(
    sprintf("[%s]", to_string(attr(x, "aliases")))
  )
}

arg_do_action <- function(self, ca) {
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

action_validate <- function(action = c("none", "command", "bool", "flag")) {
  match.arg(action %||% "none")
}


arg_get_aliases <- function(self) {
  self$aliases
}

arg_get_name <- function(self) {
  al <- self$get_aliases()
  ind <- grep("^--", al)

  if (length(ind)) {
    al[ind[1L]]
  } else {
    al[1L]
  }
}

arg_get_action  <- function(self) {
  self$action %||% "none"
}

arg_get_default <- function(self) {
  self$default
}

arg_match <- function(self, commands, n = 1L) {
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
