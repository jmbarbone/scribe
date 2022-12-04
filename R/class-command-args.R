
# TODO use `args`, `argsList`, `params` consistently as names

#' Command args
#'
#' @param x Command args; see [base::commandArgs()] for default
#' @returns A `scribeCommandArgs` Reference object
#' @export
#' @family scribe
command_args <- function(x = commandArgs(trailingOnly = TRUE)) {
  CommandArgs(input = x)
}

# ReferenceClass ----------------------------------------------------------

# nolint start: line_length_linter
# Lorem ipsum dolor sit amet, ornare ex et himenaeos aenean commodo auctor accumsan gravida.
# nolint end: line_length_linter

CommandArgs <- methods::setRefClass( # nolint: object_name_linter
  "scribeCommandArgs",
  fields = list(
    input = "character",
    working = "character",
    options = "character",
    values = "list",
    argList = "list",
    nArgs = "integer",
    resolved = "logical"
  ))

CommandArgs$methods(
  # creates the object
  initialize = function(input = "") {
    ca_initialize(.self, input = input)
  },

  show = function(...) {
    ca_show(.self, ...)
  },

  version = function() {
    print_line("{scribe} package version: ", format(ca_version()))
  },

  help = function() {
    ca_help(.self)
  },

  resolve = function() {
    ca_resolve(.self)
  },

  parse = function() {
    ca_parse(.self)
  },

  add_argument = function(
    ...,
    action = arg_actions(),
    options = NULL,
    type = arg_types(),
    default = NULL,
    n = NA_integer_,
    nargs = 1,
    help = NULL
  ) {
    ca_add_argument(
      self = .self,
      ...,
      action = action,
      options = options,
      type = type,
      default = default,
      n = n,
      nargs = nargs,
      help = help
    )
  },

  add_command = function(
    command,
    options = NULL,
    help = NULL
  ) {
    ca_add_command(
      self = .self,
      options = options,
      help = help
    )
  },

  get_args = function(i = TRUE) {
    ca_get_args(.self, i = i)
  },

  get_working = function(i = TRUE) {
    ca_get_working(.self, i = i)
  },

  remove_working = function(i) {
    ca_remove_working(.self, i = i)
  },

  get_values = function() {
    ca_get_values(.self)
  },

  set_values = function(i = TRUE, value) {
    ca_set_values(.self, i = i, value = value)
  },

  get_input = function(i = TRUE) {
    ca_get_input(.self, i = i)
  },

  set_input = function(i = NULL, value) {
    ca_set_input(.self, i = i, value = value)
  },

  get_options = function(i = TRUE) {
    ca_get_options(.self, i = i)
  },

  set_options = function(i = NULL, value) {
    ca_set_options(.self, i = i, value = value)
  }
)

# wrappers ----------------------------------------------------------------

ca_initialize <- function(self, input = NULL) {
  self$input <- input %||% character()
  self$working <- self$input
  self$argList <- list()
  self$nArgs <- 0L
  self$values <- list()
  self$resolved <- FALSE
  self
}

ca_show <- function(self, ...) {
  print_line("Initial call: ", to_string(self$get_input()))

  if (!self$resolved) {
    print_line("w Call $resolve() or $parse() to resolve arguments")
  }

  lapply(self$get_args(), print)
  invisible(self)
}

ca_version <- function() {
  utils::packageVersion("scribe")
}

ca_help <- function(self) {
  lines <- sapply(self$get_args(), arg_help)
  lines <- apply(lines, 2, format) # get consistent width
  lines <- apply(lines, paste, collapse = " : ") # middle colon
  print_lines(lines)
}

ca_resolve <- function(self) {
  # loop through the possibly arg in argList.  When found in args, extract and
  # determine what the param should be.  Take into account the action: none

  # TODO reserve [-h --help] and [--version]

  if ("--version" %in% self$get_options()) {
    return(self$version())
  }

  if (any(c("-h", "--help") %in% self$get_options())) {
    return(self$help())
  }

  if (self$resolved) {
    return(self)
  }

  # reset if not unsuccessful
  on.exit(
    expr =  if (!self$resolved) {
      self$options <- character()
      self$values <- list()
      self$working <- self$get_input()
    },
    add = TRUE
  )

  # Sort smarter.  Flag argument just need to be present and then can be
  # dropped.  Single value arguments are easier to match, should always have
  # that value present if arg is present. Non-multiple value arguments have at
  # least a limit to the number of values that can be found.
  args <- self$get_args()

  arg_order <- unique(c(
    wapply(args, function(i) i$action == "flag"),
    wapply(args, function(i) i$action == "list"),
    wapply(args, function(i) i$action == "list"),
    seq_along(args),
    # dots must always be parsed last
    wapply(args, function(i) i$action == "dots")
  ))

  arg_names <- vapply(args, function(arg) arg$get_name(), NA_character_)
  self$values <- vector("list", length(arg_order))
  names(self$values) <- arg_names[arg_order]

  for (arg in args[order(arg_order)]) {
    self$set_values(arg$get_name(), arg$parse_value(self))
  }

  if (length(self$get_working())) {
    warning(
      "Not all values parsed:\n",
      to_string(self$get_working()),
      call. = FALSE
    )
  }

  self$resolved <- TRUE
  self
}

ca_parse <- function(self) {
  self$resolve()
  values <- self$get_values()
  # clean up names
  regmatches(names(values), regexpr("^-+", names(values))) <- ""
  regmatches(names(values), gregexpr("_", names(values))) <- "_"
  values
}

ca_add_argument <- function(
    self,
    ...,
    n = NA_integer_,
    action = NULL,
    type = NULL,
    options = NULL,
    nargs = 1,
    default = NULL,
    help = NULL
) {
  arg <- new_arg(
    id = self$nArgs,
    aliases = list(...),
    action = action,
    options = options,
    type = type,
    default = default,
    help = help,
    n = NA_integer_
  )
  self$nArgs <- self$nArgs + 1L
  self$argList[[self$nArgs]] <- arg
  self$resolved <- FALSE
  invisible(self)
}

ca_add_command <- function(self, options = NULL, help = NULL) {
  stop("commandArgs$add_command() is not currently in use")
}

ca_get_working <- function(self, i = TRUE) {
  if (is.null(i)) {
    i <- length(self$get_options()) + 1L
  }

  if (length(self$working)) {
    self$working[i]
  } else {
    character()
  }
}

ca_remove_working <- function(self, i) {
  if (is.character(i)) {
    i <- match(names(self$working))
  }

  self$working <- self$working[-i]
  self
}

ca_get_args <- function(self, i = TRUE) {
  if (isTRUE(i)) {
    self$argList
  } else {
    self$argList[[i]]
  }
}

ca_get_input <- function(self, i = TRUE) {
  self$input[i]
}

ca_set_input <- function(self, i = NULL, value) {
  if (is.null(i)) {
    i <- length(self$get_options()) + 1L
  }

  self$input[i] <- value
  self
}

ca_get_options <- function(self, i = TRUE) {
  self$options[i]
}

ca_set_options <- function(self, i = NULL, value) {
  if (is.null(i)) {
    i <- length(self$get_options()) + 1L
  }

  self$options[i] <- value
  self
}

ca_get_values <- function(self, i = TRUE) {
  if (isTRUE(i)) {
    self$values
  } else {
    self$values[[i]]
  }
}

ca_set_values <- function(self, i = NULL, value) {
  if (is.null(value)) {
    return(NULL)
  }

  if (is.null(i)) {
    i <- length(self$get_values()) + 1L
  }

  self$values[[i]] <- value
  self
}

# helpers -----------------------------------------------------------------

is_command_args <- function(x) {
  identical(class(x), structure("scribeCommandArgs", package = "scribe"))
}
