
# TODO use `args`, `argsList`, `params` consistently as names

#' Command args
#'
#' @param x Command args; see [base::commandArgs()] for default
#' @param include Special default arguments to included
#' @returns A `scribeCommandArgs` Reference object
#' @export
#' @family scribe
command_args <- function(x = commandArgs(trailingOnly = TRUE), include = c("help", "version")) {
  scribeCommandArgs(input = as.character(x), include = include)
}

# ReferenceClass ----------------------------------------------------------

scribeCommandArgs <- methods::setRefClass( # nolint: object_name_linter.
  "scribeCommandArgs",
  fields = list(
    input = "character",
    working = "character",
    options = "character",
    values = "list",
    argList = "list",
    nArgs = "integer",
    resolved = "logical",
    description = "character",
    included = "character"
  )
)

scribeCommandArgs$methods(
  # creates the object
  initialize = function(input = "", include = c("help", "version")) {
    ca_initialize(.self, input = input, include = include)
  },

  show = function(...) {
    ca_show(.self, ...)
  },

  version = function() {
    print_scribe_version()
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
    convert = default_convert,
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
      convert = convert,
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

  get_input = function() {
    ca_get_input(.self)
  },

  set_input = function(value) {
    ca_set_input(.self, value = value)
  },

  get_options = function(i = TRUE) {
    ca_get_options(.self, i = i)
  },

  set_options = function(i = NULL, value) {
    ca_set_options(.self, i = i, value = value)
  },

  arg_counter = function() {
    ca_arg_counter(.self)
  },

  append_arg = function(arg) {
    ca_append_arg(.self, arg)
  },

  get_n_args = function() {
    ca_get_n_args(.self)
  },

  write_usage = function() {
    ca_write_usage(.self)
  },

  add_description = function(x) {
    ca_add_description(.self, x)
  },

  get_description = function() {
    ca_get_description(.self)
  }
)

# wrappers ----------------------------------------------------------------

ca_initialize <- function(self, input = NULL, include = c("help", "version")) {
  include <- match.arg(include)
  self$input <- input %||% character()
  self$working <- self$input
  self$argList <- list()
  self$nArgs <- 0L
  self$values <- list()
  self$resolved <- FALSE
  self$description <- NA_character_
  self$included <- include

  if ("help" %in% include) {
    self$add_argument(
      "--help",
      action = "flag",
      default = FALSE,
      help = "prints this and quietly exits"
    )
  }

  if ("version" %in% include) {
    self$add_argument(
      "--version",
      action = "flag",
      default = FALSE,
      help = "prints the version of {scribe} and quietly exits"
    )
  }

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

ca_help <- function(self) {
  pwd <- Sys.getenv("PWD")
  lines <- sapply(
    self$get_args(),
    function(arg) arg$get_help(),
    simplify = "array"
  )
  # lines[1, ] <- sprintf("[%s]", lines[1, ])
  lines <- apply(lines, 1L, format) # get consistent width
  lines <- apply(lines, 1L, paste, collapse = " : ") # middle colon

  print_lines(
    "{scribe} command_args",
    "",
    sprintf("file : %s", pwd),
    "",
    if (!is.na(self$get_description())) {
      c("DESCRIPTION", paste0("  ", self$get_description()), "")
    },
    "USAGE",
    sprintf("  %s [--help | --version]", basename(pwd)),
    sprintf("  %s %s ", basename(pwd), self$write_usage()),
    "",
    "ARGUMENTS",
    paste0("  ", lines),
    NULL
  )
}

ca_add_description <- function(self, x) {
  self$description <- x
  self
}

ca_get_description <- function(self) {
  self$description
}

ca_write_usage <- function(self) {
  x <- vapply(self$get_args(), function(arg) arg$get_help()[1], NA_character_)
  paste(sprintf("[%s]", x), collapse = " ")
}

ca_resolve <- function(self) {
  # loop through the possibly arg in argList.  When found in args, extract and
  # determine what the param should be.  Take into account the action: none
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

  arg_order <- unique(
    c(
      wapply(args, function(i) i$action == "flag"),
      wapply(args, function(i) i$action == "list"),
      seq_along(args),
      # dots must always be parsed last
      wapply(args, function(i) i$positional),
      wapply(args, function(i) i$action == "dots")
    ),
    fromLast = TRUE
  )

  arg_names <- vapply(args, function(arg) arg$get_name(), NA_character_)
  self$values <- vector("list", length(arg_order))
  names(self$values) <- arg_names[arg_order]

  for (arg in args[arg_order]) {
    self$set_values(arg$get_name(), arg$parse_value(self))
  }

  if (length(self$get_working())) {
    warning(
      "Not all values parsed:\n",
      to_string(self$get_working()),
      call. = FALSE
    )
  }

  self$values <- self$values[order(arg_order)]
  self$resolved <- TRUE

  if ("help" %in% self$included) {
    m <- match("help", names(self$values), 0L)
    if (self$values$help) {
      self$help()
      quiet_stop()
      return(self)
    }
    self$values <- self$values[-m]
  }

  if ("version" %in% self$included) {
    m <- match("version", names(self$values), 0L)
    if (self$values$version) {
      self$version()
      quiet_stop()
      return(self)
    }
    self$values <- self$values[-m]
  }

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
    convert = default_convert,
    options = NULL,
    nargs = 1L,
    default = NULL,
    help = NULL
) {
  # id starts at 0 because we want to wait for new_arg() to not fail before
  # adding to the counter
  arg <- new_arg(
    id = self$get_n_args(),
    aliases = list(...),
    action = action,
    options = options,
    convert = convert,
    default = default,
    help = help,
    n = as.integer(n)
  )

  # update arg counter now
  self$arg_counter()
  self$append_arg(arg)
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
    i <- match(i, names(self$working))
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

ca_get_input <- function(self) {
  self$input
}

ca_set_input <- function(self, value) {
  self$input <- as.character(value)
  self$working <- self$input
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

ca_arg_counter <- function(self) {
  self$nArgs <- self$get_n_args() + 1L
  self
}

ca_get_n_args <- function(self) {
  self$nArgs
}

ca_append_arg <- function(self, arg) {
  self$argList[[self$get_n_args()]] <- arg
  self
}

# helpers -----------------------------------------------------------------

is_command_args <- function(x) {
  identical(class(x), structure("scribeCommandArgs", package = "scribe"))
}
