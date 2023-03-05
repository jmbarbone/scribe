
#' Command args
#'
#' @param x,string Command args; see [base::commandArgs()] for default.  At
#'   least one has to be `NULL`.  When `string` is `NULL`, `x` is used, which
#'   defaults to `commandArgs(trailingOnly = TRUE)`.  Otherwise the value is
#'   converted to a `character`.  If `string` is not `NULL`, [scan()] will be
#'   used to split the value into
#' @param include Special default arguments to
#'   included
#' @returns A `scribeCommandArgs` Reference object
#' @export
#' @family scribe
command_args <- function(
    x = NULL,
    include = c("help", "version", NA_character_),
    string = NULL
) {
  if (is.null(string)) {
    if (is.null(x)) {
      x <- commandArgs(trailingOnly = TRUE)
    }
    x <- as.character(x)
  } else {
    if (!is.null(x)) {
      stop("'string' and 'x' cannot both be set", call. = FALSE)
    }

    string <- as.character(string)
    x <- scan(text = string, what = "character", quiet = TRUE)
  }

  scribeCommandArgs(input = as.character(x), include = include)
}

# ReferenceClass ----------------------------------------------------------

scribeCommandArgs <- methods::setRefClass( # nolint: object_name_linter.
  "scribeCommandArgs",
  fields = list(
    input = "character",
    working = "character",
    values = "list",
    argList = "list",
    nArgs = "integer",
    resolved = "logical",
    description = "character",
    included = "character",
    examples = "character",
    comments = "character"
  )
)

scribeCommandArgs$methods(
  # creates the object
  initialize = function(
    input = "",
    include = c("help", "version", NA_character_)
  ) {
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
    help = NULL,
    arg = NULL
  ) {
    ca_add_argument(
      self = .self,
      arg = arg,
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

  get_args = function(included = TRUE) {
    ca_get_args(.self, included = included)
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

  set_description = function(..., sep = "") {
    ca_set_description(.self, ..., sep = sep)
  },

  add_description = function(..., sep = "") {
    ca_add_description(.self, ..., sep = sep)
  },

  get_description = function() {
    ca_get_description(.self)
  },

  set_example = function(x = character(), comment = "", prefix = "$ ") {
    ca_set_example(
      self = .self,
      x = x,
      comment = comment,
      prefix = prefix
    )
  },

  add_example = function(x, comment = "", prefix = "$ ") {
    ca_add_example(
      self = .self,
      x = x,
      comment = comment,
      prefix = prefix
    )
  },

  get_examples = function() {
    ca_get_examples(.self)
  }
)

# wrappers ----------------------------------------------------------------

ca_initialize <- function(
    self,
    input = NULL,
    include = c("help", "version", NA_character_)
) {
  include <- match.arg(
    as.character(include),
    c("help", "version", NA_character_),
    several.ok = TRUE
  )

  include <- include[!is.na(include)]
  if (!length(include)) {
    include <- character()
  }

  self$input <- input %||% character()
  self$working <- self$input
  self$argList <- list()
  self$nArgs <- 0L
  self$resolved <- FALSE
  self$description <- character()
  self$examples <- character()
  self$comments <- character()
  self$included <- include

  if ("help" %in% include) {
    self$add_argument(
      "--help",
      action = "flag",
      default = FALSE,
      help = "prints this and quietly exits",
      options = list(no = FALSE)
    )
  }

  if ("version" %in% include) {
    self$add_argument(
      "--version",
      action = "flag",
      default = FALSE,
      help = "prints the version of {scribe} and quietly exits",
      options = list(no = FALSE)
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
  file <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(file)) {
    # nocov start
    path <- substr(file, 8, nchar(file))
    bn <- basename(path)
    # nocov end
  } else {
    path <- "{path}"
    bn <- "{command}"
  }

  lines <- sapply(
    self$get_args(),
    function(arg) arg$get_help(),
    simplify = "array"
  )
  lines <- apply(lines, 1L, format) # get consistent width
  lines <- apply(lines, 1L, paste, collapse = " : ") # middle colon

  print_lines(
    "{scribe} command_args",
    "",
    sprintf("file : %s", path),
    "",
    if (length(self$get_description())) {
      c(
        "DESCRIPTION",
        paste0("  ", self$get_description(), collapse = "\n\n"),
        ""
      )
    },
    "USAGE",
    sprintf("  %s [--help | --version]", bn),
    sprintf("  %s %s ", bn, self$write_usage()),
    "",
    "ARGUMENTS",
    paste0("  ", lines),
    if (length(self$get_examples())) {
      examples <- self$get_examples()
      comments <- self$comments
      ok <- comments != ""
      comments[ok] <- paste(" #", comments[ok])
      examples <- as.character(examples)
      c("", "EXAMPLES", paste0("  ", format(examples), comments))
    },
    NULL
  )
}

ca_set_description <- function(self, ..., sep = "") {
  x <- c(...)
  if (is.null(x)) {
    self$description <- NA_character_
    return(invisible(self))
  }
  self$description <- paste0(x, collapse = sep)
  invisible(self)
}

ca_add_description <- function(self, ..., sep = "") {
  x <- paste0(c(...), collapse = sep)

  if (nzchar(x)) {
    self$description <- c(self$description, x)
  }

  invisible(self)
}

ca_get_description <- function(self) {
  self$description
}

ca_set_example <- function(self, x = character(), comment = "", prefix = "$ ") {
  self$examples <- x
  self$comments <- ""
  invisible(self)
}

ca_add_example <- function(self, x = NULL, comment = "", prefix = "$ ") {
  if (is.null(x)) {
    return(invisible(self))
  }

  x <- paste0(prefix, x)
  self$examples <- c(self$examples, x)
  self$comments <- c(self$comments, comment)
  invisible(self)
}

ca_get_examples <- function(self) {
  self$examples
}

ca_write_usage <- function(self) {
  x <- vapply(
    self$get_args(included = FALSE),
    function(arg) arg$get_help()[1],
    NA_character_
  )
  paste(sprintf("[%s]", x), collapse = " ")
}

ca_resolve <- function(self) {
  # loop through the possibly arg in argList.  When found in args, extract and
  # determine what the param should be.  Take into account the action: none
  if (self$resolved) {
    return(invisible(self))
  }

  # reset if not unsuccessful
  on.exit(
    expr =  if (!self$resolved) {
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
      exit()
      return(invisible(self))
    }
    self$values <- self$values[-m]
  }

  if ("version" %in% self$included) {
    m <- match("version", names(self$values), 0L)
    if (self$values$version) {
      self$version()
      exit()
      return(invisible(self))
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
  regmatches(names(values), gregexpr("-", names(values))) <- "_"
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
    help = NULL,
    arg = NULL
) {
  # id starts at 0 because we want to wait for new_arg() to not fail before
  # adding to the counter
  if (is.null(arg)) {
    arg <- new_arg(
      aliases = list(...),
      action = action,
      options = options,
      convert = convert,
      default = default,
      help = help,
      n = as.integer(n)
    )
  }

  stopifnot(is_arg(arg))

  # update arg counter now
  self$arg_counter()
  self$append_arg(arg)
  self$resolved <- FALSE
  invisible(self)
}

ca_get_working <- function(self, i = TRUE) {
  if (length(self$working)) {
    self$working[i]
  } else {
    character()
  }
}

ca_remove_working <- function(self, i) {
  self$working <- self$working[-i]
  self
}

ca_get_args <- function(self, included = TRUE) {
  if (included) {
    return(self$argList)
  }

  nms <- sapply(self$argList, function(arg) arg$get_name())
  ok <- match(nms, self$included, 0L) == 0L
  self$argList[ok]
}

ca_get_input <- function(self) {
  self$input
}

ca_set_input <- function(self, value) {
  self$input <- as.character(value)
  self$working <- self$input
  self
}

ca_get_values <- function(self) {
  self$values
}

ca_set_values <- function(self, i = NULL, value) {
  stopifnot(length(i) == 1)

  if (is.null(value)) {
    return(NULL)
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
