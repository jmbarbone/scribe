
#' Command line arguments
#'
#' Make a new [scribeCommandArgs] object
#'
#' @param x,string Command line arguments; see [base::commandArgs()] for
#'   default.  At least one parameter has to be `NULL`.  When `string` is
#'   `NULL`, `x` is used, which defaults to `commandArgs(trailingOnly = TRUE)`.
#'   Otherwise the value of `x` is converted to a `character`.  If `string` is
#'   not `NULL`, [scan()] will be used to split the value into a `character`
#'   vector.
#' @param include Special default arguments to included
#' @returns A [scribeCommandArgs] object
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


#' {scribe} command arguments
#'
#' Reference class object for managing command line arguments.
#'
#' @field input `[character]`\cr A character vector of command line arguments.
#'   See also [command_args()]
#' @field working `[character]`\cr A copy of `input`.  Note: this is used to
#'   track parsing progress and is not meant to be accessed directly.
#' @field values `[list]`\cr A named `list` of values.  Empty on initialization
#'   and populated during argument resolving.
#' @field argList `[list]`\cr a List of [scribeArg]s
#' @field nArgs `[interger]`\cr A current counter for the number of [scribeArg]s
#'   included
#' @field resolved `[logical]`\cr A `logical` value indicated if the `resolve()`
#'   method has been successfully executed.
#' @field description `[character]`\cr Additional help information
#' @field included `[character]`\cr Default [scribeArg]s to include
#' @field examples `[character]`\cr Examples to print with help
#' @field comments `[character]`\cr Comments printed with
#'
#' @export
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
    "
    Initialize the \\link{scribeCommandArgs} object

    See \\strong{fields} for parameter information.
    "
    ca_initialize(.self, input = input, include = include)
  },

  show = function(...) {
    "Print the \\link{scribeCommandArgs} object"
    ca_show(.self, ...)
  },

  version = function() {
    "Print the \\link{scribe-package} version"
    print_scribe_version()
  },

  help = function() {
    "Print the help information"
    ca_help(.self)
  },

  resolve = function() {
    "Resolve the values of each \\link{scribeArg} in \\code{argList}"
    ca_resolve(.self)
  },

  parse = function() {
    "Return the parsed values of from each \\code{argList}"
    ca_parse(.self)
  },

  add_argument = function(
    ...,
    action = arg_actions(),
    options = NULL,
    convert = default_convert,
    default = NULL,
    n = NA_integer_,
    info = NULL,
    id = NULL
  ) {
    "Add a \\link{scribeArg} to \\code{argList}

    \\describe{
      \\item{\\code{...}}{Aliases}
      \\item{(other)}{See \\code{\\link[=new_arg]{new_arg()}} for details}
    }"
    ca_add_argument(
      self = .self,
      ...,
      action = action,
      options = options,
      convert = convert,
      default = default,
      n = n,
      info = info,
      id = id
    )
  },

  get_args = function(included = TRUE) {
    "Retrieve \\code{argList}"
    ca_get_args(.self, included = included)
  },

  get_working = function() {
    "Retrieve \\code{working}"
    ca_get_working(.self)
  },

  remove_working = function(i) {
    "Retrieve \\code{working}

    \\describe{
      \\item{\\code{i}}{Index value of \\code{working} to remove}
    }"
    ca_remove_working(.self, i = i)
  },

  get_values = function() {
    "Retrieve \\code{values}"
    ca_get_values(.self)
  },

  set_values = function(i = TRUE, value) {
    "Set \\code{values}

    \\describe{
      \\item{\\code{i}}{Index value of \\code{working} to set}
      \\item{\\code{value}}{The value to set}
    }"
    ca_set_values(.self, i = i, value = value)
  },

  get_input = function() {
    "Retrieve \\code{input}"
    ca_get_input(.self)
  },

  set_input = function(value) {
    "Set \\code{input}

    \\describe{
      \\item{\\code{value}}{Value to set}
    }"
    ca_set_input(.self, value = value)
  },

  arg_counter = function() {
    "Increase the value of \\code{nArgs} by \\code{1L}"
    ca_arg_counter(.self)
  },

  append_arg = function(arg) {
    "Append a new \\link{scribeArg} value to \\code{argList}

    \\describe{
      \\item{\\code{arg}}{The \\link{scribeArg} to include}
    }"
    ca_append_arg(.self, arg)
  },

  get_n_args = function() {
    "Retrieve the current number of \\link{scribeArg}s"
    ca_get_n_args(.self)
  },

  write_usage = function() {
    "Write usage information to a \\code{character} vector"
    ca_write_usage(.self)
  },

  set_description = function(..., sep = "") {
    "Set the value of \\code{description}

    \\describe{
      \\item{\\code{...}}{Information to paste into the description}
      \\item{\\code{sep}}{\\code{character} separate for \\code{...}}
    }"
    ca_set_description(.self, ..., sep = sep)
  },

  add_description = function(..., sep = "") {
    "Add a value to \\code{description}

    \\describe{
      \\item{\\code{...}}{Information to paste into the description}
      \\item{\\code{sep}}{\\code{character} separate for \\code{...}}
    }"
    ca_add_description(.self, ..., sep = sep)
  },

  get_description = function() {
    "Retrieve \\code{description}"
    ca_get_description(.self)
  },

  set_example = function(x = character(), comment = "", prefix = "$ ") {
    "Set the value of \\code{examples}

    \\describe{
      \\item{\\code{x}}{A code example as a \\code{character}}
      \\item{\\code{comment}}{An optional comment to append}
      \\item{\\code{prefix}}{An optional prefix for the example}
    }"
    ca_set_example(
      self = .self,
      x = x,
      comment = comment,
      prefix = prefix
    )
  },

  add_example = function(x, comment = "", prefix = "$ ") {
    "Add a value to \\code{examples}

    \\describe{
      \\item{\\code{x}}{A code example as a \\code{character}}
      \\item{\\code{comment}}{An optional comment to append}
      \\item{\\code{prefix}}{An optional prefix for the example}
    }"
    ca_add_example(
      self = .self,
      x = x,
      comment = comment,
      prefix = prefix
    )
  },

  get_examples = function() {
    "Retrieve \\code{examples}"
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
      n = 0,
      info = "prints this and quietly exits",
      options = list(no = FALSE)
    )
  }

  if ("version" %in% include) {
    self$add_argument(
      "--version",
      action = "flag",
      default = FALSE,
      n = 0,
      info = "prints the version of {scribe} and quietly exits",
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
    default = NULL,
    info = NULL,
    id = NULL
) {
  # id starts at 0 because we want to wait for new_arg() to not fail before
  # adding to the counter
  arg <- new_arg(
    id = id %||% self$get_n_args(),
    aliases = list(...),
    action = action,
    options = options,
    convert = convert,
    default = default,
    info = info,
    n = n
  )

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
