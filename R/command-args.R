
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
#' @param include Special default arguments to included.  See `$initialize()` in
#'   [scribeCommandArgs] for more details.
#' @param super When `TRUE` the [scribeCommandArgs] object will be initialized
#'   with standard _super_ arguments (e.g., `---help`, `---version`)
#' @examples
#' command_args()
#' command_args(c("-a", 1, "-b", 2))
#' command_args(string = "-a 1 -b 2")
#' @returns A [scribeCommandArgs] object
#' @family scribe
#' @export
command_args <- function(
    x = NULL,
    include = getOption("scribe.include", c("help", "version", NA_character_)),
    string = NULL,
    super = include
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

  scribeCommandArgs(input = x, include = include, super = super)
}

# wrappers ----------------------------------------------------------------

ca_initialize <- function(
    self,
    input = "",
    include = c("help", "version", NA_character_),
    supers = include
) {
  # default values
  self$initFields(
    args = list(),
    resolved = FALSE,
    description = character(),
    examples = character(),
    comments = character()
  )

  include <- match.arg(
    as.character(include),
    c("help", "version", NA_character_),
    several.ok = TRUE
  )

  supers <- match.arg(
    as.character(supers),
    c("help", "version", NA_character_),
    several.ok = TRUE
  )

  include <- include[!is.na(include)]
  if (!length(include)) {
    include <- character()
  }

  if ("help" %in% include) {
    self$add_argument(scribe_help_arg())
  }

  if ("version" %in% include) {
    self$add_argument(scribe_version_arg())
  }

  self$field("supers", c(
    if ("help" %in% supers) scribe_help_super(),
    if ("version" %in% supers) scribe_version_super()
  ) %||% list())

  self$field("input", as.character(input) %||% character())
  self$field("working", self$input)
  self$field("included", include)
  self$field("stop", "none")
  invisible(self)
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

  # TODO need to get an indent for the left side
  args <- sapply(self$get_args(), function(arg) arg$get_help())
  args <- simplify2array(args)
  args <- apply(args, 2L, function(row) two_column(row[1L], row[2L]))
  args <- lapply(args, function(line) matrix(unlist(line), ncol = 2L))
  args <- Reduce(rbind, args)
  args <- apply(args, 2L, format)
  args <- apply(args, 1L, paste, sep = "\n", collapse = " ")

  usage <- two_column(bn, ca_write_usage(self))
  usage <- apply(Reduce(cbind, usage), 1L, paste, collapse = " ")

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
    usage,
    "",
    "ARGUMENTS",
    args,
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

  invisible(self)
}

two_column <- function(x, y, sep = "") {
  # TODO need to implement a width argument
  right <- strwrap(y, 0.8 * getOption("width"))
  left <- character(length(right))
  left[1L] <- paste0("  ", x, sep)
  left <- format(left)
  list(left, right)
}

ca_resolve <- function(self) {
  # loop through the possibly arg in args.  When found in args, extract and
  # determine what the param should be.  Take into account the action: none
  if (self$resolved) {
    return(invisible(self))
  }

  # reset if not unsuccessful
  on.exit(
    expr =  if (!self$resolved) {
      self$field("working", self$get_input())
      self$field("stop", "none")
    },
    add = TRUE
  )

  # Sort smarter.  Flag argument just need to be present and then can be
  # dropped.  Single value arguments are easier to match, should always have
  # that value present if arg is present. Non-multiple value arguments have at
  # least a limit to the number of values that can be found.
  args <- self$get_args(included = TRUE, super = TRUE)

  arg_order <- unique(
    c(
      wapply(args, function(i) i$action == "flag"),
      wapply(args, function(i) i$action == "list"),
      seq_along(args),
      # dots must always be parsed last
      wapply(args, function(i) i$positional),
      wapply(args, function(i) i$action == "dots"),
      NULL
    ),
    fromLast = TRUE
  )

  # move stops earlier
  arg_order <- unique(c(
    wapply(args, function(i) inherits(i, "scribeSuperArg")),
    wapply(args, function(i) i$stop == "hard"),
    wapply(args, function(i) i$stop == "soft"),
    arg_order
  ))

  for (arg in args[arg_order]) {
    arg_parse_value(arg, self)
  }

  self$field("values", structure(
    lapply(args, function(arg) arg$get_value()),
    names = vapply(args, function(arg) arg$get_name(), NA_character_)
  ))

  if (length(ca_get_working(self)) && self$stop == "none") {
    warning(
      "Not all values parsed:\n",
      to_string(ca_get_working(self)),
      call. = FALSE
    )
  }

  self$field("resolved", TRUE)
  invisible(self)
}

ca_parse <- function(self) {
  self$resolve()

  for (arg in self$get_args(super = TRUE, included = TRUE)) {
    ca_do_execute(self, arg)
  }

  # clean up names
  res <- self$get_values()
  regmatches(names(res), regexpr("^-+", names(res))) <- ""
  regmatches(names(res), gregexpr("-", names(res))) <- "_"
  res
}

ca_get_input <- function(self) {
  self$input
}

ca_set_input <- function(self, value) {
  self$field("input", as.character(value))
  self$field("working", self$input)
  self$field("resolved", FALSE)
  invisible(self)
}

ca_get_values <- function(
    self,
    empty = FALSE,
    super = FALSE,
    included = FALSE
) {
  values <- self$values

  if (!included) {
    # in the event that an arg uses a name like 'version', this will only remove
    # the first instance -- which should be the 'included' args.  Should
    # probably be searching by class instead.
    m <- match(self$included, names(values), 0L)
    ok <- which(m > 0L)
    if (length(ok)) {
      values <- values[-m[ok]]
    }
  }

  if (!super && !is.null(names(values))) {
    values <- values[!startsWith(names(values), "_")]
  }

  if (!empty) {
    values <- values[vapply(values, Negate(inherits), NA, "scribe_empty_value")]
  }

  values
}

ca_set_values <- function(self, i = TRUE, value) {
  stopifnot(length(i) == 1)

  if (is.null(value)) {
    return(invisible(self))
  }

  self$field("values", replace2(self$values, i, value))
  invisible(self)
}

ca_get_args <- function(self, included = TRUE, super = FALSE) {
  args <- self$args

  if (!included) {
    nms <- sapply(args, function(arg) arg$get_name())
    ok <- match(nms, self$included, 0L) == 0L
    args <- args[ok]
  }

  if (super) {
    args <- c(self$supers, args)
  }

  args
}

ca_add_argument <- function(
    self,
    ...,
    action  = arg_actions(),
    default = NULL,
    convert = scribe_convert(),
    n       = NA_integer_,
    info    = NULL,
    options = list(),
    stop    = c("none", "hard", "soft"),
    execute = invisible
) {
  if (is_arg(..1)) {
    arg <- ..1
  } else {
    aliases <- list(...)
    nms <- names(aliases)
    bad <- nzchar(nms)

    if (any(bad)) {
      warning(
        "Aliases should be passed without names.\n",
        "Check that you have not passed a bad field name:\n",
        "  names: ", to_string(nms[bad], sep = ", "),
        call. = FALSE
      )
    }

    arg <- new_arg(
      aliases = aliases,
      action  = action,
      default = default,
      convert = convert,
      n       = as.integer(n),
      info    = info,
      options = options,
      stop    = stop,
      execute = execute
    )
  }

  stopifnot(is_arg(arg))

  ca_append_arg(self, arg)
  self$field("resolved", FALSE)
  invisible(self)
}

ca_set_description <- function(self, ..., sep = "") {
  x <- c(...)
  if (is.null(x)) {
    self$field("description", NA_character_)
    return(invisible(self))
  }
  self$field("description", paste0(x, collapse = sep))
  invisible(self)
}

ca_add_description <- function(self, ..., sep = "") {
  x <- paste0(c(...), collapse = sep)

  if (nzchar(x)) {
    self$field("description", c(self$description, x))
  }

  invisible(self)
}

ca_get_description <- function(self) {
  self$description
}

ca_set_example <- function(self, x = character(), comment = "", prefix = "$ ") {
  self$field("examples", x)
  self$field("comments", "")
  invisible(self)
}

ca_add_example <- function(self, x, comment = "", prefix = "$ ") {
  if (is.null(x)) {
    return(invisible(self))
  }

  x <- paste0(prefix, x)
  self$field("examples", c(self$examples, x))
  self$field("comments", c(self$comments, comment))
  invisible(self)
}

ca_get_examples <- function(self) {
  self$examples
}

ca_do_execute <- function(self, arg) {
  n <- length(formals(arg$execute))

  # switch(int) doesn't account for defaults
  if (n == 0L) {
    arg$execute()
  } else if (n == 1L) {
    arg$execute(arg)
  } else {
    arg$execute(arg, self)
  }

  invisible(self)
}

# internal ----------------------------------------------------------------

ca_get_working <- function(self, i = TRUE) {
  if (length(self$working)) {
    self$working[i]
  } else {
    character()
  }
}

ca_remove_working <- function(self, i) {
  self$field("working", self$working[-i])
  self
}

ca_append_arg <- function(self, arg) {
  self$field("args", replace2(self$args, length(self$args) + 1L, arg))
  invisible(self)
}

ca_write_usage <- function(self) {
  x <- vapply(
    self$get_args(included = FALSE),
    function(arg) arg$get_help()[1],
    NA_character_
  )
  paste(sprintf("[%s]", x), collapse = " ")
}

# helpers -----------------------------------------------------------------

is_command_args <- function(x) {
  identical(class(x), structure("scribeCommandArgs", package = "scribe"))
}
