
#' {scribe} command arguments
#'
#' Reference class object for managing command line arguments.
#'
#' @details This class manages the command line argument inputs when passed via
#'   the [Rscript] utility.  Take the simple script below which adds two
#'   numbers, which we will save in an executable file called `add.R`,
#'
#'   ```sh
#'   #!/usr/bin/env Rscript
#'
#'   library(scribe)
#'   ca <- command_args()
#'   ca$add_argument("--value1", default = 0L)
#'   ca$add_argument("--value2", default = 0L)
#'   args <- ca$parse()
#'   writeLines(args$value1 + args$value2)
#'   ```
#'
#'   When called by a terminal, we can pass arguments and return a function.
#'
#'   ```sh
#'   add.R --value1 10 --value2 1
#'   11
#'   ```
#'
#'   When testing, you can simulate command line arguments by passing them into
#'   the `input` field. By default, this will grab values from
#'   [base::commandArgs()], so use with the [Rscript] utility doesn't require
#'   any extra steps.
#'
#'   Most methods are designed to return `.self`, or the [scribeCommandArgs]
#'   class. The exceptions to these are the the `$get_*()` methods, which return
#'   their corresponding values, and `$parse()` which returns a named `list` of
#'   the parsed input values.
#'
#' @field input `[character]`\cr A character vector of command line arguments.
#'   See also [command_args()]
#' @field values `[list]`\cr A named `list` of values.  Empty on initialization
#'   and populated during argument resolving.
#' @field args `[list]`\cr a List of [scribeArg]s
#' @field description `[character]`\cr Additional help information
#' @field included `[character]`\cr Default [scribeArg]s to include
#' @field examples `[character]`\cr Examples to print with help
#' @field comments `[character]`\cr Comments printed with
#' @field resolved `[logical]`\cr A `logical` value indicated if the
#'   `$resolve()` method has been successfully executed.
#' @field working `[character]`\cr A copy of `input`.  Note: this is used to
#'   track parsing progress and is not meant to be accessed directly.
#' @field stop `[character]`\cr Determines parsing
#'
#' @examples
#' # command_args() is recommended over direct use of scribeCommandArgs$new()
#'
#' ca <- command_args(c(1, 2, 3, "--verbose"))
#' ca$add_argument("--verbose", action = "flag")
#' ca$add_argument("...", "values", info = "values to add", default = 0.0)
#' args <- ca$parse()
#'
#' if (args$verbose) {
#'   message("Adding ", length(args$values), " values")
#' }
#'
#' sum(args$values)
#'
#' # $parse() returns a named list, which means scribeCommandArgs can function
#' # as a wrapper for calling R functions inside Rscript
#'
#' ca <- command_args(c("mean", "--size", 20, "--absolute"))
#' ca$add_argument("fun", action = "list")
#' ca$add_argument("--size", default = 5L)
#' ca$add_argument("--absolute", action = "flag")
#' args <- ca$parse()
#'
#' my_function <- function(fun, size, absolute = FALSE) {
#'   fun <- match.fun(fun)
#'   x <- sample(size, size, replace = TRUE)
#'   res <- fun(x)
#'   if (absolute) res <- abs(res)
#'   res
#' }
#'
#' do.call(my_function, args)
#' @family scribe
#' @export
scribeCommandArgs <- methods::setRefClass( # nolint: object_name_linter.
  "scribeCommandArgs",
  fields = list(
    input = "character",
    values = "list",
    args = "list",
    description = "character",
    included = "character",
    examples = "character",
    comments = "character",
    resolved = "logical",
    working = "character",
    stop = "character"
  )
)

scribeCommandArgs$methods(
  # creates the object
  initialize = function(
    input = "",
    include = c("help", "version", NA_character_)
  ) {
    "Initialize the \\link{scribeCommandArgs} object.  The wrapper
    \\code{\\link[=command_args]{command_args()}} is recommended rather than
    calling this method directly.

    \\describe{
      \\item{\\code{input}}{A \\code{character} vector of command line arguments
        to parse}
      \\item{\\code{include}}{A character vector denoting which default
        \\link{scribeArg}s to include in \\code{args}}
    }"
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
    "Resolve the values of each \\link{scribeArg} in \\code{args}.  This method
    is called prior to $parse()"
    ca_resolve(.self)
  },

  parse = function() {
    "Return a named \\code{list} of parsed values of from each \\link{scribeArg}
    in \\code{args}"
    ca_parse(.self)
  },

  get_input = function() {
    "Retrieve \\code{input}"
    ca_get_input(.self)
  },

  set_input = function(value) {
    "Set \\code{input}.  Note: when called, \\code{resolved} is (re)set to
     \\code{FALSE} and values need to be parsed again.

    \\describe{
      \\item{\\code{value}}{Value to set}
    }"
    ca_set_input(.self, value = value)
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

  get_args = function(included = TRUE) {
    "Retrieve \\code{args}

    \\describe{
      \\item{\\code{included}}{If \\code{TRUE} also returns included default
        \\link{scribeArg}s defined in \\code{$initialize()}}
    }"
    ca_get_args(.self, included = included)
  },

  add_argument = function(
    ...,
    action = arg_actions(),
    options = NULL,
    convert = default_convert,
    default = NULL,
    n = NA_integer_,
    info = NULL,
    execute = invisible
  ) {
    "Add a \\link{scribeArg} to \\code{args}

    \\describe{
      \\item{\\code{...}}{Either aliases or a \\link{scribeArg}.  If the latter,
        all other arguments are ignored.  Note that only the first value
        (\\link{..1}) is used.}
      \\item{\\code{action}, \\code{options}, \\code{convet}, \\code{default},
        \\code{n}, \\code{info}}{See \\code{\\link[=new_arg]{new_arg()}}}
    }"
    ca_add_argument(
      self = .self,
      ...,
      action = action,
      options = options,
      convert = convert,
      default = default,
      n = n,
      info = info
    )
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
