
#' {scribe} argument
#'
#' ReferenceClass object for managing arguments
#'
#' @details The `scribeArg` class sets specifications and controls for how
#'   command line arguments are to be parsed.  These are meant to be used in
#'   conjunction with [scribeCommandArgs] and specifically with the [Rscript]
#'   utility.  However, a use can define their own `scribeArg` separately.
#'
#' @section Options:
#'
#'   Several available options
#' \describe{
#'   \item{`action="list"`}{
#'     \describe{
#'       \item{`choices`}{
#'         An explicit set of values that argument must be.  If the value parsed
#'         is not one of these, an error will occur.
#'      }
#'     }
#'   }
#'  \item{`action="flag"`}{
#'    \describe{
#'      \item{`no`}{
#'        When `TRUE` included appends `--no` to aliases to invert results\cr
#'        **Example:**\cr
#'        With the argument `new_arg("--test", options = list(no = TRUE))`,
#'        passing command arguments `--test` would set this to `TRUE` and
#'        `--no-test` explicitly set to `FALSE`.
#'      }
#'    }
#'  }
#' }
#'
#' @field aliases `[character]`\cr A vector to denote the argument's name
#' @field action `[character]`\cr An action for resolving the argument
#' @field default `[ANY]`\cr A default value
#' @field convert `[ANY]`\cr Passed to the `to` argument in [value_convert()]
#' @field n `[integer]`\cr The length of the values
#' @field info `[character]`\cr Additional information about the argument when
#'   printed
#' @field options `[list]`\cr A named list of options (see **Options**)
#' @field positional `[logical]`\cr Indicator if the argument is _positional_
#'   (i.e., not preceded by a `-` or `--` command line argument)
#'
#' @examples
#' # new_arg() is recommended over direct use of scribeArg$new()
#'
#' # arguments with `--` indicators
#' new_arg("--verbose", action = "flag")
#' new_arg(c("-f", "--force"), action = "flag")
#' new_arg("--values", action = "list")
#'
#' # positional
#' new_arg("verbose", action = "flag")
#' new_arg("value", action = "list", n = 1)
#'
#' # special `...` action which absorbs left-over arguments
#' new_arg("values", action = "dots", info = "list of values")
#' new_arg("...", info = "list of values") # defaults when alias is "..."
#' @family scribe
#' @export
scribeArg <- methods::setRefClass( # nolint: object_name_linter.
  "scribeArg",
  fields       = list(
    aliases    = "character",
    action     = "character",
    default    = "ANY",
    convert    = "ANY",
    n          = "integer",
    info       = "character",
    options    = "list",
    positional = "logical"
  )
)

scribeArg$methods(
  initialize = function(
    aliases = "",
    action  = arg_actions(),
    default = NULL,
    convert = default_convert,
    n       = NA_integer_,
    info    = NA_character_,
    options = list()
  ) {
    "
    Initialize the \\link{scribeArg} object

    See \\strong{fields} for parameter information.
    "
    arg_initialize(
      .self,
      aliases = aliases,
      action  = action,
      default = default,
      convert = convert,
      n       = n,
      info    = info,
      options = options
    )
  },

  show = function() {
    "Print the \\link{scribeArg} object"
    arg_show(.self)
  },

  help = function() {
    "Print out formatted help information"
    # should return a matrix
    arg_help(.self)
  },

  get_help = function() {
    "Retrieve help information as a \\code{character} vector"
    arg_get_help(.self)
  },

  get_aliases = function() {
    "Retrieve aliases"
    arg_get_aliases(.self)
  },

  get_name = function(clean = TRUE) {
    "Retrieve names

    \\describe{
      \\item{\\code{clean}}{When \\code{TRUE} removes \\code{-}s from text}
    }"
    arg_get_name(.self, clean = clean)
  },

  get_action = function() {
    "Retrieve action"
    arg_get_action(.self)
  },

  get_default = function() {
    "Retrieve the default value"
    arg_get_default(.self)
  }
)
