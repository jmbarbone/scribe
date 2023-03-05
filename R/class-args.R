
#' {scribe} argument
#'
#' ReferenceClass object for managing arguments
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
#' @field options `[list]`\cr A named list of options (see **Options**)
#' @field convert `[ANY]`\cr Passed to the `to` argument in [value_convert()]
#' @field default `[ANY]`\cr A default value
#' @field info `[character]`\cr Additional information about the argument when
#'   printed
#' @field n `[integer]`\cr The length of the values
#' @field positional `[logical]`\cr Indicator if the argument is _positional_
#'   (i.e., not preceded by a `-` or `--` command line argument)
#'
#' @export
scribeArg <- methods::setRefClass( # nolint: object_name_linter.
  "scribeArg",
  fields       = list(
    aliases    = "character",
    action     = "character",
    options    = "list",
    convert    = "ANY",
    default    = "ANY",
    info       = "character",
    n          = "integer",
    positional = "logical"
  )
)

scribeArg$methods(
  initialize = function(
    aliases = NULL,
    action = NULL,
    options = NULL,
    convert = NULL,
    default = NULL,
    info = NULL,
    n = NULL
  ) {
    "
    Initialize the \\link{scribeArg} object

    See \\strong{fields} for parameter information.
    "
    arg_initialize(
      .self,
      aliases = aliases,
      action  = action,
      options = options,
      convert = convert,
      default = default,
      info    = info,
      n       = n
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

  parse_value = function(command_arg) {
    "Parse argument value

    This method will likely not be called directly by the user.  Instead, this
    method is called within \\link{scribeCommandArgs}.

    \\describe{
      \\item{\\code{command_arg}}{A \\link{scribeCommandArgs}} object
    }"
    arg_parse_value(.self, ca = command_arg)
  },

  get_default = function() {
    "Retrieve the default value"
    arg_get_default(.self)
  }
)
