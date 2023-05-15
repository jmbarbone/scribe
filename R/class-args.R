
#' {scribe} argument
#'
#' ReferenceClass object for managing arguments
#'
#' @details The [scribeArg] class sets specifications and controls for how
#'   command line arguments are to be parsed.  These are meant to be used in
#'   conjunction with [scribeCommandArgs] and specifically with the [Rscript]
#'   utility.  However, a use can define their own [scribeArg] separately.
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
#' @field action `[character]`\cr An action for resolving the argument (see
#'   `default` for note on using another [scribeArg] object)
#' @field default `[ANY]`\cr A default value.  This can be another [scribeArg]
#'   object.  When that is the case, the default value and action are pass
#'   through from the other [scribeArg] object.
#' @field convert `[ANY]`\cr Passed to the `to` argument in [value_convert()]
#' @field n `[integer]`\cr The length of the values
#' @field info `[character]`\cr Additional information about the argument when
#'   printed
#' @field options `[list]`\cr A named list of options (see **Options**)
#' @field positional `[logical]`\cr Indicator if the argument is _positional_
#'   (i.e., not preceded by a `-` or `--` command line argument)
#' @field resolved `[logical]`\cr Has the object been resolved
#' @field value `[ANY]`\cr The resolve value
#' @field stop `[character]`\cr `"none"`, `"hard"`, or `"soft"`
#' @field execute `[function]`\cr (For advanced use).  A `function` to be
#'   evaluated along with the arg.  The function can have no parameters, a
#'   single parameter for the [scribeArg] object, or accept the [scribeArg]
#'   object as its first argument, and the [scribeCommandArgs] object as its
#'   second.  Both objects will be passed by position
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
    positional = "logical",
    resolved   = "logical",
    value      = "ANY",
    stop       = "character",
    execute    = "function"
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
    options = list(),
    stop    = c("none", "hard", "soft"),
    execute = invisible
  ) {
    "
    Initialize the \\link{scribeArg} object

    See \\strong{fields} for parameter information.
    "
    arg_initialize(
      self    = .self,
      aliases = aliases,
      action  = action,
      default = default,
      convert = convert,
      n       = n,
      info    = info,
      options = options,
      stop    = stop,
      execute = execute
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
  },

  get_value = function() {
    "Retrieve the resolved value"
    arg_get_value(.self)
  },

  is_resolved = function() {
    "Check if object has been resolved"
    arg_is_resolved(.self)
  }
)
