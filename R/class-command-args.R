

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
    info = NULL
  ) {
    "Add a \\link{scribeArg} to \\code{argList}

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
