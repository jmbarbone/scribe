# this will be the parent class of scribeCommandArgs
#
# this will do the basic features, but probably just return as strings
#  -- generate help
#  - generate examples
#  -

scribeCommand <- methods::setRefClass(
  "scribeCommand",
  fields = list(
    name = "character",
    description = "character",
    args = "list",
    commands = "list",
    examples = "character",
    comments = "character"
  )
)

scribeCommand$methods(
  # creates the object
  initialize = function(
    input = "",
    include = c("help", "version", NA_character_),
    supers = include
  ) {
    "Initialize the \\link{scribeCommandArgs} object.  The wrapper
    \\code{\\link[=command_args]{command_args()}} is recommended rather than
    calling this method directly.

    \\describe{
      \\item{\\code{input}}{A \\code{character} vector of command line arguments
        to parse}
      \\item{\\code{include}}{A character vector denoting which default
        \\link{scribeArg}s to include in \\code{args}}
      \\item{\\code{supers}}{A character vector denoting which default
        \\code{scribeSuperArg}s to include in \\code{supers} (i.e., arguments
        called with `---` prefixes)}
    }"
    cmd_initialize(.self, input = input, include = include, supers = supers)
  },

  show = function(...) {
    "Print the \\link{scribeCommandArgs} object"
    cmd_show(.self, ...)
  },

  version = function() {
    "Print the \\link{scribe-package} version"
    print_scribe_version()
  },

  help = function() {
    "Print the help information"
    cmd_help(.self)
  },

  resolve = function() {
    "Resolve the values of each \\link{scribeArg} in \\code{args}.  This method
    is called prior to $parse()"
    cmd_resolve(.self)
  },

  parse = function() {
    "Return a named \\code{list} of parsed values of from each \\link{scribeArg}
    in \\code{args}"
    cmd_parse(.self)
  },

  get_input = function() {
    "Retrieve \\code{input}"
    cmd_get_input(.self)
  },

  set_input = function(value) {
    "Set \\code{input}.  Note: when called, \\code{resolved} is (re)set to
     \\code{FALSE} and values need to be parsed again.

    \\describe{\\item{\\code{value}}{Value to set}}
    "
    cmd_set_input(.self, value = value)
  },

  get_values = function(
    empty = FALSE,
    super = FALSE,
    included = FALSE
  ) {
    "Retrieve \\code{values}
    \\describe{
      \\item{\\code{{empty}}}{If \\code{TRUE} returns empty values, too}
      \\item{\\code{super}}{If \\code{TRUE} also returns values from super args}
      \\item{\\code{included}}{If \\code{TRUE} also returns included default
        \\link{scribeArg}s defined in \\code{$initialize()}}
    }"
    cmd_get_values(.self, empty = empty, super = super, included = included)
  },

  set_values = function(i = TRUE, value) {
    "Set \\code{values}

    \\describe{
      \\item{\\code{i}}{Index value of \\code{working} to set}
      \\item{\\code{value}}{The value to set}
    }"
    cmd_set_values(.self, i = i, value = value)
  },

  get_args = function(included = TRUE, super = FALSE) {
    "Retrieve \\code{args}

    \\describe{
      \\item{\\code{included}}{If \\code{TRUE} also returns included default
        \\link{scribeArg}s defined in \\code{$initialize()}}
      \\item{\\code{super}}{If \\code{TRUE} also returns super args}
    }"
    cmd_get_args(.self, included = included, super = super)
  },

  add_argument = function(
    ...,
    action = arg_actions(),
    default = NULL,
    convert = scribe_convert(),
    n = NA_integer_,
    info = NULL,
    options = list(),
    stop = c("none", "hard", "soft"),
    execute = invisible
  ) {
    "Add a \\link{scribeArg} to \\code{args}

    \\describe{
      \\item{\\code{...}}{Either aliases or a \\link{scribeArg}.  If the latter,
        all other arguments are ignored.  Note that only the first value
        (\\link{..1}) is used.}
      \\item{\\code{action}, \\code{options}, \\code{convet}, \\code{default},
        \\code{n}, \\code{info}, \\code{stop}, \\code{execute}}{See
        \\code{\\link[=new_arg]{new_arg()}}}
    }"
    cmd_add_argument(
      self = .self,
      ...,
      action = action,
      default = default,
      convert = convert,
      n = n,
      info = info,
      options = options,
      stop = stop,
      execute = execute
    )
  },

  add_commands = function(commands) {
    cmd_add_commands(self = .self, commands = commands)
  },

  set_description = function(..., sep = "") {
    "Set the value of \\code{description}

    \\describe{
      \\item{\\code{...}}{Information to paste into the description}
      \\item{\\code{sep}}{\\code{character} separate for \\code{...}}
    }"
    cmd_set_description(.self, ..., sep = sep)
  },

  add_description = function(..., sep = "") {
    "Add a value to \\code{description}

    \\describe{
      \\item{\\code{...}}{Information to paste into the description}
      \\item{\\code{sep}}{\\code{character} separate for \\code{...}}
    }"
    cmd_add_description(.self, ..., sep = sep)
  },

  get_description = function() {
    "Retrieve \\code{description}"
    cmd_get_description(.self)
  },

  set_example = function(x = character(), comment = "", prefix = "$ ") {
    "Set the value of \\code{examples}

    \\describe{
      \\item{\\code{x}}{A code example as a \\code{character}}
      \\item{\\code{comment}}{An optional comment to append}
      \\item{\\code{prefix}}{An optional prefix for the example}
    }"
    cmd_set_example(
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
    cmd_add_example(
      self = .self,
      x = x,
      comment = comment,
      prefix = prefix
    )
  },

  get_examples = function() {
    "Retrieve \\code{examples}"
    cmd_get_examples(.self)
  }
)
