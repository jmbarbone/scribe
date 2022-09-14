
# TODO use `args`, `argsList`, `params` consistently as names

#' Command args
#'
#' @param x Command args; see [base::commandArgs()] for default
#' @returns A `scribeCommandArgs` Reference object
#' @export
command_args <- function(x = commandArgs(trailingOnly = TRUE)) {
  CommandArgs$new(call = x)
}

# ReferenceClass ----------------------------------------------------------

CommandArgs <- setRefClass(
  "scribeCommandArgs",
  fields = list(
    call = "character",
    commands = "character",
    argList = "list",
    nArgs = "integer",
    resolved = "logical"
  ))

CommandArgs$methods(
  # creates the object
  initialize = function(call = NULL) {
    ca_initialize(.self, call = call)
  },

  show = function(...) {
    ca_show(.self, ...)
  },

  version = function() {
    print_line("{scribe} package version: ", format(packageVersion("scribe")))
  },

  help = function() {
    ca_help(.self)
  },

  resolve = function() {
    ca_resolve_args(.self)
  },

  parse = function() {
    ca_parse(.self)
  },

  add_argument = function(
    ...,
    action = NULL,
    nargs = 1,
    default = NULL,
    help = NULL
  ) {
    ca_add_argument(
      self = .self,
      ...,
      action = action,
      nargs = nargs,
      default = default,
      help = help
    )
  }
)

# wrappers ----------------------------------------------------------------

ca_initialize <- function(self, call = NULL) {
  self$call <- call %||% "<null>"
  self$argList <- list()
  self$nArgs <- 0L
  self$commands <- character()
  self$resolved <- FALSE
  self
}

ca_show <- function(self, ...) {
  print_line("Initial call: ", to_string(self$call))

  if (self$resolved) {
    print_line("Resolved commands: ", to_string(self$commands))
  } else {
    print_line("w Arguments are unresolved")
  }

  lapply(self$argList, print)
  invisible(self)
}

ca_help <- function(self) {
  lines <- sapply(self$argList, arg_help)
  lines <- apply(lines, 2, format) # get consistent width
  lines <- apply(lines, paste, collapse = " : ") # middle colon
  print_lines(lines)
}

ca_resolve_args <- function(self) {
  # loop through the possibly arg in argList.  When found in args, extract and
  # determine what the param should be.  Take into account the action: none

  # TODO reserve [-h --help] and [--version]

  if ("--verson" %in% self$commands) {
    return(scribe_version())
  }

  if (any(c("-h", "--help")) %in% self$commands) {
    return(self$help())
  }

  # browser()
  # reset if not unsuccessful
  on.exit(if (!self$resolved) self$commands <- character(), add = TRUE)

  for (i in seq_along(self$argList)) {
    arg <- self$argList[[i]]
    m <- match(attr(arg, "alias"), self$call, 0L)
    m <- m[m > 0L]
    if (length(m) == 2L) {
      writeLines(
        sprintf("w Multiple command args matched for [%s]", to_string(attr(arg, "alias"))),
        "i Using first found"
      )
      m <- m[1L]
    }

    if (length(m) == 1L) {
      self$commands[i] <- apply_action(arg, value = self$call[m])

      act <- attr(arg, "action")
      if (identical(act, "none")) {
        self$call <- self$call[-c(m, m + 1L)]
      } else if (identical(act, "flag")) {

      }
    }
  }

  self$resolved <- TRUE
  self
}

ca_parse <- function(self) {
  self$resolve()
  lapply(self$argList, get_value)
}

ca_add_argument <- function(
    self,
    ...,
    action = NULL,
    nargs = 1,
    default = NULL,
    help = NULL
) {
  self$nArgs <- self$nArgs + 1L
  self$resolved <- FALSE
  new <- new_arg(
    self$nArgs,
    aliases = list(...),
    action = action,
    default = default,
    help = help
  )
  self$argList[[self$nArgs]] <- new
  self
}


# helpers -----------------------------------------------------------------

is_command_args <- function(x) {
  identical(class(x), structure("scribeCommandArgs", package = "scribe"))
}
