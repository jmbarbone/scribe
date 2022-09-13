command_args <- function() {
  CommandArgs$new(commandArgs(trailingOnly = TRUE))
}

CommandArgs <- setRefClass(
  "CommandArgs",
  fields = list(
    call = "character",
    commands = "character",
    argList = "list",
    nArgs = "integer",
    resolved = "logical"
  ))

CommandArgs$methods(
  # creates the object
  initialize = function(command = NULL) {
    ca_initialize(.self)
  },

  show = function(...) {
    ca_show(.self, ...)
  },

  resolve_args = function() {
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


# helpers -----------------------------------------------------------------

ca_initialize <- function(self, call = NULL) {
  self$call <- call %||% "<null>"
  self$argList <- list()
  self$nArgs <- 0L
  self$commands <- character()
  self
}

ca_show <- function(self) {
  print_line("Initial call: ", to_string(self$call))

  if (self$resolved) {
    print_line("Resolved commands: ", to_string(self$commands))
  } else {
    print_line("w Arguments are unresolved")
  }

  lapply(self$argList, print)
  invisible(self)
}

ca_resolve_args <- function(self) {
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
  self$resolve_args()
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
