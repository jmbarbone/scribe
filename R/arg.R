
#' New argument
#'
#' Create a new argument class
#'
#' @param id An integer to represent the sequence of argument
#' @param aliases One or more names to be associated with the argument
#' @param action An action for the paramater
#' @param default Default value
#' @param
#' @export
new_arg <- function(
    id,
    aliases = NULL,
    action = NULL,
    default = NULL,
    help = NULL,
    n = 0L
) {

  if (is.null(aliases)) {
    aliases <- "..."
  } else {
    aliases <- unlist(aliases, recursive = TRUE, use.names = FALSE)
    # TODO add "strict" requirement?
    stopifnot(
      grepl(ARG_PAT, aliases, ignore.case = TRUE),
      !grepl("[[:space:]]", aliases)
    )
  }

  stopifnot(
    is_intish(id),
    length(id) == 1L,
    length(aliases) > 0L,
    length(n) == 1L,
    isTRUE(n) || (is_intish(n) & n >= 0L)
  )

  structure(
    as.integer(id),
    class = "scribe_arg",
    aliases = aliases,
    # TODO consider validate_action()
    action = action,
    value = default %||% if (identical(action, "bool")) TRUE,
    help = help,
    n = as.integer(n)
  )
}

ARG_PAT <- "^-[a-z]$|^--[a-z]+$|^--[a-z](+[-]?[a-z]+)+$"

#' @export
print.scribe_arg <- function(x, ...) {
  # TODO add print_scribe.arg
  value <- attr(x, "value")
  value <-
    if (is.null(value)) {
      "<null>"
    } else {
      paste(vapply(value, format, NA_character_), collapse = " ")
    }

  aliases <- to_string(attr(x, "aliases"))
  print_line(sprintf("Argument [%s] : %s", aliases, value))
  invisible(x)
}

# helpers -----------------------------------------------------------------

get_value <- function(x) {
  stopifnot(inherits(x, "scribe_args"))
  # TODO flesh out
  attr(x, "value", exact = TRUE)
}
