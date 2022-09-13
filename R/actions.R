apply_action <- function(arg, value, action = c("none", "bool", "flag")) {
  switch(
    match.arg(action %||% "none"),
    ## none ----
    none = value %||% attr(x, "value", exact = TRUE),

    ## flag ----
    flag = if (TRUE) {
      TRUE
    },

    ## bool ---
    bool = {
      if (length(x) != 1L) {
        stop("bool action requiredl length 1L", call. = FALSE)
      }

      if (is.character(x)) {
        tolower(x) %in% c("t", "true", "f", "false")
      } else if (is.numeric(x)) {
        as.logical(x)
      }
    }
  )
}
