
# slightly different here

`%||%` <- function(x, y) { if (is_empty(x)) y else x }

is_empty <- function(x) {
  is.null(x) || !nzchar(x) || length(x) == 0L
}

to_string <- function(..., sep = " ") {
  lines <- list(...)
  lines <- unlist(lines)
  paste0(lines, collapse = sep)
}

print_lines <- function(..., sep = "\n", to = stdout()) {
  writeLines(to_string(..., sep = sep), con = to)
}

print_line <- function(...) {
  print_lines(..., sep = " ")
}

print_line0 <- function(...) {
  print_lines(..., sep = "")
}

is_intish <- function(x) {
  !is.null(x) && (is.numeric(x) | all(is.na(x))) && !isFALSE(x %% 1 == 0)
}

maybe_quit <- function(force = !interactive()) {
  if (force) quit(save = "no", status = 0L) else invisible()
}

first <- function(x) {
  if (length(x)) x[1L] else x[0L]
}
