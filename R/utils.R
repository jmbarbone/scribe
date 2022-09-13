
`%||%` <- function(x, y) { if (is.null(x)) y else x }


to_string <- function(..., sep = " ") {
  lines <- list(...)
  lines <- unlist(lines)
  paste0(lines, collapse = sep)
}

print_lines <- function(..., sep = "\n") {
  writeLines(to_string(..., sep = sep))
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
