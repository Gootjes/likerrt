

#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_scale <- function(.data, ..., .name, .label, .drop = FALSE, na.rm = FALSE) {
  dots <- quos(...)

  varnames <- sapply(X = dots, FUN = function(v) as_name(v))

  .data[[.name]] <- structure(rowMeans(.data[varnames], na.rm = na.rm), label = .label)

  if(.drop) .data[varnames] <- NULL

  .data
}
