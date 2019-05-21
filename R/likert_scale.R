
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_scale <- function(.data, ..., .name, .label = NULL, .drop = FALSE, na.rm = FALSE) {
  dots <- quos(...)

  if(missing(.name)) stop("A name must be specified")

  vnames <- vars_select(colnames(.data), !!!dots)

  if(length(vnames) == 0) stop("At least one variable must be specified")

  .data[[.name]] <- structure(rowMeans(.data[vnames], na.rm = na.rm), label = .label)

  if(.drop) .data[[.name]] else .data
}
