
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_scale <- function(.data, ..., .name, .label = NULL, .drop = missing(.name), na.rm = FALSE) {
  dots <- quos(...)

  if(missing(.name) & .drop == FALSE) stop("A name must be specified, or drop must be set to TRUE")

  vnames <- vars_select(colnames(.data), !!!dots)

  if(length(vnames) == 0) stop("At least one variable must be specified")

  v <- structure(rowMeans(.data[vnames], na.rm = na.rm), label = .label)

  if(.drop) {
    v
  } else{
    .data[[.name]] <-  v
    .data
  }
}
