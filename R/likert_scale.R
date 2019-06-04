
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_scale <- function(.data, ..., .name, .label = NULL, .drop = missing(.name), na.rm = FALSE, .strictness = c("equal")) {
  dots <- quos(...)

  if(missing(.name) & .drop == FALSE) stop("A name must be specified, or drop must be set to TRUE")

  vnames <- vars_select(colnames(.data), !!!dots)

  if(length(vnames) == 0) stop("At least one variable must be specified")

  def <- equal_labels(get_labels(.data[vnames]))
  if(!all(.strictness %in% def)) {
    stop("strictness assumption of value ranges for likert items was not met: ", .strictness)
  }

  v <- structure(rowMeans(.data[vnames], na.rm = na.rm), label = .label)

  if(.drop) {
    v
  } else{
    .data[[.name]] <-  v
    .data
  }
}

#' @importFrom base setequal
equal_labels <- function(ls) {
  nulls <- which(sapply(ls, is.null))
  if(length(nulls) > 0) {
    stop("list of labels must not contain a NULL value. Found a NULL for entry: ", names(nulls))
  }

  if(length(ls) < 2) {
    stop("list of labels must be at least of length 2")
  }

  def <- c("none")

  eq <- FALSE
  for(i in 2:length(ls)) {
    a <- ls[[1]]
    b <- sort(ls[[i]])
    if(setequal(a, b)){
      eq <- TRUE
    } else {
      eq <- FALSE
      break
    }
  }
  if(eq) {
    def <- append(def, "equal")
  }

  eq <- FALSE
  for(i in 2:length(ls)) {
    a <- ls[[1]]
    b <- ls[[i]]
    if(min(a) == min(b) & max(a) == max(b)) {
      eq <- TRUE
    } else if(min(a) == max(b) & max(a) == min(b)) {
      eq <- TRUE
    } else {
      eq <- FALSE
      break
    }
  }
  if(eq) {
    def <- append(def, "range")
  }

  def
}
