
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_scale <- function(.data, ..., .name, .label = NULL, .drop = missing(.name), na.rm = FALSE, .strictness = c("labels", "values")) {
  dots <- quos(...)

  unknowns <- setdiff(.strictness, c("labels", "values", "range", "none"))
  if(length(unknowns) > 0) stop("Unknown .stricness assumption specified: ", unknowns)

  if(missing(.name) & .drop == FALSE) stop("A name must be specified, or drop must be set to TRUE")

  vnames <- vars_select(colnames(.data), !!!dots)

  if(length(vnames) <= 1) stop("At least two variables must be specified")

  def <- equal_labels(get_labels(.data[vnames]))

  if(is.null(.strictness)) .strictness <- "none"

  if(length(setdiff(.strictness, "none")) > 0) { #if strictness other than 'none' was specified
    unmet_assumptions <- setdiff(.strictness, def)

    if("labels" %in% unmet_assumptions) {
      stop("Not all variables have value labels that are equal. (To drop this assumption, consider .strictness = c('values')")
    }

    if("values" %in% unmet_assumptions) {
      stop("Not all variables have values that are equal. (To drop this assumption, consider .strictness = c('range')")
    }

    if("range" %in% unmet_assumptions) {
      stop("Not all variables have values that are in the same range. (To drop this assumption, use .strictness = c('none')")
    }
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
    b <- ls[[i]]

    if(length(names(a)) != length(names(b))) {
      eq <- FALSE
      break
    } else if(all(names(a) == names(b))){
      eq <- TRUE
    } else {
      eq <- FALSE
      break
    }
  }
  if(eq) {
    def <- append(def, "labels")
  }


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
    def <- append(def, "values")
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
