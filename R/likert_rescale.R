
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_rescale <- function(.data, ..., .min, .max, .suffix = "") {
  dots <- quos(...)

  if(missing(.min)) stop("A min must be specified")
  if(missing(.max)) stop("A max must be specified")

  vnames <- vars_select(colnames(.data), !!!dots)

  if(length(vnames) == 0) stop("At least one variable must be specified")

  for(vname in vnames) {
    a <- .data[[vname]]

    if(!is_likerrt(a)) {
      stop("variable ", vname, " is not of class likerrt_likert")
    }

    oldRange <- attributes(.data[[vname]])$labels
    #oldLabel <- get_label(.data[[vname]])
    if(is.null(oldRange))
      stop("variable", vname, "has no labels attribute")
    oldMin <- min(oldRange)
    if(is.infinite(oldMin))
      stop("minimal value of the labels of ", vname, " is infinite\n", oldRange)
    if(is.na(oldMin))
      stop("minimal value of the labels of ", vname, " is NA\n", oldRange)
    oldMax <- max(oldRange)
    if(is.infinite(oldMax))
      stop("maximal value of the labels of ", vname, " is infinite\n", oldRange)
    if(is.na(oldMax))
      stop("maximal value of the labels of ", vname, " is NA\n", oldRange)

    oldValues <- .data[[vname]]

    newvname <- paste(vname, .suffix, sep = "")
    .data[[newvname]] <- rescale(oldValues, oldMin, oldMax, .min, .max)
    attributes(.data[[newvname]]) <- NULL
  }

  .data
}

check_valid_labels <- function() {

}

rescale <- function(x, min, max, .min, .max) {
  (((x - min) / (max-min)) * (.max-.min)) + .min
}
