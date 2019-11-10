
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_rescale <-
  function(.data,
           ...,
           .min,
           .max,
           .suffix = getOption("likerrt.rescale.suffix", ""),
           .label_suffix = getOption("likerrt.rescale.label_suffix", " (recoded)")) {

  dots <- quos(...)

  if(missing(.min)) stop("A min must be specified")
  if(missing(.max)) stop("A max must be specified")
  if(length(.min) > 1) stop(".min must be a single value")
  if(length(.max) > 1) stop(".max must be a single value")
  if(.min %in% c(Inf, NA, NaN)) stop(".min must be a real value")
  if(.max %in% c(Inf, NA, NaN)) stop(".min must be a real value")

  vnames <- vars_select(colnames(.data), !!!dots)

  if(length(vnames) == 0) stop("At least one variable must be specified")

  for(vname in vnames) {

    if(!is_likerrt(.data[[vname]])) {
      .data <- try_as_likert(.data, vname)
    }

    oldRange <- get_labels(.data[[vname]])

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
    .data[[newvname]] <- set_labels(.data[[newvname]], .value = rescale(oldRange, oldMin, oldMax, .min, .max))

    oldLabel <- get_label(.data[[vname]])
    if(!is.null(oldLabel))
      .data[[newvname]] <- set_label(.data[[newvname]], .value = paste(oldLabel, .label_suffix, sep = ""))
  }

  .data
}

check_valid_labels <- function() {

}

rescale <- function(x, min, max, .min, .max) {
  (((x - min) / (max-min)) * (.max-.min)) + .min
}
