
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
    a <<- .data[[vname]]
    oldRange <- attributes(.data[[vname]])$labels
    #oldLabel <- get_label(.data[[vname]])
    oldMin <- min(oldRange)
    oldMax <- max(oldRange)

    oldValues <- .data[[vname]]

    newvname <- paste(vname, .suffix, sep = "")
    .data[[newvname]] <- (((oldValues - oldMin) / (oldMax-oldMin)) * (.max-.min)) + .min
    attributes(.data[[newvname]]) <- NULL
  }

  .data
}
