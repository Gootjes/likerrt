
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @importFrom haven labelled
#' @export
as_likert <- function(.data, ..., .label = NULL, .labels = NULL) {
  dots <- quos(...)

  if(length(dots) > 0) {
    for(i in seq_along(dots)) {
      vname <- as_name(dots[[i]])
      vattr <<- attributes(.data[[vname]])

      if(is_likerrt(vattr)) {
        next
      }

      if(!is_haven_labelled(vattr)) {
        if(is.null(.labels)) {
          stop(vname, " is not a haven labelled and no labels are specified")
        }

        labs <- sapply(X = seq_along(.labels), FUN = function(i) {
          if(names(.labels)[i] == "") paste(.labels[i]) else names(.labels)[i]
        })

        names(.labels) <- labs

        .data[[vname]] <- labelled(.data[[vname]], labels = .labels, label = .label)
      }

      attributes(.data[[vname]])$class <- c("likerrt.likert", attributes(.data[[vname]])$class)
    }

  }

  .data
}
