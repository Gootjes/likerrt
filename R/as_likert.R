
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @importFrom haven labelled
#' @export
as_likert <- function(.data, ..., .label = NULL, .labels = NULL, .complement = TRUE) {
  dots <- quos(...)

  vnames <- vars_select(colnames(.data), !!!dots)

  for(vname in vnames) {
    vattr <- attributes(.data[[vname]])

    if(is_likerrt(vattr)) {
      next
    }

    if(!is_haven_labelled(vattr)) {
      if(is.null(.labels)) {
        stop(vname, " is not a haven labelled and no labels are specified")
      }

      if(.complement == TRUE) {
        comp <- c()
        for(v in unique(.data[[vname]])) {
          if(v %in% .labels) {

          } else {
            comp[[as.character(v)]] <- v
          }
        }

        .labels <- sort(c(.labels, comp))
      }

      labs <- sapply(X = seq_along(.labels), FUN = function(i) {
        if(names(.labels)[i] == "") paste(.labels[i]) else names(.labels)[i]
      })

      names(.labels) <- labs

      attr(.labels, which = "class") <- "likerrt_labels"

      if(!is.null(.label))
        attr(.label, which = "class") <- "likerrt_label"

      .data[[vname]] <- labelled(.data[[vname]], labels = .labels, label = .label)
    }

    attributes(.data[[vname]])$class <- c("likerrt_likert", attributes(.data[[vname]])$class)
  }



  .data
}
