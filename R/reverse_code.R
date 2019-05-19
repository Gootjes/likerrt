

#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_reverse_code <- function(.data, ..., .suffix = "") {
  dots <- quos(...)

  if(length(dots) > 0) {
    for(i in seq_along(dots)) {
      vname <- as_name(dots[[i]])
      vattr <- attributes(.data[[vname]])

      if(!is_likerrt(vattr)) {
        stop("Not a likerrt.likert")
      }

      labs <- vattr$labels

      .data[[paste(vname, .suffix, sep = "")]] <- likert_recode.likerrt.likert(x = .data[[vname]], spec = set_names(x = rev(labs), labs))
    }
  }

  .data
}

