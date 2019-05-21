
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_reverse_code <- function(.data, ..., .suffix = "") {
  dots <- quos(...)

  vnames <- vars_select(colnames(.data), !!!dots)

  for(vname in vnames) {
    vattr <- attributes(.data[[vname]])

    if(!is_haven_labelled(vattr)) {
      stop("Not a likert")
    }

    labs <- vattr$labels

    .data[[paste(vname, .suffix, sep = "")]] <- likert_recode.likerrt.likert(x = .data[[vname]], spec = set_names(x = rev(labs), labs))
  }

  .data
}

