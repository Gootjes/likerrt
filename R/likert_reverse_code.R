
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_reverse_code <- function(.data, ..., .suffix = "") {
  dots <- quos(...)

  vnames <- vars_select(colnames(.data), !!!dots)

  for(vname in vnames) {

    if(!is_likerrt(.data[[vname]])) {
      .data[[vname]] <- try_as_likert(.data[[vname]], vname)
    }

    labs <- get_labels(.data[[vname]])

    .data[[paste(vname, .suffix, sep = "")]] <- likert_recode.likerrt.likert(x = .data[[vname]], spec = set_names(x = rev(labs), labs))
  }

  .data
}

