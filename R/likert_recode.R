#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @export
likert_recode <- function(.data, ..., .spec, .default = NULL, .missing = NULL) {
  dots <- quos(...)

  vnames <- vars_select(colnames(.data), !!!dots)

  for(vname in vnames) {

    if(!is_haven_labelled(.data[[vname]])) {
      stop("variable ", vname, " is not a likert")
    }

    .data[[vname]] <- likert_recode.likerrt.likert(x = .data[[vname]], spec = .spec, .default = .default, .missing = .missing)
  }

  .data
}

#' @export
likert_recode.likerrt.likert <- function(x, spec, .default = NULL, .missing = NULL) {
  srcclass <- class(as.vector(x))

  conv <- data.frame(from = as(names(spec), srcclass), to = as(spec, srcclass))

  xnew <- sapply(X = x, FUN = function(v){
    ifrom <- which(v == conv$from)
    if(length(ifrom) != 1) {
      stop("Illegal matching for ", v)
    }

    to <- conv$to[ifrom]

  })

  attributes(xnew) <- attributes(x)

  labelsnew <- sapply(X = attributes(x)$labels, FUN = function(v){

    ifrom <- which(v == conv$from)
    if(length(ifrom) != 1) {
      stop("Illegal matching for ", v)
    }

    to <- conv$to[ifrom]

  })

  attributes(xnew)$labels <- labelsnew

  xnew
}
