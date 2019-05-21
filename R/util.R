

#' @importFrom rlang quos
#' @importFrom rlang eval_tidy
likert_attributes <- function(x, ...) {
  dots <- quos(...)

  if(length(dots) == 0) {
    attributes(x)
  } else {
    ns <- names(dots)
    ns <- ns[ns != ""]
    if(length(ns) == length(dots)) {
      for(name in ns) {
        attributes(x)[name] <- eval_tidy(dots[[name]])
      }

      x
    } else if(length(ns) == 0) {
      l <- list()
      for(d in dots) {
        l[[as_name(d)]] <- attributes(x)[[as_name(d)]]
      }

      if(length(l) == 1){
        l[[1]]
      } else {
        l
      }

    } else {
      stop("Arguments must either be all symbols, or named arguments")
    }
  }

}
