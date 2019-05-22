

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

#' @importFrom rlang quos
#' @export
get_label <- function(x, ...) {
  dots <- quos(...)

  get_attr(x, dots, .key = "label")
}

#' @importFrom rlang quos
#' @export
get_labels <- function(x, ...) {
  dots <- quos(...)

  get_attr(x, dots, .key = "labels")
}

#' @importFrom tidyselect vars_select
get_attr <- function(x, dots = list(), .key) {

  if(length(dots) == 0) {
    if(is.data.frame(x)) {
      Map(f = function(n) {
        get_attr(x[[n]], .key = .key)
      }, names(x))
    } else {
      attr(x = x, which = .key, exact = TRUE)
    }
  } else {
    if(is.data.frame(x)) {
      vnames <- vars_select(colnames(x), !!!dots)
      Map(f = function(n) {
        get_attr(x[[n]], .key = .key)
      }, vnames)
    } else {
      stop("Illegal specification: x is not a data frame")
    }
  }
}

#' @importFrom rlang quos
#' @export
set_labels <- function(x, ..., .value) {
  dots <- quos(...)

  attr(.value, which = "class") <- "likerrt_labels"

  set_attr(x = x, dots = dots, .key = "labels", .value = .value)
}


#' @importFrom rlang quos
#' @export
set_label <- function(x, ..., .value) {
  dots <- quos(...)

  attr(.value, which = "class") <- "likerrt_label"

  set_attr(x = x, dots = dots, .key = "label", .value = .value)
}

#' @importFrom tidyselect vars_select
set_attr <- function(x, dots = list(), .key, .value) {
  if(length(dots) == 0) {
    if(is.data.frame(x)) {
      for(name in names(x)) {
        attr(x = x[[name]], which = .key) <- .value
      }
    } else {
      attr(x = x, which = .key) <- .value
    }
  } else {
    if(is.data.frame(x)) {
      vnames <- vars_select(colnames(x), !!!dots)
      for(name in vnames) {
        attr(x = x[[name]], which = .key) <- .value
      }
    } else {
      stop("Illegal specification: x is not a data frame")
    }
  }

  x
}
