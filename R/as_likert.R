
#' @importFrom tidyselect vars_select
#' @importFrom rlang as_name
#' @importFrom rlang quos
#' @importFrom haven labelled
#' @export
as_likert <- function(.data, ..., .label = NULL, .labels = NULL, .complement = TRUE) {
  dots <- quos(...)

  vnames <- vars_select(colnames(.data), !!!dots)

  for(vname in vnames) {

    if(is_likerrt(.data[[vname]])) {
      next
    }


    el <- get_label(.data[[vname]])

    if(is.null(el)) {
      if(is.null(.label)) {
        #.label <- ""
        #stop(vname, " cannot be coerced to likert as no label is specified and .label is NULL")
      }
    } else {
      if(is.null(.label)) {
        .label <- as_likerrt_label(el)
      }
    }


    els <- get_labels(.data[[vname]])

    if(is.null(els)) {
      if(is.null(.labels)) {
        stop(vname, " cannot be coerced to likert as no labels are specified and .labels is NULL")
      }
    } else {
      if(is.null(.labels)) {
        .labels <- as_likerrt_labels(els)
      }
    }

    .labels <- prettify_labels(.labels, .data[[vname]], .complement = .complement)

    if(!is.null(.label))
      .data[[vname]] <- set_label(.data[[vname]], .value = .label)
    .data[[vname]] <- set_labels(.data[[vname]], .value = .labels)

    .data[[vname]] <- set_as_likert(.data[[vname]])

  }

  .data
}

set_as_likert <- function(x) {
  attr(x, which = "class") <- union(c("likerrt_likert", "haven_labelled"), class(x))
  x
}

prettify_labels <- function(ls, x = NULL, .complement) {
  if(.complement == TRUE) {

    if(is.null(x)) {
      stop("x cannot be NULL when .complement is TRUE")
    }

    comp <- c()
    for(v in unique(x)) {
      if(v %in% ls) {

      } else {
        comp[[as.character(v)]] <- v
      }
    }

    ls <- sort(c(ls, comp))
  }

  labs <- sapply(X = seq_along(ls), FUN = function(i) {
    if(names(ls)[i] == "") paste(ls[i]) else names(ls)[i]
  })

  names(ls) <- labs

  ls
}

#' IDEA: convert to `label<-` <- function(x, value) {}
as_likerrt_label <- function(x) {
  attr(x, which = "class") <- union("likerrt_label", attr(x, which = "class", exact = TRUE))
  x
}

as_likerrt_labels <- function(x) {
  attr(x, which = "class") <- union("likerrt_labels", attr(x, which = "class", exact = TRUE))
  x
}

