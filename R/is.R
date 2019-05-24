
is_haven_labelled <- function(x) {
  inherits_any(x, "haven_labelled")
}

#' @importFrom rlang inherits_any
is_likerrt <- function(x) {
  inherits_any(x, "likerrt_likert") & check_labels_validity(get_labels(x))
}

check_labels_validity <- function(labels) {
  length(labels) > 1 & length(names(labels)) == length(labels)
}
