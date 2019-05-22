
#' @importFrom haven as_factor
#' @export
haven::as_factor

#' @export
print.likerrt_labels <- function(x, ...) {
  d <- data.frame(value = unclass(x), label = names(x), stringsAsFactors = FALSE)
  print(d, row.names = FALSE)
}

#' @export
print.likerrt_label <- function(x, ...) {
  cat(" label:\n")
  cat("   ", x, sep = "")
  cat("\n")
}
