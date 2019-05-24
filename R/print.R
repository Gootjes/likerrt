
#' @importFrom haven as_factor
#' @export
haven::as_factor

#' @export
print.likerrt_labels <- function(x, ...) {
  nx <- names(x)
  if(is.null(nx)) {
    nx <- rep(NA, length(x))
  }
  d <- data.frame(value = unclass(x), label = nx, stringsAsFactors = FALSE)
  print(d, row.names = FALSE)
}

#' @export
print.likerrt_label <- function(x, ...) {
  cat(" label:\n")
  cat("   ", x, sep = "")
  cat("\n")
}
