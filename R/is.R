
is_haven_labelled <- function(a) {
  "haven_labelled" %in% a$class & "labels" %in% names(a)
}

is_likerrt <- function(a) {
  "likerrt_likert" %in% a$class & "labels" %in% names(a)
}

