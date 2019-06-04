
context("rescale")

library(tidyverse)
library(rlang)

d <- data.frame(A = c(1, 2, 3), B = c(2,3,1), C= c(3,1,2)) %>%
  as_likert(A, B, C, .labels = c("A"=1, "B"=2, "C"=3))

testthat::expect_equal(object =
                         d %>%
                         likert_rescale(A, B, C, .min = 1, .max = 7),
                       expected = structure(list(A = structure(c(1, 4, 7), labels = structure(c(A = 1,
                                                                                                B = 4, C = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                    "haven_labelled", "numeric")), B = structure(c(4, 7, 1), labels = structure(c(A = 1,
                                                                                                                                                                                                                                  B = 4, C = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                                                                                                                                                      "haven_labelled", "numeric")), C = structure(c(7, 1, 4), labels = structure(c(A = 1,
                                                                                                                                                                                                                                                                                                                                                                    B = 4, C = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                                                                                                                                                                                                                                                                                        "haven_labelled", "numeric"))), row.names = c(NA, -3L), class = "data.frame"))



d[2,"A"] <- NA

testthat::expect_equal(object =
                         d %>%
                         likert_rescale(A, B, C, .min = 1, .max = 7),
                       expected = structure(list(A = structure(c(1, NA, 7), labels = structure(c(A = 1,
                                                                                                 B = 4, C = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                     "haven_labelled", "numeric")),
                                                 B = structure(c(4, 7, 1), labels = structure(c(A = 1, B = 4,
                                                                                                C = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                             "haven_labelled", "numeric")),
                                                 C = structure(c(7, 1, 4), labels = structure(c(A = 1, B = 4,
                                                                                                C = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                             "haven_labelled", "numeric"))), row.names = c(NA, -3L), class = "data.frame"))

a <- structure(list(A = structure(c(1:7),
                                  label = "Variable A",
                                  labels = c(`Do not agree` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `Does agree` = 7
                                  ),
                                  class = c("haven_labelled"))),
               row.names = c(NA, -7L),
               class = "data.frame") %>%
  likert_rescale(A, .min = 1, .max = 3) %>% pluck("A") %>% round(2)

b <- structure(c(1, 1.33, 1.67, 2, 2.33, 2.67, 3), label = structure("Variable A (recoded)", class = "likerrt_label"), labels = structure(c(`Do not agree` = 1,
                                                                                                                                            `2` = 1.33333333333333, `3` = 1.66666666666667, `4` = 2, `5` = 2.33333333333333,
                                                                                                                                            `6` = 2.66666666666667, `Does agree` = 3), class = "likerrt_labels"), class = "haven_labelled")


testthat::expect_equal(object = get_label(a), expected = get_label(b))
testthat::expect_equal(object = a, expected = b)

