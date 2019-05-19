
context("as_likert")

library(tidyverse)
library(rlang)

testthat::expect_equal(object =
                         data.frame(A = c(1, NA, 3), B = 1:3, C= 3:1) %>%
                         as_likert(A, B, C, .labels = c("A"=1, "B"=2, "C"=3)) %>%
                         likert_reverse_code(C, .suffix = "r") %>%
                         likert_scale(A, B, Cr, .name = "D", .label = "D measure", .drop = TRUE),
                       expected = structure(list(C = structure(3:1,
                                                               labels = c(A = 1, B = 2, C = 3),
                                                               class = c("likerrt.likert", "haven_labelled")),
                                                 D = structure(c(1, NA, 3), label = "D measure")),
                                            row.names = c(NA, -3L), class = "data.frame"))
