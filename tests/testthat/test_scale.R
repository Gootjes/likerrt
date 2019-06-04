
context("scale")

library(tidyverse)
library(rlang)

testthat::expect_equal(object =
                         data.frame(A = c(1, NA, 3), B = 1:3, C= 3:1) %>%
                         as_likert(A, B, C, .labels = c("A"=1, "B"=2, "C"=3)) %>%
                         likert_reverse_code(C, .suffix = "r") %>%
                         likert_scale(A, B, Cr, .name = "D", .label = "D measure", .drop = TRUE),
                       expected = structure(c(1, NA, 3), label = "D measure"))

testthat::expect_equal(object =
                         data.frame(A = c(1, NA, 3), B = 1:3, C= 3:1) %>%
                         as_likert(A, B, C, .labels = c("A"=1, "B"=2, "C"=3)) %>%
                         likert_reverse_code(C, .suffix = "r") %>%
                         likert_scale(A, B, Cr, .name = "D", .label = "D measure", .drop = FALSE),
                       expected = structure(list(A = structure(c(1, NA, 3), labels = structure(c(A = 1, B = 2, C = 3), class = "likerrt_labels"), class = c("likerrt_likert", "haven_labelled", "numeric")), B = structure(1:3, labels = structure(c(A = 1, B = 2, C = 3), class = "likerrt_labels"), class = c("likerrt_likert", "haven_labelled", "integer")), C = structure(3:1, labels = structure(c(A = 1, B = 2, C = 3), class = "likerrt_labels"), class = c("likerrt_likert", "haven_labelled", "integer")), Cr = structure(1:3, labels = structure(3:1, .Names = c("A", "B", "C")), class = c("likerrt_likert", "haven_labelled", "integer")), D = structure(c(1, NA, 3), label = "D measure")), row.names = c(NA, -3L), class = "data.frame"))

testthat::expect_equal(object =
                         data.frame(A = c(1, NA, 3), B = 1:3, C= 3:1) %>%
                         as_likert(A, B, C, .labels = c("A"=1, "B"=2, "C"=3)) %>%
                         likert_reverse_code(C) %>%
                         likert_scale(A, B, C, .name = "D", .label = "D measure", .drop = FALSE),
                       expected =
                         structure(list(A = structure(c(1, NA, 3), labels = structure(c(A = 1, B = 2, C = 3), class = "likerrt_labels"), class = c("likerrt_likert", "haven_labelled", "numeric")), B = structure(1:3, labels = structure(c(A = 1, B = 2, C = 3), class = "likerrt_labels"), class = c("likerrt_likert", "haven_labelled", "integer")), C = structure(1:3, labels = structure(3:1, .Names = c("A", "B", "C")), class = c("likerrt_likert", "haven_labelled", "integer")), D = structure(c(1, NA, 3), label = "D measure")), row.names = c(NA, -3L), class = "data.frame"))

testthat::expect_equal(object =
                         data.frame(A = c(1, NA, 3), B = 1:3, C= 3:1) %>%
                         as_likert(A, B, C, .labels = c("A"=1, "B"=2, "C"=3)) %>%
                         likert_reverse_code(C) %>%
                         likert_scale(A, B, C, .name = "D", .label = "D measure", .drop = FALSE, na.rm = TRUE),
                       expected =
                         structure(list(A = structure(c(1, NA, 3), labels = structure(c(A = 1, B = 2, C = 3), class = "likerrt_labels"), class = c("likerrt_likert", "haven_labelled", "numeric")), B = structure(1:3, labels = structure(c(A = 1, B = 2, C = 3), class = "likerrt_labels"), class = c("likerrt_likert", "haven_labelled", "integer")), C = structure(1:3, labels = structure(3:1, .Names = c("A", "B", "C")), class = c("likerrt_likert", "haven_labelled", "integer")), D = structure(c(1, 2, 3), label = "D measure")), row.names = c(NA, -3L), class = "data.frame"))


testthat::expect_error(object =
                         data.frame(A = c(1:7, 1:3), B = 1:10) %>%
                         as_likert(A, .labels = c("Disagree" = 1, "Agree" = 7)) %>%
                         as_likert(B, .labels = c("Not at all" = 1, "A lot" = 10)) %>%
                         likert_scale(A, B, .name = "C", .label = "Scale of A and B"))

testthat::expect_error(object =
                         data.frame(A = c(1:7, 1:3), B = 1:10) %>%
                         as_likert(A, .labels = c("Disagree" = 1, "Agree" = 7)) %>%
                         as_likert(B, .labels = c("Not at all" = 1, "A lot" = 10)) %>%
                         likert_scale(A, B, .name = "C", .label = "Scale of A and B", .strictness = "range"))

testthat::expect_equal(object =
                         data.frame(A = c(1:7, 1:3), B = 1:10) %>%
                         as_likert(A, .labels = c("Disagree" = 1, "Agree" = 7)) %>%
                         as_likert(B, .labels = c("Not at all" = 1, "A lot" = 10)) %>%
                         likert_scale(A, B, .name = "C", .label = "Scale of A and B", .strictness = "none"),
                       expected = structure(list(A = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L,
                                                                 2L, 3L), labels = structure(c(Disagree = 1, `2` = 2, `3` = 3,
                                                                                               `4` = 4, `5` = 5, `6` = 6, Agree = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                                           "haven_labelled", "integer")), B = structure(1:10, labels = structure(c(`Not at all` = 1,
                                                                                                                                                                                                                                                   `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7, `8` = 8,
                                                                                                                                                                                                                                                   `9` = 9, `A lot` = 10), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                                                                                                                                                                                "haven_labelled", "integer")), C = structure(c(1, 2, 3, 4, 5,
                                                                                                                                                                                                                                                                                                                                                               6, 7, 4.5, 5.5, 6.5), label = "Scale of A and B")), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                 -10L), class = "data.frame"))

testthat::expect_equal(object =
                         data.frame(A = c(1:7, 1:3), B = 1:10) %>%
                         as_likert(A, .labels = c("Disagree" = 1, "Agree" = 7)) %>%
                         as_likert(B, .labels = c("Not at all" = 1, "A lot" = 10)) %>%
                         likert_rescale(B, .min = 1, .max = 7) %>%
                         likert_scale(A, B, .name = "C", .label = "Scale of A and B", .strictness = "range"),
                       expected = structure(list(A = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L,
                                                                 2L, 3L), labels = structure(c(Disagree = 1, `2` = 2, `3` = 3,
                                                                                               `4` = 4, `5` = 5, `6` = 6, Agree = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                                           "haven_labelled", "integer")), B = structure(c(1, 1.66666666666667,
                                                                                                                                                                                                                          2.33333333333333, 3, 3.66666666666667, 4.33333333333333, 5, 5.66666666666667,
                                                                                                                                                                                                                          6.33333333333333, 7), labels = structure(c(`Not at all` = 1,
                                                                                                                                                                                                                                                                     `2` = 1.66666666666667, `3` = 2.33333333333333, `4` = 3, `5` = 3.66666666666667,
                                                                                                                                                                                                                                                                     `6` = 4.33333333333333, `7` = 5, `8` = 5.66666666666667, `9` = 6.33333333333333,
                                                                                                                                                                                                                                                                     `A lot` = 7), class = "likerrt_labels"), class = c("likerrt_likert",
                                                                                                                                                                                                                                                                                                                        "haven_labelled", "integer")), C = structure(c(1, 1.83333333333333,
                                                                                                                                                                                                                                                                                                                                                                       2.66666666666667, 3.5, 4.33333333333333, 5.16666666666667, 6,
                                                                                                                                                                                                                                                                                                                                                                       3.33333333333333, 4.16666666666667, 5), label = "Scale of A and B")), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                           -10L), class = "data.frame"))
