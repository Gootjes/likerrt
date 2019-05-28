
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
