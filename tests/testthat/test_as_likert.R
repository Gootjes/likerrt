
context("as_likert")

library(tidyverse)
library(rlang)


testthat::expect_equal(
  object = data.frame(A = sample(1:7, 100, replace = T)) %>%
    as_likert(A, .label = "Variable A", .labels = c("Do not agree" = 1, 2, 3, 4, 5, 6, "Does agree" = 7)) %>%
    pluck("A") %>% attr(which = "labels"), expected = c(`Do not agree` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6,
                                                        `Does agree` = 7))

testthat::expect_equal(
  object = structure(list(A = structure(c(1:7),
                                        label = "Variable A",
                                        labels = c(`Do not agree` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `Does agree` = 7
                                        ),
                                        class = c("haven_labelled"))),
                     row.names = c(NA, -7L),
                     class = "data.frame") %>%
    as_likert(A, .label = "Variable A", .labels = c("Do not agree" = 1, 2, 3, 4, 5, 6, "Does agree" = 7)) %>%
    pluck("A") %>% attr(which = "labels"), expected = c(`Do not agree` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6,
                                                        `Does agree` = 7))
