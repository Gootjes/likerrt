
context("likert_recode")

library(tidyverse)
library(rlang)


testthat::expect_equal(
  object = data.frame(A = sample(1:7, 100, replace = T)) %>%
    as_likert(A, .label = "Variable A", .labels = c("Do not agree" = 1, 2, 3, 4, 5, 6, "Does agree" = 7)) %>%
    likert_recode(A, .spec = c(`1`=7, `2`=6, `3`=5, `4`=4, `5`=3, `6`=2, `7`=1)) %>%
    pluck("A") %>% attr(which = "labels"), expected = c(`Do not agree` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2,
                                                        `Does agree` = 1))


testthat::expect_equal(
  object = data.frame(A = c(NA, sample(1:7, 99, replace = T))) %>%
    as_likert(A, .label = "Variable A", .labels = c("Do not agree" = 1, 2, 3, 4, 5, 6, "Does agree" = 7)) %>%
    likert_recode(A, .spec = c(`1`=7, `2`=6, `3`=5, `4`=4, `5`=3, `6`=2, `7`=1)) %>%
    pluck("A") %>% attr(which = "labels"), expected = c(`Do not agree` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2,
                                                        `Does agree` = 1))
