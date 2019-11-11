


context("likert_reverse_code")

library(tidyverse)
library(rlang)


testthat::expect_equal(
  object = data.frame(A = sample(1:7, 100, replace = T)) %>%
    as_likert(
      A,
      .label = "Variable A",
      .labels = c("Do not agree" = 1, 2, 3, 4, 5, 6, "Does agree" = 7)
    ) %>%
    likert_reverse_code(A) %>%
    pluck("A") %>% attr(which = "labels"),
  expected = c(
    `Do not agree` = 7,
    `2` = 6,
    `3` = 5,
    `4` = 4,
    `5` = 3,
    `6` = 2,
    `Does agree` = 1
  )
)

testthat::expect_equal(
  object = data.frame(A = rep(c(1:7, NA), times = 2)) %>%
    as_likert(
      A,
      .label = "Variable A",
      .labels = c("Do not agree" = 1, 2, 3, 4, 5, 6, "Does agree" = 7)
    ) %>%
    likert_reverse_code(A) %>%
    pluck("A") %>% is.na(),
  expected = c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE
  )
)

testthat::expect_equal(
  object = data.frame(A = rep(c(1:7, NA), times = 2)) %>%
    as_likert(
      A,
      .label = "Variable A",
      .labels = c("Do not agree" = 1, 2, 3, 4, 5, 6, "Does agree" = 7)
    ) %>%
    likert_reverse_code(A) %>%
    pluck("A") %>% attr(which = "labels"),
  expected = c(
    `Do not agree` = 7,
    `2` = 6,
    `3` = 5,
    `4` = 4,
    `5` = 3,
    `6` = 2,
    `Does agree` = 1
  )
)
