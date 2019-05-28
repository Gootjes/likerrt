
context("rescale")

library(tidyverse)
library(rlang)

d <- data.frame(A = c(1, 2, 3), B = c(2,3,1), C= c(3,1,2)) %>%
  as_likert(A, B, C, .labels = c("A"=1, "B"=2, "C"=3))

testthat::expect_equal(object =
                         d %>%
                         likert_rescale(A, B, C, .min = 1, .max = 7),
                       expected = structure(list(A = c(1, 4, 7), B = c(4, 7, 1), C = c(7, 1, 4)), row.names = c(NA, -3L), class = "data.frame"))



d[2,"A"] <- NA

testthat::expect_equal(object =
                         d %>%
                         likert_rescale(A, B, C, .min = 1, .max = 7),
                       expected = structure(list(A = c(1, NA, 7), B = c(4, 7, 1), C = c(7, 1, 4)), row.names = c(NA, -3L), class = "data.frame"))
