context("rescale")

library(tidyverse)
library(rlang)
library(haven)

n1 <- 100
values1 <- 1:10
gen <- function(values, n) sample(values, n, replace=TRUE, prob=rbeta(length(values), 1, 1))
d1 <- data.frame(A = gen(values1, n1), B = gen(values1, n1), C = gen(values1, n1)) %>%
  as_likert(A, B, C, .labels = c("lowest" = 1, 2, 3, 4, 5, 6, 7, 8, 9, "highest"=10))

i_na <- sort(sample(1:nrow(d1), size = nrow(d1)/4, replace = FALSE))

d2 <- d1
d2$A[i_na] <- NA


a <- d1 %>% likert_rescale(A, B, C, .min = 0, .max = 1) %>% zap_labels()
b <- d1 %>% mutate_all(~ (. - 1)/9) %>% zap_labels()

testthat::expect_equal(a, b)


a <- d1 %>% likert_rescale(A, B, C, .min = 1, .max = 0) %>% zap_labels()
b <- d1 %>% mutate_all(~ 1-((. - 1)/9)) %>% zap_labels()

testthat::expect_equal(a, b)


a <- d1 %>% likert_rescale(A, .min = 1, .max = -1, .suffix = "rec") %>% zap_labels()
b <- d1 %>% mutate(Arec = 1+(((A - 1)/9)*-1)*2) %>% zap_labels()

testthat::expect_equal(a, b)


a <- d1 %>% likert_rescale(A, B, C, .min = 0, .max = 1)
a_labels <- a %>% get_labels()

testthat::expect_equal(a_labels$A %>% unclass() %>% as.vector(), seq(from = 0, to = 1, length.out = 10))

testthat::expect_equal(a %>% as_factor(), d1 %>% as_factor())  # TODO: Is this desired?


a <- d2 %>% likert_rescale(A, B, C, .min = 1, .max = -1) %>% zap_labels()
b <- d2 %>% mutate_all(~ 1+(((. - 1)/9)*-1)*2) %>% zap_labels()

testthat::expect_equal(a, b)


a <- d2 %>% likert_rescale(A, .min = 1, .max = -1) %>% likert_scale(A, B, .name = "C", .assumptions = c('none'))
b <- d2 %>% mutate(A = 1+(((A - 1)/9)*-1)*2) %>% likert_scale(A, B, .name = "C")

testthat::expect_equal(a %>% zap_labels(), b %>% zap_labels())



